(in-package :cl-sip.test)

;;;; The same flows over real sockets on the loopback interface, with the
;;;; event loop running in its own thread.

(def-suite sockets :in cl-sip)
(in-suite sockets)

(defun socket-call-flow (transport-spec target-format)
  (let* ((bob-call nil)
         (alice (sip:make-user-agent :aor "sip:alice@127.0.0.1"
                                     :transports (list (append transport-spec '(:port 0)))))
         (bob (sip:make-user-agent :aor "sip:bob@127.0.0.1"
                                   :transports (list (append transport-spec '(:port 0)))
                                   :on-incoming-call
                                   (lambda (call)
                                     (setf bob-call call)
                                     (sip:ring call)
                                     (sip:answer call :sdp *answer*)))))
    (sip:start alice)
    (sip:start bob)
    (unwind-protect
         (let* ((port (sip:transport-port (first (sip:stack-transports (sip:ua-stack bob)))))
                (call (sip:invite alice (format nil target-format port) :sdp *offer*)))
           (is-true (sip:wait-until (lambda () (eq :established (sip:call-state call)))))
           (is-true (sip:wait-until (lambda () (and bob-call
                                                    (eq :established (sip:call-state bob-call))))))
           (is (string= *answer* (sip:call-remote-sdp call)))
           (is (string= *offer* (sip:call-remote-sdp bob-call)))
           (let (response)
             (sip:options alice (format nil target-format port)
                          :on-response (lambda (r) (setf response r)))
             (is-true (sip:wait-until (lambda () response)))
             (is (= 200 (sip:response-status response))))
           (sip:hangup call)
           (is-true (sip:wait-until (lambda () (eq :terminated (sip:call-state call)))))
           (is-true (sip:wait-until (lambda () (eq :terminated (sip:call-state bob-call)))))
           (is (eq :local-hangup (sip:call-cause call)))
           (is (eq :remote-hangup (sip:call-cause bob-call))))
      (sip:stop alice)
      (sip:stop bob))))

(test call-over-udp
  (socket-call-flow '(:udp :host "127.0.0.1") "sip:bob@127.0.0.1:~d"))

(test call-over-tcp
  (socket-call-flow '(:tcp :host "127.0.0.1") "sip:bob@127.0.0.1:~d;transport=tcp"))

(test register-over-udp
  (let* ((registrar (sip:make-registrar :realm "test" :users '(("carol" . "secret"))))
         (server (sip:make-user-agent :aor "sip:registrar@127.0.0.1"
                                      :transports '((:udp :host "127.0.0.1" :port 0))
                                      :registrar registrar))
         (carol nil))
    (sip:start server)
    (unwind-protect
         (let ((port (sip:transport-port (first (sip:stack-transports (sip:ua-stack server))))))
           (setf carol (sip:make-user-agent :aor "sip:carol@127.0.0.1"
                                            :transports '((:udp :host "127.0.0.1" :port 0))
                                            :credentials '(("test" "carol" "secret"))))
           (sip:start carol)
           (let ((registration (sip:register carol
                                             :registrar (format nil "sip:127.0.0.1:~d" port)
                                             :expires 300)))
             (is-true (sip:wait-until
                       (lambda () (eq :registered (sip:registration-state registration)))))
             (is (= 300 (sip:registration-expires registration)))
             (sip:unregister registration)
             (is-true (sip:wait-until
                       (lambda () (eq :unregistered (sip:registration-state registration)))))))
      (when carol (sip:stop carol))
      (sip:stop server))))

(test rtp-session-loopback
  (let* ((received '())
         (receiver (rtp:rtp-session-start
                    (rtp:make-rtp-session :on-packet (lambda (session packet)
                                                       (declare (ignore session))
                                                       (push packet received)))))
         (sender (rtp:rtp-session-start
                  (rtp:make-rtp-session :remote-host "127.0.0.1"
                                        :remote-port (rtp:rtp-session-local-port receiver)
                                        :payload-type 0))))
    (unwind-protect
         (let ((frame (rtp:ulaw-encode (make-array 160 :element-type '(signed-byte 16)
                                                       :initial-element 1000))))
           (rtp:rtp-send sender frame :marker t)
           (rtp:rtp-send sender frame)
           (rtp:rtp-send-dtmf sender #\5)
           (is-true (sip:wait-until (lambda () (= 9 (length received)))))
           (let ((packets (sort (copy-list received) #'<
                                :key (lambda (p)
                                       (mod (- (rtp:rtp-packet-sequence p)
                                               (rtp:rtp-packet-sequence (first (last received))))
                                            65536)))))
             (is (= 160 (length (rtp:rtp-packet-payload (first packets)))))
             (is-true (rtp:rtp-packet-marker (first packets)))
             ;; Timestamps advance by the number of samples sent.
             (is (= 160 (mod (- (rtp:rtp-packet-timestamp (second packets))
                                (rtp:rtp-packet-timestamp (first packets)))
                             (expt 2 32))))
             (is (= (rtp:rtp-session-ssrc sender) (rtp:rtp-session-remote-ssrc receiver)))
             (is (= 5 (rtp:telephone-event-event
                       (rtp:decode-telephone-event
                        (rtp:rtp-packet-payload (first (last packets)))))))
             (is (= 9 (rtp:rtp-session-packets-sent sender)
                    (rtp:rtp-session-packets-received receiver)))))
      (rtp:rtp-session-stop sender)
      (rtp:rtp-session-stop receiver))))

(defun start-stun-server (reported-host reported-port)
  "A one-socket STUN server on loopback that tells every client it appears
as REPORTED-HOST:REPORTED-PORT.  Returns its port and a function to stop it."
  (let* ((socket (usocket:socket-connect nil nil :protocol :datagram
                                                 :local-host "127.0.0.1" :local-port 0))
         (running t)
         (thread
           (bt:make-thread
            (lambda ()
              (let ((buffer (make-array 512 :element-type '(unsigned-byte 8))))
                (loop while running
                      do (when (usocket:wait-for-input socket :timeout 1/10 :ready-only t)
                           (multiple-value-bind (data size host port)
                               (usocket:socket-receive socket buffer (length buffer))
                             (declare (ignore data))
                             (when (and size (= size 20))
                               (let ((reply (make-array 32 :element-type '(unsigned-byte 8)
                                                           :initial-element 0))
                                     (address (sip:parse-ipv4 reported-host)))
                                 (replace reply buffer :end2 20)
                                 (setf (aref reply 0) 1 (aref reply 1) 1 ; success response
                                       (aref reply 3) 12
                                       (aref reply 21) #x20 (aref reply 23) 8
                                       (aref reply 25) 1)
                                 (let ((xport (logxor reported-port #x2112))
                                       (xaddr (logxor address #x2112A442)))
                                   (setf (aref reply 26) (ldb (byte 8 8) xport)
                                         (aref reply 27) (ldb (byte 8 0) xport))
                                   (dotimes (i 4)
                                     (setf (aref reply (+ 28 i))
                                           (ldb (byte 8 (* 8 (- 3 i))) xaddr))))
                                 (usocket:socket-send socket reply 32
                                                      :host host :port port)))))))))))
    (values (usocket:get-local-port socket)
            (lambda ()
              (setf running nil)
              (bt:join-thread thread)
              (usocket:socket-close socket)))))

(test rtp-session-port-range-and-stun
  (multiple-value-bind (stun-port stop-stun) (start-stun-server "203.0.113.7" 40123)
    (let (first second)
      (unwind-protect
           (progn
             (setf first (rtp:rtp-session-start
                          (rtp:make-rtp-session :port-range '(47310 . 47312)
                                                :stun (cons "127.0.0.1" stun-port))))
             ;; Ports come from the range, and the next session takes the next one.
             (is (= 47310 (rtp:rtp-session-local-port first)))
             (is (string= "203.0.113.7" (rtp:rtp-session-public-host first)))
             (is (= 40123 (rtp:rtp-session-public-port first)))
             (setf second (rtp:rtp-session-start
                           (rtp:make-rtp-session :port-range '(47310 . 47312))))
             (is (= 47311 (rtp:rtp-session-local-port second)))
             (is (null (rtp:rtp-session-public-host second)))
             ;; An unreachable STUN server just leaves the address unknown.
             (rtp:rtp-session-stop second)
             (setf second (rtp:rtp-session-start
                           (rtp:make-rtp-session :stun '("127.0.0.1" . 9))))
             (is (null (rtp:rtp-session-public-host second))))
        (when first (rtp:rtp-session-stop first))
        (when second (rtp:rtp-session-stop second))
        (funcall stop-stun)))))
