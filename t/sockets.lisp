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
