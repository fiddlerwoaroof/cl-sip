(in-package :cl-sip.examples)

;;;; A user agent server that answers every call and echoes the caller's
;;;; audio back.
;;;;
;;;;   (defvar *uas* (start-echo-uas :port 5060))
;;;;   ... call sip:echo@<this host>:5060 ...
;;;;   (stop-echo-uas *uas*)

(defun start-echo-uas (&key (host "127.0.0.1") (port 5060) (aor "sip:echo@localhost"))
  (let ((ua (sip:make-user-agent :aor aor :transports `((:udp :host ,host :port ,port)))))
    (setf (sip:ua-on-incoming-call ua)
          (lambda (call)
            (handler-case
                (let* ((offer (sdp:parse-sdp (sip:call-remote-sdp call)))
                       (session (rtp:rtp-session-start
                                 (rtp:make-rtp-session
                                  :local-host host
                                  ;; Send every packet straight back.
                                  :on-packet (lambda (session packet)
                                               (rtp:rtp-send
                                                session (rtp:rtp-packet-payload packet)
                                                :payload-type (rtp:rtp-packet-payload-type packet)
                                                :marker (rtp:rtp-packet-marker packet))))))
                       (address (sip:transport-advertised-host
                                 (first (sip:stack-transports (sip:ua-stack ua))))))
                  (multiple-value-bind (answer accepted)
                      (sdp:make-answer offer :address address
                                             :port (rtp:rtp-session-local-port session))
                    (cond
                      (accepted
                       ;; In an answer we made, media goes to the offer's address.
                       (multiple-value-bind (codecs direction remote-host remote-port)
                           (sdp:negotiated-audio answer offer)
                         (declare (ignore codecs direction))
                         (setf (rtp:rtp-session-remote-host session) remote-host
                               (rtp:rtp-session-remote-port session) remote-port))
                       (setf (sip:call-user-data call) session
                             (sip:call-on-event call)
                             (lambda (call event &key &allow-other-keys)
                               (when (eq event :terminated)
                                 ;; Stop the RTP thread off the event loop.
                                 (let ((session (sip:call-user-data call)))
                                   (bt:make-thread
                                    (lambda () (rtp:rtp-session-stop session)))))))
                       (sip:ring call)
                       (sip:answer call :sdp (sdp:sdp-string answer)))
                      (t
                       (rtp:rtp-session-stop session)
                       (sip:reject call 488)))))
              ;; No offer, or one we cannot read.
              (error () (sip:reject call 488)))))
    (sip:start ua)))

(defun stop-echo-uas (ua)
  (sip:stop ua))
