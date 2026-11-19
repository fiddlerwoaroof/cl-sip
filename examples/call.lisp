(in-package :cl-sip.examples)

;;;; Place a call, send one second of a 440 Hz tone as G.711 mu-law plus a
;;;; DTMF digit, count what comes back, and hang up.
;;;;
;;;;   (call-example "sip:echo@127.0.0.1:5060")

(defun tone-frame (frame-number &key (frequency 440) (samples 160) (rate 8000))
  (let ((frame (make-array samples :element-type '(signed-byte 16))))
    (dotimes (i samples frame)
      (setf (aref frame i)
            (round (* 8000 (sin (/ (* 2 pi frequency (+ i (* frame-number samples)))
                                   rate))))))))

(defun call-example (target &key (host "127.0.0.1") (port 0) (aor "sip:caller@localhost"))
  "Returns the call's final cause and the number of RTP packets received."
  (let* ((ua (sip:make-user-agent :aor aor :transports `((:udp :host ,host :port ,port))))
         (session (rtp:rtp-session-start (rtp:make-rtp-session :local-host host)))
         (call nil))
    (sip:start ua)
    (unwind-protect
         (progn
           (setf call
                 (sip:invite
                  ua target
                  :sdp (sdp:sdp-string
                        (sdp:make-audio-offer
                         :address (sip:transport-advertised-host
                                   (first (sip:stack-transports (sip:ua-stack ua))))
                         :port (rtp:rtp-session-local-port session)))
                  :on-event (lambda (call event &key status cause &allow-other-keys)
                              (declare (ignore call))
                              (format t "~&call event: ~a~@[ ~a~]~%" event (or status cause)))))
           (cond
             ((sip:wait-until (lambda () (member (sip:call-state call)
                                                 '(:established :terminated)))
                              :timeout 35)
              (when (eq (sip:call-state call) :established)
                ;; We made the offer, so the answer says where to send media.
                (multiple-value-bind (codecs direction remote-host remote-port)
                    (sdp:negotiated-audio (sdp:parse-sdp (sip:call-remote-sdp call)))
                  (declare (ignore direction))
                  (setf (rtp:rtp-session-remote-host session) remote-host
                        (rtp:rtp-session-remote-port session) remote-port
                        (rtp:rtp-session-payload-type session)
                        (sdp:codec-payload-type (first codecs)))
                  (let ((encode (if (string-equal (sdp:codec-name (first codecs)) "PCMA")
                                    #'rtp:alaw-encode
                                    #'rtp:ulaw-encode))
                        (dtmf (find "telephone-event" codecs
                                    :key #'sdp:codec-name :test #'string-equal)))
                    (dotimes (i 50)
                      (rtp:rtp-send session (funcall encode (tone-frame i)) :marker (zerop i))
                      (sleep 0.02))
                    (when dtmf
                      (rtp:rtp-send-dtmf session #\1
                                         :payload-type (sdp:codec-payload-type dtmf)))
                    (sleep 0.2)))
                (sip:hangup call)
                (sip:wait-until (lambda () (eq (sip:call-state call) :terminated)))))
             (t (sip:hangup call)))
           (values (sip:call-cause call) (rtp:rtp-session-packets-received session)))
      (rtp:rtp-session-stop session)
      (sip:stop ua))))
