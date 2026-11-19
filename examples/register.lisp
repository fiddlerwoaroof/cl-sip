(in-package :cl-sip.examples)

;;;; Register with a SIP registrar and stay registered.
;;;;
;;;;   (register-example "sip:alice@example.com" "alice" "secret")
;;;;
;;;; Returns the user agent and the registration; call (sip:unregister
;;;; registration) and (sip:stop ua) when done.

(defun register-example (aor username password
                         &key registrar (host "0.0.0.0") (port 5060) (expires 3600))
  (let* ((ua (sip:make-user-agent
              :aor aor
              :transports `((:udp :host ,host :port ,port))
              ;; A NIL realm offers these credentials to whichever realm asks.
              :credentials `((nil ,username ,password))))
         (registration
           (progn
             (sip:start ua)
             (sip:register
              ua :registrar registrar :expires expires
              :on-event (lambda (registration event &key expires status reason bindings
                                 &allow-other-keys)
                          (declare (ignore registration))
                          (ecase event
                            (:registered
                             (format t "~&registered for ~d s; bindings:~{ ~a~}~%" expires
                                     (mapcar (lambda (b)
                                               (sip:uri-string (sip:name-addr-uri b)))
                                             bindings)))
                            (:unregistered (format t "~&unregistered~%"))
                            (:bindings (format t "~&~d binding(s)~%" (length bindings)))
                            (:failed (format t "~&registration failed: ~a~%"
                                             (or status reason)))))))))
    (values ua registration)))
