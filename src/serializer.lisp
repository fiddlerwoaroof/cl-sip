(in-package :cl-sip)

;;;; Message -> octets

(defun start-line (message)
  (etypecase message
    (request (format nil "~a ~a SIP/2.0" (request-method message)
                     (uri-string (request-uri message))))
    (response (format nil "SIP/2.0 ~d ~a" (response-status message)
                      (response-reason message)))))

(defun serialize-message (message)
  "Serialize MESSAGE to an octet vector.  Content-Length is always computed
from the body and written last."
  (let* ((body (message-body message))
         (crlf (coerce '(#\Return #\Newline) 'string))
         (head (with-output-to-string (out)
                 (write-string (start-line message) out)
                 (write-string crlf out)
                 (loop for (name . value) in (message-headers message)
                       unless (string= name "Content-Length")
                         do (format out "~a: ~a~a" name value crlf))
                 (format out "Content-Length: ~d~a~a" (length body) crlf crlf)))
         (head-octets (string-to-octets head))
         (result (make-array (+ (length head-octets) (length body))
                             :element-type '(unsigned-byte 8))))
    (replace result head-octets)
    (when body (replace result body :start1 (length head-octets)))
    result))

(defun message-string (message)
  "The message as text, for logging and tests."
  (octets-to-string (serialize-message message)))
