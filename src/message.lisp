(in-package :cl-sip)

;;;; SIP messages (RFC 3261 section 7)

(defclass message ()
  ((headers :initarg :headers :initform '() :accessor message-headers
            :documentation "Ordered list of (canonical-name . raw-value).")
   (body :initarg :body :initform nil :accessor message-body
         :documentation "Octet vector or NIL.")
   ;; Where the message came from; filled in by the transport on receipt.
   (transport :initform nil :accessor message-transport)
   (remote-host :initform nil :accessor message-remote-host)
   (remote-port :initform nil :accessor message-remote-port)
   (connection :initform nil :accessor message-connection)))

(defclass request (message)
  ((method :initarg :method :accessor request-method :type string)
   (uri :initarg :uri :accessor request-uri)
   (next-hop :initarg :next-hop :initform nil :accessor request-next-hop
             :documentation "URI to send to when it cannot be derived from the
headers: a request formed for a strict router goes to its Request-URI even
though a loose Route follows (section 8.1.2).")))

(defclass response (message)
  ((status :initarg :status :accessor response-status :type integer)
   (reason :initarg :reason :accessor response-reason :type string)))

(defmethod print-object ((m request) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~a ~a" (request-method m) (uri-string (request-uri m)))))

(defmethod print-object ((m response) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~d ~a" (response-status m) (response-reason m))))

(defun request-p (x) (typep x 'request))
(defun response-p (x) (typep x 'response))

(defparameter *reason-phrases*
  '((100 . "Trying") (180 . "Ringing") (181 . "Call Is Being Forwarded")
    (182 . "Queued") (183 . "Session Progress") (200 . "OK") (202 . "Accepted")
    (489 . "Bad Event")
    (300 . "Multiple Choices") (301 . "Moved Permanently")
    (302 . "Moved Temporarily") (305 . "Use Proxy") (380 . "Alternative Service")
    (400 . "Bad Request") (401 . "Unauthorized") (402 . "Payment Required")
    (403 . "Forbidden") (404 . "Not Found") (405 . "Method Not Allowed")
    (406 . "Not Acceptable") (407 . "Proxy Authentication Required")
    (408 . "Request Timeout") (410 . "Gone") (413 . "Request Entity Too Large")
    (414 . "Request-URI Too Long") (415 . "Unsupported Media Type")
    (416 . "Unsupported URI Scheme") (420 . "Bad Extension")
    (421 . "Extension Required") (423 . "Interval Too Brief")
    (480 . "Temporarily Unavailable") (481 . "Call/Transaction Does Not Exist")
    (482 . "Loop Detected") (483 . "Too Many Hops") (484 . "Address Incomplete")
    (485 . "Ambiguous") (486 . "Busy Here") (487 . "Request Terminated")
    (488 . "Not Acceptable Here") (491 . "Request Pending")
    (493 . "Undecipherable") (500 . "Server Internal Error")
    (501 . "Not Implemented") (502 . "Bad Gateway") (503 . "Service Unavailable")
    (504 . "Server Time-out") (505 . "Version Not Supported")
    (513 . "Message Too Large") (600 . "Busy Everywhere") (603 . "Decline")
    (604 . "Does Not Exist Anywhere") (606 . "Not Acceptable")))

(defun reason-phrase (status)
  (or (cdr (assoc status *reason-phrases*))
      (case (floor status 100)
        (1 "Informational") (2 "Success") (3 "Redirection")
        (4 "Client Error") (5 "Server Error") (6 "Global Failure")
        (t "Unknown"))))

;;;; Raw header access

(defun header-values (message name)
  "All raw values of header NAME, in message order."
  (let ((name (canonical-header-name name)))
    (loop for (n . v) in (message-headers message)
          when (string-equal n name) collect v)))

(defun header (message name)
  "First raw value of header NAME, or NIL."
  (let ((name (canonical-header-name name)))
    (cdr (assoc name (message-headers message) :test #'string-equal))))

(defun header-string (value)
  (etypecase value
    (string value)
    (integer (princ-to-string value))
    (name-addr (format-name-addr value))
    (via (format-via value))
    (cseq (format-cseq value))
    ((or sip-uri generic-uri) (format nil "<~a>" (uri-string value)))))

(defun remove-headers (message name)
  (let ((name (canonical-header-name name)))
    (setf (message-headers message)
          (remove name (message-headers message) :key #'car :test #'string-equal))
    message))

(defun add-header (message name value)
  "Append a header line."
  (setf (message-headers message)
        (append (message-headers message)
                (list (cons (canonical-header-name name) (header-string value)))))
  message)

(defun prepend-header (message name value)
  "Insert a header line before any existing NAME header (or at the top)."
  (let* ((name (canonical-header-name name))
         (entry (cons name (header-string value)))
         (headers (message-headers message))
         (pos (position name headers :key #'car :test #'string-equal)))
    (setf (message-headers message)
          (if pos
              (append (subseq headers 0 pos) (list entry) (nthcdr pos headers))
              (cons entry headers)))
    message))

(defun (setf header) (value message name)
  "Replace all NAME headers by a single one (in the position of the first),
or remove them when VALUE is NIL."
  (let* ((name (canonical-header-name name))
         (headers (message-headers message))
         (pos (position name headers :key #'car :test #'string-equal)))
    (cond ((null value) (remove-headers message name))
          (pos
           (setf (message-headers message)
                 (append (subseq headers 0 pos)
                         (list (cons name (header-string value)))
                         (remove name (nthcdr (1+ pos) headers)
                                 :key #'car :test #'string-equal))))
          (t (add-header message name value)))
    value))

(defun remove-first-header (message name)
  (let* ((name (canonical-header-name name))
         (headers (message-headers message))
         (pos (position name headers :key #'car :test #'string-equal)))
    (when pos
      (setf (message-headers message)
            (append (subseq headers 0 pos) (nthcdr (1+ pos) headers))))
    message))

;;;; Typed views

(defun msg-vias (message)
  (mapcar #'parse-via (header-values message "Via")))

(defun msg-top-via (message)
  (let ((raw (header message "Via")))
    (and raw (parse-via raw))))

(defun msg-from (message)
  (let ((raw (header message "From")))
    (and raw (parse-name-addr raw))))

(defun msg-to (message)
  (let ((raw (header message "To")))
    (and raw (parse-name-addr raw))))

(defun msg-from-tag (message)
  (let ((from (msg-from message)))
    (and from (name-addr-tag from))))

(defun msg-to-tag (message)
  (let ((to (msg-to message)))
    (and to (name-addr-tag to))))

(defun msg-call-id (message)
  (header message "Call-ID"))

(defun msg-cseq (message)
  (let ((raw (header message "CSeq")))
    (and raw (parse-cseq raw))))

(defun msg-contacts (message)
  "List of NAME-ADDRs; the wildcard contact appears as the keyword :STAR."
  (loop for raw in (header-values message "Contact")
        collect (if (string= (trim-ws raw) "*") :star (parse-name-addr raw))))

(defun msg-contact-uri (message)
  "URI of the first Contact, or NIL."
  (let ((contact (first (msg-contacts message))))
    (and (name-addr-p contact) (name-addr-uri contact))))

(defun msg-routes (message)
  (mapcar #'parse-name-addr (header-values message "Route")))

(defun msg-record-routes (message)
  (mapcar #'parse-name-addr (header-values message "Record-Route")))

(defun msg-integer-header (message name)
  (let ((raw (header message name)))
    (and raw (all-digits-p (trim-ws raw)) (parse-integer (trim-ws raw)))))

(defun msg-expires (message) (msg-integer-header message "Expires"))
(defun msg-max-forwards (message) (msg-integer-header message "Max-Forwards"))

(defun msg-content-type (message)
  "Media type (\"application/sdp\") downcased, without parameters."
  (let ((raw (header message "Content-Type")))
    (and raw
         (string-downcase
          (remove-if #'ws-char-p (subseq raw 0 (position #\; raw)))))))

(defun msg-option-tags (message name)
  (mapcar #'trim-ws (header-values message name)))

(defun message-body-string (message)
  (let ((body (message-body message)))
    (and body (octets-to-string body))))

(defun (setf message-body-string) (string message)
  (setf (message-body message) (ensure-octets string))
  string)

;;;; Construction

(defun normalize-headers (headers)
  "Accept an alist or plist-like list of (name . value) / (name value)."
  (loop for entry in headers
        collect (cons (canonical-header-name (car entry))
                      (header-string (if (consp (cdr entry)) (cadr entry) (cdr entry))))))

(defun make-request (method uri &key headers body)
  (make-instance 'request :method method :uri (ensure-uri uri)
                          :headers (normalize-headers headers)
                          :body (ensure-octets body)))

(defun make-response (status &key reason headers body)
  (make-instance 'response :status status
                           :reason (or reason (reason-phrase status))
                           :headers (normalize-headers headers)
                           :body (ensure-octets body)))

(defun copy-message (message)
  (let ((copy (etypecase message
                (request (make-instance 'request :method (request-method message)
                                                 :uri (request-uri message)
                                                 :next-hop (request-next-hop message)))
                (response (make-instance 'response :status (response-status message)
                                                   :reason (response-reason message))))))
    (setf (message-headers copy) (copy-alist (message-headers message))
          (message-body copy) (message-body message)
          (message-transport copy) (message-transport message)
          (message-remote-host copy) (message-remote-host message)
          (message-remote-port copy) (message-remote-port message)
          (message-connection copy) (message-connection message))
    copy))

(defun make-response-for (request status &key reason to-tag headers body)
  "Build a response to REQUEST following section 8.2.6.2: From, Call-ID, CSeq
and all Via values are copied in order; To is copied and TO-TAG is added when
the request's To has no tag."
  (let ((response (make-response status :reason reason)))
    (dolist (via (header-values request "Via"))
      (add-header response "Via" via))
    (add-header response "From" (or (header request "From") ""))
    (let* ((raw-to (or (header request "To") ""))
           (to (and to-tag (ignore-errors (parse-name-addr raw-to)))))
      (add-header response "To"
                  (if (and to (not (name-addr-tag to)))
                      (name-addr-with-tag to to-tag)
                      raw-to)))
    (add-header response "Call-ID" (or (header request "Call-ID") ""))
    (add-header response "CSeq" (or (header request "CSeq") ""))
    (loop for (name . value) in (normalize-headers headers)
          do (add-header response name value))
    (setf (message-body response) (ensure-octets body))
    ;; Remember where to send it.
    (setf (message-transport response) (message-transport request)
          (message-remote-host response) (message-remote-host request)
          (message-remote-port response) (message-remote-port request)
          (message-connection response) (message-connection request))
    response))
