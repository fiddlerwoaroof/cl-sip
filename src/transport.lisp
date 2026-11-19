(in-package :cl-sip)

;;;; The stack object and the transport layer (RFC 3261 section 18, RFC 3581)

(defclass stack ()
  ((scheduler :initarg :scheduler :reader stack-scheduler)
   (transports :initform '() :accessor stack-transports)
   (client-transactions :initform (make-hash-table :test #'equal)
                        :reader stack-client-transactions)
   (server-transactions :initform (make-hash-table :test #'equal)
                        :reader stack-server-transactions)
   (t1 :initarg :t1 :initform 1/2 :accessor stack-t1)
   (t2 :initarg :t2 :initform 4 :accessor stack-t2)
   (t4 :initarg :t4 :initform 5 :accessor stack-t4)
   (request-handler :initform nil :accessor stack-request-handler
                    :documentation "(lambda (request server-transaction)) for new requests.")
   (ack-handler :initform nil :accessor stack-ack-handler
                :documentation "(lambda (request)) for ACKs matching no transaction (ACK to 2xx).")
   (stray-response-handler :initform nil :accessor stack-stray-response-handler
                           :documentation "(lambda (response)) for 2xx INVITE responses matching no transaction.")
   (trace-function :initarg :trace :initform nil :accessor stack-trace-function
                   :documentation "(lambda (direction message host port)); direction is :in or :out.")))

(defun make-stack (&key scheduler (t1 1/2) (t2 4) (t4 5) trace)
  (make-instance 'stack :scheduler (or scheduler (make-scheduler))
                        :t1 t1 :t2 t2 :t4 t4 :trace trace))

;;; Transport protocol

(defclass transport ()
  ((stack :initform nil :accessor transport-stack)
   (host :initarg :host :initform "127.0.0.1" :accessor transport-host
         :documentation "Local address the transport is bound to.")
   (port :initarg :port :initform 5060 :accessor transport-port)
   (advertised-host :initarg :advertised-host :initform nil
                    :documentation "Host to place in Via and Contact; defaults to HOST.")
   (public-host :initarg :public-host :initform nil :accessor transport-public-host
                :documentation "Address this transport is reached at from outside
a NAT, when known; used in Contact for peers on the public side.")
   (public-port :initarg :public-port :initform nil :accessor transport-public-port)))

(defgeneric transport-protocol (transport)
  (:documentation "Transport name as it appears in Via: \"UDP\", \"TCP\"."))

(defgeneric transport-reliable-p (transport)
  (:method ((transport transport)) nil))

(defgeneric transport-send (transport octets host port connection)
  (:documentation "Send OCTETS to HOST:PORT, reusing CONNECTION when the
transport is connection-oriented and it is still open.  Signals
TRANSPORT-ERROR on failure."))

(defgeneric transport-start (transport)
  (:method ((transport transport)) transport))

(defgeneric transport-stop (transport)
  (:method ((transport transport)) transport))

(defun transport-advertised-host (transport)
  (or (slot-value transport 'advertised-host) (transport-host transport)))

(defun add-transport (stack transport)
  (setf (transport-stack transport) stack
        (stack-transports stack) (append (stack-transports stack) (list transport)))
  transport)

(defun stack-find-transport (stack protocol)
  "Transport for PROTOCOL (a string, any case); with NIL, the first one."
  (or (if protocol
          (find protocol (stack-transports stack)
                :key #'transport-protocol :test #'string-equal)
          (first (stack-transports stack)))
      (error 'transport-error
             :cause (format nil "no transport for ~a" (or protocol "any protocol")))))

(defun stack-trace-message (stack direction message host port)
  (when (stack-trace-function stack)
    (funcall (stack-trace-function stack) direction message host port)))

;;; Receiving (sections 18.1.2, 18.2.1)

(defun transport-receive (transport octets remote-host remote-port &optional connection)
  "Called by a transport, from any thread, with one received message."
  (let ((stack (transport-stack transport)))
    (post (stack-scheduler stack)
          (lambda ()
            (stack-deliver stack transport octets remote-host remote-port connection)))))

(defun host-equal (a b)
  (string-equal (string-trim "[]" a) (string-trim "[]" b)))

(defun note-request-source (request)
  "Section 18.2.1 and RFC 3581 section 4: record where the request really
came from in the top Via."
  (let* ((via (msg-top-via request))
         (host (message-remote-host request))
         (rport-requested (and (nth-value 1 (via-param via "rport"))
                               (null (via-param via "rport")))))
    (when (or rport-requested (not (host-equal (via-host via) host)))
      (setf (via-params via) (set-param (via-params via) "received" host))
      (when rport-requested
        (setf (via-params via)
              (set-param (via-params via) "rport"
                         (princ-to-string (message-remote-port request)))))
      (let ((cell (assoc "Via" (message-headers request) :test #'string=)))
        (setf (cdr cell) (format-via via))))))

(defun response-for-us-p (stack response)
  "Section 18.1.2: the top Via must name one of our transports."
  (let ((via (msg-top-via response)))
    (some (lambda (transport)
            (and (host-equal (via-host via) (transport-advertised-host transport))
                 (eql (or (via-port via) 5060) (transport-port transport))))
          (stack-transports stack))))

(defun stack-deliver (stack transport octets remote-host remote-port connection)
  (let ((message (handler-case (parse-message octets)
                   (sip-parse-error () nil))))
    (when message
      (setf (message-transport message) transport
            (message-remote-host message) remote-host
            (message-remote-port message) remote-port
            (message-connection message) connection)
      (stack-trace-message stack :in message remote-host remote-port)
      (let ((problem (validate-message message)))
        (cond
          ((and problem (request-p message))
           ;; Answer what we can; a request too broken to answer is dropped.
           (unless (string= (request-method message) "ACK")
             (ignore-errors
              (note-request-source message)
              (stack-send-response
               stack (make-response-for message 400 :reason problem :to-tag (make-tag))))))
          (problem nil)
          ((request-p message)
           (note-request-source message)
           (stack-dispatch-request stack message))
          ((rest (header-values message "Via")) nil) ; section 8.1.3.3
          ((response-for-us-p stack message)
           (stack-dispatch-response stack message)))))))

;;; Sending requests (section 18.1.1)

(defun next-hop-uri (request)
  "Section 8.1.2: the first Route if it is a loose router, else the
Request-URI."
  (let ((route (first (header-values request "Route"))))
    (or (request-next-hop request)
        (and route
             (let ((uri (name-addr-uri (parse-name-addr route))))
               (and (uri-loose-route-p uri) uri)))
        (request-uri request))))

(defun uri-destination (uri)
  "Host, port and transport name (or NIL) a request for URI is sent to.
No RFC 3263 lookups: the host is used as given."
  (unless (sip-uri-p uri)
    (error 'transport-error :cause (format nil "cannot route to ~a" (uri-string uri))))
  (let ((transport (uri-param uri "transport")))
    (values (or (uri-param uri "maddr") (sip-uri-host uri))
            (or (sip-uri-port uri) (if (eq (sip-uri-scheme uri) :sips) 5061 5060))
            (and transport (string-upcase transport)))))

(defun stack-prepare-request (stack request &key branch)
  "Choose the transport and destination for REQUEST and push its Via.
Returns transport, host, port."
  (multiple-value-bind (host port protocol) (uri-destination (next-hop-uri request))
    (let ((transport (stack-find-transport stack protocol)))
      (prepend-header
       request "Via"
       (make-via :transport (transport-protocol transport)
                 :host (transport-advertised-host transport)
                 :port (transport-port transport)
                 :params (list (cons "branch" (or branch (make-branch)))
                               (cons "rport" nil))))
      (values transport host port))))

(defun stack-send-message (stack message transport host port &optional connection)
  (stack-trace-message stack :out message host port)
  (handler-case
      (transport-send transport (serialize-message message) host port connection)
    (transport-error (e) (error e))
    (error (e) (error 'transport-error :cause e))))

(defun stack-send-stateless (stack request &key branch)
  "Send REQUEST without a transaction (used for the ACK to a 2xx)."
  (multiple-value-bind (transport host port)
      (stack-prepare-request stack request :branch branch)
    (stack-send-message stack request transport host port)
    (values transport host port)))

;;; Sending responses (section 18.2.2, RFC 3581 section 4)

(defun response-destination (response)
  "Host and port from the top Via of RESPONSE."
  (let* ((via (msg-top-via response))
         (received (via-param via "received"))
         (rport (via-param via "rport"))
         (maddr (via-param via "maddr")))
    (cond (maddr (values maddr (or (via-port via) 5060)))
          (received (values received
                            (or (and rport (all-digits-p rport) (parse-integer rport))
                                (via-port via)
                                5060)))
          (t (values (via-host via) (or (via-port via) 5060))))))

(defun stack-send-response (stack response)
  (let ((transport (or (message-transport response)
                       (stack-find-transport stack (via-transport (msg-top-via response))))))
    (multiple-value-bind (host port) (response-destination response)
      (stack-send-message stack response transport host port
                          (message-connection response)))))
