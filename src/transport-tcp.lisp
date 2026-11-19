(in-package :cl-sip)

;;;; TCP transport (RFC 3261 section 18: connections are shared between
;;;; client and server roles and indexed by the far end's address).

(defclass tcp-connection ()
  ((socket :initarg :socket :reader connection-socket)
   (host :initarg :host :reader connection-host)
   (port :initarg :port :reader connection-port)
   (lock :initform (bt:make-lock "sip-tcp-write") :reader connection-lock)
   (open :initform t :accessor connection-open-p)))

(defclass tcp-transport (transport)
  ((listener :initform nil :accessor transport-listener)
   (thread :initform nil :accessor transport-thread)
   (running :initform nil :accessor transport-running)
   (connections :initform (make-hash-table :test #'equal) :reader transport-connections)
   (lock :initform (bt:make-lock "sip-tcp-connections") :reader transport-lock)))

(defun make-tcp-transport (&key (host "127.0.0.1") (port 5060) advertised-host)
  (make-instance 'tcp-transport :host host :port port :advertised-host advertised-host))

(defmethod transport-protocol ((transport tcp-transport)) "TCP")
(defmethod transport-reliable-p ((transport tcp-transport)) t)

(defun close-connection (transport connection)
  (when (connection-open-p connection)
    (setf (connection-open-p connection) nil)
    (bt:with-lock-held ((transport-lock transport))
      (let ((key (endpoint-key (connection-host connection) (connection-port connection))))
        (when (eq (gethash key (transport-connections transport)) connection)
          (remhash key (transport-connections transport)))))
    (ignore-errors (usocket:socket-close (connection-socket connection)))))

(defun start-connection-reader (transport connection)
  (bt:make-thread
   (lambda ()
     (let ((stream (usocket:socket-stream (connection-socket connection)))
           (framer (make-instance 'framer))
           (buffer (make-array 4096 :element-type '(unsigned-byte 8))))
       (handler-case
           (loop while (and (transport-running transport) (connection-open-p connection))
                 do ;; Block for one octet, then take whatever else has arrived.
                    (let ((first (read-byte stream nil nil)))
                      (unless first (return))
                      (setf (aref buffer 0) first)
                      (let ((n 1))
                        (loop while (and (< n (length buffer)) (listen stream))
                              do (setf (aref buffer n) (read-byte stream))
                                 (incf n))
                        (framer-push framer buffer :end n)))
                    (loop for message = (framer-next framer)
                          while message
                          do (transport-receive transport (serialize-message message)
                                                (connection-host connection)
                                                (connection-port connection)
                                                connection)))
         (error () nil))
       (close-connection transport connection)))
   :name (format nil "sip-tcp-~a:~d" (connection-host connection) (connection-port connection))))

(defun register-connection (transport socket host port)
  (let ((connection (make-instance 'tcp-connection :socket socket :host host :port port)))
    (bt:with-lock-held ((transport-lock transport))
      (setf (gethash (endpoint-key host port) (transport-connections transport)) connection))
    (start-connection-reader transport connection)
    connection))

(defmethod transport-start ((transport tcp-transport))
  (let ((listener (usocket:socket-listen (transport-host transport) (transport-port transport)
                                         :reuse-address t :element-type '(unsigned-byte 8))))
    (setf (transport-listener transport) listener
          (transport-port transport) (usocket:get-local-port listener)
          (transport-running transport) t)
    (when (and (wildcard-host-p (transport-host transport))
               (null (slot-value transport 'advertised-host)))
      (setf (slot-value transport 'advertised-host) (guess-local-address)))
    (setf (transport-thread transport)
          (bt:make-thread
           (lambda ()
             (loop while (transport-running transport)
                   do (handler-case
                          (when (usocket:wait-for-input listener :timeout 1/2 :ready-only t)
                            (let ((socket (usocket:socket-accept
                                           listener :element-type '(unsigned-byte 8))))
                              (register-connection
                               transport socket
                               (address-string (usocket:get-peer-address socket))
                               (usocket:get-peer-port socket))))
                        (error ()
                          (unless (transport-running transport) (return))))))
           :name (format nil "sip-tcp-listen-~d" (transport-port transport))))
    transport))

(defmethod transport-stop ((transport tcp-transport))
  (when (transport-running transport)
    (setf (transport-running transport) nil)
    (when (transport-thread transport)
      (bt:join-thread (transport-thread transport))
      (setf (transport-thread transport) nil))
    (usocket:socket-close (transport-listener transport))
    (dolist (connection (bt:with-lock-held ((transport-lock transport))
                          (alexandria:hash-table-values (transport-connections transport))))
      (close-connection transport connection)))
  transport)

(defmethod transport-send ((transport tcp-transport) octets host port connection)
  (handler-case
      (let* ((host (string-trim "[]" host))
             (connection
               (or (and connection (connection-open-p connection) connection)
                   (let ((existing (bt:with-lock-held ((transport-lock transport))
                                     (gethash (endpoint-key host port)
                                              (transport-connections transport)))))
                     (and existing (connection-open-p existing) existing))
                   (register-connection
                    transport
                    (usocket:socket-connect host port :element-type '(unsigned-byte 8))
                    host port))))
        (bt:with-lock-held ((connection-lock connection))
          (let ((stream (usocket:socket-stream (connection-socket connection))))
            (write-sequence octets stream)
            (finish-output stream))))
    (error (e) (error 'transport-error :cause e))))
