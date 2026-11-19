(in-package :cl-sip)

;;;; UDP transport

(defclass udp-transport (transport)
  ((socket :initform nil :accessor transport-socket)
   (thread :initform nil :accessor transport-thread)
   (running :initform nil :accessor transport-running)))

(defun make-udp-transport (&key (host "127.0.0.1") (port 5060) advertised-host)
  (make-instance 'udp-transport :host host :port port :advertised-host advertised-host))

(defmethod transport-protocol ((transport udp-transport)) "UDP")

(defun address-string (address)
  (etypecase address
    (string address)
    (vector (if (= (length address) 4)
                (format nil "~{~d~^.~}" (coerce address 'list))
                (usocket:host-to-hostname address)))))

(defun guess-local-address ()
  "The address this host would use to reach the wider network, found by
connecting (not sending on) a UDP socket."
  (or (ignore-errors
       (let ((socket (usocket:socket-connect "192.0.2.1" 9 :protocol :datagram)))
         (unwind-protect (address-string (usocket:get-local-address socket))
           (usocket:socket-close socket))))
      "127.0.0.1"))

(defun wildcard-host-p (host)
  (member host '("0.0.0.0" "::" "[::]") :test #'string=))

(defmethod transport-start ((transport udp-transport))
  (let ((socket (usocket:socket-connect nil nil :protocol :datagram
                                                :local-host (transport-host transport)
                                                :local-port (transport-port transport))))
    (setf (transport-socket transport) socket
          (transport-port transport) (usocket:get-local-port socket)
          (transport-running transport) t)
    (when (and (wildcard-host-p (transport-host transport))
               (null (slot-value transport 'advertised-host)))
      (setf (slot-value transport 'advertised-host) (guess-local-address)))
    (setf (transport-thread transport)
          (bt:make-thread
           (lambda ()
             (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
               (loop while (transport-running transport)
                     do (handler-case
                            (when (usocket:wait-for-input socket :timeout 1/2 :ready-only t)
                              (multiple-value-bind (data size host port)
                                  (usocket:socket-receive socket buffer (length buffer))
                                (declare (ignore data))
                                (when (and size (plusp size))
                                  (transport-receive transport (subseq buffer 0 size)
                                                     (address-string host) port))))
                          (error ()
                            (unless (transport-running transport) (return)))))))
           :name (format nil "sip-udp-~d" (transport-port transport))))
    transport))

(defmethod transport-stop ((transport udp-transport))
  (when (transport-running transport)
    (setf (transport-running transport) nil)
    (when (transport-thread transport)
      (bt:join-thread (transport-thread transport))
      (setf (transport-thread transport) nil))
    (usocket:socket-close (transport-socket transport))
    (setf (transport-socket transport) nil))
  transport)

(defmethod transport-send ((transport udp-transport) octets host port connection)
  (declare (ignore connection))
  (handler-case
      (usocket:socket-send (transport-socket transport) octets (length octets)
                           :host (string-trim "[]" host) :port port)
    (error (e) (error 'transport-error :cause e))))
