(in-package :cl-sip)

;;;; In-process transport: stacks attached to the same MEMORY-NETWORK
;;;; exchange messages without sockets.  Used by the tests and handy for
;;;; experiments at the REPL.

(defclass memory-network ()
  ((endpoints :initform (make-hash-table :test #'equal) :reader network-endpoints)
   (log :initform '() :accessor network-log
        :documentation "Every message sent, newest first, as
(message from-host from-port to-host to-port).")
   (keepalives :initform 0 :accessor network-keepalives
               :documentation "Count of non-SIP datagrams sent (keep-alives).")
   (filter :initarg :filter :initform nil :accessor network-filter
           :documentation "(lambda (message from-host to-host)) returning
:DROP to lose the message, an integer N to deliver N copies, or anything else
to deliver it once.")))

(defun make-memory-network (&key filter)
  (make-instance 'memory-network :filter filter))

(defclass memory-transport (transport)
  ((network :initarg :network :reader transport-network)
   (reliable :initarg :reliable :initform nil)
   (nat :initarg :nat :initform nil :reader transport-nat
        :documentation "(host . port) this endpoint appears as to its peers,
simulating a NAT; it is also reachable at that address.")))

(defun make-memory-transport (network host &key (port 5060) reliable nat)
  (make-instance 'memory-transport :network network :host host :port port
                                   :reliable reliable :nat nat))

(defmethod transport-protocol ((transport memory-transport))
  (if (slot-value transport 'reliable) "TCP" "UDP"))

(defmethod transport-reliable-p ((transport memory-transport))
  (slot-value transport 'reliable))

(defun endpoint-key (host port)
  (format nil "~a:~d" (string-downcase host) port))

(defmethod transport-start ((transport memory-transport))
  (let ((endpoints (network-endpoints (transport-network transport)))
        (nat (transport-nat transport)))
    (setf (gethash (endpoint-key (transport-host transport) (transport-port transport))
                   endpoints)
          transport)
    (when nat
      (setf (gethash (endpoint-key (car nat) (cdr nat)) endpoints) transport))
    transport))

(defmethod transport-stop ((transport memory-transport))
  (remhash (endpoint-key (transport-host transport) (transport-port transport))
           (network-endpoints (transport-network transport)))
  transport)

(defmethod transport-send ((transport memory-transport) octets host port connection)
  (declare (ignore connection))
  (let* ((network (transport-network transport))
         (message (handler-case (parse-message octets)
                    (sip-parse-error () nil)))   ; keep-alives are not messages
         (verdict (and message (network-filter network)
                       (funcall (network-filter network) message
                                (transport-host transport) host)))
         (peer (gethash (endpoint-key host port) (network-endpoints network))))
    (if message
        (push (list message (transport-host transport) (transport-port transport) host port)
              (network-log network))
        (incf (network-keepalives network)))
    (cond ((eq verdict :drop))
          ((null peer)
           (when (transport-reliable-p transport)
             (error 'transport-error :cause (format nil "no route to ~a:~d" host port))))
          (t
           (let ((nat (transport-nat transport)))
             (dotimes (i (if (integerp verdict) verdict 1))
               (transport-receive peer octets
                                  (if nat (car nat) (transport-host transport))
                                  (if nat (cdr nat) (transport-port transport)))))))))

(defun network-messages (network)
  "Messages sent so far, oldest first."
  (reverse (mapcar #'first (network-log network))))
