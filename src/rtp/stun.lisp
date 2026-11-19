(in-package :cl-sip.rtp)

;;;; STUN Binding requests (RFC 8489), just enough to ask a server which
;;;; public address and port a UDP socket appears as from outside a NAT.

(defconstant +stun-magic-cookie+ #x2112A442)

(defun make-stun-transaction-id ()
  (let ((id (make-array 12 :element-type '(unsigned-byte 8))))
    (dotimes (i 12 id)
      (setf (aref id i) (random 256)))))

(defun encode-stun-binding-request (transaction-id)
  "A Binding request with no attributes (section 5: 20-octet header)."
  (let ((buffer (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0)))
    (put-u16 buffer 0 #x0001)           ; Binding request
    (put-u16 buffer 2 0)                ; no attributes
    (put-u32 buffer 4 +stun-magic-cookie+)
    (replace buffer transaction-id :start1 8)
    buffer))

(defun decode-stun-binding-response (buffer transaction-id &key (end (length buffer)))
  "The mapped address in a Binding success response, as a dotted-quad string
and a port; NIL when BUFFER is not a matching response.  XOR-MAPPED-ADDRESS
is preferred, MAPPED-ADDRESS accepted (section 14.1, 14.2).  IPv4 only."
  (when (and (>= end 20)
             (= (get-u16 buffer 0) #x0101)
             (= (get-u32 buffer 4) +stun-magic-cookie+)
             (not (mismatch buffer transaction-id :start1 8 :end1 20))
             (<= (+ 20 (get-u16 buffer 2)) end))
    (let ((limit (+ 20 (get-u16 buffer 2)))
          (pos 20)
          (plain nil))
      (flet ((dotted (address)
               (format nil "~d.~d.~d.~d" (ldb (byte 8 24) address) (ldb (byte 8 16) address)
                       (ldb (byte 8 8) address) (ldb (byte 8 0) address))))
        (loop while (<= (+ pos 4) limit)
              do (let ((type (get-u16 buffer pos))
                       (length (get-u16 buffer (+ pos 2)))
                       (value (+ pos 4)))
                   (when (and (>= length 8) (<= (+ value 8) limit)
                              (= (aref buffer (1+ value)) 1)) ; IPv4
                     (cond ((= type #x0020)
                            (return-from decode-stun-binding-response
                              (values (dotted (logxor (get-u32 buffer (+ value 4))
                                                      +stun-magic-cookie+))
                                      (logxor (get-u16 buffer (+ value 2))
                                              (ash +stun-magic-cookie+ -16)))))
                           ((= type #x0001)
                            (setf plain (list (dotted (get-u32 buffer (+ value 4)))
                                              (get-u16 buffer (+ value 2)))))))
                   ;; Attributes are padded to a multiple of four octets.
                   (incf pos (+ 4 (* 4 (ceiling length 4))))))
        (and plain (values-list plain))))))

(defun stun-query (socket host port &key (attempts 3) (timeout 1/2))
  "Ask the STUN server at HOST:PORT for the public address of the UDP
SOCKET.  Returns host and port, or NIL when the server does not answer.
Must run before anything else reads from the socket."
  (let ((buffer (make-array 512 :element-type '(unsigned-byte 8))))
    (dotimes (i attempts)
      (let* ((id (make-stun-transaction-id))
             (request (encode-stun-binding-request id)))
        (handler-case
            (progn
              (usocket:socket-send socket request (length request) :host host :port port)
              (when (usocket:wait-for-input socket :timeout timeout :ready-only t)
                (multiple-value-bind (data size)
                    (usocket:socket-receive socket buffer (length buffer))
                  (declare (ignore data))
                  (multiple-value-bind (public-host public-port)
                      (and size (decode-stun-binding-response buffer id :end size))
                    (when public-host
                      (return-from stun-query (values public-host public-port)))))))
          (error () nil))))
    nil))
