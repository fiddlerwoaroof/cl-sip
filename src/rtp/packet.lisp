(defpackage :cl-sip.rtp
  (:use :cl)
  (:export
   #:rtp-error
   #:rtp-packet #:make-rtp-packet #:rtp-packet-p
   #:rtp-packet-marker #:rtp-packet-payload-type #:rtp-packet-sequence
   #:rtp-packet-timestamp #:rtp-packet-ssrc #:rtp-packet-csrcs
   #:rtp-packet-extension #:rtp-packet-payload
   #:encode-rtp-packet #:decode-rtp-packet
   ;; G.711
   #:ulaw-encode-sample #:ulaw-decode-sample #:alaw-encode-sample #:alaw-decode-sample
   #:ulaw-encode #:ulaw-decode #:alaw-encode #:alaw-decode
   ;; RFC 4733
   #:telephone-event #:make-telephone-event #:telephone-event-event
   #:telephone-event-end #:telephone-event-volume #:telephone-event-duration
   #:encode-telephone-event #:decode-telephone-event
   #:dtmf-event-code #:dtmf-event-char #:dtmf-packets
   ;; sessions
   #:rtp-session #:make-rtp-session #:rtp-session-start #:rtp-session-stop
   #:rtp-session-local-port #:rtp-session-remote-host #:rtp-session-remote-port
   #:rtp-session-ssrc #:rtp-session-payload-type #:rtp-session-on-packet
   #:rtp-session-packets-sent #:rtp-session-packets-received
   #:rtp-session-remote-ssrc #:rtp-send #:rtp-send-dtmf))

(in-package :cl-sip.rtp)

;;;; RTP data packets (RFC 3550 section 5.1)

(define-condition rtp-error (error)
  ((message :initarg :message :reader rtp-error-message))
  (:report (lambda (c s) (format s "RTP error: ~a" (rtp-error-message c)))))

(defstruct rtp-packet
  (marker nil)
  (payload-type 0 :type (integer 0 127))
  (sequence 0 :type (unsigned-byte 16))
  (timestamp 0 :type (unsigned-byte 32))
  (ssrc 0 :type (unsigned-byte 32))
  (csrcs '() :type list)
  (extension nil)                       ; NIL or (profile-id . octets), length a multiple of 4
  (payload (make-array 0 :element-type '(unsigned-byte 8))))

(defun put-u16 (buffer offset value)
  (setf (aref buffer offset) (ldb (byte 8 8) value)
        (aref buffer (+ offset 1)) (ldb (byte 8 0) value)))

(defun put-u32 (buffer offset value)
  (setf (aref buffer offset) (ldb (byte 8 24) value)
        (aref buffer (+ offset 1)) (ldb (byte 8 16) value)
        (aref buffer (+ offset 2)) (ldb (byte 8 8) value)
        (aref buffer (+ offset 3)) (ldb (byte 8 0) value)))

(defun get-u16 (buffer offset)
  (logior (ash (aref buffer offset) 8) (aref buffer (+ offset 1))))

(defun get-u32 (buffer offset)
  (logior (ash (aref buffer offset) 24) (ash (aref buffer (+ offset 1)) 16)
          (ash (aref buffer (+ offset 2)) 8) (aref buffer (+ offset 3))))

(defun encode-rtp-packet (packet)
  (let* ((csrcs (rtp-packet-csrcs packet))
         (extension (rtp-packet-extension packet))
         (payload (rtp-packet-payload packet))
         (header-length (+ 12 (* 4 (length csrcs))
                           (if extension (+ 4 (length (cdr extension))) 0)))
         (buffer (make-array (+ header-length (length payload))
                             :element-type '(unsigned-byte 8)))
         (offset 12))
    (when (> (length csrcs) 15)
      (error 'rtp-error :message "more than 15 CSRCs"))
    (when (and extension (plusp (mod (length (cdr extension)) 4)))
      (error 'rtp-error :message "extension length must be a multiple of 4"))
    (setf (aref buffer 0) (logior (ash 2 6) (if extension #x10 0) (length csrcs))
          (aref buffer 1) (logior (if (rtp-packet-marker packet) #x80 0)
                                  (rtp-packet-payload-type packet)))
    (put-u16 buffer 2 (rtp-packet-sequence packet))
    (put-u32 buffer 4 (rtp-packet-timestamp packet))
    (put-u32 buffer 8 (rtp-packet-ssrc packet))
    (dolist (csrc csrcs)
      (put-u32 buffer offset csrc)
      (incf offset 4))
    (when extension
      (put-u16 buffer offset (car extension))
      (put-u16 buffer (+ offset 2) (floor (length (cdr extension)) 4))
      (replace buffer (cdr extension) :start1 (+ offset 4))
      (incf offset (+ 4 (length (cdr extension)))))
    (replace buffer payload :start1 offset)
    buffer))

(defun decode-rtp-packet (buffer &key (start 0) (end (length buffer)))
  (when (< (- end start) 12)
    (error 'rtp-error :message "packet shorter than the fixed header"))
  (let* ((b0 (aref buffer start))
         (b1 (aref buffer (+ start 1)))
         (csrc-count (ldb (byte 4 0) b0))
         (offset (+ start 12)))
    (unless (= (ldb (byte 2 6) b0) 2)
      (error 'rtp-error :message "not RTP version 2"))
    (when (> (+ offset (* 4 csrc-count)) end)
      (error 'rtp-error :message "truncated CSRC list"))
    (let ((csrcs (loop repeat csrc-count
                       collect (prog1 (get-u32 buffer offset) (incf offset 4))))
          (extension nil))
      (when (logbitp 4 b0)
        (when (> (+ offset 4) end)
          (error 'rtp-error :message "truncated header extension"))
        (let ((profile (get-u16 buffer offset))
              (length (* 4 (get-u16 buffer (+ offset 2)))))
          (when (> (+ offset 4 length) end)
            (error 'rtp-error :message "truncated header extension"))
          (setf extension (cons profile (subseq buffer (+ offset 4) (+ offset 4 length))))
          (incf offset (+ 4 length))))
      (when (logbitp 5 b0)
        ;; Padding: the last octet says how many octets to drop.
        (let ((padding (if (> end offset) (aref buffer (1- end)) 0)))
          (when (or (zerop padding) (> padding (- end offset)))
            (error 'rtp-error :message "bad padding"))
          (decf end padding)))
      (make-rtp-packet :marker (logbitp 7 b1)
                       :payload-type (ldb (byte 7 0) b1)
                       :sequence (get-u16 buffer (+ start 2))
                       :timestamp (get-u32 buffer (+ start 4))
                       :ssrc (get-u32 buffer (+ start 8))
                       :csrcs csrcs
                       :extension extension
                       :payload (coerce (subseq buffer offset end)
                                        '(simple-array (unsigned-byte 8) (*)))))))
