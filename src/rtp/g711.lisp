(in-package :cl-sip.rtp)

;;;; G.711 (PCMU payload type 0, PCMA payload type 8; RFC 3551 section 4.5.14)
;;;; Samples are 16-bit signed linear PCM.

(defun segment (value ends)
  (or (position-if (lambda (end) (<= value end)) ends) 8))

(defun ulaw-encode-sample (sample)
  (let* ((value (ash sample -2))
         (mask (if (minusp value) #x7F #xFF))
         (value (+ (min (abs value) 8159) #x21))
         (seg (segment value '(#x3F #x7F #xFF #x1FF #x3FF #x7FF #xFFF #x1FFF))))
    (if (>= seg 8)
        (logxor #x7F mask)
        (logxor (logior (ash seg 4) (logand (ash value (- (1+ seg))) #xF)) mask))))

(defun ulaw-decode-sample (byte)
  (let* ((u (logand (lognot byte) #xFF))
         (value (ash (+ (ash (logand u #xF) 3) #x84) (ash (logand u #x70) -4))))
    (if (logtest u #x80) (- #x84 value) (- value #x84))))

(defun alaw-encode-sample (sample)
  (let* ((value (ash sample -3))
         (mask (if (minusp value) #x55 #xD5))
         (value (if (minusp value) (- (- value) 1) value))
         (seg (segment value '(#x1F #x3F #x7F #xFF #x1FF #x3FF #x7FF #xFFF))))
    (if (>= seg 8)
        (logxor #x7F mask)
        (logxor (logior (ash seg 4)
                        (logand (ash value (if (< seg 2) -1 (- seg))) #xF))
                mask))))

(defun alaw-decode-sample (byte)
  (let* ((a (logxor byte #x55))
         (seg (ash (logand a #x70) -4))
         (value (ash (logand a #xF) 4))
         (value (case seg
                  (0 (+ value 8))
                  (1 (+ value #x108))
                  (t (ash (+ value #x108) (1- seg))))))
    (if (logtest a #x80) value (- value))))

(defun map-samples (function input element-type)
  (let ((output (make-array (length input) :element-type element-type)))
    (dotimes (i (length input) output)
      (setf (aref output i) (funcall function (aref input i))))))

(defun ulaw-encode (samples) (map-samples #'ulaw-encode-sample samples '(unsigned-byte 8)))
(defun ulaw-decode (octets) (map-samples #'ulaw-decode-sample octets '(signed-byte 16)))
(defun alaw-encode (samples) (map-samples #'alaw-encode-sample samples '(unsigned-byte 8)))
(defun alaw-decode (octets) (map-samples #'alaw-decode-sample octets '(signed-byte 16)))
