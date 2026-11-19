(in-package :cl-sip.test)

(def-suite rtp :in cl-sip)
(in-suite rtp)

(defun octets (&rest values)
  (coerce values '(simple-array (unsigned-byte 8) (*))))

(test rtp-header-rfc3550-5.1
  (let* ((packet (rtp:make-rtp-packet :marker t :payload-type 0 :sequence #x1234
                                      :timestamp #x01020304 :ssrc #xDEADBEEF
                                      :payload (octets 1 2 3)))
         (encoded (rtp:encode-rtp-packet packet)))
    (is (equalp (octets #x80 #x80 #x12 #x34 1 2 3 4 #xDE #xAD #xBE #xEF 1 2 3) encoded))
    (is (equalp packet (rtp:decode-rtp-packet encoded))))
  ;; CSRC list and header extension.
  (let* ((packet (rtp:make-rtp-packet :payload-type 96 :sequence 65535 :timestamp 0 :ssrc 1
                                      :csrcs '(2 3) :extension (cons #xBEDE (octets 9 9 9 9))
                                      :payload (octets 7)))
         (encoded (rtp:encode-rtp-packet packet)))
    (is (= #x92 (aref encoded 0)))
    (is (= 29 (length encoded)))
    (is (equalp packet (rtp:decode-rtp-packet encoded))))
  ;; Padding is stripped.
  (let ((packet (rtp:decode-rtp-packet
                 (octets #xA0 0 0 1 0 0 0 0 0 0 0 9 5 6 0 2))))
    (is (equalp (octets 5 6) (rtp:rtp-packet-payload packet))))
  (signals rtp:rtp-error (rtp:decode-rtp-packet (octets #x80 0 0)))
  (signals rtp:rtp-error (rtp:decode-rtp-packet (octets #x40 0 0 1 0 0 0 0 0 0 0 9)))
  (signals rtp:rtp-error (rtp:decode-rtp-packet (octets #xA0 0 0 1 0 0 0 0 0 0 0 9 1 9))))

(test g711
  ;; Reference points of ITU-T G.711.
  (is (= #xFF (rtp:ulaw-encode-sample 0)))
  (is (= #x80 (rtp:ulaw-encode-sample 32767)))
  (is (= #x00 (rtp:ulaw-encode-sample -32768)))
  (is (= 0 (rtp:ulaw-decode-sample #xFF)))
  (is (= 32124 (rtp:ulaw-decode-sample #x80)))
  (is (= -32124 (rtp:ulaw-decode-sample #x00)))
  (is (= #xD5 (rtp:alaw-encode-sample 0)))
  (is (= #xAA (rtp:alaw-encode-sample 32767)))
  (is (= #x2A (rtp:alaw-encode-sample -32768)))
  (is (= 32256 (rtp:alaw-decode-sample #xAA)))
  (is (= -32256 (rtp:alaw-decode-sample #x2A)))
  ;; Companding is lossy but bounded, and idempotent after one pass.
  (loop for sample from -32768 to 32767 by 97
        do (let ((u (rtp:ulaw-decode-sample (rtp:ulaw-encode-sample sample)))
                 (a (rtp:alaw-decode-sample (rtp:alaw-encode-sample sample))))
             (unless (and (<= (abs (- u sample)) (max 16 (* 0.07 (abs sample)) 700))
                          (<= (abs (- a sample)) (max 16 (* 0.07 (abs sample)) 700)))
               (fail "sample ~d decoded as ~d (u-law), ~d (A-law)" sample u a))
             (unless (and (= (rtp:ulaw-encode-sample u) (rtp:ulaw-encode-sample sample))
                          (= (rtp:alaw-encode-sample a) (rtp:alaw-encode-sample sample)))
               (fail "re-encoding ~d is not stable" sample))))
  (let ((samples (make-array 4 :element-type '(signed-byte 16) :initial-contents '(0 1000 -1000 20000))))
    (is (= 4 (length (rtp:ulaw-encode samples))))
    (is (typep (rtp:alaw-decode (rtp:alaw-encode samples)) '(vector (signed-byte 16))))))

(test telephone-event-rfc4733
  (let ((event (rtp:make-telephone-event :event 5 :end t :volume 10 :duration 800)))
    (is (equalp (octets 5 #x8A #x03 #x20) (rtp:encode-telephone-event event)))
    (is (equalp event (rtp:decode-telephone-event (rtp:encode-telephone-event event)))))
  (is (equal '(0 9 10 11 12 15) (mapcar #'rtp:dtmf-event-code '(#\0 #\9 #\* #\# #\a #\D))))
  (is (char= #\# (rtp:dtmf-event-char 11)))
  (signals rtp:rtp-error (rtp:dtmf-event-code #\x))
  ;; Section 2.5.1: one digit, 100 ms at 8 kHz in 20 ms packets.
  (let* ((packets (rtp:dtmf-packets #\7 :sequence 100 :timestamp 5000 :ssrc 42))
         (events (mapcar (lambda (p) (rtp:decode-telephone-event (rtp:rtp-packet-payload p)))
                         packets)))
    (is (= 7 (length packets)))
    (is (equal '(t nil nil nil nil nil nil) (mapcar #'rtp:rtp-packet-marker packets)))
    (is (equal '(100 101 102 103 104 105 106) (mapcar #'rtp:rtp-packet-sequence packets)))
    ;; The timestamp stays at the start of the event; the duration grows.
    (is (every (lambda (p) (= 5000 (rtp:rtp-packet-timestamp p))) packets))
    (is (equal '(160 320 480 640 800 800 800) (mapcar #'rtp:telephone-event-duration events)))
    (is (equal '(nil nil nil nil t t t) (mapcar #'rtp:telephone-event-end events)))
    (is (every (lambda (e) (= 7 (rtp:telephone-event-event e))) events))
    (is (every (lambda (p) (= 101 (rtp:rtp-packet-payload-type p))) packets))))
