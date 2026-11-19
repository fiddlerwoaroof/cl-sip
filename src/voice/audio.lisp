(in-package :cl-sip.voice)

;;;; Audio helpers: WAV files, resampling, and a paced RTP sender.
;;;; Samples are vectors of 16-bit signed linear PCM, mono.

(deftype samples () '(simple-array (signed-byte 16) (*)))

(defun make-samples (n)
  (make-array n :element-type '(signed-byte 16) :initial-element 0))

;;; WAV (RIFF) files

(defun u16le (octets i) (logior (aref octets i) (ash (aref octets (+ i 1)) 8)))
(defun u32le (octets i) (logior (u16le octets i) (ash (u16le octets (+ i 2)) 16)))

(defun read-file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun read-wav (path)
  "Read a PCM WAV file (8- or 16-bit, any channel count).  Returns mono
samples and the sample rate."
  (let* ((octets (read-file-octets path))
         (n (length octets))
         (pos 12)
         channels rate bits data-start data-length)
    (unless (and (>= n 12)
                 (equalp (subseq octets 0 4) #(82 73 70 70))      ; RIFF
                 (equalp (subseq octets 8 12) #(87 65 86 69)))    ; WAVE
      (error "~a is not a WAV file" path))
    ;; Walk the chunks; writers add all sorts (JUNK, LIST, FLLR) around fmt/data.
    (loop while (<= (+ pos 8) n)
          do (let ((id (map 'string #'code-char (subseq octets pos (+ pos 4))))
                   (size (u32le octets (+ pos 4))))
               (cond ((string= id "fmt ")
                      (unless (= (u16le octets (+ pos 8)) 1)
                        (error "~a is not linear PCM (format ~d)" path (u16le octets (+ pos 8))))
                      (setf channels (u16le octets (+ pos 10))
                            rate (u32le octets (+ pos 12))
                            bits (u16le octets (+ pos 22))))
                     ((string= id "data")
                      (setf data-start (+ pos 8)
                            data-length (min size (- n pos 8)))
                      (return)))
               (incf pos (+ 8 size (mod size 2)))))
    (unless (and channels data-start) (error "~a has no audio data" path))
    (unless (member bits '(8 16)) (error "~a: unsupported sample size ~d" path bits))
    (let* ((bytes (/ bits 8))
           (frames (floor data-length (* bytes channels)))
           (samples (make-samples frames)))
      (dotimes (f frames)
        (let ((sum 0))
          (dotimes (c channels)
            (let ((i (+ data-start (* (+ (* f channels) c) bytes))))
              (incf sum (if (= bits 16)
                            (let ((v (u16le octets i))) (if (>= v 32768) (- v 65536) v))
                            (* 256 (- (aref octets i) 128))))))
          (setf (aref samples f) (round sum channels))))
      (values samples rate))))

(defun write-wav (path samples rate)
  "Write mono 16-bit PCM."
  (with-open-file (out path :element-type '(unsigned-byte 8)
                            :direction :output :if-exists :supersede)
    (let ((data-length (* 2 (length samples))))
      (flet ((tag (string) (write-sequence (map 'vector #'char-code string) out))
             (u32 (v) (dotimes (i 4) (write-byte (ldb (byte 8 (* 8 i)) v) out)))
             (u16 (v) (dotimes (i 2) (write-byte (ldb (byte 8 (* 8 i)) v) out))))
        (tag "RIFF") (u32 (+ 36 data-length)) (tag "WAVE")
        (tag "fmt ") (u32 16) (u16 1) (u16 1) (u32 rate) (u32 (* 2 rate)) (u16 2) (u16 16)
        (tag "data") (u32 data-length)
        (loop for s across samples do (u16 (logand s #xFFFF))))))
  path)

(defun resample (samples from-rate to-rate)
  "Linear-interpolation resampling; adequate for speech."
  (if (= from-rate to-rate)
      samples
      (let* ((n (length samples))
             (m (floor (* n to-rate) from-rate))
             (out (make-samples m)))
        (dotimes (i m out)
          (multiple-value-bind (j frac) (floor (* i from-rate) to-rate)
            (let ((a (aref samples (min j (1- n))))
                  (b (aref samples (min (1+ j) (1- n)))))
              (setf (aref out i) (round (+ a (* (- b a) (/ frac to-rate)))))))))))

(defun rms (samples &key (start 0) (end (length samples)))
  "Root-mean-square level of SAMPLES."
  (if (>= start end)
      0
      (sqrt (/ (loop for i from start below end
                     sum (let ((s (aref samples i))) (* s s)))
               (- end start)))))

(defun concatenate-samples (chunks)
  (let ((out (make-samples (reduce #'+ chunks :key #'length)))
        (pos 0))
    (dolist (chunk chunks out)
      (replace out chunk :start1 pos)
      (incf pos (length chunk)))))

;;; Paced output: sends queued audio as 20 ms RTP packets in real time.

(defconstant +frame-samples+ 160)       ; 20 ms at 8 kHz

(defclass audio-out ()
  ((session :initarg :session :reader audio-out-session)
   (encoder :initarg :encoder :initform #'rtp:ulaw-encode :accessor audio-out-encoder)
   (queue :initform '() :accessor audio-out-queue
          :documentation "Sample vectors waiting to be sent, oldest first.")
   (offset :initform 0 :accessor audio-out-offset
           :documentation "Samples of the first queued vector already sent.")
   (lock :initform (bt:make-lock "audio-out") :reader audio-out-lock)
   (running :initform t :accessor audio-out-running)
   (sending :initform nil :accessor audio-out-sending)
   (thread :initform nil :accessor audio-out-thread)))

(defun audio-out-next-frame (out)
  "Pop the next 160 samples, padding the tail of the queue with silence.
NIL when nothing is queued."
  (bt:with-lock-held ((audio-out-lock out))
    (when (audio-out-queue out)
      (let ((frame (make-samples +frame-samples+))
            (filled 0))
        (loop while (and (< filled +frame-samples+) (audio-out-queue out))
              do (let* ((chunk (first (audio-out-queue out)))
                        (take (min (- +frame-samples+ filled)
                                   (- (length chunk) (audio-out-offset out)))))
                   (replace frame chunk :start1 filled
                                        :start2 (audio-out-offset out)
                                        :end2 (+ (audio-out-offset out) take))
                   (incf filled take)
                   (incf (audio-out-offset out) take)
                   (when (>= (audio-out-offset out) (length chunk))
                     (pop (audio-out-queue out))
                     (setf (audio-out-offset out) 0))))
        frame))))

(defun audio-out-loop (out)
  (let* ((units internal-time-units-per-second)
         (tick (floor units 50))
         (next (get-internal-real-time))
         (talking nil))
    (loop while (audio-out-running out)
          do (let ((frame (audio-out-next-frame out)))
               (setf (audio-out-sending out) (and frame t))
               (handler-case
                   (if frame
                       (progn
                         ;; The marker bit flags the first packet of a talk spurt.
                         (rtp:rtp-send (audio-out-session out)
                                       (funcall (audio-out-encoder out) frame)
                                       :marker (not talking))
                         (setf talking t))
                       (progn
                         (rtp:rtp-skip (audio-out-session out) +frame-samples+)
                         (setf talking nil)))
                 (error () nil)))
             (incf next tick)
             (let ((wait (- next (get-internal-real-time))))
               (if (plusp wait)
                   (sleep (/ wait units))
                   ;; Fell behind (machine asleep, long GC): do not burst.
                   (when (< wait (- (* 5 tick)))
                     (setf next (get-internal-real-time))))))))

(defun make-audio-out (session &key (encoder #'rtp:ulaw-encode))
  "Start sending to SESSION's remote address.  Audio is queued with
AUDIO-OUT-ENQUEUE as 8 kHz samples."
  (let ((out (make-instance 'audio-out :session session :encoder encoder)))
    (setf (audio-out-thread out)
          (bt:make-thread (lambda () (audio-out-loop out)) :name "audio-out"))
    out))

(defun audio-out-enqueue (out samples)
  (when (plusp (length samples))
    (bt:with-lock-held ((audio-out-lock out))
      (setf (audio-out-queue out) (append (audio-out-queue out) (list samples)))))
  out)

(defun audio-out-flush (out)
  "Drop everything not yet sent (used when the caller interrupts)."
  (bt:with-lock-held ((audio-out-lock out))
    (setf (audio-out-queue out) '()
          (audio-out-offset out) 0))
  out)

(defun audio-out-idle-p (out)
  (bt:with-lock-held ((audio-out-lock out))
    (and (null (audio-out-queue out)) (not (audio-out-sending out)))))

(defun audio-out-stop (out)
  "Stop the sender.  Does not wait for its thread, so it is safe to call
from anywhere."
  (setf (audio-out-running out) nil)
  out)
