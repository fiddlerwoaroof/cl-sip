(in-package :cl-sip.voice)

;;;; Voice activity detection: an energy gate that cuts the incoming audio
;;;; into utterances.  Deliberately simple; telephone audio from a softphone
;;;; is close-miked and G.711 silence is very quiet.

(defclass vad ()
  ((threshold :initarg :threshold :initform 600 :accessor vad-threshold
              :documentation "RMS level (16-bit scale) above which a frame is speech.")
   (start-frames :initarg :start-frames :initform 3
                 :documentation "Consecutive loud frames that begin an utterance.")
   (end-frames :initarg :end-frames :initform 35
               :documentation "Consecutive quiet frames that end one (35 = 700 ms).")
   (pre-roll :initarg :pre-roll :initform 10
             :documentation "Frames kept from before the start, so the first
syllable is not clipped.")
   (min-frames :initarg :min-frames :initform 12
               :documentation "Utterances with fewer speech frames are noise.")
   (max-frames :initarg :max-frames :initform 1500
               :documentation "Longest utterance (1500 = 30 s) before it is cut.")
   (speaking :initform nil :accessor vad-speaking)
   (loud :initform 0) (quiet :initform 0) (speech-frames :initform 0)
   (history :initform '())               ; recent frames while idle, newest first
   (frames :initform '())))              ; frames of the current utterance, newest first

(defun make-vad (&rest args)
  (apply #'make-instance 'vad args))

(defun vad-push (vad frame)
  "Feed one frame of samples.  Returns a list of events: :START when speech
begins, (:UTTERANCE samples) when it ends."
  (with-slots (threshold start-frames end-frames pre-roll min-frames max-frames
               speaking loud quiet speech-frames history frames)
      vad
    (let ((is-loud (> (rms frame) threshold))
          (events '()))
      (cond
        ((not speaking)
         (push frame history)
         (when (> (length history) (+ pre-roll start-frames))
           (setf history (subseq history 0 (+ pre-roll start-frames))))
         (setf loud (if is-loud (1+ loud) 0))
         (when (>= loud start-frames)
           (setf speaking t
                 frames history
                 history '()
                 quiet 0
                 speech-frames loud
                 loud 0)
           (push :start events)))
        (t
         (push frame frames)
         (if is-loud
             (setf quiet 0 speech-frames (1+ speech-frames))
             (incf quiet))
         (when (or (>= quiet end-frames) (>= (length frames) max-frames))
           (when (>= speech-frames min-frames)
             (push (list :utterance (concatenate-samples (reverse frames))) events))
           (setf speaking nil frames '() quiet 0 speech-frames 0))))
      (nreverse events))))
