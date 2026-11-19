(in-package :cl-sip.voice)

;;;; A voice agent for one call: caller audio -> utterances -> text ->
;;;; language model -> speech -> RTP.
;;;;
;;;; Three threads touch an agent.  The RTP receive thread decodes audio and
;;;; runs the voice-activity detector; a worker thread runs the slow stages
;;;; (transcribe, generate, synthesize) one turn at a time; the AUDIO-OUT
;;;; thread paces playback.  A caller who starts talking while the agent is
;;;; speaking interrupts it: playback is dropped and the reply in progress
;;;; is abandoned.

(defclass voice-agent ()
  ((session :initarg :session :reader agent-session)
   (out :initarg :out :reader agent-out)
   (vad :initarg :vad :reader agent-vad)
   (stt :initarg :stt :reader agent-stt)
   (llm :initarg :llm :reader agent-llm)
   (tts :initarg :tts :reader agent-tts)
   (barge-in :initarg :barge-in :initform t :reader agent-barge-in)
   (log :initarg :log :initform nil :reader agent-log
        :documentation "(lambda (who text)) called for each thing said; WHO is
:caller, :agent or :error.")
   (history :initform '() :accessor agent-history
            :documentation "Conversation so far, newest first.")
   (generation :initform 0 :accessor agent-generation
               :documentation "Incremented on interruption; work started under
an older generation is discarded.")
   (mailbox :initform '() :accessor agent-mailbox)
   (lock :initform (bt:make-lock "voice-agent") :reader agent-lock)
   (wakeup :initform (bt:make-condition-variable) :reader agent-wakeup)
   (running :initform t :accessor agent-running)
   (busy :initform nil :accessor agent-busy)
   (ignore-utterance :initform nil :accessor agent-ignore-utterance)
   (thread :initform nil :accessor agent-thread)))

(defun agent-messages (agent)
  "The conversation as a list of (role . text), oldest first."
  (reverse (agent-history agent)))

(defun agent-note (agent who text)
  (when (agent-log agent)
    (ignore-errors (funcall (agent-log agent) who text))))

(defun agent-post (agent item)
  (bt:with-lock-held ((agent-lock agent))
    (setf (agent-mailbox agent) (append (agent-mailbox agent) (list item)))
    (bt:condition-notify (agent-wakeup agent))))

(defun agent-take (agent)
  "Next item for the worker; NIL once the agent has stopped."
  (bt:with-lock-held ((agent-lock agent))
    (loop
      (cond ((not (agent-running agent)) (return nil))
            ((agent-mailbox agent) (return (pop (agent-mailbox agent))))
            (t (bt:condition-wait (agent-wakeup agent) (agent-lock agent) :timeout 1))))))

(defun agent-speaking-p (agent)
  (or (agent-busy agent) (not (audio-out-idle-p (agent-out agent)))))

(defun agent-interrupt (agent)
  (incf (agent-generation agent))
  (audio-out-flush (agent-out agent))
  (abort-response (agent-llm agent)))

;;; Incoming audio (RTP receive thread)

(defun agent-handle-packet (agent packet)
  (let ((decoder (case (rtp:rtp-packet-payload-type packet)
                   (0 #'rtp:ulaw-decode)
                   (8 #'rtp:alaw-decode))))
    (when (and decoder (plusp (length (rtp:rtp-packet-payload packet))))
      (dolist (event (vad-push (agent-vad agent)
                               (funcall decoder (rtp:rtp-packet-payload packet))))
        (cond
          ((eq event :start)
           (cond ((not (agent-speaking-p agent))
                  (setf (agent-ignore-utterance agent) nil))
                 ((agent-barge-in agent)
                  (setf (agent-ignore-utterance agent) nil)
                  (agent-interrupt agent))
                 ;; Without barge-in, speech over the agent's own is dropped.
                 (t (setf (agent-ignore-utterance agent) t))))
          ((agent-ignore-utterance agent))
          (t (agent-post agent event)))))))

;;; Turns (worker thread)

(defun agent-speak (agent text generation)
  "Synthesize TEXT and queue it, unless the caller has interrupted since
GENERATION.  Returns true when the audio was queued."
  (when (= generation (agent-generation agent))
    (let ((samples (synthesize (agent-tts agent) text)))
      (when (= generation (agent-generation agent))
        (audio-out-enqueue (agent-out agent) samples)
        (agent-note agent :agent text)
        t))))

(defun agent-turn (agent samples)
  (let ((generation (agent-generation agent))
        (text (transcribe (agent-stt agent) samples 8000)))
    (when text
      (agent-note agent :caller text)
      (push (cons :user text) (agent-history agent)))
    ;; If the caller carried on talking while this was being transcribed,
    ;; keep what they said and answer once they have finished.
    (when (and text (= generation (agent-generation agent)))
      (let ((pending "")
            (spoken '()))
        (flet ((say (sentence)
                 (when (agent-speak agent sentence generation)
                   (push sentence spoken))))
          (handler-case
              (multiple-value-bind (reply stop-reason)
                  (respond (agent-llm agent) (agent-messages agent)
                           (lambda (piece)
                             (when (= generation (agent-generation agent))
                               ;; Speak each sentence as soon as it is complete.
                               (multiple-value-bind (sentences rest)
                                   (split-sentences (concatenate 'string pending piece))
                                 (setf pending rest)
                                 (mapc #'say sentences)))))
                (declare (ignore reply))
                (let ((rest (string-trim '(#\Space #\Newline) pending)))
                  (when (find-if #'alphanumericp rest) (say rest)))
                (when (and (equal stop-reason "refusal") (null spoken))
                  (say "Sorry, I can't help with that.")))
            (llm-error (e)
              (agent-note agent :error (princ-to-string e))
              ;; Forget the question so the history stays well formed.
              (pop (agent-history agent))
              (agent-speak agent "Sorry, I'm having trouble thinking right now."
                           generation)
              (return-from agent-turn))))
        ;; Record what was actually said; an interrupted reply is cut short.
        (when spoken
          (push (cons :assistant (format nil "~{~a~^ ~}" (reverse spoken)))
                (agent-history agent)))))))

(defun agent-loop (agent)
  (loop for item = (agent-take agent)
        while item
        do (setf (agent-busy agent) t)
           (handler-case
               (ecase (first item)
                 (:say (agent-speak agent (second item) (agent-generation agent)))
                 (:utterance (agent-turn agent (second item))))
             (error (e) (agent-note agent :error (princ-to-string e))))
           (setf (agent-busy agent) nil)))

;;; Lifecycle

(defun start-agent (session &key stt llm tts (codec "PCMU") greeting (barge-in t) log vad)
  "Attach a voice agent to the started RTP SESSION, whose remote address is
already set.  CODEC is the negotiated audio codec name (PCMU or PCMA)."
  (let* ((agent (make-instance
                 'voice-agent
                 :session session
                 :out (make-audio-out session
                                      :encoder (if (string-equal codec "PCMA")
                                                   #'rtp:alaw-encode
                                                   #'rtp:ulaw-encode))
                 :vad (or vad (make-vad))
                 :stt stt :llm llm :tts tts :barge-in barge-in :log log)))
    (setf (rtp:rtp-session-payload-type session) (if (string-equal codec "PCMA") 8 0)
          (rtp:rtp-session-on-packet session)
          (lambda (session packet)
            (declare (ignore session))
            (agent-handle-packet agent packet))
          (agent-thread agent)
          (bt:make-thread (lambda () (agent-loop agent)) :name "voice-agent"))
    (when greeting (agent-post agent (list :say greeting)))
    agent))

(defun stop-agent (agent)
  "Stop the agent's threads.  The RTP session is left to its owner."
  (bt:with-lock-held ((agent-lock agent))
    (setf (agent-running agent) nil)
    (bt:condition-notify (agent-wakeup agent)))
  (incf (agent-generation agent))
  (abort-response (agent-llm agent))
  (audio-out-stop (agent-out agent))
  agent)
