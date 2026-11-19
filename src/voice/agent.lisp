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
   (tools :initarg :tools :initform '() :reader agent-tools
          :documentation "Tool providers for this call only (see *CALL-TOOLS*).")
   (event-type :initarg :event-type :initform nil :reader agent-event-type
               :documentation "Payload type of telephone events on the call, if any.")
   (on-digit :initarg :on-digit :initform nil :reader agent-on-digit
             :documentation "(lambda (character)) called once for each key the
caller presses.")
   (last-event :initform nil :accessor agent-last-event
               :documentation "RTP timestamp of the key press last reported.")
   (waiting :initform nil :accessor agent-waiting
            :documentation "True while the caller is waiting for someone to pick
up: what they say meanwhile is not for the agent.")
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

(defun agent-handle-key (agent packet)
  "A telephone event (RFC 4733).  One key press arrives as several packets
with the same timestamp; it is reported once."
  (let ((event (ignore-errors
                (rtp:decode-telephone-event (rtp:rtp-packet-payload packet))))
        (timestamp (rtp:rtp-packet-timestamp packet)))
    (when (and event (not (eql timestamp (agent-last-event agent))))
      (setf (agent-last-event agent) timestamp)
      (let ((key (rtp:dtmf-event-char (rtp:telephone-event-event event))))
        (when (and key (agent-on-digit agent))
          ;; A caller who starts dialling has stopped listening.
          (unless (agent-waiting agent) (agent-interrupt agent))
          (ignore-errors (funcall (agent-on-digit agent) key)))))))

(defun agent-handle-packet (agent packet)
  (when (and (agent-event-type agent)
             (eql (rtp:rtp-packet-payload-type packet) (agent-event-type agent)))
    (return-from agent-handle-packet (agent-handle-key agent packet)))
  (let ((decoder (case (rtp:rtp-packet-payload-type packet)
                   (0 #'rtp:ulaw-decode)
                   (8 #'rtp:alaw-decode))))
    (when (and decoder (not (agent-waiting agent))
               (plusp (length (rtp:rtp-packet-payload packet))))
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

(defun speakable (text)
  "TEXT without the formatting marks a model lets slip, which would be read
aloud: emphasis, headings, code quotes and wiki-link brackets."
  (string-trim " " (remove-if (lambda (c) (find c "*#`[]")) text)))

(defun agent-speak (agent text generation)
  "Synthesize TEXT and queue it, unless the caller has interrupted since
GENERATION.  Returns true when the audio was queued."
  (when (and (= generation (agent-generation agent))
             (find-if #'alphanumericp (setf text (speakable text))))
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
                  (let ((*call-tools* (agent-tools agent)))
                    (respond (agent-llm agent) (agent-messages agent)
                             (lambda (piece)
                               (when (= generation (agent-generation agent))
                                 ;; Speak each sentence as soon as it is complete.
                                 (multiple-value-bind (sentences rest)
                                     (split-sentences (concatenate 'string pending piece))
                                   (setf pending rest)
                                   (mapc #'say sentences))))))
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

;;; Waiting for someone to pick up

(defun ringback-samples (seconds)
  "The North American ringing tone: 440 and 480 Hz, two seconds on and four
off, for SECONDS."
  (let ((samples (make-samples (* 8000 seconds))))
    (dotimes (i (length samples) samples)
      (when (< (mod i 48000) 16000)
        (let ((time (/ i 8000d0)))
          (setf (aref samples i)
                (round (* 3000 (+ (sin (* 2 pi 440 time)) (sin (* 2 pi 480 time)))))))))))

(defun agent-wait (agent)
  "Stop listening to the caller: they are waiting for someone else."
  (setf (agent-waiting agent) t))

(defun agent-ringback (agent seconds)
  "Play the ringing tone after whatever is being said, while the caller waits."
  (when (agent-waiting agent)
    (audio-out-enqueue (agent-out agent) (ringback-samples seconds))))

(defun agent-resume (agent)
  "The wait is over and the caller is still with the agent."
  (when (agent-waiting agent)
    (audio-out-flush (agent-out agent))
    (setf (agent-waiting agent) nil)))

(defun agent-say (agent text)
  "Have the agent say TEXT when it is next free."
  (agent-post agent (list :say text)))

(defun agent-loop (agent)
  (loop for item = (agent-take agent)
        while item
        do (setf (agent-busy agent) t)
           (handler-case
               (ecase (first item)
                 (:say (agent-speak agent (second item) (agent-generation agent)))
                 (:ringback (agent-ringback agent (second item)))
                 (:utterance (agent-turn agent (second item))))
             (error (e) (agent-note agent :error (princ-to-string e))))
           (setf (agent-busy agent) nil)))

;;; Lifecycle

(defun start-agent (session &key stt llm tts (codec "PCMU") greeting (barge-in t) log vad
                             tools event-type on-digit)
  "Attach a voice agent to the started RTP SESSION, whose remote address is
already set.  CODEC is the negotiated audio codec name (PCMU or PCMA).
TOOLS are tool providers for this call alone.  ON-DIGIT is called with each
key the caller presses, when EVENT-TYPE is the payload type the call uses
for telephone events."
  (let* ((agent (make-instance
                 'voice-agent
                 :session session
                 :out (make-audio-out session
                                      :encoder (if (string-equal codec "PCMA")
                                                   #'rtp:alaw-encode
                                                   #'rtp:ulaw-encode))
                 :vad (or vad (make-vad))
                 :stt stt :llm llm :tts tts :barge-in barge-in :log log
                 :tools tools :event-type event-type :on-digit on-digit)))
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
