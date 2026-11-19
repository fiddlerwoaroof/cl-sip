(in-package :cl-sip.voice)

;;;; The three pluggable stages of a voice turn.
;;;;
;;;;   (transcribe stt samples rate)        -> text or NIL
;;;;   (respond llm messages on-text)       -> full text, stop reason
;;;;   (synthesize tts text)                -> 8 kHz samples
;;;;
;;;; MESSAGES is a list of (role . text) with role :user or :assistant.

(defgeneric transcribe (stt samples rate)
  (:documentation "Speech to text.  Returns NIL when nothing was said."))

(defgeneric synthesize (tts text)
  (:documentation "Text to speech.  Returns 16-bit samples at 8 kHz."))

(defgeneric respond (llm messages on-text)
  (:documentation "Generate a reply to MESSAGES, calling ON-TEXT with each
piece of text as it arrives.  Returns the whole reply and the stop reason."))

(defgeneric abort-response (llm)
  (:documentation "Stop a RESPOND in progress on another thread, if any.")
  (:method (llm) (declare (ignore llm)) nil))

;;; Plain functions as backends (tests, experiments)

(defclass function-stt () ((function :initarg :function)))
(defun make-function-stt (function) (make-instance 'function-stt :function function))
(defmethod transcribe ((stt function-stt) samples rate)
  (funcall (slot-value stt 'function) samples rate))

(defclass function-tts () ((function :initarg :function)))
(defun make-function-tts (function) (make-instance 'function-tts :function function))
(defmethod synthesize ((tts function-tts) text)
  (funcall (slot-value tts 'function) text))

(defclass function-llm () ((function :initarg :function)))
(defun make-function-llm (function)
  "FUNCTION takes the message list and returns the reply as a string."
  (make-instance 'function-llm :function function))
(defmethod respond ((llm function-llm) messages on-text)
  (let ((text (funcall (slot-value llm 'function) messages)))
    (funcall on-text text)
    (values text "end_turn")))

;;; Speech to text: whisper.cpp

(defclass whisper-stt ()
  ((program :initarg :program :initform (or (uiop:getenv "WHISPER_CLI") "whisper-cli"))
   (model :initarg :model :initform (uiop:getenv "WHISPER_MODEL"))
   (language :initarg :language :initform "en")))

(defun make-whisper-stt (&rest args &key program model language)
  "Transcribe with whisper.cpp's whisper-cli.  MODEL is the path to a ggml
model file and defaults to $WHISPER_MODEL."
  (declare (ignore program model language))
  (apply #'make-instance 'whisper-stt args))

(defun clean-transcript (text)
  "Drop whisper's non-speech annotations such as [BLANK_AUDIO] or (wind)."
  (let ((out (make-string-output-stream))
        (depth 0))
    (loop for c across text
          do (cond ((find c "[(") (incf depth))
                   ((find c "])") (setf depth (max 0 (1- depth))))
                   ((zerop depth) (write-char c out))))
    (let ((cleaned (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (substitute #\Space #\Newline (get-output-stream-string out)))))
      (and (find-if #'alphanumericp cleaned) cleaned))))

(defmethod transcribe ((stt whisper-stt) samples rate)
  (with-slots (program model language) stt
    (unless model
      (error "No whisper model: set WHISPER_MODEL or pass :model."))
    (uiop:with-temporary-file (:pathname path :type "wav")
      ;; Whisper wants 16 kHz input.
      (write-wav path (resample samples rate 16000) 16000)
      (clean-transcript
       (uiop:run-program (list program "-m" (namestring model) "-f" (namestring path)
                               "-l" language "-nt" "-np")
                         :output :string :error-output nil)))))

;;; Text to speech: macOS `say`, or any command that writes a WAV file

(defclass say-tts ()
  ((voice :initarg :voice :initform nil)
   (rate :initarg :rate :initform nil :documentation "Words per minute.")))

(defun make-say-tts (&key voice rate)
  "Speak with the macOS speech synthesizer.  VOICE is a name from `say -v ?`."
  (make-instance 'say-tts :voice voice :rate rate))

(defmethod synthesize ((tts say-tts) text)
  (with-slots (voice rate) tts
    (uiop:with-temporary-file (:pathname wav :type "wav")
      (uiop:with-temporary-file (:pathname input :type "txt")
        (with-open-file (out input :direction :output :if-exists :supersede
                                   :external-format :utf-8)
          (write-string text out))
        (uiop:run-program `("say" ,@(when voice (list "-v" voice))
                                  ,@(when rate (list "-r" (princ-to-string rate)))
                                  "-o" ,(namestring wav) "--data-format=LEI16@8000"
                                  "-f" ,(namestring input))
                          :output nil :error-output nil)
        (multiple-value-bind (samples wav-rate) (read-wav wav)
          (resample samples wav-rate 8000))))))

(defclass command-tts ()
  ((command :initarg :command)))

(defun make-command-tts (command)
  "Speak with an external program, for instance a local voice-cloning model.
COMMAND is a list of arguments in which :TEXT is replaced by the text to
speak and :OUTPUT by the path of the WAV file the program must write."
  (make-instance 'command-tts :command command))

(defmethod synthesize ((tts command-tts) text)
  (uiop:with-temporary-file (:pathname wav :type "wav")
    (uiop:run-program (loop for item in (slot-value tts 'command)
                            collect (case item
                                      (:text text)
                                      (:output (namestring wav))
                                      (t item)))
                      :output nil :error-output nil)
    (multiple-value-bind (samples rate) (read-wav wav)
      (resample samples rate 8000))))

;;; Text to speech: a long-running worker process, so that a large model is
;;; loaded once rather than for every sentence

(defclass worker-tts ()
  ((command :initarg :command)
   (process :initform nil)
   (lock :initform (bt:make-lock "worker-tts"))))

(defun worker-read-reply (process)
  "Next JSON line from the worker; other output is skipped."
  (loop for line = (read-line (uiop:process-info-output process) nil nil)
        do (cond ((null line) (error "The speech worker exited unexpectedly."))
                 ((alexandria:starts-with-subseq "{" line) (return (jzon:parse line))))))

(defun make-worker-tts (command)
  "Speak through a worker process started with COMMAND (a list of strings)
and kept running.  The worker announces itself with a JSON line containing
\"ready\", then answers each request line {\"text\": ..., \"out\": path} by
writing a WAV file to that path and replying {\"ok\": true}.  Blocks until
the worker is ready."
  (let ((tts (make-instance 'worker-tts :command command)))
    (with-slots (process) tts
      (setf process (uiop:launch-program command :input :stream :output :stream
                                                 :error-output nil))
      (unless (gethash "ready" (worker-read-reply process))
        (error "The speech worker did not start: ~s" command)))
    tts))

(defun make-chatterbox-tts (&key voice script)
  "Speak with the Chatterbox Turbo model through tools/chatterbox_tts.py,
run by uv.  VOICE is a reference recording of the voice to imitate; without
it the model's built-in voice is used.  Loading takes around half a minute."
  (make-worker-tts
   `("uv" "run" "--quiet"
          ,(namestring (or script
                           (asdf:system-relative-pathname "cl-sip" "tools/chatterbox_tts.py")))
          ,@(when voice (list "--voice" (namestring voice))))))

(defmethod synthesize ((tts worker-tts) text)
  (with-slots (process lock) tts
    (uiop:with-temporary-file (:pathname wav :type "wav")
      (bt:with-lock-held (lock)
        (let ((input (uiop:process-info-input process)))
          (write-line (jzon:stringify (json-object "text" text "out" (namestring wav)))
                      input)
          (finish-output input))
        (let ((reply (worker-read-reply process)))
          (unless (eq (gethash "ok" reply) t)
            (error "Speech synthesis failed: ~a" (gethash "error" reply)))))
      (multiple-value-bind (samples rate) (read-wav wav)
        (resample samples rate 8000)))))

(defgeneric stop-backend (backend)
  (:documentation "Release whatever BACKEND holds (worker processes).")
  (:method (backend) (declare (ignore backend)) nil))

(defmethod stop-backend ((tts worker-tts))
  (with-slots (process) tts
    (when process
      (ignore-errors (close (uiop:process-info-input process)))
      (ignore-errors (uiop:terminate-process process))
      (ignore-errors (uiop:wait-process process))
      (setf process nil))))

;;; Language model: Claude over the Messages API, streamed

(define-condition llm-error (error)
  ((message :initarg :message :reader llm-error-message))
  (:report (lambda (c s) (format s "Language model request failed: ~a" (llm-error-message c)))))

(defparameter *phone-system-prompt*
  (format nil "You are a voice assistant talking with someone on a telephone call. ~
Everything you write is converted to speech, and everything they say reaches ~
you through speech recognition, so expect the occasional misheard word and ~
ask when something does not make sense.

Speak the way a person does on the phone: plain conversational sentences, ~
usually one to three per turn. Give the answer first and offer more detail ~
only if it seems wanted. Never use formatting of any kind, because it would ~
be read aloud: no lists, headings, asterisks, emoji or code. Write numbers, ~
dates and abbreviations the way they should be pronounced."))

(defclass claude-llm ()
  ((model :initarg :model :initform "claude-haiku-5-5")
   (system :initarg :system :initform *phone-system-prompt*)
   (max-tokens :initarg :max-tokens :initform 1024)
   (effort :initarg :effort :initform "low")
   (thinking :initarg :thinking :initform nil
             :documentation "NIL turns thinking off, for the quickest first word.")
   (api-key :initarg :api-key :initform nil
            :documentation "Defaults to $ANTHROPIC_API_KEY, read at request time.")
   (program :initarg :program :initform "curl")
   (url :initarg :url :initform "https://api.anthropic.com/v1/messages")
   (process :initform nil)))

(defun make-claude-llm (&rest args &key model system max-tokens effort thinking api-key
                                     program url)
  "Reply with a Claude model (Claude Haiku 5.5 by default) through the
Messages API, streaming so speech can start with the first sentence."
  (declare (ignore model system max-tokens effort thinking api-key program url))
  (apply #'make-instance 'claude-llm args))

(defun json-object (&rest plist)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash key table) value))
    table))

(defun claude-request-body (llm messages)
  (with-slots (model system max-tokens effort thinking) llm
    (jzon:stringify
     (json-object
      "model" model
      "max_tokens" max-tokens
      "stream" t
      "system" system
      "thinking" (json-object "type" (if thinking "adaptive" "disabled"))
      "output_config" (json-object "effort" effort)
      "messages" (map 'vector
                      (lambda (m)
                        (json-object "role" (string-downcase (symbol-name (car m)))
                                     "content" (cdr m)))
                      messages)))))

(defun handle-stream-event (event on-text)
  "Process one decoded server-sent event.  Returns the stop reason when the
event carries one."
  (let ((type (gethash "type" event)))
    (cond
      ((equal type "content_block_delta")
       (let ((delta (gethash "delta" event)))
         (when (equal (gethash "type" delta) "text_delta")
           (funcall on-text (gethash "text" delta))))
       nil)
      ((equal type "message_delta")
       (let ((reason (gethash "stop_reason" (gethash "delta" event))))
         (and (stringp reason) reason)))
      ((equal type "error")
       (let ((detail (gethash "error" event)))
         (error 'llm-error
                :message (format nil "~a: ~a" (gethash "type" detail)
                                 (gethash "message" detail))))))))

(defmethod respond ((llm claude-llm) messages on-text)
  (with-slots (api-key program url process) llm
    (let ((key (or api-key (uiop:getenv "ANTHROPIC_API_KEY")))
          (text (make-string-output-stream))
          (stop-reason nil))
      (unless (and key (plusp (length key)))
        (error 'llm-error :message "ANTHROPIC_API_KEY is not set"))
      (uiop:with-temporary-file (:pathname body :type "json")
        (with-open-file (out body :direction :output :if-exists :supersede
                                  :external-format :utf-8)
          (write-string (claude-request-body llm messages) out))
        ;; The request, key included, goes to curl on standard input so that
        ;; it never appears in the process list.
        (let ((curl (uiop:launch-program (list program "--silent" "--show-error"
                                               "--no-buffer" "--config" "-")
                                         :input :stream :output :stream
                                         :error-output nil)))
          (setf process curl)
          (unwind-protect
               (progn
                 (let ((config (uiop:process-info-input curl)))
                   (format config "url = \"~a\"~%" url)
                   (format config "header = \"x-api-key: ~a\"~%" key)
                   (format config "header = \"anthropic-version: 2026-10-21\"~%")
                   (format config "header = \"content-type: application/json\"~%")
                   (format config "data-binary = \"@~a\"~%" (namestring body))
                   (close config))
                 (loop with output = (uiop:process-info-output curl)
                       for line = (read-line output nil nil)
                       while line
                       do (let ((line (string-right-trim '(#\Return) line)))
                            (cond
                              ((alexandria:starts-with-subseq "data:" line)
                               (let ((reason (handle-stream-event
                                              (jzon:parse (subseq line 5))
                                              (lambda (piece)
                                                (write-string piece text)
                                                (funcall on-text piece)))))
                                 (when reason (setf stop-reason reason))))
                              ;; A failed request answers with one JSON object
                              ;; instead of an event stream.
                              ((alexandria:starts-with-subseq "{" line)
                               (handle-stream-event (jzon:parse line) on-text))))))
            (setf process nil)
            (when (uiop:process-alive-p curl)
              (uiop:terminate-process curl))
            (uiop:wait-process curl)
            (ignore-errors (uiop:close-streams curl)))))
      (values (get-output-stream-string text) stop-reason))))

(defmethod abort-response ((llm claude-llm))
  (let ((process (slot-value llm 'process)))
    (when process
      (ignore-errors (uiop:terminate-process process)))))

;;; Cutting a text stream into speakable pieces

(defun split-sentences (text)
  "Split TEXT at sentence ends.  Returns the complete sentences and the
unfinished remainder, so a streamed reply can be spoken sentence by
sentence."
  (let ((sentences '())
        (start 0)
        (n (length text)))
    (loop for i from 0 below n
          do (let ((c (char text i)))
               (when (or (char= c #\Newline)
                         (and (find c ".!?")
                              (< (1+ i) n)
                              (find (char text (1+ i)) '(#\Space #\Newline))
                              ;; Not after "Mr." or "e.g.": too short to be a sentence.
                              (> (- i start) 3)))
                 (let ((sentence (string-trim '(#\Space #\Newline) (subseq text start (1+ i)))))
                   (when (find-if #'alphanumericp sentence)
                     (push sentence sentences)))
                 (setf start (1+ i)))))
    (values (nreverse sentences) (subseq text start))))
