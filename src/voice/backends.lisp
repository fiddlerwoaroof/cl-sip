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

(defparameter *tool-prompt*
  (format nil "You can look things up with tools. A lookup takes a few ~
seconds, during which the caller hears nothing, so say a few words first, ~
such as \"Let me check.\" Then answer from what you found in your own words, ~
briefly; never read out long passages, page addresses or raw data.")
  "Added to the system prompt when the model has tools.")

(defclass claude-llm ()
  ((model :initarg :model :initform "claude-haiku-5-5")
   (system :initarg :system :initform *phone-system-prompt*)
   (max-tokens :initarg :max-tokens :initform 1024)
   (effort :initarg :effort :initform "low")
   (thinking :initarg :thinking :initform nil
             :documentation "NIL turns thinking off, for the quickest first word.")
   (api-key :initarg :api-key :initform nil
            :documentation "Defaults to $ANTHROPIC_API_KEY, read at request time.")
   (tools :initarg :tools :initform '()
          :documentation "Tool providers (see LIST-TOOLS) the model may call.")
   (max-tool-rounds :initarg :max-tool-rounds :initform 4
                    :documentation "Rounds of tool calls allowed in one reply.")
   (max-tool-result :initarg :max-tool-result :initform 12000
                    :documentation "Characters of a tool result passed to the model.")
   (log :initarg :log :initform nil :accessor llm-log
        :documentation "(lambda (who text)) told of tool calls and failures;
WHO is :tool or :error.")
   (program :initarg :program :initform "curl")
   (url :initarg :url :initform "https://api.anthropic.com/v1/messages")
   (process :initform nil)
   (aborted :initform nil)))

(defun make-claude-llm (&rest args &key model system max-tokens effort thinking api-key
                                     tools max-tool-rounds max-tool-result log
                                     program url)
  "Reply with a Claude model (Claude Haiku 5.5 by default) through the
Messages API, streaming so speech can start with the first sentence.  TOOLS
is a list of tool providers, such as MAKE-MCP-SERVER returns, that the model
may call while replying."
  (declare (ignore model system max-tokens effort thinking api-key tools max-tool-rounds
                   max-tool-result log program url))
  (apply #'make-instance 'claude-llm args))

(defun llm-note (llm who control &rest args)
  (when (llm-log llm)
    (ignore-errors (funcall (llm-log llm) who (apply #'format nil control args)))))

(defun claude-request-body (llm messages &key tools instructions reference
                                           (allow-tools t))
  "MESSAGES are (role . text) pairs or ready-made message objects.  TOOLS
are tool definitions, INSTRUCTIONS their providers' advice on using them and
REFERENCE their standing reference material; with ALLOW-TOOLS false the
model must answer without calling one."
  (with-slots (model system max-tokens effort thinking) llm
    (jzon:stringify
     (apply #'json-object
            "model" model
            "max_tokens" max-tokens
            "stream" t
            "system" (let ((text (if tools
                                     (format nil "~a~%~%~a~{~%~%~a~}" system *tool-prompt*
                                             instructions)
                                     system)))
                       (if reference
                           ;; Long and the same every turn: cached, with the
                           ;; tool definitions before it, so that only the
                           ;; conversation is read afresh.
                           (vector (json-object
                                    "type" "text"
                                    "text" (format nil "~a~{~%~%~a~}" text reference)
                                    "cache_control" (json-object "type" "ephemeral")))
                           text))
            "thinking" (json-object "type" (if thinking "adaptive" "disabled"))
            "output_config" (json-object "effort" effort)
            "messages" (map 'vector
                            (lambda (m)
                              (if (hash-table-p m)
                                  m
                                  (json-object "role" (string-downcase (symbol-name (car m)))
                                               "content" (cdr m))))
                            messages)
            (when tools
              (list "tools" (coerce tools 'vector)
                    "tool_choice" (json-object "type" (if allow-tools "auto" "none"))))))))

(defun handle-stream-event (event on-text &optional blocks)
  "Process one decoded server-sent event.  Returns the stop reason when the
event carries one.  The content blocks of the reply are assembled in the
hash table BLOCKS, by index, when it is given."
  (let* ((type (gethash "type" event))
         (block (and blocks (gethash (gethash "index" event) blocks))))
    (flet ((extend (key piece)
             (when block
               (setf (gethash key block)
                     (concatenate 'string (or (gethash key block) "") piece)))))
      (cond
        ((equal type "content_block_start")
         (when blocks
           (let ((new (gethash "content_block" event)))
             ;; A tool's arguments arrive as pieces of JSON text.
             (when (equal (gethash "type" new) "tool_use")
               (setf (gethash "input" new) ""))
             (setf (gethash (gethash "index" event) blocks) new)))
         nil)
        ((equal type "content_block_delta")
         (let* ((delta (gethash "delta" event))
                (kind (gethash "type" delta)))
           (cond ((equal kind "text_delta")
                  (extend "text" (gethash "text" delta))
                  (funcall on-text (gethash "text" delta)))
                 ((equal kind "input_json_delta")
                  (extend "input" (gethash "partial_json" delta)))
                 ((equal kind "thinking_delta")
                  (extend "thinking" (gethash "thinking" delta)))
                 ((equal kind "signature_delta")
                  (when block
                    (setf (gethash "signature" block) (gethash "signature" delta))))))
         nil)
        ((equal type "content_block_stop")
         (when (and block (equal (gethash "type" block) "tool_use"))
           (let ((json (gethash "input" block)))
             (setf (gethash "input" block)
                   (if (and (stringp json) (find #\{ json))
                       (jzon:parse json)
                       (json-object)))))
         nil)
        ((equal type "message_delta")
         (let ((reason (gethash "stop_reason" (gethash "delta" event))))
           (and (stringp reason) reason)))
        ((equal type "error")
         (let ((detail (gethash "error" event)))
           (error 'llm-error
                  :message (format nil "~a: ~a" (gethash "type" detail)
                                   (gethash "message" detail)))))))))

(defun claude-stream (llm key body on-text)
  "Send the request BODY and follow the streamed reply.  Returns its content
blocks in order and the stop reason."
  (with-slots (program url process) llm
    (let ((blocks (make-hash-table))
          (stop-reason nil))
      (uiop:with-temporary-file (:pathname file :type "json")
        (with-open-file (out file :direction :output :if-exists :supersede
                                  :external-format :utf-8)
          (write-string body out))
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
                   (format config "data-binary = \"@~a\"~%" (namestring file))
                   (close config))
                 (loop with output = (uiop:process-info-output curl)
                       for line = (read-line output nil nil)
                       while line
                       do (let ((line (string-right-trim '(#\Return) line)))
                            (cond
                              ((alexandria:starts-with-subseq "data:" line)
                               (let ((reason (handle-stream-event
                                              (jzon:parse (subseq line 5)) on-text blocks)))
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
      (values (loop for index in (sort (alexandria:hash-table-keys blocks) #'<)
                    for block = (gethash index blocks)
                    ;; An empty text block may not be sent back.
                    unless (and (equal (gethash "type" block) "text")
                                (equal (gethash "text" block) ""))
                      collect block)
              stop-reason))))

(defvar *call-tools* '()
  "Tool providers that belong to the call being answered, besides the
model's own: bound by the voice agent around RESPOND.")

(defun claude-tools (llm)
  "The definitions of every tool LLM may call, an alist from tool name to
provider, and the providers' instructions.  A provider that cannot be reached
is left out."
  (let ((definitions '())
        (providers '())
        (instructions '()))
    (dolist (provider (append (slot-value llm 'tools) *call-tools*))
      (handler-case
          (dolist (definition (list-tools provider))
            (push definition definitions)
            (push (cons (gethash "name" definition) provider) providers)
            (pushnew (tool-instructions provider) instructions :test #'equal))
        (error (e)
          (llm-note llm :error "tools unavailable: ~a" e))))
    (values (nreverse definitions) providers (remove nil (nreverse instructions)))))

(defun spoken-query (messages)
  "What the caller has just said, with the remark before it so that a
follow-up such as \"who influenced him?\" still finds its subject."
  (let ((said (loop for m in (reverse messages)
                    when (and (consp m) (eq (car m) :user)) collect (cdr m))))
    (when (and said (consp (car (last messages))) (eq (car (car (last messages))) :user))
      (let ((query (format nil "~{~a~^~%~}" (reverse (subseq said 0 (min 2 (length said)))))))
        (subseq query (max 0 (- (length query) 500)))))))

(defun claude-context (llm providers messages)
  "What the tool providers have found for the caller's latest words."
  (let ((query (spoken-query messages)))
    (when query
      (loop for provider in (remove-duplicates (mapcar #'cdr providers))
            for text = (handler-case (tool-context provider query)
                         (error (e)
                           (llm-note llm :error "no context: ~a" e)
                           nil))
            when text
              do (llm-note llm :tool "~d characters of context for ~s" (length text) query)
              and collect text))))

(defun add-context (messages context)
  "MESSAGES with the texts CONTEXT attached to the last one, the caller's."
  (if (null context)
      messages
      (append (butlast messages)
              (let ((last (car (last messages))))
                (list (json-object
                       "role" "user"
                       "content" (map 'vector
                                      (lambda (text) (json-object "type" "text" "text" text))
                                      (cons (cdr last) context))))))))

(defun claude-reference (llm providers)
  "The standing reference material of the tool providers."
  (loop for provider in (remove-duplicates (mapcar #'cdr providers))
        for text = (handler-case (tool-reference provider)
                     (error (e)
                       (llm-note llm :error "no reference material: ~a" e)
                       nil))
        when text collect text))

(defun claude-run-tool (llm providers call)
  "Carry out the tool_use block CALL.  Returns the tool_result block."
  (let ((name (gethash "name" call))
        (input (gethash "input" call))
        (limit (slot-value llm 'max-tool-result)))
    (llm-note llm :tool "~a ~a" name (jzon:stringify input))
    (multiple-value-bind (text error-p)
        (handler-case
            (let ((provider (cdr (assoc name providers :test #'equal))))
              (if provider
                  (call-tool provider name input)
                  (values (format nil "There is no tool named ~a." name) t)))
          (error (e) (values (princ-to-string e) t)))
      (when error-p
        (llm-note llm :error "~a failed: ~a" name text))
      (json-object "type" "tool_result"
                   "tool_use_id" (gethash "id" call)
                   "is_error" (and error-p t)
                   "content" (cond ((zerop (length text)) "(no output)")
                                   ((> (length text) limit)
                                    (format nil "~a~%[truncated]" (subseq text 0 limit)))
                                   (t text))))))

(defmethod respond ((llm claude-llm) messages on-text)
  (with-slots (api-key max-tool-rounds aborted) llm
    (let ((key (or api-key (uiop:getenv "ANTHROPIC_API_KEY")))
          (text (make-string-output-stream))
          (stop-reason nil))
      (unless (and key (plusp (length key)))
        (error 'llm-error :message "ANTHROPIC_API_KEY is not set"))
      (setf aborted nil)
      (multiple-value-bind (tools providers instructions) (claude-tools llm)
        (setf messages (add-context messages (claude-context llm providers messages)))
        (let ((reference (claude-reference llm providers)))
        (flet ((emit (piece)
                 (write-string piece text)
                 (funcall on-text piece)))
          (loop
            for round from 0
            do (multiple-value-bind (blocks reason)
                   (claude-stream llm key
                                  (claude-request-body llm messages
                                                       :tools tools
                                                       :instructions instructions
                                                       :reference reference
                                                       ;; The last round has to be an answer.
                                                       :allow-tools (< round max-tool-rounds))
                                  #'emit)
                 (setf stop-reason reason)
                 (let ((calls (remove "tool_use" blocks
                                      :key (lambda (b) (gethash "type" b))
                                      :test-not #'equal)))
                   (when (or (not (equal reason "tool_use")) (null calls) aborted
                             (>= round max-tool-rounds))
                     (return))
                   ;; End the sentence said so far, so that it is spoken
                   ;; while the tools run.
                   (emit (string #\Newline))
                   (let ((results (map 'vector
                                       (lambda (call) (claude-run-tool llm providers call))
                                       calls)))
                     (when aborted (return))
                     (setf messages
                           (append messages
                                   (list (json-object "role" "assistant"
                                                      "content" (coerce blocks 'vector))
                                         (json-object "role" "user"
                                                      "content" results)))))))))))
      (values (get-output-stream-string text) stop-reason))))

(defmethod abort-response ((llm claude-llm))
  (setf (slot-value llm 'aborted) t)
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
