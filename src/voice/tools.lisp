(in-package :cl-sip.voice)

;;;; Tools the language model may call during a turn.
;;;;
;;;;   (list-tools provider)             -> tool definitions
;;;;   (call-tool provider name input)   -> result text, error flag
;;;;   (tool-instructions provider)      -> advice for the system prompt
;;;;   (tool-context provider query)     -> material found for what was said
;;;;   (tool-reference provider)         -> standing reference material
;;;;
;;;; A definition is a JSON object with "name", "description" and
;;;; "input_schema", as the Messages API takes them.  INPUT is the decoded
;;;; JSON object of arguments.

(defun json-object (&rest plist)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash key table) value))
    table))

(defgeneric list-tools (provider)
  (:documentation "The definitions of the tools PROVIDER offers."))

(defgeneric tool-instructions (provider)
  (:documentation "Advice to the model on using PROVIDER's tools, or NIL.")
  (:method (provider) (declare (ignore provider)) nil))

(defgeneric tool-context (provider query)
  (:documentation "Material bearing on QUERY, what the caller has just said,
to put before the model together with it; or NIL.")
  (:method (provider query) (declare (ignore provider query)) nil))

(defgeneric tool-reference (provider)
  (:documentation "Reference material that stays the same from turn to turn,
a catalog for instance, to follow the system prompt; or NIL.  Being constant
it can be cached by the model's API, so it may be long.")
  (:method (provider) (declare (ignore provider)) nil))

(defgeneric call-tool (provider name input)
  (:documentation "Run the tool NAME with the arguments INPUT.  Returns the
result as text and true when the tool reported a failure."))

;;; A Lisp function as a tool (tests, small local tools)

(defclass function-tool ()
  ((definition :initarg :definition)
   (function :initarg :function)
   (instructions :initarg :instructions :initform nil :reader tool-instructions)))

(defun make-function-tool (name description function &key schema instructions)
  "A tool that calls FUNCTION with the decoded arguments and returns its
value, a string.  SCHEMA is the JSON schema of the arguments; INSTRUCTIONS
is added to the model's system prompt."
  (make-instance 'function-tool
                 :function function :instructions instructions
                 :definition (json-object
                              "name" name
                              "description" description
                              "input_schema" (or schema
                                                 (json-object "type" "object"
                                                              "properties" (json-object))))))

(defmethod list-tools ((tool function-tool))
  (list (slot-value tool 'definition)))

(defmethod call-tool ((tool function-tool) name input)
  (declare (ignore name))
  (values (funcall (slot-value tool 'function) input) nil))

;;; A Model Context Protocol server over HTTP

(define-condition mcp-error (error)
  ((message :initarg :message :reader mcp-error-message)
   (status :initarg :status :initform nil :reader mcp-error-status
           :documentation "The HTTP status, when that is what went wrong."))
  (:report (lambda (c s) (format s "MCP request failed: ~a" (mcp-error-message c)))))

(defparameter *mcp-protocol-version* "2025-06-18")

(defclass mcp-server ()
  ((name :initarg :name :initform "mcp" :reader mcp-server-name)
   (url :initarg :url)
   (token :initarg :token :initform nil
          :documentation "Sent as \"Authorization: Bearer TOKEN\".")
   (program :initarg :program :initform "curl")
   (timeout :initarg :timeout :initform 15 :documentation "Seconds per tool call.")
   (quick-timeout :initarg :quick-timeout :initform 5
                  :documentation "Seconds for connecting, listing and seeding.")
   (retry-delay :initarg :retry-delay :initform 60
                :documentation "Seconds a server that did not answer is left alone,
so that a dead one costs a caller one wait and not one per turn.")
   (retry-after :initform 0)
   (seed :initarg :seed :initform nil
         :documentation "How to gather context for each thing the caller says:
a plist of :TOOL, the tool to call; :ARGUMENT, the name of the argument that
takes the caller's words; :EXTRA, further arguments as a plist; and :HEADER,
text put before the result.")
   (instructions :initarg :instructions :initform nil :reader tool-instructions
                 :documentation "Advice to the model on using these tools.")
   (exclude :initarg :exclude :initform '()
            :documentation "Names of tools the server has that are not offered.")
   (pinned :initarg :pinned :initform '()
           :documentation "Documents kept before the model throughout: a list of
(title tool . arguments), each fetched by calling TOOL with the plist
ARGUMENTS.")
   (reference :initform nil)
   (reference-time :initform 0)
   (reference-life :initarg :reference-life :initform 600
                   :documentation "Seconds before the pinned documents are fetched afresh.")
   (session-id :initform nil)
   (protocol-version :initform nil)
   (connected :initform nil)
   (tools :initform nil)
   (next-id :initform 0)
   (lock :initform (bt:make-lock "mcp-server"))))

(defun make-mcp-server (&rest args &key name url token program timeout quick-timeout
                                     retry-delay exclude instructions seed pinned
                                     reference-life)
  "The tools of an MCP server reached over HTTP at URL, optionally with a
bearer TOKEN, except those named in EXCLUDE: a tool that takes many seconds
is no use to someone waiting on the telephone.  Nothing is sent until the
tools are first needed.  INSTRUCTIONS, a string, is added to the model's
system prompt to say how the tools are meant to be used.  SEED names a tool
to run on whatever the caller says, a search usually, whose result reaches
the model along with their words:

  (:tool \"search\" :argument \"query\" :extra (\"n\" 6) :header \"Pages found:\")

PINNED lists documents the model should have before it all the time, each
as (title tool . arguments):

  ((\"The catalog of pages\" \"read_page\" \"ref\" \"index\"))"
  (declare (ignore name url token program timeout quick-timeout retry-delay exclude
                   instructions seed pinned reference-life))
  (apply #'make-instance 'mcp-server args))

(defun curl-config-value (value)
  "VALUE quoted for a curl configuration file."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for c across (princ-to-string value)
          do (cond ((find c "\\\"") (write-char #\\ out) (write-char c out))
                   ((< (char-code c) 32))
                   (t (write-char c out))))
    (write-char #\" out)))

(defun mcp-parse-headers (text)
  "The status and session id from the response headers curl saved."
  (let ((status nil) (session nil))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (let* ((line (string-trim '(#\Return #\Space) line))
             (colon (position #\: line)))
        (cond ((alexandria:starts-with-subseq "HTTP/" line)
               (let ((space (position #\Space line)))
                 (setf status (and space
                                   (parse-integer line :start (1+ space) :junk-allowed t)))))
              ((and colon (string-equal "mcp-session-id" (subseq line 0 colon)))
               (setf session (string-trim " " (subseq line (1+ colon))))))))
    (values status session)))

(defun mcp-parse-body (text)
  "The JSON-RPC messages in a response body, which is either one JSON
document or an event stream."
  (flet ((parse (string) (ignore-errors (jzon:parse string))))
    (let ((trimmed (string-left-trim '(#\Space #\Tab #\Newline #\Return) text)))
      (cond ((zerop (length trimmed)) '())
            ((find (char trimmed 0) "{[")
             (let ((value (parse trimmed)))
               (cond ((hash-table-p value) (list value))
                     ((stringp value) '())
                     (t (coerce value 'list)))))
            (t
             (loop for line in (uiop:split-string text :separator '(#\Newline))
                   for message = (and (alexandria:starts-with-subseq "data:" line)
                                      (parse (subseq line 5)))
                   when (hash-table-p message) collect message))))))

(defvar *mcp-timeout* nil
  "Overrides a server's timeout, in seconds, for the requests made within.")

(defun mcp-post (server message)
  "Send one JSON-RPC MESSAGE.  Returns the HTTP status and the messages in
the answer."
  (with-slots (url token program timeout session-id protocol-version
               retry-after retry-delay) server
    (uiop:with-temporary-file (:pathname body :type "json")
      (uiop:with-temporary-file (:pathname headers :type "txt")
        (with-open-file (out body :direction :output :if-exists :supersede
                                  :external-format :utf-8)
          (write-string (jzon:stringify message) out))
        ;; As with the model, the token goes to curl on standard input so
        ;; that it never appears in the process list.
        (let ((config
                (with-output-to-string (out)
                  (flet ((line (key value)
                           (format out "~a = ~a~%" key (curl-config-value value))))
                    (line "url" url)
                    (line "max-time" (or *mcp-timeout* timeout))
                    (line "header" "content-type: application/json")
                    (line "header" "accept: application/json, text/event-stream")
                    (when token
                      (line "header" (format nil "Authorization: Bearer ~a" token)))
                    (when session-id
                      (line "header" (format nil "Mcp-Session-Id: ~a" session-id)))
                    (when protocol-version
                      (line "header" (format nil "MCP-Protocol-Version: ~a" protocol-version)))
                    (line "dump-header" (namestring headers))
                    (line "data-binary" (format nil "@~a" (namestring body)))))))
          (multiple-value-bind (output error-output code)
              (with-input-from-string (input config)
                (uiop:run-program (list program "--silent" "--show-error" "--config" "-")
                                  :input input :output :string :error-output :string
                                  :ignore-error-status t))
            (unless (eql code 0)
              (setf retry-after (+ (get-universal-time) retry-delay))
              (error 'mcp-error
                     :message (string-trim '(#\Space #\Newline) (or error-output ""))))
            (multiple-value-bind (status session)
                (mcp-parse-headers (uiop:read-file-string headers))
              (when session (setf session-id session))
              (values (or status 200) (mcp-parse-body output)))))))))

(defun mcp-request (server method &optional params)
  "Call METHOD and return its result."
  (multiple-value-bind (status messages)
      (mcp-post server (json-object "jsonrpc" "2.0"
                                    "id" (incf (slot-value server 'next-id))
                                    "method" method
                                    "params" (or params (json-object))))
    (let ((reply (find-if-not (lambda (m) (nth-value 1 (gethash "method" m))) messages)))
      (cond ((not (<= 200 status 299))
             (error 'mcp-error
                    :status status
                    :message (format nil "HTTP ~d~@[: ~a~]" status
                                     (and reply
                                          (let ((detail (gethash "error" reply)))
                                            (if (hash-table-p detail)
                                                (gethash "message" detail)
                                                detail))))))
            ((null reply)
             (error 'mcp-error :message (format nil "no answer to ~a" method)))
            ((gethash "error" reply)
             (error 'mcp-error :message (gethash "message" (gethash "error" reply))))
            (t (gethash "result" reply))))))

(defun mcp-connect (server)
  (with-slots (session-id protocol-version connected) server
    (setf session-id nil protocol-version nil connected nil)
    (let ((result (mcp-request
                   server "initialize"
                   (json-object "protocolVersion" *mcp-protocol-version*
                                "capabilities" (json-object)
                                "clientInfo" (json-object "name" "cl-sip-voice"
                                                          "version" "0.1")))))
      (setf protocol-version (or (gethash "protocolVersion" result)
                                 *mcp-protocol-version*))
      (mcp-post server (json-object "jsonrpc" "2.0"
                                    "method" "notifications/initialized"))
      (setf connected t))))

(defun mcp-call (server method &optional params)
  "MCP-REQUEST on a connected server.  A server that has forgotten the
session, having restarted for instance, is connected to again."
  (with-slots (connected retry-after quick-timeout) server
    (when (< (get-universal-time) retry-after)
      (error 'mcp-error
             :message (format nil "the server was not answering; trying again in ~d s"
                              (- retry-after (get-universal-time)))))
    (flet ((attempt ()
             (unless connected
               (let ((*mcp-timeout* quick-timeout)) (mcp-connect server)))
             (mcp-request server method params)))
      (handler-case (attempt)
        (mcp-error (e)
          (unless (member (mcp-error-status e) '(400 404))
            (error e))
          (setf connected nil)
          (attempt))))))

(defmethod list-tools ((server mcp-server))
  (with-slots (tools lock exclude quick-timeout) server
    (bt:with-lock-held (lock)
      (or tools
          (setf tools
                (map 'list
                     (lambda (tool)
                       (json-object
                        "name" (gethash "name" tool)
                        "description" (or (gethash "description" tool) "")
                        "input_schema" (or (gethash "inputSchema" tool)
                                           (json-object "type" "object"))))
                     (remove-if (lambda (tool)
                                  (member (gethash "name" tool) exclude :test #'equal))
                                (gethash "tools" (let ((*mcp-timeout* quick-timeout))
                                                   (mcp-call server "tools/list"))))))))))

(defmethod tool-reference ((server mcp-server))
  (with-slots (pinned reference reference-time reference-life) server
    (when pinned
      (when (or (null reference)
                (> (get-universal-time) (+ reference-time reference-life)))
        (setf reference
              (format nil "~{~a~^~%~%~}"
                      (loop for (title tool . arguments) in pinned
                            collect (multiple-value-bind (text error-p)
                                        (call-tool server tool (apply #'json-object arguments))
                                      (when error-p
                                        (error 'mcp-error :message text))
                                      (format nil "# ~a~%~%~a" title text))))
              reference-time (get-universal-time)))
      reference)))

(defmethod tool-context ((server mcp-server) query)
  (destructuring-bind (&key tool argument extra header) (slot-value server 'seed)
    (when tool
      (multiple-value-bind (text error-p)
          (let ((*mcp-timeout* (slot-value server 'quick-timeout)))
            (call-tool server tool (apply #'json-object argument query extra)))
        (when error-p
          (error 'mcp-error :message text))
        (and (find-if #'alphanumericp text)
             (format nil "~@[~a~%~%~]~a" header text))))))

(defmethod call-tool ((server mcp-server) name input)
  (let ((result (bt:with-lock-held ((slot-value server 'lock))
                  (mcp-call server "tools/call"
                            (json-object "name" name
                                         "arguments" (or input (json-object)))))))
    (values (format nil "~{~a~^~%~}"
                    (loop for item across (or (gethash "content" result) #())
                          when (equal (gethash "type" item) "text")
                            collect (gethash "text" item)))
            (eq (gethash "isError" result) t))))
