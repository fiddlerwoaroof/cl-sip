(in-package :cl-sip.test)

(def-suite voice :in cl-sip)
(in-suite voice)

(defun tone (n &key (amplitude 8000) (frequency 440))
  (let ((samples (make-array n :element-type '(signed-byte 16))))
    (dotimes (i n samples)
      (setf (aref samples i) (round (* amplitude (sin (/ (* 2 pi frequency i) 8000))))))))

(defun silence (n)
  (make-array n :element-type '(signed-byte 16) :initial-element 0))

(test wav-round-trip-and-resampling
  (uiop:with-temporary-file (:pathname path :type "wav")
    (let ((samples (tone 800)))
      (voice:write-wav path samples 8000)
      (multiple-value-bind (read rate) (voice:read-wav path)
        (is (= 8000 rate))
        (is (equalp samples read)))))
  ;; Stereo 8-bit input is mixed down and widened.
  (uiop:with-temporary-file (:pathname path :type "wav")
    (with-open-file (out path :element-type '(unsigned-byte 8) :direction :output
                              :if-exists :supersede)
      (write-sequence
       (concatenate 'vector
                    (map 'vector #'char-code "RIFF") #(44 0 0 0) (map 'vector #'char-code "WAVE")
                    ;; An unknown chunk before fmt must be skipped.
                    (map 'vector #'char-code "JUNK") #(2 0 0 0 9 9)
                    (map 'vector #'char-code "fmt ") #(16 0 0 0  1 0  2 0  64 31 0 0
                                                       128 62 0 0  2 0  8 0)
                    (map 'vector #'char-code "data") #(4 0 0 0  128 128  255 129))
       out))
    (multiple-value-bind (read rate) (voice:read-wav path)
      (is (= 8000 rate))
      (is (equalp #(0 16384) read))))
  (is (= 1600 (length (voice:resample (tone 800) 8000 16000))))
  (is (= 400 (length (voice:resample (tone 800) 8000 4000))))
  (is (< 5000 (voice:rms (voice:resample (tone 800) 8000 16000)) 6000))
  (is (zerop (voice:rms (silence 160)))))

(test voice-activity-detection
  (let ((vad (voice:make-vad))
        (events '()))
    (flet ((feed (frames maker)
             (dotimes (i frames)
               (setf events (append events (voice:vad-push vad (funcall maker 160)))))))
      ;; Silence, then a click too short to count, produce nothing.
      (feed 20 #'silence)
      (feed 2 #'tone)
      (feed 50 #'silence)
      (is (null events))
      ;; One second of speech, then quiet: start is reported promptly, the
      ;; utterance after 700 ms of silence.
      (feed 3 #'tone)
      (is (equal '(:start) events))
      (feed 47 #'tone)
      (feed 34 #'silence)
      (is (equal '(:start) events))
      (feed 1 #'silence)
      (is (= 2 (length events)))
      (destructuring-bind (kind samples) (second events)
        (is (eq :utterance kind))
        ;; 50 speech frames + 35 trailing + up to 10 of pre-roll.
        (is (<= (* 160 85) (length samples) (* 160 95)))
        (is (> (voice:rms samples :end (* 160 40)) 1000)))
      ;; A new utterance can follow.
      (setf events '())
      (feed 20 #'tone)
      (feed 35 #'silence)
      (is (equal '(:start :utterance) (mapcar (lambda (e) (if (consp e) (car e) e)) events))))))

(test sentence-splitting-for-streaming
  (multiple-value-bind (sentences rest)
      (voice:split-sentences "The capital of France is Paris. It has about 2.1 million people! And")
    (is (equal '("The capital of France is Paris." "It has about 2.1 million people!") sentences))
    (is (string= " And" rest)))
  (multiple-value-bind (sentences rest) (voice:split-sentences "Still going")
    (is (null sentences))
    (is (string= "Still going" rest)))
  ;; A sentence end is only trusted once the next word has started.
  (is (null (voice:split-sentences "This might be the end.")))
  (is (equal '("First line here" "Second line here")
             (voice:split-sentences (format nil "First line here~%Second line here~%")))))

(test transcript-cleaning
  (is (string= "From Laudato Si', see the logos page."
               (voice::speakable "From *Laudato Si'*, see the [[logos]] page.")))
  (is (string= "Hello there." (voice::clean-transcript (format nil " [BLANK_AUDIO] Hello there. (wind)~%"))))
  (is (null (voice::clean-transcript " [BLANK_AUDIO] ")))
  (is (null (voice::clean-transcript "..."))))

(test dialplan-patterns
  (let ((plan '(("600" :echo) ("7XX" :menu) ("9*" :outside) (:default :agent))))
    (is (eq :echo (voice::dialplan-action plan "600")))
    (is (eq :menu (voice::dialplan-action plan "742")))
    (is (eq :agent (voice::dialplan-action plan "7a2")))
    (is (eq :outside (voice::dialplan-action plan "915551212")))
    (is (eq :outside (voice::dialplan-action plan "9")))
    (is (eq :agent (voice::dialplan-action plan "alice")))
    (is (null (voice::dialplan-action '(("600" :echo)) "601")))))

(test worker-tts-protocol
  ;; A stand-in worker: announces readiness, then writes the same WAV for
  ;; every request, failing when asked to say "fail".
  (uiop:with-temporary-file (:pathname sample :type "wav" :keep t)
    (voice:write-wav sample (tone 2400) 24000)
    (uiop:with-temporary-file (:pathname script :type "sh" :keep t)
      (with-open-file (out script :direction :output :if-exists :supersede)
        (format out "#!/bin/sh
echo 'loading noise that is not JSON'
echo '{\"ready\": true}'
while read -r line; do
  case \"$line\" in
    *fail*) echo '{\"ok\": false, \"error\": \"no voice\"}' ;;
    *) out=$(printf '%s' \"$line\" | sed 's/.*\"out\":\"\\([^\"]*\\)\".*/\\1/')
       cp '~a' \"$out\"; echo '{\"ok\": true}' ;;
  esac
done
" (namestring sample)))
      (let ((tts (voice:make-worker-tts (list "sh" (namestring script)))))
        (unwind-protect
             (progn
               ;; 24 kHz output is brought down to the telephone rate.
               (is (= 800 (length (voice:synthesize tts "Hello there."))))
               (is (= 800 (length (voice:synthesize tts "Again."))))
               (signals error (voice:synthesize tts "please fail"))
               ;; The worker survives a failed request.
               (is (= 800 (length (voice:synthesize tts "Still here.")))))
          (voice:stop-backend tts)
          (delete-file script)
          (delete-file sample))))))

;;; The Claude backend, against a stand-in for curl that replays a stream.

(defun fake-curl (output)
  "A script that records its arguments and standard input, then prints
OUTPUT.  Returns its path."
  (let ((path (uiop:with-temporary-file (:pathname p :type "sh" :keep t) p)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "#!/bin/sh~%echo \"$@\" > \"$0.args\"~%cat > \"$0.stdin\"~%cat <<'END_OF_STREAM'~%~aEND_OF_STREAM~%"
              output))
    (uiop:run-program (list "chmod" "+x" (namestring path)))
    path))

(defun cleanup-fake-curl (path)
  (dolist (suffix '("" ".args" ".stdin"))
    (ignore-errors (delete-file (format nil "~a~a" (namestring path) suffix)))))

(test claude-streaming-request
  (let* ((stream (format nil "~{~a~%~}"
                         '("event: message_start"
                           "data: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_1\"}}"
                           ""
                           "event: content_block_start"
                           "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"thinking\",\"thinking\":\"\"}}"
                           ""
                           "event: content_block_delta"
                           "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"signature_delta\",\"signature\":\"abc\"}}"
                           ""
                           "event: ping"
                           "data: {\"type\":\"ping\"}"
                           ""
                           "event: content_block_delta"
                           "data: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"text_delta\",\"text\":\"Paris, \"}}"
                           ""
                           "event: content_block_delta"
                           "data: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"text_delta\",\"text\":\"of course.\"}}"
                           ""
                           "event: message_delta"
                           "data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":5}}"
                           ""
                           "event: message_stop"
                           "data: {\"type\":\"message_stop\"}")))
         (curl (fake-curl stream))
         (pieces '()))
    (unwind-protect
         (multiple-value-bind (text stop-reason)
             (voice:respond (voice:make-claude-llm :program (namestring curl)
                                                   :api-key "test-key-123")
                            '((:user . "What is the capital of France?"))
                            (lambda (piece) (push piece pieces)))
           ;; Only text deltas are spoken; thinking and pings are skipped.
           (is (string= "Paris, of course." text))
           (is (equal '("Paris, " "of course.") (reverse pieces)))
           (is (string= "end_turn" stop-reason))
           ;; The key travels on standard input, never on the command line.
           (let ((args (uiop:read-file-string (format nil "~a.args" (namestring curl))))
                 (stdin (uiop:read-file-string (format nil "~a.stdin" (namestring curl)))))
             (is (null (search "test-key-123" args)))
             (is (search "x-api-key: test-key-123" stdin))
             (is (search "anthropic-version: 2026-10-21" stdin))
             (is (search "https://api.anthropic.com/v1/messages" stdin))))
      (cleanup-fake-curl curl)))
  ;; The request body.
  (let ((body (com.inuoe.jzon:parse
               (voice::claude-request-body (voice:make-claude-llm)
                                           '((:user . "Hi") (:assistant . "Hello.") (:user . "Bye"))))))
    (is (string= "claude-haiku-5-5" (gethash "model" body)))
    (is (eq t (gethash "stream" body)))
    (is (string= "disabled" (gethash "type" (gethash "thinking" body))))
    (is (string= "low" (gethash "effort" (gethash "output_config" body))))
    (is (equal '("user" "assistant" "user")
               (map 'list (lambda (m) (gethash "role" m)) (gethash "messages" body))))
    (is (null (nth-value 1 (gethash "temperature" body)))))
  ;; An API error arrives as a plain JSON object.
  (let ((curl (fake-curl (format nil "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"invalid x-api-key\"}}~%"))))
    (unwind-protect
         (signals voice:llm-error
           (voice:respond (voice:make-claude-llm :program (namestring curl) :api-key "bad")
                          '((:user . "Hi")) #'identity))
      (cleanup-fake-curl curl)))
  ;; No key at all is reported before anything is sent.
  (let ((saved (uiop:getenv "ANTHROPIC_API_KEY")))
    (unless (and saved (plusp (length saved)))
      (signals voice:llm-error
        (voice:respond (voice:make-claude-llm) '((:user . "Hi")) #'identity)))))

;;; Tools: the model asks for one, gets the result, and carries on.

(defun install-fixture (name &optional (tag ""))
  "Copy the script t/fixtures/NAME somewhere temporary, where it may leave
its records.  Returns the path of the copy."
  (let ((path (uiop:with-temporary-file (:pathname p :suffix tag :type "sh" :keep t) p)))
    (uiop:copy-file (asdf:system-relative-pathname "cl-sip" (format nil "t/fixtures/~a" name))
                    path)
    (uiop:run-program (list "chmod" "+x" (namestring path)))
    path))

(defun fixture-file (path suffix)
  (format nil "~a.~a" (namestring path) suffix))

(defun cleanup-fixture (path)
  ;; Everything the script left beside itself: NAME.sh.count, NAME.sh.out.1...
  (dolist (file (uiop:directory-files (uiop:pathname-directory-pathname path)))
    (when (alexandria:starts-with-subseq (file-namestring path) (file-namestring file))
      (ignore-errors (delete-file file)))))

(defun sse (&rest events)
  (format nil "~{data: ~a~%~%~}" events))

(test claude-tool-use
  (let ((curl (install-fixture "replay-curl.sh"))
        (inputs '())
        (pieces '())
        (logged '()))
    (unwind-protect
         (progn
           (with-open-file (out (fixture-file curl "out.1") :direction :output :if-exists :supersede)
             (write-string
              (sse "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}"
                   "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Let me check.\"}}"
                   "{\"type\":\"content_block_stop\",\"index\":0}"
                   "{\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_1\",\"name\":\"lookup\",\"input\":{}}}"
                   "{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"query\\\": \\\"the \"}}"
                   "{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"answer\\\"}\"}}"
                   "{\"type\":\"content_block_stop\",\"index\":1}"
                   "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"}}")
              out))
           (with-open-file (out (fixture-file curl "out.2") :direction :output :if-exists :supersede)
             (write-string
              (sse "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}"
                   "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"It is forty-two.\"}}"
                   "{\"type\":\"content_block_stop\",\"index\":0}"
                   "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"}}")
              out))
           (multiple-value-bind (text stop-reason)
               (voice:respond
                (voice:make-claude-llm
                 :program (namestring curl) :api-key "k"
                 :log (lambda (who text) (push (list who text) logged))
                 :tools (list (voice:make-function-tool
                               "lookup" "Look something up."
                               (lambda (input)
                                 (push (gethash "query" input) inputs)
                                 "42")
                               :instructions "Ask politely.")))
                '((:user . "What is the answer?"))
                (lambda (piece) (push piece pieces)))
             (is (string= "end_turn" stop-reason))
             (is (equal '("the answer") inputs))
             ;; The words before the lookup are closed off so they can be
             ;; spoken while it runs.
             (is (equal (list "Let me check." (string #\Newline) "It is forty-two.")
                        (reverse pieces)))
             (is (string= (format nil "Let me check.~%It is forty-two.") text))
             (is (equal '((:tool "lookup {\"query\":\"the answer\"}")) logged)))
           ;; The first request offers the tool.
           (let ((body (com.inuoe.jzon:parse
                        (uiop:read-file-string (fixture-file curl "body.1")))))
             (is (equal '("lookup") (map 'list (lambda (tool) (gethash "name" tool))
                                         (gethash "tools" body))))
             (is (string= "auto" (gethash "type" (gethash "tool_choice" body))))
             (is (search "Let me check" (gethash "system" body)))
             (is (search "Ask politely." (gethash "system" body)))
             (is (= 1 (length (gethash "messages" body)))))
           ;; The second carries the call and its result back.
           (let* ((body (com.inuoe.jzon:parse
                         (uiop:read-file-string (fixture-file curl "body.2"))))
                  (messages (gethash "messages" body))
                  (assistant (gethash "content" (aref messages 1)))
                  (result (aref (gethash "content" (aref messages 2)) 0)))
             (is (= 3 (length messages)))
             (is (string= "assistant" (gethash "role" (aref messages 1))))
             (is (equal '("text" "tool_use")
                        (map 'list (lambda (b) (gethash "type" b)) assistant)))
             (is (string= "the answer" (gethash "query" (gethash "input" (aref assistant 1)))))
             (is (string= "user" (gethash "role" (aref messages 2))))
             (is (string= "tool_result" (gethash "type" result)))
             (is (string= "toolu_1" (gethash "tool_use_id" result)))
             (is (string= "42" (gethash "content" result)))
             (is (eq nil (gethash "is_error" result)))))
      (cleanup-fixture curl)))
  ;; A tool that fails is reported to the model rather than ending the turn,
  ;; and long results are cut.
  (let* ((llm (voice:make-claude-llm :max-tool-result 8))
         (providers (list (cons "boom" (voice:make-function-tool
                                        "boom" "" (lambda (input)
                                                    (declare (ignore input))
                                                    (error "it broke"))))
                          (cons "long" (voice:make-function-tool
                                        "long" "" (lambda (input)
                                                    (declare (ignore input))
                                                    "0123456789")))))
         (call (lambda (name)
                 (voice::claude-run-tool
                  llm providers
                  (com.inuoe.jzon:parse
                   (format nil "{\"type\":\"tool_use\",\"id\":\"t\",\"name\":\"~a\",\"input\":{}}"
                           name))))))
    (is (eq t (gethash "is_error" (funcall call "boom"))))
    (is (search "it broke" (gethash "content" (funcall call "boom"))))
    (is (eq t (gethash "is_error" (funcall call "missing"))))
    (is (string= (format nil "01234567~%[truncated]") (gethash "content" (funcall call "long")))))
  ;; Without tools the request does not mention them.
  (let ((body (com.inuoe.jzon:parse
               (voice::claude-request-body (voice:make-claude-llm) '((:user . "Hi"))))))
    (is (null (nth-value 1 (gethash "tools" body))))
    (is (string= voice:*phone-system-prompt* (gethash "system" body))))
  ;; Reference material joins the system prompt as one cached block.
  (let* ((body (com.inuoe.jzon:parse
                (voice::claude-request-body
                 (voice:make-claude-llm) '((:user . "Hi"))
                 :tools (voice:list-tools (voice:make-function-tool "t" "" #'identity))
                 :instructions '("Ask politely.") :reference '("# Catalog"))))
         (system (gethash "system" body)))
    (is (= 1 (length system)))
    (is (string= "ephemeral" (gethash "type" (gethash "cache_control" (aref system 0)))))
    (is (alexandria:starts-with-subseq voice:*phone-system-prompt*
                                       (gethash "text" (aref system 0))))
    (is (alexandria:ends-with-subseq (format nil "Ask politely.~%~%# Catalog")
                                     (gethash "text" (aref system 0)))))
  ;; In the last round allowed, the model has to answer.
  (let ((body (com.inuoe.jzon:parse
               (voice::claude-request-body
                (voice:make-claude-llm) '((:user . "Hi"))
                :tools (voice:list-tools (voice:make-function-tool "t" "" #'identity))
                :allow-tools nil))))
    (is (string= "none" (gethash "type" (gethash "tool_choice" body))))))

(test mcp-client
  (let* ((curl (install-fixture "mcp-curl.sh"))
         (server (voice:make-mcp-server :url "http://localhost:1/mcp" :token "cap-token-9"
                                        :program (namestring curl)
                                        :seed '(:tool "search" :argument "query"
                                                :extra ("n" 6) :header "Found:")
                                        :pinned '(("The catalog" "search" "query" "all")))))
    (unwind-protect
         (progn
           (let ((tools (voice:list-tools server)))
             (is (= 1 (length tools)))
             (is (string= "search" (gethash "name" (first tools))))
             (is (string= "Search the wiki." (gethash "description" (first tools))))
             (is (string= "object" (gethash "type" (gethash "input_schema" (first tools))))))
           ;; Text content is joined; anything else is left out.
           (multiple-value-bind (text error-p)
               (voice:call-tool server "search"
                                (com.inuoe.jzon:parse "{\"query\":\"lisp\"}"))
             (is (string= (format nil "Page one~%Page two") text))
             (is (null error-p)))
           (signals voice:mcp-error
             (voice:call-tool server "nope" (com.inuoe.jzon:parse "{}")))
           (voice:list-tools server)
           ;; Tools can be withheld.
           (is (null (voice:list-tools
                      (voice:make-mcp-server :url "http://localhost:1/mcp"
                                             :program (namestring curl)
                                             :exclude '("search")))))
           ;; Each thing the caller says is searched for.
           (is (string= (format nil "Found:~%~%Page one~%Page two")
                        (voice::tool-context server "lisp macros")))
           ;; Pinned documents are fetched once and kept.
           (is (string= (format nil "# The catalog~%~%Page one~%Page two")
                        (voice::tool-reference server)))
           (is (eq (voice::tool-reference server) (voice::tool-reference server)))
           (let ((args (uiop:read-file-string (fixture-file curl "args")))
                 (stdin (uiop:read-file-string (fixture-file curl "stdin")))
                 (requests (uiop:read-file-lines (fixture-file curl "requests"))))
             ;; The token travels on standard input, never on the command line.
             (is (null (search "cap-token-9" args)))
             (is (search "Authorization: Bearer cap-token-9" stdin))
             (is (search "http://localhost:1/mcp" stdin))
             ;; The session and negotiated version are echoed back.
             (is (search "Mcp-Session-Id: session-7" stdin))
             (is (search "MCP-Protocol-Version: 2025-03-26" stdin))
             ;; One handshake, one listing (it is remembered), two calls.
             (is (= 10 (length requests)))
             (is (search "\"query\":\"lisp macros\"" (ninth requests)))
             (is (search "\"n\":6" (ninth requests)))
             (is (search "\"method\":\"initialize\"" (first requests)))
             (is (search "notifications/initialized" (second requests)))
             (is (search "tools/list" (third requests)))
             (is (search "\"arguments\":{\"query\":\"lisp\"}" (fourth requests)))))
      (cleanup-fixture curl)))
  ;; A server that does not answer is left alone for a while.
  (let ((server (voice:make-mcp-server :url "http://localhost:1/mcp" :program "false")))
    (signals voice:mcp-error (voice:list-tools server))
    (handler-case (progn (voice:list-tools server) (fail "no error"))
      (voice:mcp-error (e) (is (search "trying again" (princ-to-string e))))))
  ;; What was found travels with the caller's words, and the search covers
  ;; the remark before, for the sake of follow-ups.
  (let* ((messages '((:user . "Tell me about Aquinas.") (:assistant . "He was a friar.")
                     (:user . "Who influenced him?")))
         (with (voice::add-context messages '("Found: a page")))
         (content (gethash "content" (third with))))
    (is (string= (format nil "Tell me about Aquinas.~%Who influenced him?")
                 (voice::spoken-query messages)))
    (is (null (voice::spoken-query (butlast messages))))
    (is (eq messages (voice::add-context messages '())))
    (is (equal (butlast messages) (butlast with)))
    (is (equal '("Who influenced him?" "Found: a page")
               (map 'list (lambda (part) (gethash "text" part)) content))))
  ;; A server that refuses the token: the error says so, and a model given
  ;; that server simply goes without its tools.
  (let* ((curl (install-fixture "mcp-curl.sh" "-reject"))
         (server (voice:make-mcp-server :url "http://localhost:1/mcp" :token "bad"
                                        :program (namestring curl)))
         (logged '()))
    (unwind-protect
         (progn
           (handler-case (progn (voice:list-tools server) (fail "no error"))
             (voice:mcp-error (e)
               (is (search "403" (princ-to-string e)))
               (is (search "mcp capability" (princ-to-string e)))))
           (is (null (voice::claude-tools
                      (voice:make-claude-llm
                       :tools (list server)
                       :log (lambda (who text) (push (list who text) logged))))))
           (is (eq :error (first (first logged)))))
      (cleanup-fixture curl))))

;;; End to end over loopback, with stand-in speech and language backends.

(defun call-phone-server (server extension function)
  "Call EXTENSION on SERVER and run FUNCTION with the call, the caller's RTP
session and a function returning the packets received so far."
  (let* ((received '())
         (lock (bt:make-lock))
         (session (rtp:rtp-session-start
                   (rtp:make-rtp-session
                    :on-packet (lambda (session packet)
                                 (declare (ignore session))
                                 (bt:with-lock-held (lock) (push packet received))))))
         (ua (sip:make-user-agent :aor "sip:tester@127.0.0.1"
                                  :transports '((:udp :host "127.0.0.1" :port 0)))))
    (sip:start ua)
    (unwind-protect
         (let ((call (sip:invite
                      ua (format nil "sip:~a@127.0.0.1:~d" extension (voice:server-port server))
                      :sdp (sdp:sdp-string
                            (sdp:make-audio-offer :address "127.0.0.1"
                                                  :port (rtp:rtp-session-local-port session))))))
           (when (sip:wait-until (lambda () (eq :established (sip:call-state call))))
             (multiple-value-bind (codecs direction host port)
                 (sdp:negotiated-audio (sdp:parse-sdp (sip:call-remote-sdp call)))
               (declare (ignore codecs direction))
               (setf (rtp:rtp-session-remote-host session) host
                     (rtp:rtp-session-remote-port session) port)))
           (funcall function call session
                    (lambda () (bt:with-lock-held (lock) (length received))))
           (sip:hangup call)
           (sip:wait-until (lambda () (eq :terminated (sip:call-state call)))))
      (rtp:rtp-session-stop session)
      (sip:stop ua))))

(defun send-audio (session samples)
  (loop for start from 0 below (length samples) by 160
        do (rtp:rtp-send session (rtp:ulaw-encode (subseq samples start (+ start 160))))
           (sleep 1/500)))

(test phone-server-end-to-end
  (let* ((heard '())
         (asked '())
         (spoken '())
         (server (voice:start-server
                  :port 0 :tcp nil :log-stream nil
                  :greeting "Hello."
                  :stt (voice:make-function-stt
                        (lambda (samples rate)
                          (push (list (length samples) rate) heard)
                          "What is the capital of France?"))
                  :llm (voice:make-function-llm
                        (lambda (messages)
                          (push messages asked)
                          "It is Paris. Anything else?"))
                  :tts (voice:make-function-tts
                        (lambda (text)
                          (push text spoken)
                          (tone 1600))))))
    (unwind-protect
         (progn
           ;; The agent: greeting, then one spoken exchange.
           (call-phone-server
            server "1"
            (lambda (call session packets)
              (is (eq :established (sip:call-state call)))
              ;; Greeting: 1600 samples = 10 packets.
              (is-true (sip:wait-until (lambda () (>= (funcall packets) 10))))
              (sleep 1/4)
              (send-audio session (concatenate '(vector (signed-byte 16))
                                               (tone 8000) (silence 6400)))
              ;; Two sentences of reply: 20 more packets.
              (is-true (sip:wait-until (lambda () (>= (funcall packets) 30))))
              (is (equal '("Hello." "It is Paris." "Anything else?") (reverse spoken)))
              (is (equal '(((:user . "What is the capital of France?"))) asked))
              (is (= 1 (length heard)))
              (is (= 8000 (second (first heard))))
              (is (> (first (first heard)) 8000))))
           ;; Extension 600 echoes audio back.
           (call-phone-server
            server "600"
            (lambda (call session packets)
              (declare (ignore call))
              (send-audio session (tone 1600))
              (is-true (sip:wait-until (lambda () (= (funcall packets) 10))))))
           ;; Extension 700 speaks and then hangs up by itself.
           (setf spoken '())
           (call-phone-server
            server "700"
            (lambda (call session packets)
              (declare (ignore session))
              (is-true (sip:wait-until (lambda () (>= (funcall packets) 10))))
              (is-true (sip:wait-until (lambda () (eq :terminated (sip:call-state call)))))
              (is (eq :remote-hangup (sip:call-cause call)))
              (is (= 1 (length spoken))))))
      (voice:stop-server server))))

;;; Trunk: a stand-in provider on loopback that the server registers with.

(test source-screening
  (let ((server (voice:start-server :port 0 :tcp nil :log-stream nil
                                    :allow '("192.168.7.0/24")
                                    :trunk '(:registrar "sip:127.0.0.1:9" :username "u"
                                             :password "p" :ranges ("198.51.100.0/28")))))
    (unwind-protect
         (progn
           (is (eq :trunk (voice::server-source-kind server "198.51.100.3")))
           (is (eq :local (voice::server-source-kind server "127.0.0.1")))
           (is (eq :local (voice::server-source-kind server "192.168.7.20")))
           (is (null (voice::server-source-kind server "192.168.8.20")))
           (is (null (voice::server-source-kind server "198.51.100.16")))
           (is (null (voice::server-source-kind server "203.0.113.50"))))
      (voice:stop-server server)))
  (is (equal voice:*flowroute-signalling-ranges* '("34.210.91.112/28" "34.226.36.32/28")))
  (is (voice::same-number-p "+1 (206) 555-0199" "2065550199"))
  (is (voice::same-number-p "12065550199" "+12065550199"))
  (is (not (voice::same-number-p "12065550198" "2065550199")))
  (is (not (voice::same-number-p "" "2065550199"))))

(test trunk-registration-and-inbound-call
  (let* ((registrar (sip:make-registrar :realm "provider" :users '(("12345678" . "secret"))))
         (provider (sip:make-user-agent :aor "sip:12065550199@127.0.0.1"
                                        :transports '((:udp :host "127.0.0.1" :port 0))
                                        :registrar registrar))
         (server nil)
         (spoken '()))
    (sip:start provider)
    (unwind-protect
         (let ((provider-port (sip:transport-port
                               (first (sip:stack-transports (sip:ua-stack provider))))))
           (setf server
                 (voice:start-server
                  :port 0 :tcp nil :log-stream nil :greeting "Hello."
                  :dialplan '(("12065550100" :agent) (:default (:reject 404)))
                  :allowed-callers '("206-555-0199")
                  :trunk (list :registrar (format nil "sip:127.0.0.1:~d" provider-port)
                               :username "12345678" :password "secret"
                               :ranges '("127.0.0.0/8"))
                  :stt (voice:make-function-stt (lambda (samples rate)
                                                  (declare (ignore samples rate)) nil))
                  :llm (voice:make-function-llm (lambda (messages)
                                                  (declare (ignore messages)) "Fine."))
                  :tts (voice:make-function-tts (lambda (text)
                                                  (push text spoken) (tone 1600)))))
           ;; The server registers with digest credentials, as the tech prefix.
           (is-true (sip:wait-until
                     (lambda () (eq :registered (sip:registration-state
                                                 (voice:server-registration server))))))
           (let ((contacts (sip:registrar-lookup registrar provider "sip:12345678@127.0.0.1")))
             (is (= 1 (length contacts)))
             (is (= (voice:server-port server) (sip:sip-uri-port (first contacts))))
             (flet ((call-number (ua number function)
                      ;; As a provider does: the INVITE goes to the registered
                      ;; contact, and the number called is in the To header.
                      (let* ((packets 0)
                             (session (rtp:rtp-session-start
                                       (rtp:make-rtp-session
                                        :on-packet (lambda (s p)
                                                     (declare (ignore s p))
                                                     (incf packets)))))
                             (call (sip:invite
                                    ua (first contacts)
                                    :to (format nil "<sip:+~a@127.0.0.1>" number)
                                    :sdp (sdp:sdp-string
                                          (sdp:make-audio-offer
                                           :port (rtp:rtp-session-local-port session))))))
                        (unwind-protect
                             (progn
                               (sip:wait-until (lambda () (member (sip:call-state call)
                                                                  '(:established :terminated))))
                               (funcall function call (lambda () packets))
                               (sip:hangup call)
                               (sip:wait-until (lambda () (eq :terminated (sip:call-state call)))))
                          (rtp:rtp-session-stop session)))))
               ;; A call to our number from an allowed caller reaches the agent.
               (call-number provider "12065550100"
                            (lambda (call packets)
                              (is (eq :established (sip:call-state call)))
                              (is-true (sip:wait-until (lambda () (>= (funcall packets) 10))))
                              (is (equal '("Hello.") spoken))))
               ;; Another number is not in the dialplan.
               (call-number provider "12065550111"
                            (lambda (call packets)
                              (declare (ignore packets))
                              (is (eq :terminated (sip:call-state call)))
                              (is (= 404 (sip:call-status call)))))
               ;; A caller who is not on the list is declined.
               (let ((stranger (sip:make-user-agent
                                :aor "sip:13125550123@127.0.0.1"
                                :transports '((:udp :host "127.0.0.1" :port 0)))))
                 (sip:start stranger)
                 (unwind-protect
                      (call-number stranger "12065550100"
                                   (lambda (call packets)
                                     (declare (ignore packets))
                                     (is (eq :terminated (sip:call-state call)))
                                     (is (= 603 (sip:call-status call)))))
                   (sip:stop stranger)))))
           ;; Stopping the server removes its registration.
           (voice:stop-server server)
           (setf server nil)
           (is (null (sip:registrar-lookup registrar provider "sip:12345678@127.0.0.1"))))
      (when server (voice:stop-server server))
      (sip:stop provider))))
