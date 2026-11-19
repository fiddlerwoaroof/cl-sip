(in-package :cl-sip.voice)

;;;; A small phone server: softphones register with it, and each call is
;;;; routed by the dialled extension to the voice agent, an echo test, or a
;;;; spoken or recorded announcement.

(defparameter *default-dialplan*
  '(("600" :echo)
    ("700" (:say "This is the test line. Your call is working."))
    (:default :agent))
  "List of (pattern action).  PATTERN is an extension, in which X stands for
any digit and a trailing * for any remaining characters, or :DEFAULT.
ACTION is :AGENT, :ECHO, (:SAY text), (:PLAY wav-path), (:REJECT status), or
a function of the server, the call and the extension.")

(defparameter *flowroute-signalling-ranges*
  '("34.210.91.112/28" "34.226.36.32/28")
  "Source addresses of Flowroute's SIP signalling (US-West-OR and US-East-VA),
from their firewall documentation as of November 2026.  Calls from these
addresses are treated as trunk calls.")

(defclass phone-server ()
  ((ua :initarg :ua :reader server-ua)
   (trunk-ranges :initarg :trunk-ranges :initform '() :accessor server-trunk-ranges
                 :documentation "CIDRs the trunk's signalling comes from.")
   (allow :initarg :allow :initform '() :accessor server-allow
          :documentation "CIDRs of phones allowed to register and call, besides
this machine itself.")
   (allowed-callers :initarg :allowed-callers :initform nil
                    :accessor server-allowed-callers
                    :documentation "Caller numbers accepted from the trunk; NIL
accepts any.")
   (extensions :initarg :extensions :initform '() :accessor server-extensions
               :documentation "The extension list (see extensions.lisp).  When
there is one, only its extensions may register, and phones must authenticate
their calls.")
   (ring-seconds :initarg :ring-seconds :initform 30 :accessor server-ring-seconds
                 :documentation "How long an extension rings before the call counts
as unanswered.")
   (trunk :initarg :trunk :initform nil :accessor server-trunk
          :documentation "The trunk's settings, as given to START-SERVER.")
   (outbound-caller-id :initarg :outbound-caller-id :initform nil
                       :accessor server-outbound-caller-id
                       :documentation "The number shown on calls placed through the
trunk.  Without it nobody can dial out.")
   (outbound-patterns :initarg :outbound-patterns
                      :initform '("1XXXXXXXXXX" "XXXXXXXXXX")
                      :accessor server-outbound-patterns
                      :documentation "Dialled numbers that go out through the trunk.")
   (bridges :initform '() :accessor server-bridges
            :documentation "Bridged calls in progress (see bridge.lisp).")
   (max-calls :initarg :max-calls :initform 2 :accessor server-max-calls
              :documentation "Agent calls allowed at once.")
   (active-calls :initform 0 :accessor server-active-calls)
   (rtp-port-range :initarg :rtp-port-range :initform nil :accessor server-rtp-port-range)
   (stun :initarg :stun :initform nil :accessor server-stun)
   (registration :initform nil :accessor server-registration)
   (dropped :initform '() :accessor server-dropped
            :documentation "Sources already reported as dropped.")
   (host :initarg :host :reader server-host)
   (dialplan :initarg :dialplan :accessor server-dialplan)
   (stt :initarg :stt :accessor server-stt)
   (llm :initarg :llm :accessor server-llm)
   (tts :initarg :tts :accessor server-tts)
   (greeting :initarg :greeting :accessor server-greeting)
   (barge-in :initarg :barge-in :accessor server-barge-in)
   (log-stream :initarg :log-stream :accessor server-log-stream)))

(defun server-port (server)
  (sip:transport-port (first (sip:stack-transports (sip:ua-stack (server-ua server))))))

(defun server-log (server control &rest args)
  (let ((stream (server-log-stream server)))
    (when stream
      (multiple-value-bind (sec min hour) (get-decoded-time)
        (format stream "~&~2,'0d:~2,'0d:~2,'0d ~?~%" hour min sec control args)
        (force-output stream)))))

;;; Dialplan

(defun extension-matches-p (pattern extension)
  (cond ((eq pattern :default) t)
        ((not (stringp pattern)) nil)
        (t
         (let ((n (length pattern)))
           (and (plusp n)
                (if (char= (char pattern (1- n)) #\*)
                    (and (>= (length extension) (1- n))
                         (extension-matches-p (subseq pattern 0 (1- n))
                                              (subseq extension 0 (1- n))))
                    (and (= (length extension) n)
                         (every (lambda (p c)
                                  (if (char= p #\X) (digit-char-p c) (char-equal p c)))
                                pattern extension))))))))

(defun dialplan-action (dialplan extension)
  (second (find-if (lambda (entry) (extension-matches-p (first entry) extension))
                   dialplan)))

(defun server-route (server dialled)
  "What to do with a call to DIALLED: an explicit dialplan entry first, then
an extension or group of that number, then the dialplan's default."
  (or (dialplan-action (remove :default (server-dialplan server) :key #'first) dialled)
      (let ((extension (find-extension (server-extensions server) dialled)))
        (and extension (list :extension extension)))
      (and (server-outbound-caller-id server)
           (server-trunk server)
           (some (lambda (pattern) (extension-matches-p pattern dialled))
                 (server-outbound-patterns server))
           :trunk)
      (dialplan-action (server-dialplan server) dialled)))

;;; Who is calling

(defun server-transport (server)
  (first (sip:stack-transports (sip:ua-stack (server-ua server)))))

(defun range-match-p (host port range)
  "RANGE is a network in CIDR form, or one endpoint as \"address:port\"
(a provider reached through a fixed port, or a stand-in for one)."
  (let ((colon (position #\: range)))
    (if colon
        (and port
             (string= host (subseq range 0 colon))
             (eql port (parse-integer range :start (1+ colon) :junk-allowed t)))
        (sip:cidr-match-p host range))))

(defun server-source-kind (server host &optional port)
  "How a request from HOST is treated: :TRUNK, :LOCAL, or NIL (ignored)."
  (cond ((some (lambda (range) (range-match-p host port range))
               (server-trunk-ranges server))
         :trunk)
        ((or (sip:cidr-match-p host "127.0.0.0/8")
             (equal host (sip:transport-advertised-host (server-transport server)))
             (some (lambda (cidr) (sip:cidr-match-p host cidr)) (server-allow server)))
         :local)))

(defun server-screen-request (server request)
  "Request filter: anything not from the trunk or an allowed phone is dropped
without an answer, so scanners learn nothing."
  (let ((host (sip:message-remote-host request)))
    (or (and (server-source-kind server host (sip:message-remote-port request)) t)
        (progn
          (unless (member host (server-dropped server) :test #'equal)
            (when (< (length (server-dropped server)) 100)
              (push host (server-dropped server)))
            (server-log server "ignoring ~a from ~a (not an allowed source)"
                        (sip:request-method request) host))
          nil))))

(defun digits (string)
  (remove-if-not #'digit-char-p (or string "")))

(defun same-number-p (a b)
  "Compare telephone numbers, ignoring punctuation and a country prefix."
  (let ((a (digits a)) (b (digits b)))
    (and (plusp (length a)) (plusp (length b))
         (flet ((tail (s) (subseq s (max 0 (- (length s) 10)))))
           (string= (tail a) (tail b))))))

;;; Calls

(defun uri-user (uri)
  (or (and (sip:sip-uri-p uri) (sip:sip-uri-user uri)) ""))

(defun call-kind (server call)
  (server-source-kind server (sip:message-remote-host (sip:call-invite call))
                      (sip:message-remote-port (sip:call-invite call))))

(defun call-extension (server call)
  "What was dialled.  A phone puts it in the Request-URI.  A trunk delivers
calls to our registered contact, so the number called is in the To header."
  (let ((invite (sip:call-invite call)))
    (if (eq (call-kind server call) :trunk)
        (normalize-number (uri-user (sip:name-addr-uri (sip:msg-to invite))))
        (normalize-number (uri-user (sip:request-uri invite))))))

(defun call-caller-extension (call)
  "The extension that placed CALL, once it has authenticated."
  (getf (sip:call-user-data call) :caller))

(defun server-authenticate-call (server call)
  "The extension placing CALL, proven by digest credentials.  Without them
the call is refused with a challenge and NIL is returned; the phone then
sends the INVITE again with credentials."
  (let ((registrar (sip:ua-registrar (server-ua server))))
    (multiple-value-bind (user problem)
        (sip:registrar-authenticate registrar (sip:call-invite call))
      (let ((extension (and user (find-extension (server-extensions server) user))))
        (cond (extension
               (setf (getf (sip:call-user-data call) :caller) extension))
              (t
               (when (eq problem :bad)
                 (server-log server "wrong password in a call from ~a" (call-caller call)))
               (sip:reject call 401
                           `(("WWW-Authenticate"
                              ,(sip:registrar-make-challenge
                                registrar :stale (eq problem :stale)))))
               nil))))))

(defun call-caller (call)
  (let ((from (sip:msg-from (sip:call-invite call))))
    (sip:uri-string (sip:name-addr-uri from))))

(defun call-caller-number (call)
  (uri-user (sip:name-addr-uri (sip:msg-from (sip:call-invite call)))))

(defun in-background (function)
  "Run FUNCTION off the SIP event loop; media setup and teardown block."
  (bt:make-thread function :name "phone-server-task"))

(defun play-then-hang-up (server call session codec samples-function)
  "Play the samples SAMPLES-FUNCTION returns, then end the call."
  (let ((out (make-audio-out session :encoder (if (string-equal codec "PCMA")
                                                  #'rtp:alaw-encode
                                                  #'rtp:ulaw-encode))))
    (setf (rtp:rtp-session-payload-type session) (if (string-equal codec "PCMA") 8 0))
    (in-background
     (lambda ()
       (handler-case
           (progn
             (audio-out-enqueue out (funcall samples-function))
             (loop until (or (audio-out-idle-p out)
                             (eq (sip:call-state call) :terminated))
                   do (sleep 1/10))
             (sleep 3/10))
         (error (e) (server-log server "playback failed: ~a" e)))
       (audio-out-stop out)
       (sip:hangup call)))
    out))

(defun server-start-media (server call action session codec)
  "Start ACTION on an established call.  Returns a function that stops it."
  (cond
    ((eq action :agent)
     (let ((agent (start-agent session
                               :stt (server-stt server) :llm (server-llm server)
                               :tts (server-tts server) :codec codec
                               :greeting (server-greeting server)
                               :barge-in (server-barge-in server)
                               :log (lambda (who text)
                                      (server-log server "~(~a~): ~a" who text)))))
       (lambda () (stop-agent agent))))
    ((eq action :echo)
     (setf (rtp:rtp-session-on-packet session)
           (lambda (session packet)
             (rtp:rtp-send session (rtp:rtp-packet-payload packet)
                           :payload-type (rtp:rtp-packet-payload-type packet)
                           :marker (rtp:rtp-packet-marker packet))))
     (lambda ()))
    ((and (consp action) (eq (first action) :say))
     (let ((out (play-then-hang-up server call session codec
                                   (lambda () (synthesize (server-tts server)
                                                          (second action))))))
       (lambda () (audio-out-stop out))))
    ((and (consp action) (eq (first action) :play))
     (let ((out (play-then-hang-up server call session codec
                                   (lambda ()
                                     (multiple-value-bind (samples rate)
                                         (read-wav (second action))
                                       (resample samples rate 8000))))))
       (lambda () (audio-out-stop out))))
    (t (lambda ()))))

(defun server-handle-call (server call)
  (when (and (server-extensions server)
             (eq (call-kind server call) :local)
             (not (server-authenticate-call server call)))
    (return-from server-handle-call))
  (let* ((kind (call-kind server call))
         (extension (call-extension server call))
         (action (server-route server extension))
         (agent (eq action :agent)))
    (server-log server "~(~a~) call from ~a to ~s -> ~s" kind
                (let ((caller (call-caller-extension call)))
                  (if caller (extension-label caller) (call-caller call)))
                extension
                (if (functionp action) :function action))
    (when (eq kind :trunk)
      ;; Worth seeing while a trunk is being set up: how the number arrives.
      (server-log server "  request-uri ~a, to ~a"
                  (sip:uri-string (sip:request-uri (sip:call-invite call)))
                  (sip:header (sip:call-invite call) "To")))
    (cond
      ((null action) (sip:reject call 404))
      ((and (eq kind :trunk) (server-allowed-callers server)
            (notany (lambda (number) (same-number-p number (call-caller-number call)))
                    (server-allowed-callers server)))
       (server-log server "  caller is not on the allowed list")
       (sip:reject call 603))
      ((and agent (>= (server-active-calls server) (server-max-calls server)))
       (server-log server "  too many calls in progress")
       (sip:reject call 486))
      ((functionp action) (funcall action server call extension))
      ((and (consp action) (eq (first action) :extension))
       (server-call-extension server call (second action)))
      ((eq action :trunk)
       (server-call-trunk server call extension))
      ((and (consp action) (eq (first action) :reject))
       (sip:reject call (or (second action) 486)))
      (t
       ;; Answered here: the agent, the echo, an announcement.
       (let ((a (server-open-leg server :call call :trunk (eq kind :trunk))))
         (cond ((leg-answer-offer server a (sip:call-remote-sdp call))
                (server-answer-with server a action))
               (t
                (leg-close a)
                (sip:reject call 488))))))))

;;; Server

(defun parse-host-port (string default-port)
  (let ((colon (position #\: string :from-end t)))
    (if colon
        (cons (subseq string 0 colon) (parse-integer string :start (1+ colon)))
        (cons string default-port))))

(defun server-register-trunk (server trunk)
  (destructuring-bind (&key registrar (expires 300) (keepalive 25) &allow-other-keys) trunk
    (let ((ua (server-ua server)))
      (setf (server-registration server)
            (sip:register
             ua :registrar registrar :expires expires :keepalive keepalive
             :on-event
             (lambda (registration event &key expires status reason &allow-other-keys)
               (case event
                 (:registered
                  (server-log server "registered with ~a as ~a for ~d s" registrar
                              (sip:uri-string
                               (sip:name-addr-uri (sip:registration-contact registration)))
                              expires))
                 (:failed
                  (server-log server "registration with ~a failed: ~a" registrar
                              (or status reason))
                  ;; Bad credentials will not improve; anything else might.
                  (unless (member status '(401 403 407))
                    (sip:schedule (sip:ua-scheduler ua) 60
                                  (lambda () (sip:refresh-registration registration))))))))))))

(defun start-server (&key (host "127.0.0.1") (port 5060) users
                       (dialplan *default-dialplan*)
                       (stt (make-whisper-stt)) (llm (make-claude-llm)) (tts (make-say-tts))
                       (greeting "Hello! What can I do for you?")
                       (barge-in t) (tcp t) (log-stream *standard-output*)
                       trunk allow allowed-callers (max-calls 2) rtp-port-range stun
                       extensions (ring-seconds 30) outbound-caller-id)
  "Start a phone server on HOST:PORT (UDP, and TCP unless TCP is NIL).

Softphones may register with any user name; with USERS, an alist of
(name . password), registration requires digest authentication.  Calls are
routed by DIALPLAN (see *DEFAULT-DIALPLAN*).

OUTBOUND-CALLER-ID, one of your numbers at the provider, lets extensions
marked :OUTBOUND dial telephone numbers through the trunk.

EXTENSIONS is the extension list, as forms (see PARSE-EXTENSION) or as
LOAD-EXTENSIONS returns it.  With one, only those extensions may register,
each under its own number and password, and a phone's calls must carry the
same credentials.  Only this machine may register
and call unless ALLOW lists further networks, e.g. (\"192.168.1.0/24\"), in
which case HOST must be \"0.0.0.0\" or an address on that network.

TRUNK connects the server to a SIP provider by registering with it:

  (:registrar \"sip:us-west-or.sip.flowroute.com\" :username ... :password ...)

with optional :EXPIRES, :KEEPALIVE and :RANGES (the provider's signalling
addresses; Flowroute's by default).  HOST must then be reachable from the
internet side, normally \"0.0.0.0\".  Trunk calls are routed by the number
called, limited to ALLOWED-CALLERS when given, and to MAX-CALLS agent calls
at once.  For their audio to arrive through a NAT, either forward
RTP-PORT-RANGE, a (low . high) pair of UDP ports, to this machine, or name
a STUN server (\"host:port\")."
  (let* ((extensions (parse-extensions extensions))
         (users (or users
                    (loop for extension in extensions
                          when (extension-password extension)
                            collect (cons (extension-number extension)
                                          (extension-password extension)))))
         (trunk-user (getf trunk :username))
         (registrar (and trunk (sip:ensure-uri (getf trunk :registrar))))
         (ua (sip:make-user-agent
              :aor (if trunk
                       (format nil "sip:~a@~a" trunk-user (sip:sip-uri-host registrar))
                       (format nil "sip:server@~a"
                               (if (string= host "0.0.0.0") "localhost" host)))
              :transports `((:udp :host ,host :port ,port)
                            ,@(when tcp `((:tcp :host ,host :port ,port))))
              :credentials (when trunk `((nil ,trunk-user ,(getf trunk :password))))
              :registrar (sip:make-registrar :realm "cl-sip" :users users
                                             :strict (and extensions t))
              :user-agent-name "cl-sip-voice/0.1"))
         (server (make-instance 'phone-server
                                :ua ua :host host :dialplan dialplan
                                :stt stt :llm llm :tts tts :greeting greeting
                                :barge-in barge-in :log-stream log-stream
                                :trunk-ranges (and trunk
                                                   (getf trunk :ranges
                                                         *flowroute-signalling-ranges*))
                                :extensions extensions :ring-seconds ring-seconds
                                :trunk trunk
                                :outbound-caller-id (and outbound-caller-id
                                                         (normalize-number outbound-caller-id))
                                :allow allow :allowed-callers allowed-callers
                                :max-calls max-calls :rtp-port-range rtp-port-range
                                :stun (etypecase stun
                                        (null nil)
                                        (cons stun)
                                        (string (parse-host-port stun 3478))))))
    (when extensions
      (server-offer-dialog-events server))
    ;; Tool calls the model makes are worth seeing next to the conversation.
    (when (and (typep llm 'claude-llm) (null (llm-log llm)))
      (setf (llm-log llm)
            (lambda (who text) (server-log server "~(~a~): ~a" who text))))
    (setf (sip:ua-request-filter ua)
          (lambda (request) (server-screen-request server request))
          (sip:ua-on-incoming-call ua)
          (lambda (call)
            (handler-case (server-handle-call server call)
              (error (e)
                (server-log server "error handling call: ~a" e)
                (ignore-errors (sip:reject call 500))))))
    (sip:start ua)
    (server-log server "listening on ~a:~d" host (server-port server))
    (when trunk
      (when (sip:cidr-match-p host "127.0.0.0/8")
        (server-log server "warning: bound to ~a, which a trunk on the internet cannot reach"
                    host))
      (server-register-trunk server trunk))
    server))

(defun stop-server (server)
  "Unregister from the trunk, stop listening and shut down any worker
processes the backends run."
  (let ((registration (server-registration server)))
    (when (and registration (eq (sip:registration-state registration) :registered))
      (sip:unregister registration)
      (sip:wait-until (lambda () (not (eq (sip:registration-state registration)
                                          :registering)))
                      :timeout 3)))
  (sip:stop (server-ua server))
  (mapc #'stop-backend (list (server-stt server) (server-llm server) (server-tts server)))
  server)

;;; Configuration from the environment (bin/voice-server)

(defparameter *wiki-instructions*
  (format nil "The tools reach a personal wiki of pages on concepts, people ~
and sources, and you are its voice: the caller is talking with you about ~
what is in it. Its catalog follows these instructions: every page, with a ~
line on each. Treat the catalog as a map, not a source. It shows what ~
exists and what to read, but a one-line summary is not enough to answer ~
from. A name in double brackets is a page: a bare name is one of the ~
wiki's own pages, wp: a cached Wikipedia article, rldev:, osdev: and lm: ~
other cached wikis, source: text extracted from a source and raw: a raw ~
source; read_page takes any of them. With each thing the caller says you are given a list of wiki ~
pages that may bear on it, found by searching for their words. The list has ~
only names and snippets, so when the caller asks about something the wiki ~
might cover, read the one or two most promising pages, from the list or the ~
catalog, with read_page, ~
several at once if you like, before you answer. Prefer the wiki's own pages ~
to raw sources and cached Wikipedia articles, and follow a double-bracketed ~
link in a page when the answer lies behind it. The list is only a first ~
guess, and the catalog covers only the wiki's own pages, not the cached ~
wikis and sources, which only a search finds. So if neither has a fitting ~
page, always search again in other words, plainer ~
and shorter, before concluding that the wiki has nothing. Ignore the list ~
when the caller is only chatting.

Answer from what the pages say, as conversation. Give the gist in two or ~
three sentences, mentioning in passing which page it comes from, then stop ~
and offer in a few words to go further; never recite a page or list ~
everything it covers. The pages are written in Markdown, and none of its ~
marks, such as asterisks or brackets, may appear in what you say. Let the caller steer, and read ~
further pages as the talk moves on. If the wiki has nothing on the subject, ~
say so, then offer what you know yourself, making clear it is not from the ~
wiki. Use the graph query only for questions about how entries relate, such ~
as who wrote a work or who influenced whom.")
  "How the agent is to use the LLM-wiki tools.  This is the routine of the
wiki's own ask_wiki (candidates from a search, then reading pages, then an
answer grounded in them) carried out by the model, turn by turn.")

(defparameter *wiki-seed*
  (list :tool "search" :argument "query" :extra '("n" 8)
        :header (format nil "Wiki pages that may bear on what the caller just ~
said (names and snippets only; read a page before relying on it):"))
  "The search run on each thing the caller says; see MAKE-MCP-SERVER.")

(defparameter *wiki-pinned*
  '(("The wiki's catalog: every page, with a one-line summary" "read_page" "ref" "index"))
  "What the agent has before it throughout a call; see MAKE-MCP-SERVER.")

(defun env (name)
  (uiop:getenvp name))

(defun env-list (name)
  (let ((value (env name)))
    (and value
         (remove "" (mapcar (lambda (s) (string-trim " " s)) (uiop:split-string value :separator ","))
                 :test #'string=))))

(defun start-server-from-environment (&rest overrides)
  "Start a server configured by environment variables:

  VOICE_HOST, VOICE_PORT        address to listen on (127.0.0.1:5060)
  VOICE_ALLOW                   comma-separated networks allowed to call
  VOICE_TTS=chatterbox          speak with Chatterbox instead of `say`
  VOICE_REFERENCE               voice recording for Chatterbox to imitate
  FLOWROUTE_USERNAME, FLOWROUTE_PASSWORD
                                SIP credentials; setting both enables the trunk
  FLOWROUTE_POP                 us-west-or (default) or us-east-va
  VOICE_ALLOWED_CALLERS         comma-separated numbers accepted from the trunk
  VOICE_MAX_CALLS               agent calls at once (2)
  VOICE_RTP_PORTS               e.g. 40000-40019, a UDP range forwarded to here
  VOICE_STUN                    STUN server, host:port
  VOICE_EXTENSIONS              file with the extension list (extensions.lisp
                                when there is one)
  VOICE_RING_SECONDS            how long an extension rings (30)
  VOICE_OUTBOUND_CALLER_ID      your number at the provider; setting it lets
                                extensions marked :outbound dial out
  VOICE_WIKI_TOKEN              capability token for the LLM-wiki MCP server;
                                setting it lets the agent consult the wiki
  VOICE_WIKI_URL                where that server is (http://localhost:8788/mcp)
  VOICE_WIKI_EXCLUDE            comma-separated wiki tools to withhold (ask_wiki,
                                which takes too long for a phone call)

OVERRIDES are START-SERVER keyword arguments that take precedence."
  (let* ((trunk (and (env "FLOWROUTE_USERNAME") (env "FLOWROUTE_PASSWORD")
                     (list :registrar (format nil "sip:~a.sip.flowroute.com"
                                              (or (env "FLOWROUTE_POP") "us-west-or"))
                           :username (env "FLOWROUTE_USERNAME")
                           :password (env "FLOWROUTE_PASSWORD"))))
         (ports (env "VOICE_RTP_PORTS"))
         (dash (and ports (position #\- ports))))
    (apply #'start-server
           (append
            overrides
            (list :host (or (env "VOICE_HOST") (if trunk "0.0.0.0" "127.0.0.1"))
                  :port (parse-integer (or (env "VOICE_PORT") "5060"))
                  :allow (env-list "VOICE_ALLOW")
                  :trunk trunk
                  :allowed-callers (env-list "VOICE_ALLOWED_CALLERS")
                  :max-calls (parse-integer (or (env "VOICE_MAX_CALLS") "2"))
                  :rtp-port-range (and dash
                                       (cons (parse-integer ports :end dash)
                                             (parse-integer ports :start (1+ dash))))
                  :stun (env "VOICE_STUN")
                  :extensions (let ((path (or (env "VOICE_EXTENSIONS")
                                              (and (probe-file "extensions.lisp")
                                                   "extensions.lisp"))))
                                (and path (load-extensions path)))
                  :ring-seconds (parse-integer (or (env "VOICE_RING_SECONDS") "30"))
                  :outbound-caller-id (env "VOICE_OUTBOUND_CALLER_ID")
                  :llm (make-claude-llm
                        ;; Room for a few whole pages, and for a second search.
                        :max-tool-result 40000 :max-tool-rounds 6
                        :tools (when (env "VOICE_WIKI_TOKEN")
                                 (list (make-mcp-server
                                        :name "llm-wiki"
                                        :url (or (env "VOICE_WIKI_URL")
                                                 "http://localhost:8788/mcp")
                                        :token (env "VOICE_WIKI_TOKEN")
                                        :instructions *wiki-instructions*
                                        :seed *wiki-seed*
                                        :pinned *wiki-pinned*
                                        :exclude (or (env-list "VOICE_WIKI_EXCLUDE")
                                                     '("ask_wiki"))))))
                  :tts (if (equal (env "VOICE_TTS") "chatterbox")
                           (make-chatterbox-tts :voice (env "VOICE_REFERENCE"))
                           (make-say-tts)))))))
