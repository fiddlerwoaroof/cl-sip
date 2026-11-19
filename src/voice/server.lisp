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

;;; Who is calling

(defun server-transport (server)
  (first (sip:stack-transports (sip:ua-stack (server-ua server)))))

(defun server-source-kind (server host)
  "How a request from HOST is treated: :TRUNK, :LOCAL, or NIL (ignored)."
  (cond ((some (lambda (cidr) (sip:cidr-match-p host cidr)) (server-trunk-ranges server))
         :trunk)
        ((or (sip:cidr-match-p host "127.0.0.0/8")
             (equal host (sip:transport-advertised-host (server-transport server)))
             (some (lambda (cidr) (sip:cidr-match-p host cidr)) (server-allow server)))
         :local)))

(defun server-screen-request (server request)
  "Request filter: anything not from the trunk or an allowed phone is dropped
without an answer, so scanners learn nothing."
  (let ((host (sip:message-remote-host request)))
    (or (and (server-source-kind server host) t)
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
  (server-source-kind server (sip:message-remote-host (sip:call-invite call))))

(defun call-extension (server call)
  "What was dialled.  A phone puts it in the Request-URI.  A trunk delivers
calls to our registered contact, so the number called is in the To header."
  (let ((invite (sip:call-invite call)))
    (if (eq (call-kind server call) :trunk)
        (string-left-trim "+" (uri-user (sip:name-addr-uri (sip:msg-to invite))))
        (uri-user (sip:request-uri invite)))))

(defun call-caller (call)
  (let ((from (sip:msg-from (sip:call-invite call))))
    (sip:uri-string (sip:name-addr-uri from))))

(defun call-caller-number (call)
  (uri-user (sip:name-addr-uri (sip:msg-from (sip:call-invite call)))))

(defun in-background (function)
  "Run FUNCTION off the SIP event loop; media setup and teardown block."
  (bt:make-thread function :name "phone-server-task"))

(defun server-accept-media (server call)
  "Negotiate audio for CALL.  Returns the RTP session and the codec name, or
NIL after rejecting the call."
  (let* ((trunk (eq (call-kind server call) :trunk))
         (offer (handler-case (sdp:parse-sdp (or (sip:call-remote-sdp call) ""))
                  (error () nil)))
         (session (and offer
                       (rtp:rtp-session-start
                        (rtp:make-rtp-session :local-host (server-host server) :latch t
                                              :port-range (server-rtp-port-range server)
                                              ;; Only the trunk is across a NAT.
                                              :stun (and trunk (server-stun server))))))
         (transport (server-transport server)))
    (multiple-value-bind (answer accepted)
        (if offer
            (sdp:make-answer
             offer
             ;; The trunk's media comes from the internet, so it needs our
             ;; public address; a local phone needs the local one.
             :address (or (and trunk (or (rtp:rtp-session-public-host session)
                                         (sip:transport-public-host transport)))
                          (sip:transport-advertised-host transport))
             :port (or (and trunk (rtp:rtp-session-public-port session))
                       (rtp:rtp-session-local-port session)))
            (values nil nil))
      (cond
        (accepted
         (multiple-value-bind (codecs direction remote-host remote-port)
             (sdp:negotiated-audio answer offer)
           (declare (ignore direction))
           (setf (rtp:rtp-session-remote-host session) remote-host
                 (rtp:rtp-session-remote-port session) remote-port)
           (sip:answer call :sdp (sdp:sdp-string answer))
           (values session
                   (sdp:codec-name
                    (find-if-not (lambda (c)
                                   (string-equal (sdp:codec-name c) "telephone-event"))
                                 codecs)))))
        (t
         (when session (in-background (lambda () (rtp:rtp-session-stop session))))
         (sip:reject call 488)
         nil)))))

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
  (let* ((kind (call-kind server call))
         (extension (call-extension server call))
         (action (dialplan-action (server-dialplan server) extension))
         (agent (eq action :agent)))
    (server-log server "~(~a~) call from ~a to ~s -> ~s" kind (call-caller call) extension
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
      ((and (consp action) (eq (first action) :reject))
       (sip:reject call (or (second action) 486)))
      (t
       (let ((stop nil) (session nil) (codec nil))
         (setf (sip:call-on-event call)
               (lambda (call event &key cause &allow-other-keys)
                 (case event
                   (:established
                    (setf stop (server-start-media server call action session codec)))
                   (:terminated
                    (server-log server "call from ~a ended (~(~a~))" (call-caller call) cause)
                    (when (and agent session) (decf (server-active-calls server)))
                    (when stop (funcall stop))
                    (when session
                      (let ((session session))
                        (in-background (lambda () (rtp:rtp-session-stop session)))))))))
         (multiple-value-setq (session codec) (server-accept-media server call))
         (when (and agent session) (incf (server-active-calls server))))))))

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
                       trunk allow allowed-callers (max-calls 2) rtp-port-range stun)
  "Start a phone server on HOST:PORT (UDP, and TCP unless TCP is NIL).

Softphones may register with any user name; with USERS, an alist of
(name . password), registration requires digest authentication.  Calls are
routed by DIALPLAN (see *DEFAULT-DIALPLAN*).  Only this machine may register
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
  (let* ((trunk-user (getf trunk :username))
         (registrar (and trunk (sip:ensure-uri (getf trunk :registrar))))
         (ua (sip:make-user-agent
              :aor (if trunk
                       (format nil "sip:~a@~a" trunk-user (sip:sip-uri-host registrar))
                       (format nil "sip:server@~a"
                               (if (string= host "0.0.0.0") "localhost" host)))
              :transports `((:udp :host ,host :port ,port)
                            ,@(when tcp `((:tcp :host ,host :port ,port))))
              :credentials (when trunk `((nil ,trunk-user ,(getf trunk :password))))
              :registrar (sip:make-registrar :realm "cl-sip" :users users)
              :user-agent-name "cl-sip-voice/0.1"))
         (server (make-instance 'phone-server
                                :ua ua :host host :dialplan dialplan
                                :stt stt :llm llm :tts tts :greeting greeting
                                :barge-in barge-in :log-stream log-stream
                                :trunk-ranges (and trunk
                                                   (getf trunk :ranges
                                                         *flowroute-signalling-ranges*))
                                :allow allow :allowed-callers allowed-callers
                                :max-calls max-calls :rtp-port-range rtp-port-range
                                :stun (etypecase stun
                                        (null nil)
                                        (cons stun)
                                        (string (parse-host-port stun 3478))))))
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
                  :tts (if (equal (env "VOICE_TTS") "chatterbox")
                           (make-chatterbox-tts :voice (env "VOICE_REFERENCE"))
                           (make-say-tts)))))))
