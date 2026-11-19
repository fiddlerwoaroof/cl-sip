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

(defclass phone-server ()
  ((ua :initarg :ua :reader server-ua)
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

;;; Calls

(defun call-extension (call)
  (let ((uri (sip:request-uri (sip:call-invite call))))
    (or (and (sip:sip-uri-p uri) (sip:sip-uri-user uri)) "")))

(defun call-caller (call)
  (let ((from (sip:msg-from (sip:call-invite call))))
    (sip:uri-string (sip:name-addr-uri from))))

(defun in-background (function)
  "Run FUNCTION off the SIP event loop; media setup and teardown block."
  (bt:make-thread function :name "phone-server-task"))

(defun server-accept-media (server call)
  "Negotiate audio for CALL.  Returns the RTP session and the codec name, or
NIL after rejecting the call."
  (let* ((offer (handler-case (sdp:parse-sdp (or (sip:call-remote-sdp call) ""))
                  (error () nil)))
         (session (and offer
                       (rtp:rtp-session-start
                        (rtp:make-rtp-session :local-host (server-host server) :latch t)))))
    (multiple-value-bind (answer accepted)
        (if offer
            (sdp:make-answer offer
                             :address (sip:transport-advertised-host
                                       (first (sip:stack-transports
                                               (sip:ua-stack (server-ua server)))))
                             :port (rtp:rtp-session-local-port session))
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
  (let* ((extension (call-extension call))
         (action (dialplan-action (server-dialplan server) extension)))
    (server-log server "call from ~a to ~s -> ~s" (call-caller call) extension
                (if (functionp action) :function action))
    (cond
      ((null action) (sip:reject call 404))
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
                    (when stop (funcall stop))
                    (when session
                      (let ((session session))
                        (in-background (lambda () (rtp:rtp-session-stop session)))))))))
         (multiple-value-setq (session codec) (server-accept-media server call)))))))

;;; Server

(defun start-server (&key (host "127.0.0.1") (port 5060) users
                       (dialplan *default-dialplan*)
                       (stt (make-whisper-stt)) (llm (make-claude-llm)) (tts (make-say-tts))
                       (greeting "Hello! What can I do for you?")
                       (barge-in t) (tcp t) (log-stream *standard-output*))
  "Start a phone server on HOST:PORT (UDP, and TCP unless TCP is NIL).

Softphones may register with any user name; with USERS, an alist of
(name . password), registration requires digest authentication.  Calls are
routed by DIALPLAN (see *DEFAULT-DIALPLAN*).  With the default host only
programs on this machine can reach it; use \"0.0.0.0\" to accept phones on
the local network."
  (let* ((ua (sip:make-user-agent
              :aor (format nil "sip:server@~a" (if (string= host "0.0.0.0") "localhost" host))
              :transports `((:udp :host ,host :port ,port)
                            ,@(when tcp `((:tcp :host ,host :port ,port))))
              :registrar (sip:make-registrar :realm "cl-sip" :users users)
              :user-agent-name "cl-sip-voice/0.1"))
         (server (make-instance 'phone-server
                                :ua ua :host host :dialplan dialplan
                                :stt stt :llm llm :tts tts :greeting greeting
                                :barge-in barge-in :log-stream log-stream)))
    (setf (sip:ua-on-incoming-call ua)
          (lambda (call)
            (handler-case (server-handle-call server call)
              (error (e)
                (server-log server "error handling call: ~a" e)
                (ignore-errors (sip:reject call 500))))))
    (sip:start ua)
    (server-log server "listening on ~a:~d" host (server-port server))
    server))

(defun stop-server (server)
  (sip:stop (server-ua server))
  server)
