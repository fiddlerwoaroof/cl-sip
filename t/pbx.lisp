(in-package :cl-sip.test)

(def-suite pbx :in cl-sip)
(in-suite pbx)

;;;; The PBX is tested by simulation: stand-in phones are user agents on
;;;; loopback sockets, registering with and calling a real phone server.

(defparameter *extensions*
  '(("100" :password "alice-pw" :name "Alice")
    ("101" :password "bob-pw" :name "Bob")
    ("102" :password "carol-pw" :name "Carol")))

(defun start-pbx (&rest args &key (extensions *extensions*) &allow-other-keys)
  ;; The caller's arguments come first, so they win over the defaults.
  (apply #'voice:start-server
         (append
          (alexandria:remove-from-plist args :extensions)
          (list :port 0 :tcp nil :log-stream nil :greeting nil
                :extensions extensions
                :stt (voice:make-function-stt (lambda (samples rate)
                                                (declare (ignore samples rate)) nil))
                :llm (voice:make-function-llm (lambda (messages)
                                                (declare (ignore messages)) "Fine."))
                :tts (voice:make-function-tts (lambda (text)
                                                (declare (ignore text)) (tone 1600)))))))

(defun pbx-uri (server &optional user)
  (format nil "sip:~@[~a@~]127.0.0.1:~d" user (voice:server-port server)))

(defun start-phone (number password &rest args)
  "A stand-in softphone for extension NUMBER."
  (let ((ua (apply #'sip:make-user-agent
                   :aor (format nil "sip:~a@127.0.0.1" number)
                   :credentials (and password `((nil ,number ,password)))
                   :transports '((:udp :host "127.0.0.1" :port 0))
                   args)))
    (sip:start ua)
    ua))

(defun register-phone (ua server)
  "Register UA with SERVER and wait for the outcome, :registered or :failed."
  (let ((registration (sip:register ua :registrar (pbx-uri server) :expires 300)))
    (sip:wait-until (lambda () (member (sip:registration-state registration)
                                       '(:registered :failed))))
    (sip:registration-state registration)))

(defmacro with-phones ((server &rest phones) &body body)
  "PHONES are (variable number password); each is started, and stopped at
the end together with SERVER."
  `(let (,@(loop for (var number password) in phones
                 collect `(,var (start-phone ,number ,password))))
     (unwind-protect (progn ,@body)
       ,@(loop for (var) in phones collect `(sip:stop ,var))
       (voice:stop-server ,server))))

(defun wait-for-call (call &rest states)
  (sip:wait-until (lambda () (member (sip:call-state call) states)) :timeout 5))

(test extension-list
  (let ((extensions (voice:parse-extensions
                     '(("100" :password "a" :name "Alice" :no-answer "101" :ring-seconds 20)
                       ("101" :password "b" :busy :agent)
                       ("200" :ring ("100" "101") :name "Desk")))))
    (is (= 3 (length extensions)))
    (is (string= "Alice" (voice:extension-name (voice:find-extension extensions "100"))))
    (is (null (voice:find-extension extensions "999"))))
  ;; Mistakes are caught when the list is read, not when a call arrives.
  (signals error (voice:parse-extensions '(("100"))))
  (signals error (voice:parse-extensions '(("100" :password "a") ("100" :password "b"))))
  (signals error (voice:parse-extensions '(("100" :password "a" :colour "red"))))
  (signals error (voice:parse-extensions '(("1 00" :password "a"))))
  (signals error (voice:parse-extensions '(("200" :ring ("100")))))
  (signals error (voice:parse-extensions '(("100" :password "a" :forward 7))))
  ;; From a file: data only.
  (uiop:with-temporary-file (:pathname path :type "lisp" :stream out :direction :output)
    (format out "; the office~%(\"100\" :password \"a\" :name \"Alice\")~%(\"101\" :password \"b\" :forward :agent)~%")
    (close out)
    (is (equal '("100" "101")
               (mapcar #'voice:extension-number (voice:load-extensions path)))))
  (uiop:with-temporary-file (:pathname path :type "lisp" :stream out :direction :output)
    (format out "(\"100\" :password #.(string #\\a))~%")
    (close out)
    (signals error (voice:load-extensions path))))

(test dialled-numbers-rfc3966
  (is (string= "12065550100" (voice:normalize-number "+1-206-555-0100")))
  (is (string= "12065550100" (voice:normalize-number "1 (206) 555.0100")))
  (is (string= "100" (voice:normalize-number "100;phone-context=office.example.com")))
  (is (string= "*72" (voice:normalize-number "*72")))
  ;; Names are not numbers.
  (is (string= "alice.smith" (voice:normalize-number "alice.smith")))
  (is (string= "" (voice:normalize-number nil))))

(test extensions-register-as-themselves
  (let ((server (start-pbx)))
    (with-phones (server (alice "100" "alice-pw") (mallory "101" "wrong")
                         (stranger "555" "whatever") (eve "101" nil))
      (is (eq :registered (register-phone alice server)))
      (is (eq :failed (register-phone mallory server)))
      (is (eq :failed (register-phone stranger server)))
      (let ((registrar (sip:ua-registrar (voice:server-ua server))))
        (is (= 1 (length (sip:registrar-lookup-user registrar (voice:server-ua server) "100"))))
        (is (null (sip:registrar-lookup-user registrar (voice:server-ua server) "101")))
        ;; Alice's password does not let anyone register Bob's number.
        (setf (sip:ua-credentials eve) '((nil "100" "alice-pw")))
        (let ((registration (sip:register eve :registrar (pbx-uri server))))
          (sip:wait-until (lambda () (member (sip:registration-state registration)
                                             '(:registered :failed))))
          (is (eq :failed (sip:registration-state registration)))
          (is (= 403 (sip:response-status (sip:registration-last-response registration))))
          (is (null (sip:registrar-lookup-user registrar (voice:server-ua server) "101"))))))))

(test calls-are-authenticated
  (let ((server (start-pbx)))
    (with-phones (server (alice "100" "alice-pw") (nobody "100" nil) (mallory "100" "guess"))
      ;; With credentials the challenge is answered and the call goes through
      ;; (700 is the spoken test line, which hangs up by itself).
      (let ((call (sip:invite alice (pbx-uri server "700")
                              :sdp (sdp:sdp-string (sdp:make-audio-offer :port 40000)))))
        (is-true (wait-for-call call :established))
        (sip:hangup call)
        (is-true (wait-for-call call :terminated)))
      ;; Without, or with the wrong ones, it is refused.
      (dolist (phone (list nobody mallory))
        (let ((call (sip:invite phone (pbx-uri server "700")
                                :sdp (sdp:sdp-string (sdp:make-audio-offer :port 40000)))))
          (is-true (wait-for-call call :terminated))
          (is (= 401 (sip:call-status call))))))))

;;;; Bridged calls.  A desk is a stand-in phone that answers by itself.

(defvar *sessions* '() "RTP sessions opened by the current test, to be stopped.")

(defstruct desk ua session (calls '()) (received '()) (requests '()) (lock (bt:make-lock)))

(defun desk-count (desk &optional payload-type)
  (bt:with-lock-held ((desk-lock desk))
    (count-if (lambda (p) (or (null payload-type)
                              (= payload-type (rtp:rtp-packet-payload-type p))))
              (desk-received desk))))

(defun desk-call (desk) (first (desk-calls desk)))

(defun open-desk (ua)
  (let* ((desk (make-desk :ua ua))
         (session (rtp:rtp-session-start
                   (rtp:make-rtp-session
                    :on-packet (lambda (session packet)
                                 (declare (ignore session))
                                 (bt:with-lock-held ((desk-lock desk))
                                   (push packet (desk-received desk))))))))
    (push session *sessions*)
    (setf (desk-session desk) session)
    desk))

(defun desk-answer-sdp (desk offer-text &rest args)
  "Answer OFFER-TEXT for DESK's session and point the session at the offerer."
  (let* ((offer (sdp:parse-sdp offer-text))
         (answer (apply #'sdp:make-answer offer
                        :address "127.0.0.1"
                        :port (rtp:rtp-session-local-port (desk-session desk))
                        args)))
    (multiple-value-bind (codecs direction host port) (sdp:negotiated-audio answer offer)
      (declare (ignore codecs direction))
      (setf (rtp:rtp-session-remote-host (desk-session desk)) host
            (rtp:rtp-session-remote-port (desk-session desk)) port))
    (sdp:sdp-string answer)))

(defun attend (ua &key (after 0) status codecs early on-event)
  "Make UA behave like a phone on a desk: it rings, and after AFTER seconds
answers (never, when AFTER is NIL).  With STATUS it refuses instead.  With
EARLY it sends a 183 with a session description before ringing on.  Returns
the DESK, which records the calls and the audio received."
  (let ((desk (open-desk ua)))
    (setf (sip:ua-on-incoming-call ua)
          (lambda (call)
            (push call (desk-calls desk))
            (when on-event (setf (sip:call-on-event call) on-event))
            (cond
              (status (sip:reject call status))
              (t
               (flet ((answer-sdp ()
                        (apply #'desk-answer-sdp desk (sip:call-remote-sdp call)
                               (and codecs (list :codecs codecs)))))
                 (if early
                     (sip:ring call :status 183 :sdp (answer-sdp))
                     (sip:ring call))
                 (when after
                   (sip:schedule (sip:ua-scheduler ua) after
                                 (lambda ()
                                   (when (eq (sip:call-state call) :ringing)
                                     (sip:answer call :sdp (answer-sdp)))))))))))
    desk))

(defun dial (desk server number &key on-event (codecs sdp:*default-codecs*))
  "Call NUMBER from DESK's phone.  Media is pointed at the server as soon
as a session description arrives."
  (let ((session (desk-session desk)))
    (sip:invite (desk-ua desk) (pbx-uri server number)
                :sdp (sdp:sdp-string
                      (sdp:make-audio-offer :port (rtp:rtp-session-local-port session)
                                            :codecs codecs))
                :on-event
                (lambda (call event &rest args &key sdp &allow-other-keys)
                  (when (and sdp (member event '(:progress :established)))
                    (multiple-value-bind (codecs direction host port)
                        (sdp:negotiated-audio (sdp:parse-sdp sdp))
                      (declare (ignore codecs direction))
                      (setf (rtp:rtp-session-remote-host session) host
                            (rtp:rtp-session-remote-port session) port)))
                  (when on-event (apply on-event call event args))))))

(defun say (desk packets &key (payload-type 0))
  "Send PACKETS packets of tone from DESK."
  (let ((payload (if (= payload-type 8)
                     (rtp:alaw-encode (tone 160))
                     (rtp:ulaw-encode (tone 160)))))
    (dotimes (i packets)
      (rtp:rtp-send (desk-session desk) payload :payload-type payload-type)
      (sleep 1/500))))

(defun hears (desk packets &optional payload-type)
  (sip:wait-until (lambda () (>= (desk-count desk payload-type) packets)) :timeout 5))

(defmacro with-pbx ((server start-form &rest phones) &body body)
  "Run BODY with SERVER bound to the result of START-FORM and each of PHONES,
(variable number password), a registered stand-in phone."
  `(let ((*sessions* '())
         (,server ,start-form))
     (with-phones (,server ,@phones)
       (unwind-protect
            (progn
              ,@(loop for (var) in phones
                      collect `(is (eq :registered (register-phone ,var ,server))))
              ,@body)
         (mapc #'rtp:rtp-session-stop *sessions*)))))

(test call-between-extensions
  (with-pbx (server (start-pbx) (alice "100" "alice-pw") (bob "101" "bob-pw"))
    (multiple-value-bind (on-event events) (recorder)
      (let* ((caller (open-desk alice))
             (callee (attend bob :after 1/4))
             (call (dial caller server "101" :on-event on-event)))
        (is-true (wait-for-call call :established))
        ;; Bob's phone rang, and Alice was told so (RFC 3960: she makes her
        ;; own ringback).
        (is (member :ringing (event-names (funcall events))))
        ;; Bob sees who is calling as the server vouches for it.
        (let ((from (sip:msg-from (sip:call-invite (desk-call callee)))))
          (is (string= "Alice" (sip:name-addr-display-name from)))
          (is (string= "100" (sip:sip-uri-user (sip:name-addr-uri from)))))
        (is (string= "101" (sip:sip-uri-user
                            (sip:name-addr-uri
                             (sip:msg-to (sip:call-invite (desk-call callee)))))))
        (is (= 1 (length (voice::server-bridges server))))
        ;; Audio flows both ways through the server.
        (is-true (wait-for-call (desk-call callee) :established))
        (say caller 10)
        (is-true (hears callee 10 0))
        (say callee 5)
        (is-true (hears caller 5 0))
        ;; Hanging up one side releases the other.
        (sip:hangup call)
        (is-true (wait-for-call (desk-call callee) :terminated))
        (is (eq :remote-hangup (sip:call-cause (desk-call callee))))
        (is-true (sip:wait-until (lambda () (null (voice::server-bridges server)))))))))

(test callee-hangs-up
  (with-pbx (server (start-pbx) (alice "100" "alice-pw") (bob "101" "bob-pw"))
    (let* ((caller (open-desk alice))
           (callee (attend bob))
           (call (dial caller server "101")))
      (is-true (wait-for-call call :established))
      (is-true (wait-for-call (desk-call callee) :established))
      (sip:hangup (desk-call callee))
      (is-true (wait-for-call call :terminated))
      (is (eq :remote-hangup (sip:call-cause call))))))

(test unreachable-busy-and-declined
  (with-pbx (server (start-pbx) (alice "100" "alice-pw") (bob "101" "bob-pw"))
    (let ((caller (open-desk alice)))
      ;; Carol (102) is listed but has no phone registered.
      (let ((call (dial caller server "102")))
        (is-true (wait-for-call call :terminated))
        (is (= 480 (sip:call-status call))))
      (attend bob :status 486)
      (let ((call (dial caller server "101")))
        (is-true (wait-for-call call :terminated))
        (is (= 486 (sip:call-status call))))
      (attend bob :status 603)
      (let ((call (dial caller server "101")))
        (is-true (wait-for-call call :terminated))
        (is (= 603 (sip:call-status call))))
      (is (null (voice::server-bridges server))))))

(test caller-gives-up-while-ringing
  (with-pbx (server (start-pbx) (alice "100" "alice-pw") (bob "101" "bob-pw"))
    (let* ((caller (open-desk alice))
           (callee (attend bob :after nil))
           (call (dial caller server "101")))
      (is-true (wait-for-call call :ringing))
      (sip:hangup call)
      (is-true (sip:wait-until (lambda () (and (desk-call callee)
                                               (eq :terminated
                                                   (sip:call-state (desk-call callee)))))))
      (is (eq :remote-cancel (sip:call-cause (desk-call callee))))
      (is-true (wait-for-call call :terminated))
      (is (eq :cancelled (sip:call-cause call))))))

(test nobody-answers
  (with-pbx (server (start-pbx :ring-seconds 1/2) (alice "100" "alice-pw") (bob "101" "bob-pw"))
    (let* ((caller (open-desk alice))
           (callee (attend bob :after nil))
           (call (dial caller server "101")))
      (is-true (wait-for-call call :terminated))
      (is (= 480 (sip:call-status call)))
      ;; Bob's phone stops ringing.
      (is-true (wait-for-call (desk-call callee) :terminated))
      (is (eq :remote-cancel (sip:call-cause (desk-call callee)))))))

(test every-phone-of-an-extension-rings
  (let ((cancels '()))
    (with-pbx (server (start-pbx) (alice "100" "alice-pw") (bob "101" "bob-pw"))
      ;; Bob has a second phone, which never answers.
      (let ((second (start-phone "101" "bob-pw"
                                 :trace (lambda (direction message host port)
                                          (declare (ignore host port))
                                          (when (and (eq direction :in)
                                                     (sip:request-p message)
                                                     (string= "CANCEL"
                                                              (sip:request-method message)))
                                            (push message cancels))))))
        (unwind-protect
             (progn
               (is (eq :registered (register-phone second server)))
               (let* ((caller (open-desk alice))
                      (first-desk (attend bob :after 1/4))
                      (second-desk (attend second :after nil))
                      (call (dial caller server "101")))
                 (is-true (wait-for-call call :established))
                 (is-true (wait-for-call (desk-call first-desk) :established))
                 ;; The phone that lost is told why (RFC 3326).
                 (is-true (wait-for-call (desk-call second-desk) :terminated))
                 (is (= 1 (length cancels)))
                 (is (search "cause=200" (sip:header (first cancels) "Reason")))
                 (say caller 5)
                 (is-true (hears first-desk 5))
                 (is (zerop (desk-count second-desk)))
                 (sip:hangup call)
                 (is-true (wait-for-call call :terminated))))
          (sip:stop second))))))

(test ring-group
  (with-pbx (server (start-pbx :extensions (append *extensions*
                                                   '(("200" :ring ("101" "102") :name "Desk"))))
                    (alice "100" "alice-pw") (bob "101" "bob-pw") (carol "102" "carol-pw"))
    (let* ((caller (open-desk alice))
           (bob-desk (attend bob :after nil))
           (carol-desk (attend carol :after 1/4))
           (call (dial caller server "200")))
      (is-true (wait-for-call call :established))
      (is-true (wait-for-call (desk-call carol-desk) :established))
      (is-true (wait-for-call (desk-call bob-desk) :terminated))
      (say caller 5)
      (is-true (hears carol-desk 5))
      (sip:hangup call)
      (is-true (wait-for-call (desk-call carol-desk) :terminated)))))

(test bridge-transcodes-and-relays-telephone-events
  (with-pbx (server (start-pbx) (alice "100" "alice-pw") (bob "101" "bob-pw"))
    (let* ((caller (open-desk alice))
           ;; Bob's phone only speaks A-law; Alice's numbers telephone events
           ;; 96 where the server's own offer to Bob says 101.
           (callee (attend bob :codecs (list (sdp:make-codec :payload-type 8 :name "PCMA")
                                             (sdp:make-codec :payload-type 101
                                                             :name "telephone-event"
                                                             :fmtp "0-15"))))
           (call (dial caller server "101"
                       :codecs (list (sdp:make-codec :payload-type 0 :name "PCMU")
                                     (sdp:make-codec :payload-type 96
                                                     :name "telephone-event"
                                                     :fmtp "0-15")))))
      (is-true (wait-for-call call :established))
      (is-true (wait-for-call (desk-call callee) :established))
      (say caller 5 :payload-type 0)
      (is-true (hears callee 5 8))
      (is (zerop (desk-count callee 0)))
      ;; The tone survives the change of law.
      (let ((payload (rtp:rtp-packet-payload (first (desk-received callee)))))
        (is (< 4000 (voice:rms (rtp:alaw-decode payload)) 7000)))
      (say callee 5 :payload-type 8)
      (is-true (hears caller 5 0))
      ;; A digit pressed by Alice (payload type 96) reaches Bob as 101.
      (let ((sent (rtp:rtp-send-dtmf (desk-session caller) #\5 :payload-type 96)))
        (is-true (hears callee (length sent) 101))
        (let ((event (rtp:decode-telephone-event
                      (rtp:rtp-packet-payload
                       (find 101 (desk-received callee) :key #'rtp:rtp-packet-payload-type)))))
          (is (= 5 (rtp:telephone-event-event event)))))
      (sip:hangup call)
      (is-true (wait-for-call call :terminated)))))

(test early-media-is-relayed
  (with-pbx (server (start-pbx) (alice "100" "alice-pw") (bob "101" "bob-pw"))
    (multiple-value-bind (on-event events) (recorder)
      (let* ((caller (open-desk alice))
             (callee (attend bob :after 1 :early t))
             (call (dial caller server "101" :on-event on-event)))
        ;; A 183 with a session description reaches Alice before any answer,
        (is-true (sip:wait-until
                  (lambda () (find :progress (funcall events) :key #'car))))
        (let ((progress (find :progress (funcall events) :key #'car)))
          (is (= 183 (getf (cdr progress) :status)))
          (is-true (getf (cdr progress) :sdp)))
        (is (not (eq :established (sip:call-state call))))
        ;; and what Bob's side plays is heard meanwhile.
        (say callee 5)
        (is-true (hears caller 5))
        (is-true (wait-for-call call :established))
        (say callee 5)
        (is-true (hears caller 10))
        (sip:hangup call)
        (is-true (wait-for-call call :terminated))))))

;;; Forwarding (RFC 5359 sections 2.7 to 2.9) and History-Info (RFC 7044)

(defun forwarding-extensions (&rest bob-options)
  `(("100" :password "alice-pw" :name "Alice")
    ("101" :password "bob-pw" :name "Bob" ,@bob-options)
    ("102" :password "carol-pw" :name "Carol")))

(defun history (call)
  (sip:header-values (sip:call-invite call) "History-Info"))

(test forward-unconditionally
  (with-pbx (server (start-pbx :extensions (forwarding-extensions :forward "102"))
                    (alice "100" "alice-pw") (bob "101" "bob-pw") (carol "102" "carol-pw"))
    (let* ((caller (open-desk alice))
           (bob-desk (attend bob))
           (carol-desk (attend carol))
           (call (dial caller server "101")))
      (is-true (wait-for-call call :established))
      (is-true (wait-for-call (desk-call carol-desk) :established))
      (is (null (desk-calls bob-desk)))
      ;; Carol's phone can see the call was meant for Bob, and why it came
      ;; to her: 101 (moved), then 102 as a different user, then her phone.
      (let ((entries (history (desk-call carol-desk))))
        (is (= 3 (length entries)))
        (is (search "sip:101@" (first entries)))
        (is (search "Reason=SIP%3Bcause%3D302" (first entries)))
        (is (search ";index=1" (first entries)))
        (is (search "sip:102@" (second entries)))
        (is (search ";index=1.1;mp=1" (second entries)))
        (is (search ";index=1.2;rc=1.1" (third entries))))
      ;; She still sees Alice as the caller.
      (is (string= "100" (sip:sip-uri-user
                          (sip:name-addr-uri
                           (sip:msg-from (sip:call-invite (desk-call carol-desk)))))))
      (say caller 5)
      (is-true (hears carol-desk 5))
      (sip:hangup call)
      (is-true (wait-for-call call :terminated)))))

(test forward-when-busy
  (with-pbx (server (start-pbx :extensions (forwarding-extensions :busy "102"))
                    (alice "100" "alice-pw") (bob "101" "bob-pw") (carol "102" "carol-pw"))
    (let* ((caller (open-desk alice))
           (bob-desk (attend bob :status 486))
           (carol-desk (attend carol))
           (call (dial caller server "101")))
      (is-true (wait-for-call call :established))
      (is (= 1 (length (desk-calls bob-desk))))
      ;; Bob's own phone was told nothing of a forward; Carol's is.
      (is (= 2 (length (history (desk-call bob-desk)))))
      (is (search "cause%3D486" (first (history (desk-call carol-desk)))))
      (sip:hangup call)
      (is-true (wait-for-call call :terminated)))))

(test forward-on-no-answer-to-the-agent
  (with-pbx (server (start-pbx :extensions (forwarding-extensions :no-answer :agent
                                                                  :ring-seconds 1/2)
                               :greeting "Hello.")
                    (alice "100" "alice-pw") (bob "101" "bob-pw"))
    (let* ((caller (open-desk alice))
           (bob-desk (attend bob :after nil))
           (call (dial caller server "101")))
      ;; Bob does not pick up, so the server answers and the agent greets
      ;; Alice (1600 samples of stand-in speech: ten packets).
      (is-true (wait-for-call call :established))
      (is-true (wait-for-call (desk-call bob-desk) :terminated))
      (is-true (hears caller 10))
      (is (= 1 (voice::server-active-calls server)))
      (sip:hangup call)
      (is-true (wait-for-call call :terminated))
      (is-true (sip:wait-until (lambda () (zerop (voice::server-active-calls server))))))))

(test forward-when-unreachable-and-to-the-dialplan
  ;; Nobody is registered as Bob: the no-answer target applies at once.
  (with-pbx (server (start-pbx :extensions (forwarding-extensions :no-answer "102"))
                    (alice "100" "alice-pw") (carol "102" "carol-pw"))
    (let* ((caller (open-desk alice))
           (carol-desk (attend carol))
           (call (dial caller server "101")))
      (is-true (wait-for-call call :established))
      (is (search "cause%3D480" (first (history (desk-call carol-desk)))))
      (sip:hangup call)
      (is-true (wait-for-call call :terminated))))
  ;; A target need not be an extension: 700 is the spoken test line, which
  ;; talks and then hangs up.
  (with-pbx (server (start-pbx :extensions (forwarding-extensions :forward "700"))
                    (alice "100" "alice-pw"))
    (let* ((caller (open-desk alice))
           (call (dial caller server "101")))
      (is-true (wait-for-call call :established))
      (is-true (hears caller 10))
      (is-true (wait-for-call call :terminated))
      (is (eq :remote-hangup (sip:call-cause call))))))

(test forwarding-loops-end
  (with-pbx (server (start-pbx :extensions '(("100" :password "alice-pw")
                                             ("101" :password "bob-pw" :forward "102")
                                             ("102" :password "carol-pw" :forward "101")))
                    (alice "100" "alice-pw"))
    (let* ((caller (open-desk alice))
           (call (dial caller server "101")))
      (is-true (wait-for-call call :terminated))
      (is (= 482 (sip:call-status call)))
      (is (null (voice::server-bridges server))))))

;;; Transfer (RFC 3515, RFC 5589; RFC 5359 sections 2.4 and 2.5)

(defun progress-reports (events)
  "The (status final) pairs of the :refer-progress events in EVENTS."
  (loop for (name . args) in (funcall events)
        when (eq name :refer-progress)
          collect (list (getf args :status) (getf args :final))))

(test unattended-transfer-rfc5359-2.4
  (with-pbx (server (start-pbx) (alice "100" "alice-pw") (bob "101" "bob-pw")
                    (carol "102" "carol-pw"))
    (multiple-value-bind (on-bob bob-events) (recorder)
      (let* ((caller (open-desk alice))
             (bob-desk (attend bob :on-event on-bob))
             (carol-desk (attend carol :after 1/4))
             (call (dial caller server "101")))
        (is-true (wait-for-call call :established))
        (is-true (wait-for-call (desk-call bob-desk) :established))
        ;; Bob, who was called, sends Alice on to Carol.
        (sip:refer (desk-call bob-desk) (pbx-uri server "102"))
        (is-true (sip:wait-until (lambda () (find :refer-accepted (funcall bob-events)
                                                  :key #'car))))
        ;; Carol's phone rings with Alice as the caller, sent by Bob.
        (is-true (sip:wait-until (lambda () (desk-call carol-desk))))
        (let ((invite (sip:call-invite (desk-call carol-desk))))
          (is (string= "100" (sip:sip-uri-user (sip:name-addr-uri (sip:msg-from invite)))))
          (is (search "sip:101@" (sip:header invite "Referred-By"))))
        (is-true (wait-for-call (desk-call carol-desk) :established))
        ;; Bob is told how it went, then released; Alice's call never changed.
        (is-true (wait-for-call (desk-call bob-desk) :terminated))
        (is (equal '((100 nil) (200 t)) (progress-reports bob-events)))
        (is (eq :established (sip:call-state call)))
        (say caller 5)
        (is-true (hears carol-desk 5))
        (say carol-desk 4)
        (is-true (hears caller 4))
        (is (zerop (desk-count bob-desk)))
        (is (= 1 (length (voice::server-bridges server))))
        (sip:hangup call)
        (is-true (wait-for-call (desk-call carol-desk) :terminated))
        (is-true (sip:wait-until (lambda () (null (voice::server-bridges server)))))))))

(test failed-transfer-leaves-the-call-alone
  (with-pbx (server (start-pbx :dialplan '((:default (:reject 404))))
                    (alice "100" "alice-pw") (bob "101" "bob-pw") (carol "102" "carol-pw"))
    (multiple-value-bind (on-alice alice-events) (recorder)
      (let* ((caller (open-desk alice))
             (bob-desk (attend bob))
             (call (dial caller server "101" :on-event on-alice)))
        (attend carol :status 486)
        (is-true (wait-for-call call :established))
        (is-true (wait-for-call (desk-call bob-desk) :established))
        ;; This time the caller transfers, and Carol is busy.
        (sip:refer call (pbx-uri server "102"))
        (is-true (sip:wait-until
                  (lambda () (equal '((100 nil) (486 t)) (progress-reports alice-events)))))
        (is (eq :established (sip:call-state call)))
        (is (eq :established (sip:call-state (desk-call bob-desk))))
        (say caller 5)
        (is-true (hears bob-desk 5))
        ;; Nor does this dialplan have anything at 555.
        (sip:refer call (pbx-uri server "555"))
        (is-true (sip:wait-until
                  (lambda () (equal '((100 nil) (486 t) (100 nil) (404 t))
                                    (progress-reports alice-events)))))
        (is (eq :established (sip:call-state call)))
        (say caller 3)
        (is-true (hears bob-desk 8))
        (sip:hangup call)
        (is-true (wait-for-call call :terminated))))))

(test attended-transfer-rfc5359-2.5
  (with-pbx (server (start-pbx) (alice "100" "alice-pw") (bob "101" "bob-pw")
                    (carol "102" "carol-pw"))
    (multiple-value-bind (on-alice alice-events) (recorder)
      (multiple-value-bind (on-bob bob-events) (recorder)
        (let* ((line-1 (open-desk alice))
               (line-2 (open-desk alice))
               (bob-desk (attend bob :on-event on-bob))
               (carol-desk (attend carol))
               (first-call (dial line-1 server "101" :on-event on-alice)))
          (is-true (wait-for-call first-call :established))
          (is-true (wait-for-call (desk-call bob-desk) :established))
          ;; Alice holds Bob, calls Carol to ask, then joins the two.
          (sip:reinvite first-call
                        :sdp (sdp:sdp-string
                              (sdp:hold (sdp:parse-sdp (sip:call-local-sdp first-call)))))
          (is-true (sip:wait-until (lambda () (find :reinvite (funcall bob-events)
                                                    :key #'car))))
          (let ((second-call (dial line-2 server "102")))
            (is-true (wait-for-call second-call :established))
            (is-true (wait-for-call (desk-call carol-desk) :established))
            (is (= 2 (length (voice::server-bridges server))))
            (sip:refer first-call (pbx-uri server "102") :replaces second-call)
            ;; Both of Alice's calls are released,
            (is-true (wait-for-call first-call :terminated))
            (is-true (wait-for-call second-call :terminated))
            (is (equal '((100 nil) (200 t)) (progress-reports alice-events)))
            ;; Bob is taken off hold,
            (is-true (sip:wait-until
                      (lambda () (= 2 (count :reinvite (funcall bob-events) :key #'car)))))
            (is (eq :sendrecv
                    (audio-direction
                     (getf (cdr (find :reinvite (reverse (funcall bob-events)) :key #'car))
                           :sdp))))
            ;; and Bob and Carol are talking.
            (is (eq :established (sip:call-state (desk-call bob-desk))))
            (is (eq :established (sip:call-state (desk-call carol-desk))))
            (is (= 1 (length (voice::server-bridges server))))
            (say bob-desk 5)
            (is-true (hears carol-desk 5))
            (say carol-desk 4)
            (is-true (hears bob-desk 4))
            (sip:hangup (desk-call bob-desk))
            (is-true (wait-for-call (desk-call carol-desk) :terminated))))))))

(test transfer-to-the-agent
  (with-pbx (server (start-pbx :greeting nil) (alice "100" "alice-pw") (bob "101" "bob-pw"))
    (let* ((caller (open-desk alice))
           (bob-desk (attend bob))
           (call (dial caller server "101")))
      (is-true (wait-for-call call :established))
      (is-true (wait-for-call (desk-call bob-desk) :established))
      ;; 700 is the spoken test line: Alice hears it, then it hangs up.
      (sip:refer (desk-call bob-desk) (pbx-uri server "700"))
      (is-true (wait-for-call (desk-call bob-desk) :terminated))
      (is-true (hears caller 10))
      (is-true (wait-for-call call :terminated))
      (is (null (voice::server-bridges server))))))

(test refer-rfc3515
  ;; The protocol on its own, between two plain agents.
  (multiple-value-bind (world alice bob incoming bob-events) (call-pair :on-incoming :answer)
    (declare (ignorable bob))
    (multiple-value-bind (on-event events) (recorder)
      (let ((call (sip:invite alice *bob* :sdp *offer* :on-event on-event)))
        (settle world)
        (clear-wire world)
        ;; Declined unless the application wants them.
        (sip:refer call "sip:carol@chicago.example.com")
        (settle world)
        (is (equal '("REFER" "603/REFER") (wire world)))
        (is (eq :refer-rejected (car (first (last (funcall events))))))
        ;; Accepted: 202, an immediate NOTIFY, and the event.
        (clear-wire world)
        (setf (sip:call-accept-refer (funcall incoming)) t)
        (sip:refer call "sip:carol@chicago.example.com" :replaces call)
        (settle world)
        (is (equal '("REFER" "202/REFER" "NOTIFY" "200/NOTIFY") (wire world)))
        (let ((notify (first (wire-messages world "NOTIFY")))
              (refer (cdr (first (last (funcall bob-events))))))
          (is (alexandria:starts-with-subseq "refer;id=" (sip:header notify "Event")))
          (is (search "active" (sip:header notify "Subscription-State")))
          (is (string= "message/sipfrag" (sip:msg-content-type notify)))
          (is (alexandria:starts-with-subseq "SIP/2.0 100 Trying"
                                             (sip:message-body-string notify)))
          (is (string= "carol" (sip:sip-uri-user
                                (sip:name-addr-uri (getf refer :refer-to)))))
          ;; The Replaces names the dialog from the far party's side.
          (is (eq (funcall incoming)
                  (sip:find-replaced-call bob (getf refer :replaces))))
          ;; Progress and the final outcome.
          (clear-wire world)
          (funcall (getf refer :notify) 180)
          (funcall (getf refer :notify) 200 t)
          (settle world)
          (is (equal '("NOTIFY" "NOTIFY" "200/NOTIFY" "200/NOTIFY") (wire world)))
          (is (search "terminated" (sip:header (second (wire-messages world "NOTIFY"))
                                               "Subscription-State")))
          (is (equal '((100 nil) (180 nil) (200 t)) (progress-reports events))))
        ;; Two Refer-To headers, or none, are an error (section 2.4.2).
        (clear-wire world)
        (let ((bad (sip::call-make-request call "REFER")))
          (sip::ua-send alice bad (lambda (&rest args) (declare (ignore args)))
                        :dialog (sip:call-dialog call)))
        (settle world)
        (is (equal '("REFER" "400/REFER") (wire world)))))))

;;; Dialling out, against a stand-in for the provider

(defmacro with-provider ((provider port) &body body)
  "A stand-in SIP provider on loopback that accepts the server's
registration."
  `(let* ((,provider (sip:make-user-agent
                      :aor "sip:provider@127.0.0.1"
                      :transports '((:udp :host "127.0.0.1" :port 0))
                      :registrar (sip:make-registrar :realm "provider"
                                                     :users '(("12345678" . "secret")))))
          (,port (progn (sip:start ,provider)
                        (sip:transport-port
                         (first (sip:stack-transports (sip:ua-stack ,provider)))))))
     (unwind-protect (progn ,@body)
       (sip:stop ,provider))))

(defun trunk-settings (port)
  ;; Everything here lives on 127.0.0.1, so the provider is told apart from
  ;; the phones by its port.
  (list :registrar (format nil "sip:127.0.0.1:~d" port)
        :username "12345678" :password "secret"
        :ranges (list (format nil "127.0.0.1:~d" port))))

(defparameter *outbound-extensions*
  '(("100" :password "alice-pw" :name "Alice" :outbound t)
    ("101" :password "bob-pw" :name "Bob")
    ("102" :password "carol-pw" :name "Carol" :outbound t :forward "206-555-0188")))

(test dialling-out-through-the-trunk
  (with-provider (provider port)
    (with-pbx (server (start-pbx :extensions *outbound-extensions*
                                 :trunk (trunk-settings port)
                                 :outbound-caller-id "+1 206 555 0100"
                                 :dialplan '((:default (:reject 404))))
                      (alice "100" "alice-pw") (bob "101" "bob-pw"))
      (let ((network (attend provider :after 1/4 :early t))
            (caller (open-desk alice)))
        ;; Alice may dial out.  The provider gets the number in full, from
        ;; the server's own number, under her name.
        (let ((call (dial caller server "206-555-0123")))
          (is-true (wait-for-call call :established))
          (let ((invite (sip:call-invite (desk-call network))))
            (is (string= "12065550123" (sip:sip-uri-user (sip:request-uri invite))))
            (is (string= "12065550100"
                         (sip:sip-uri-user (sip:name-addr-uri (sip:msg-from invite)))))
            (is (string= "Alice" (sip:name-addr-display-name (sip:msg-from invite)))))
          (is-true (wait-for-call (desk-call network) :established))
          (say caller 5)
          (is-true (hears network 5))
          (say network 5)
          (is-true (hears caller 5))
          (sip:hangup call)
          (is-true (wait-for-call (desk-call network) :terminated)))
        ;; Bob may not.
        (let ((call (dial (open-desk bob) server "12065550123")))
          (is-true (wait-for-call call :terminated))
          (is (= 403 (sip:call-status call))))
        (is (= 1 (length (desk-calls network))))
        ;; Carol's calls are forwarded to her mobile, on her authority, even
        ;; when the caller could not dial out himself.
        (let ((call (dial (open-desk bob) server "102")))
          (is-true (wait-for-call call :established))
          (is (= 2 (length (desk-calls network))))
          (is (string= "12065550188"
                       (sip:sip-uri-user
                        (sip:request-uri (sip:call-invite (desk-call network))))))
          (sip:hangup call)
          (is-true (wait-for-call call :terminated)))
        ;; A call from outside to an outside number is not sent back out.
        (let ((call (sip:invite provider (pbx-uri server "12345678")
                                :to "<sip:+12065550177@127.0.0.1>"
                                :sdp (sdp:sdp-string (sdp:make-audio-offer :port 40000)))))
          (is-true (wait-for-call call :terminated))
          (is (= 403 (sip:call-status call)))
          (is (= 2 (length (desk-calls network)))))))))

(test no-dialling-out-unless-configured
  ;; Without a caller ID the same number is just another dialplan entry.
  (with-pbx (server (start-pbx :extensions *outbound-extensions*
                               :dialplan '((:default (:reject 404))))
                    (alice "100" "alice-pw"))
    (let ((call (dial (open-desk alice) server "2065550123")))
      (is-true (wait-for-call call :terminated))
      (is (= 404 (sip:call-status call)))))
  (is (voice::range-match-p "127.0.0.1" 5080 "127.0.0.1:5080"))
  (is (not (voice::range-match-p "127.0.0.1" 5081 "127.0.0.1:5080")))
  (is (voice::range-match-p "34.210.91.115" 5060 "34.210.91.112/28")))

(defun audio-direction (sdp-text)
  (let ((session (sdp:parse-sdp sdp-text)))
    (sdp:media-direction (first (sdp:session-media session)) session)))

(test hold-and-resume-rfc5359-2.1
  (with-pbx (server (start-pbx) (alice "100" "alice-pw") (bob "101" "bob-pw"))
    (multiple-value-bind (on-alice alice-events) (recorder)
      (multiple-value-bind (on-bob bob-events) (recorder)
        (let* ((caller (open-desk alice))
               (callee (attend bob :on-event on-bob))
               (call (dial caller server "101" :on-event on-alice)))
          (is-true (wait-for-call call :established))
          (is-true (wait-for-call (desk-call callee) :established))
          (say caller 3)
          (is-true (hears callee 3))
          (flet ((last-event (events name)
                   (find name (reverse (funcall events)) :key #'car))
                 (count-events (events name)
                   (count name (funcall events) :key #'car)))
            ;; Alice puts the call on hold: her offer says she only sends.
            (sip:reinvite call :sdp (sdp:sdp-string
                                     (sdp:hold (sdp:parse-sdp (sip:call-local-sdp call)))))
            (is-true (sip:wait-until (lambda () (last-event alice-events :reinvite-accepted))))
            ;; The server answers that it only receives (RFC 3264 section 6.1),
            (is (eq :recvonly
                    (audio-direction (getf (cdr (last-event alice-events :reinvite-accepted))
                                           :sdp))))
            ;; and tells Bob's phone the call is held.
            (is-true (sip:wait-until (lambda () (last-event bob-events :reinvite))))
            (is (eq :sendonly
                    (audio-direction (getf (cdr (last-event bob-events :reinvite)) :sdp))))
            ;; Nothing is relayed meanwhile, in either direction.
            (say caller 5)
            (say callee 5)
            (sleep 1/4)
            (is (= 3 (desk-count callee)))
            (is (zerop (desk-count caller)))
            ;; Resume.
            (sip:reinvite call :sdp (sdp:sdp-string
                                     (sdp:resume (sdp:parse-sdp (sip:call-local-sdp call)))))
            (is-true (sip:wait-until
                      (lambda () (= 2 (count-events alice-events :reinvite-accepted)))))
            (is (eq :sendrecv
                    (audio-direction (getf (cdr (last-event alice-events :reinvite-accepted))
                                           :sdp))))
            (is-true (sip:wait-until (lambda () (= 2 (count-events bob-events :reinvite)))))
            (is (eq :sendrecv
                    (audio-direction (getf (cdr (last-event bob-events :reinvite)) :sdp))))
            (say caller 4)
            (is-true (hears callee 7))
            (say callee 2)
            (is-true (hears caller 2))
            ;; An UPDATE with an offer is answered the same way (RFC 3311).
            (sip:update call :sdp (sdp:sdp-string
                                   (sdp:bump-version (sdp:parse-sdp (sip:call-local-sdp call)))))
            (is-true (sip:wait-until (lambda () (last-event alice-events :update-accepted))))
            (is (eq :sendrecv
                    (audio-direction (getf (cdr (last-event alice-events :update-accepted))
                                           :sdp)))))
          (sip:hangup call)
          (is-true (wait-for-call call :terminated)))))))

(test rtp-forwarding
  ;; The relay keeps a stream's shape: gaps and markers pass through, on the
  ;; forwarding session's own numbering.
  (let* ((received '())
         (lock (bt:make-lock))
         (sink (rtp:rtp-session-start
                (rtp:make-rtp-session
                 :on-packet (lambda (s p) (declare (ignore s))
                              (bt:with-lock-held (lock) (push p received))))))
         (out (rtp:rtp-session-start
               (rtp:make-rtp-session :remote-host "127.0.0.1"
                                     :remote-port (rtp:rtp-session-local-port sink)))))
    (unwind-protect
         (flet ((packet (sequence timestamp &key marker (ssrc 7))
                  (rtp:make-rtp-packet :sequence sequence :timestamp timestamp :ssrc ssrc
                                       :marker marker :payload-type 0
                                       :payload (rtp:ulaw-encode (tone 160)))))
           (rtp:rtp-forward out (packet 65535 1000 :marker t))
           (rtp:rtp-forward out (packet 0 1160))
           ;; One packet lost, and a pause in the talking.
           (rtp:rtp-forward out (packet 2 5000) :payload-type 8)
           ;; Another source takes over; the stream continues unbroken.
           (rtp:rtp-forward out (packet 400 90000 :ssrc 9))
           (is-true (sip:wait-until (lambda () (= 4 (length received)))))
           (let* ((packets (sort (copy-list received) #'<
                                 :key (lambda (p)
                                        (mod (- (rtp:rtp-packet-sequence p)
                                                (rtp:rtp-packet-sequence
                                                 (car (last received))))
                                             65536))))
                  (base (first packets)))
             (flet ((sequence-of (p) (mod (- (rtp:rtp-packet-sequence p)
                                             (rtp:rtp-packet-sequence base))
                                          65536))
                    (time-of (p) (mod (- (rtp:rtp-packet-timestamp p)
                                         (rtp:rtp-packet-timestamp base))
                                      (expt 2 32))))
               (is (equal '(0 1 3 4) (mapcar #'sequence-of packets)))
               (is (equal '(0 160 4000 4160) (mapcar #'time-of packets)))
               (is (equal '(t nil nil nil) (mapcar #'rtp:rtp-packet-marker packets)))
               (is (equal '(0 0 8 0) (mapcar #'rtp:rtp-packet-payload-type packets)))
               (is (every (lambda (p) (= (rtp:rtp-session-ssrc out) (rtp:rtp-packet-ssrc p)))
                          packets)))))
      (rtp:rtp-session-stop out)
      (rtp:rtp-session-stop sink))))

;;; Calls the server answers itself follow the same offer/answer rules.

(test holding-a-call-to-the-agent
  (with-pbx (server (start-pbx :greeting "Hello.") (alice "100" "alice-pw"))
    (multiple-value-bind (on-event events) (recorder)
      (let* ((caller (open-desk alice))
             (call (dial caller server "0" :on-event on-event)))
        (is-true (wait-for-call call :established))
        (is-true (hears caller 10))
        (flet ((answered-direction ()
                 (audio-direction
                  (getf (cdr (find :reinvite-accepted (reverse (funcall events)) :key #'car))
                        :sdp))))
          ;; Held: the answer says the server will only listen.
          (sip:reinvite call :sdp (sdp:sdp-string
                                   (sdp:hold (sdp:parse-sdp (sip:call-local-sdp call)))))
          (is-true (sip:wait-until
                    (lambda () (find :reinvite-accepted (funcall events) :key #'car))))
          (is (eq :recvonly (answered-direction)))
          (sip:reinvite call :sdp (sdp:sdp-string
                                   (sdp:resume (sdp:parse-sdp (sip:call-local-sdp call)))))
          (is-true (sip:wait-until
                    (lambda () (= 2 (count :reinvite-accepted (funcall events) :key #'car)))))
          (is (eq :sendrecv (answered-direction))))
        (sip:hangup call)
        (is-true (wait-for-call call :terminated))))))

;;; Busy lamps: dialog state of an extension (RFC 4235)

(defun lamp-states (events)
  "For each dialog document received, the states of the dialogs in it."
  (loop for (name . args) in (funcall events)
        when (eq name :notify)
          collect (let ((body (or (getf args :body) "")))
                    (loop for start = (search "<state>" body) then (search "<state>" body
                                                                           :start2 (1+ start))
                          while start
                          collect (subseq body (+ start 7) (search "</state>" body
                                                                   :start2 start))))))

(test busy-lamp-rfc4235
  (with-pbx (server (start-pbx) (alice "100" "alice-pw") (bob "101" "bob-pw")
                    (carol "102" "carol-pw"))
    (multiple-value-bind (on-bob bob-lamp) (recorder)
      (multiple-value-bind (on-alice alice-lamp) (recorder)
        ;; Carol's phone has lamps for Bob and for Alice.
        (let ((watch-bob (sip:subscribe carol (pbx-uri server "101") "dialog"
                                        :accept "application/dialog-info+xml"
                                        :on-event on-bob))
              (watch-alice (sip:subscribe carol (pbx-uri server "100") "dialog"
                                          :on-event on-alice)))
          (is-true (sip:wait-until (lambda () (and (lamp-states bob-lamp)
                                                   (lamp-states alice-lamp)))))
          (is (eq :active (sip:subscription-state watch-bob)))
          ;; Idle: a document with no dialogs in it.
          (is (equal '(()) (lamp-states bob-lamp)))
          (let ((document (getf (cdr (find :notify (funcall bob-lamp) :key #'car)) :body)))
            (is (search "urn:ietf:params:xml:ns:dialog-info" document))
            (is (search "state=\"full\"" document))
            (is (search "entity=\"sip:101@" document)))
          (is (string= "application/dialog-info+xml"
                       (getf (cdr (find :notify (funcall bob-lamp) :key #'car))
                             :content-type)))
          ;; Alice calls Bob: his lamp shows ringing, hers that she is dialling.
          (let* ((caller (open-desk alice))
                 (callee (attend bob :after 1/2))
                 (call (dial caller server "101")))
            (is-true (sip:wait-until (lambda () (equal '(() ("early"))
                                                       (lamp-states bob-lamp)))))
            (is (equal '(() ("trying")) (lamp-states alice-lamp)))
            (let ((document (getf (cdr (first (last (funcall bob-lamp)))) :body)))
              (is (search "direction=\"recipient\"" document))
              (is (search "<identity>sip:100@" document)))
            ;; Answered: both are in a call.
            (is-true (wait-for-call call :established))
            (is-true (sip:wait-until (lambda () (equal '(() ("early") ("confirmed"))
                                                       (lamp-states bob-lamp)))))
            (is-true (sip:wait-until (lambda () (equal '(() ("trying") ("confirmed"))
                                                       (lamp-states alice-lamp)))))
            ;; Over: both idle again.
            (sip:hangup call)
            (is-true (wait-for-call (desk-call callee) :terminated))
            (is-true (sip:wait-until (lambda () (equal '(() ("early") ("confirmed") ())
                                                       (lamp-states bob-lamp)))))
            (is-true (sip:wait-until (lambda () (equal '(() ("trying") ("confirmed") ())
                                                       (lamp-states alice-lamp))))))
          ;; The version counts up with each document (RFC 4235 section 4.1).
          (is (equal '(0 1 2 3)
                     (loop for (name . args) in (funcall bob-lamp)
                           when (eq name :notify)
                             collect (let* ((body (getf args :body))
                                            (at (search "version=\"" body :start2 20)))
                                       (parse-integer body :start (+ at 9)
                                                           :junk-allowed t)))))
          (sip:unsubscribe watch-bob)
          (sip:unsubscribe watch-alice)
          (is-true (sip:wait-until
                    (lambda () (null (sip:ua-event-subscriptions (voice:server-ua server)))))))))
    ;; Only extensions may watch, and only extensions can be watched.
    (let ((stranger (start-phone "555" "nothing")))
      (unwind-protect
           (flet ((refusal (ua number)
                    (multiple-value-bind (on-event events) (recorder)
                      (sip:subscribe ua (pbx-uri server number) "dialog" :on-event on-event)
                      (is-true (sip:wait-until (lambda () (funcall events))))
                      (getf (cdr (first (funcall events))) :status))))
             (is (= 401 (refusal stranger "101")))
             (is (= 404 (refusal carol "999"))))
        (sip:stop stranger)))))

;;; Handing a call from the agent to a phone

(defun press (desk keys)
  "Press KEYS, a string, on DESK's phone."
  (loop for key across keys
        do (rtp:rtp-send-dtmf (desk-session desk) key)
           (sleep 1/20)))

(defun talking-pbx (spoken &rest args)
  "A server whose agent's every sentence is pushed onto the list held by
the function SPOKEN (called with a sentence)."
  (apply #'start-pbx
         :tts (voice:make-function-tts (lambda (text)
                                         (funcall spoken text)
                                         (tone 1600)))
         args))

(defmacro with-spoken ((spoken said) &body body)
  "SPOKEN records sentences; (SAID text) waits until TEXT has been spoken."
  (let ((list (gensym)) (lock (gensym)))
    `(let ((,list '()) (,lock (bt:make-lock)))
       (flet ((,spoken (text) (bt:with-lock-held (,lock) (push text ,list)))
              (,said (text)
                (sip:wait-until (lambda ()
                                  (bt:with-lock-held (,lock)
                                    (find text ,list :test #'string=)))
                                :timeout 5)))
         (declare (ignorable #',said))
         ,@body))))

(test agent-puts-a-caller-through-by-keypad
  (with-spoken (spoken said)
    (with-pbx (server (talking-pbx #'spoken :greeting "Which extension?")
                      (alice "100" "alice-pw") (bob "101" "bob-pw"))
      (let* ((caller (open-desk alice))
             (callee (attend bob :after 1/4))
             (call (dial caller server "0")))
        (is-true (wait-for-call call :established))
        (is-true (said "Which extension?"))
        (is (= 1 (voice::server-active-calls server)))
        (press caller "101")
        ;; Bob's phone rings, showing Alice, while she is told so.
        (is-true (sip:wait-until (lambda () (desk-call callee))))
        (is-true (said "Calling Bob."))
        (let ((from (sip:msg-from (sip:call-invite (desk-call callee)))))
          (is (string= "Alice" (sip:name-addr-display-name from)))
          (is (string= "100" (sip:sip-uri-user (sip:name-addr-uri from)))))
        (is-true (wait-for-call (desk-call callee) :established))
        ;; The agent has left and the two are bridged.
        (is-true (sip:wait-until (lambda () (zerop (voice::server-active-calls server)))))
        (is-true (sip:wait-until
                  (lambda ()
                    (let ((bridge (first (voice::server-bridges server))))
                      (and bridge (eq :connected (voice::bridge-state bridge)))))))
        (is (eq :established (sip:call-state call)))
        (sleep 1/4)                     ; the agent's last packets drain
        (let ((before (desk-count callee 0)))
          (say caller 10)
          (is-true (hears callee (+ before 10) 0)))
        (let ((before (desk-count caller 0)))
          (say callee 10)
          (is-true (hears caller (+ before 10) 0)))
        ;; Either side hanging up ends the other's call.
        (sip:hangup (desk-call callee))
        (is-true (wait-for-call call :terminated))
        (is-true (sip:wait-until (lambda () (null (voice::server-bridges server)))))))))

(test agent-keypad-wrong-numbers-and-no-answer
  (with-spoken (spoken said)
    (with-pbx (server (talking-pbx #'spoken :greeting nil :ring-seconds 1/2)
                      (alice "100" "alice-pw") (bob "101" "bob-pw"))
      (let* ((caller (open-desk alice))
             (callee (attend bob :after nil))
             (call (dial caller server "0")))
        (is-true (wait-for-call call :established))
        (is-true (sip:wait-until (lambda () (= 1 (voice::server-active-calls server)))))
        ;; No extension begins with 9: said at once.
        (press caller "9")
        (is-true (said "There is no extension 9."))
        ;; Carol is listed, but has no phone.
        (press caller "102")
        (is-true (said "Carol is not available right now."))
        ;; Bob's phone rings and is not picked up: the agent comes back.
        (press caller "101")
        (is-true (said "Calling Bob."))
        (is-true (sip:wait-until (lambda () (desk-call callee))))
        (is-true (said "Bob is not answering. Is there anything else I can do?"))
        (is-true (wait-for-call (desk-call callee) :terminated))
        (is (eq :established (sip:call-state call)))
        (is (= 1 (voice::server-active-calls server)))
        (is (null (voice::server-bridges server)))
        ;; A star starts the number again.
        (press caller "1*101")
        (is-true (sip:wait-until (lambda () (= 2 (length (desk-calls callee))))))
        (sip:hangup call)
        (is-true (wait-for-call call :terminated))
        (is-true (sip:wait-until (lambda () (zerop (voice::server-active-calls server)))))))))

(test agent-keypad-waits-when-numbers-overlap
  (let ((pause voice::*digit-pause*))
    (setf voice::*digit-pause* 1/2)
    (unwind-protect
         (with-spoken (spoken said)
           (with-pbx (server (talking-pbx #'spoken :greeting nil
                                          :extensions '(("10" :password "dave-pw" :name "Dave")
                                                        ("100" :password "alice-pw" :name "Alice")
                                                        ("101" :password "bob-pw" :name "Bob")))
                             (alice "100" "alice-pw") (dave "10" "dave-pw"))
             (let* ((caller (open-desk alice))
                    (callee (attend dave))
                    (call (dial caller server "0")))
               (is-true (wait-for-call call :established))
               (is-true (sip:wait-until (lambda () (= 1 (voice::server-active-calls server)))))
               ;; 10 could still become 100 or 101: it is rung after a pause.
               (press caller "10")
               (is (null (desk-call callee)))
               (is-true (sip:wait-until (lambda () (desk-call callee))))
               (is-true (wait-for-call (desk-call callee) :established))
               (sip:hangup call)
               (is-true (wait-for-call (desk-call callee) :terminated)))))
      (setf voice::*digit-pause* pause))))

(defun transfer-input (number)
  (let ((input (make-hash-table :test 'equal)))
    (setf (gethash "extension" input) number)
    input))

(defun asking-pbx (results &rest args)
  "A server whose stand-in model answers everything the caller says by
asking to be put through to the number the caller named; what the tool
returns is pushed with RESULTS."
  (apply #'start-pbx
         :stt (voice:make-function-stt (let ((numbers (list "102" "101")))
                                         (lambda (samples rate)
                                           (declare (ignore samples rate))
                                           (pop numbers))))
         :llm (voice:make-function-llm
               (lambda (messages)
                 (let ((tool (first voice::*call-tools*)))
                   (funcall results
                            (voice:call-tool tool "transfer_call"
                                             (transfer-input
                                              (cdr (first (last messages))))))
                   "Done.")))
         args))

(test agent-puts-a-caller-through-when-asked
  (with-spoken (results returned)
    (with-pbx (server (asking-pbx #'results :greeting nil)
                      (alice "100" "alice-pw") (bob "101" "bob-pw"))
      (let* ((caller (open-desk alice))
             (callee (attend bob :after 1/2))
             (call (dial caller server "0")))
        (is-true (wait-for-call call :established))
        (is-true (sip:wait-until (lambda () (= 1 (voice::server-active-calls server)))))
        ;; The model is told who can be reached.
        (is (search "100 (Alice); 101 (Bob); 102 (Carol)"
                    (voice::handoff-directory server)))
        ;; First she asks for Carol, who has no phone: the model hears why.
        (send-audio (desk-session caller)
                    (concatenate '(vector (signed-byte 16)) (tone 8000) (silence 6400)))
        (is-true (returned "Carol is not available right now."))
        ;; Then for Bob.  She hears ringing until he answers.
        (sleep 1/2)
        (let ((before (desk-count caller 0)))
          (send-audio (desk-session caller)
                      (concatenate '(vector (signed-byte 16)) (tone 8000) (silence 6400)))
          (is-true (sip:wait-until (lambda () (desk-call callee))))
          (is-true (hears caller (+ before 5) 0)))
        (is-true (wait-for-call (desk-call callee) :established))
        (is-true (returned "The caller has been put through."))
        (is-true (sip:wait-until (lambda () (zerop (voice::server-active-calls server)))))
        (is (= 1 (length (voice::server-bridges server))))
        (sip:hangup call)
        (is-true (wait-for-call (desk-call callee) :terminated))))))

(test caller-hangs-up-while-being-put-through
  (with-spoken (spoken said)
    (with-pbx (server (talking-pbx #'spoken :greeting nil)
                      (alice "100" "alice-pw") (bob "101" "bob-pw"))
      (let* ((caller (open-desk alice))
             (callee (attend bob :after nil))
             (call (dial caller server "0")))
        (is-true (wait-for-call call :established))
        (is-true (sip:wait-until (lambda () (= 1 (voice::server-active-calls server)))))
        (press caller "101")
        (is-true (sip:wait-until (lambda () (desk-call callee))))
        (sip:hangup call)
        ;; Bob's phone stops ringing.
        (is-true (wait-for-call (desk-call callee) :terminated))
        (is-true (sip:wait-until (lambda () (zerop (voice::server-active-calls server)))))
        (is (null (voice::server-bridges server)))))))
