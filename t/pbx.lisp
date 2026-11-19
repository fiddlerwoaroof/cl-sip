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
  (apply #'voice:start-server
         :port 0 :tcp nil :log-stream nil :greeting nil
         :extensions extensions
         :stt (voice:make-function-stt (lambda (samples rate)
                                         (declare (ignore samples rate)) nil))
         :llm (voice:make-function-llm (lambda (messages)
                                         (declare (ignore messages)) "Fine."))
         :tts (voice:make-function-tts (lambda (text)
                                         (declare (ignore text)) (tone 1600)))
         (alexandria:remove-from-plist args :extensions)))

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
