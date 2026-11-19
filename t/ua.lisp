(in-package :cl-sip.test)

(def-suite ua :in cl-sip)
(in-suite ua)

;;;; Registration (RFC 3261 section 10, RFC 3665 section 2)

(defun registration-world (&key (password "zanzibar") (min-expires 60) (users '(("bob" . "zanzibar"))))
  (let* ((world (new-world))
         (registrar (sip:make-registrar :realm "biloxi.example.com" :users users
                                        :min-expires min-expires))
         (server (agent world "sip:registrar@biloxi.example.com" "biloxi.example.com"
                        :registrar registrar))
         (bob (agent world "sip:bob@biloxi.example.com" "client.biloxi.example.com"
                     :display-name "Bob"
                     :credentials `(("biloxi.example.com" "bob" ,password)))))
    (values world bob server registrar)))

(test register-with-digest-rfc3665-2.1
  (multiple-value-bind (world bob server registrar) (registration-world)
    (multiple-value-bind (on-event events) (recorder)
      (let ((registration (sip:register bob :expires 3600 :on-event on-event)))
        (settle world)
        (is (equal '("REGISTER" "401/REGISTER" "REGISTER" "200/REGISTER") (wire world)))
        (is (eq :registered (sip:registration-state registration)))
        (is (= 3600 (sip:registration-expires registration)))
        (is (equal '(:registered) (event-names (funcall events))))
        (destructuring-bind (first second) (wire-messages world "REGISTER")
          ;; Section 10.2: Request-URI is the domain, To and From the AOR.
          (is (string= "sip:biloxi.example.com" (sip:uri-string (sip:request-uri first))))
          (is (string= "sip:bob@biloxi.example.com"
                       (sip:uri-string (sip:name-addr-uri (sip:msg-to first)))))
          (is (null (sip:msg-to-tag first)))
          (is (string= "sip:bob@client.biloxi.example.com:5060"
                       (sip:uri-string (sip:msg-contact-uri first))))
          (is (null (sip:header first "Authorization")))
          ;; Section 22.2: same Call-ID, CSeq incremented, new branch.
          (is (string= (sip:msg-call-id first) (sip:msg-call-id second)))
          (is (= 1 (sip:cseq-number (sip:msg-cseq first))))
          (is (= 2 (sip:cseq-number (sip:msg-cseq second))))
          (is (string/= (sip:via-branch (sip:msg-top-via first))
                        (sip:via-branch (sip:msg-top-via second))))
          (let ((credentials (sip:parse-credentials (sip:header second "Authorization"))))
            (is (string= "bob" (sip:credentials-username credentials)))
            (is (string= "sip:biloxi.example.com" (sip:credentials-uri credentials)))
            (is (string= "00000001" (sip:credentials-nc credentials)))
            (is (sip:verify-credentials credentials :password "zanzibar" :method "REGISTER"))))
        (is (equal '("sip:bob@client.biloxi.example.com:5060")
                   (mapcar #'sip:uri-string
                           (sip:registrar-lookup registrar server "sip:bob@biloxi.example.com"))))
        ;; RFC 3665 2.3: fetching the bindings.
        (sip:fetch-bindings registration)
        (settle world)
        (let ((event (first (last (funcall events)))))
          (is (eq :bindings (car event)))
          (is (= 1 (length (getf (cdr event) :bindings)))))
        ;; The binding is refreshed at half its lifetime.
        (clear-wire world)
        (tick world 1801)
        (is (find "200/REGISTER" (wire world) :test #'string=))
        (is (eq :registered (sip:registration-state registration)))
        ;; RFC 3665 2.4: cancelling the registration.
        (sip:unregister registration)
        (settle world)
        (is (eq :unregistered (sip:registration-state registration)))
        (is (null (sip:registrar-lookup registrar server "sip:bob@biloxi.example.com")))
        ;; No further refreshes.
        (clear-wire world)
        (tick world 4000)
        (is (null (wire world)))))))

(test register-wrong-password-rfc3665-2.5
  (multiple-value-bind (world bob) (registration-world :password "wrong")
    (multiple-value-bind (on-event events) (recorder)
      (let ((registration (sip:register bob :on-event on-event)))
        (settle world)
        ;; The second 401 is final: the same credentials are not retried.
        (is (equal '("REGISTER" "401/REGISTER" "REGISTER" "401/REGISTER") (wire world)))
        (is (eq :failed (sip:registration-state registration)))
        (let ((event (first (funcall events))))
          (is (eq :failed (car event)))
          (is (= 401 (getf (cdr event) :status))))))))

(test register-without-credentials-fails
  (multiple-value-bind (world bob) (registration-world)
    (setf (sip:ua-credentials bob) '())
    (let ((registration (sip:register bob)))
      (settle world)
      (is (equal '("REGISTER" "401/REGISTER") (wire world)))
      (is (eq :failed (sip:registration-state registration))))))

(test register-interval-too-brief-10.2.8
  (multiple-value-bind (world bob) (registration-world :users nil :min-expires 600)
    (let ((registration (sip:register bob :expires 60)))
      (settle world)
      (is (equal '("REGISTER" "423/REGISTER" "REGISTER" "200/REGISTER") (wire world)))
      (is (= 600 (sip::msg-integer-header (first (wire-messages world 423)) "Min-Expires")
             (sip:registration-expires registration)))
      (is (eq :registered (sip:registration-state registration))))))

(test register-timeout
  (let* ((world (new-world))
         (bob (agent world "sip:bob@nowhere.example.com" "client.example.com")))
    (multiple-value-bind (on-event events) (recorder)
      (let ((registration (sip:register bob :on-event on-event)))
        (tick world 33)
        (is (eq :failed (sip:registration-state registration)))
        (is (equal '(:failed :reason :timeout) (first (funcall events))))))))

(test registrar-contact-rules-10.3
  (multiple-value-bind (world bob server registrar) (registration-world :users nil)
    (flet ((send (cseq &rest headers)
             (let (status)
               (sip:ua-send bob
                            (sip:ua-make-request bob "REGISTER" "sip:biloxi.example.com"
                                                 :to "sip:bob@biloxi.example.com"
                                                 :call-id "fixed" :cseq cseq :headers headers)
                            (lambda (creq event argument)
                              (declare (ignore creq event))
                              (setf status (and argument (sip:response-status argument)))))
               (settle world)
               status))
           (bindings ()
             (length (sip:registrar-lookup registrar server "sip:bob@biloxi.example.com"))))
      (is (= 200 (send 1 '("Contact" "<sip:bob@a.example.com>, <sip:bob@b.example.com>")
                       '("Expires" "600"))))
      (is (= 2 (bindings)))
      ;; Same Call-ID with a CSeq that is not higher must fail.
      (is (= 500 (send 1 '("Contact" "<sip:bob@a.example.com>;expires=0"))))
      (is (= 2 (bindings)))
      ;; expires=0 removes one binding.
      (is (= 200 (send 2 '("Contact" "<sip:bob@a.example.com>;expires=0"))))
      (is (= 1 (bindings)))
      ;; "*" requires Expires: 0 and no other contact.
      (is (= 400 (send 3 '("Contact" "*"))))
      (is (= 200 (send 4 '("Contact" "*") '("Expires" "0"))))
      (is (= 0 (bindings)))
      ;; Bindings expire on their own.
      (is (= 200 (send 5 '("Contact" "<sip:bob@a.example.com>;expires=100"))))
      (tick world 99)
      (is (= 1 (bindings)))
      (tick world 2)
      (is (= 0 (bindings))))))

;;;; General UAS behaviour (section 8.2) and OPTIONS (section 11)

(defun pair (&rest bob-args)
  (let* ((world (new-world))
         (alice (agent world "sip:alice@atlanta.example.com" "client.atlanta.example.com"
                       :display-name "Alice"))
         (bob (apply #'agent world "sip:bob@biloxi.example.com" "client.biloxi.example.com"
                     :display-name "Bob" bob-args)))
    (values world alice bob)))

(defparameter *bob* "sip:bob@client.biloxi.example.com")

(defun final-response (world ua request)
  (let (response)
    (sip:ua-send ua request
                 (lambda (creq event argument)
                   (declare (ignore creq))
                   (when (and (eq event :response) (>= (sip:response-status argument) 200))
                     (setf response argument))))
    (settle world)
    response))

(test options-11
  (multiple-value-bind (world alice) (pair)
    (let (response)
      (sip:options alice *bob* :on-response (lambda (r) (setf response r)))
      (settle world)
      (is (= 200 (sip:response-status response)))
      (is (equal '("INVITE" "ACK" "BYE" "CANCEL" "OPTIONS" "UPDATE" "REFER" "NOTIFY")
                 (sip:header-values response "Allow")))
      (is (string= "application/sdp" (sip:header response "Accept")))
      (is (sip:msg-to-tag response))
      (let ((request (first (wire-messages world "OPTIONS"))))
        ;; Section 8.1.1: the mandatory header fields.
        (is (= 70 (sip:msg-max-forwards request)))
        (is (string= "Alice" (sip:name-addr-display-name (sip:msg-from request))))
        (is (sip:msg-from-tag request))
        (is (null (sip:msg-to-tag request)))
        (is (null (sip:validate-message request)))))
    ;; Nobody home: NIL after timer F.
    (let ((response :unset))
      (sip:options alice "sip:x@nowhere.example.com" :on-response (lambda (r) (setf response r)))
      (tick world 33)
      (is (null response)))))

(test uas-request-checks-8.2
  (multiple-value-bind (world alice) (pair)
    ;; Unknown method: 405 with Allow (section 8.2.1).
    (let ((response (final-response world alice (sip:ua-make-request alice "PUBLISH" *bob*))))
      (is (= 405 (sip:response-status response)))
      (is (search "INVITE" (sip:header response "Allow"))))
    ;; Required extension we do not have: 420 with Unsupported (8.2.2.3).
    (let ((response (final-response
                     world alice
                     (sip:ua-make-request alice "OPTIONS" *bob*
                                          :headers '(("Require" "100rel, foo"))))))
      (is (= 420 (sip:response-status response)))
      (is (equal '("100rel" "foo") (sip:header-values response "Unsupported"))))
    ;; A request for a dialog that does not exist: 481 (section 12.2.2).
    (let ((response (final-response
                     world alice
                     (sip:ua-make-request alice "BYE" *bob*
                                          :to "<sip:bob@biloxi.example.com>;tag=nosuch"))))
      (is (= 481 (sip:response-status response))))
    (is (= 481 (sip:response-status
                (final-response world alice (sip:ua-make-request alice "BYE" *bob*)))))
    ;; CANCEL matching no transaction: 481 (section 9.2).
    (is (= 481 (sip:response-status
                (final-response world alice (sip:ua-make-request alice "CANCEL" *bob*)))))
    ;; REGISTER at an agent that is not a registrar.
    (is (= 405 (sip:response-status
                (final-response world alice (sip:ua-make-request alice "REGISTER" *bob*)))))
    ;; INVITE with a body we cannot interpret: 415 (section 8.2.3).
    (let ((response (final-response
                     world alice
                     (sip:ua-make-request alice "INVITE" *bob*
                                          :headers '(("Content-Type" "text/plain"))
                                          :body "hello"))))
      (is (= 415 (sip:response-status response)))
      (is (string= "application/sdp" (sip:header response "Accept"))))))

(test merged-request-8.2.2.2
  (multiple-value-bind (world alice bob) (pair)
    (declare (ignorable bob))
    (let* ((request (sip:ua-make-request alice "OPTIONS" *bob*))
           (twin (sip:copy-message request))
           (first (final-response world alice request))
           ;; The same request arriving again over another path: same From
           ;; tag, Call-ID and CSeq but a different branch.
           (second (final-response world alice twin)))
      (is (= 200 (sip:response-status first)))
      (is (= 482 (sip:response-status second))))))

;;;; Sessions

(defparameter *offer* (format nil "v=0~c~co=alice 1 1 IN IP4 192.0.2.101~c~cs=-~c~cc=IN IP4 192.0.2.101~c~ct=0 0~c~cm=audio 49172 RTP/AVP 0~c~c"
                              #\Return #\Newline #\Return #\Newline #\Return #\Newline
                              #\Return #\Newline #\Return #\Newline #\Return #\Newline))
(defparameter *answer* (format nil "v=0~c~co=bob 2 2 IN IP4 192.0.2.201~c~cs=-~c~cc=IN IP4 192.0.2.201~c~ct=0 0~c~cm=audio 3456 RTP/AVP 0~c~c"
                               #\Return #\Newline #\Return #\Newline #\Return #\Newline
                               #\Return #\Newline #\Return #\Newline #\Return #\Newline))

(defun call-pair (&key (on-incoming :ring) reliable)
  "Alice and Bob, with Bob's incoming calls collected.  ON-INCOMING is
:ring, :answer, :busy, :ignore or a function of the call."
  (let* ((world (new-world))
         (incoming nil)
         (bob-events nil)
         (alice (agent world "sip:alice@atlanta.example.com" "client.atlanta.example.com"
                       :display-name "Alice" :reliable reliable))
         (bob (agent world "sip:bob@biloxi.example.com" "client.biloxi.example.com"
                     :display-name "Bob" :reliable reliable)))
    (multiple-value-bind (on-event events) (recorder)
      (setf bob-events events
            (sip:ua-on-incoming-call bob)
            (lambda (call)
              (setf incoming call
                    (sip:call-on-event call) on-event)
              (etypecase on-incoming
                (function (funcall on-incoming call))
                (symbol (ecase on-incoming
                          (:ring (sip:ring call))
                          (:answer (sip:ring call) (sip:answer call :sdp *answer*))
                          (:busy (sip:reject call 486))
                          (:ignore nil)))))))
    (values world alice bob (lambda () incoming) bob-events)))

(defun bob-target (&optional reliable)
  (if reliable
      "sip:bob@client.biloxi.example.com;transport=tcp"
      *bob*))

(test successful-session-rfc3665-3.1
  (multiple-value-bind (world alice bob incoming bob-events) (call-pair)
    (declare (ignorable bob))
    (multiple-value-bind (on-event events) (recorder)
      (let ((call (sip:invite alice *bob* :sdp *offer* :on-event on-event)))
        (is (eq :calling (sip:call-state call)))
        (settle world)
        ;; F1 INVITE, F2 180 Ringing.
        (is (equal '("INVITE" "180/INVITE") (wire world)))
        (is (eq :ringing (sip:call-state call)))
        (is (eq :ringing (sip:call-state (funcall incoming))))
        (is (string= *offer* (sip:call-remote-sdp (funcall incoming))))
        (is (eq :early (sip:dialog-state (sip:call-dialog call))))
        (let ((invite (first (wire-messages world "INVITE"))))
          (is (string= "application/sdp" (sip:msg-content-type invite)))
          (is (string= "sip:alice@client.atlanta.example.com:5060"
                       (sip:uri-string (sip:msg-contact-uri invite)))))
        ;; F3 200 OK, F4 ACK.
        (sip:answer (funcall incoming) :sdp *answer*)
        (settle world)
        (is (equal '("INVITE" "180/INVITE" "200/INVITE" "ACK") (wire world)))
        (is (eq :established (sip:call-state call)))
        (is (eq :established (sip:call-state (funcall incoming))))
        (is (string= *answer* (sip:call-remote-sdp call)))
        (is (equal '(:ringing :established) (event-names (funcall events))))
        (is (equal '(:established) (event-names (funcall bob-events))))
        (is (eq :confirmed (sip:dialog-state (sip:call-dialog call))))
        (let ((invite (first (wire-messages world "INVITE")))
              (ringing (first (wire-messages world 180)))
              (ok (first (wire-messages world 200)))
              (ack (first (wire-messages world "ACK"))))
          ;; The 180 and the 200 carry the same To tag (one dialog).
          (is (string= (sip:msg-to-tag ringing) (sip:msg-to-tag ok)))
          (is (sip:msg-contact-uri ok))
          ;; Section 13.2.2.4: the ACK goes to the remote target, has the
          ;; INVITE's CSeq number and its own branch.
          (is (string= "sip:bob@client.biloxi.example.com:5060"
                       (sip:uri-string (sip:request-uri ack))))
          (is (= (sip:cseq-number (sip:msg-cseq invite)) (sip:cseq-number (sip:msg-cseq ack))))
          (is (string= "ACK" (sip:cseq-method (sip:msg-cseq ack))))
          (is (string/= (sip:via-branch (sip:msg-top-via invite))
                        (sip:via-branch (sip:msg-top-via ack))))
          (is (string= (sip:msg-to-tag ok) (sip:msg-to-tag ack))))
        ;; Nothing is retransmitted once the ACK has arrived.
        (tick world 40)
        (is (= 4 (length (wire world))))
        ;; F5 BYE (from Bob, as in the RFC), F6 200 OK.
        (sip:hangup (funcall incoming))
        (settle world)
        (is (equal '("BYE" "200/BYE") (nthcdr 4 (wire world))))
        (is (eq :terminated (sip:call-state call)))
        (is (eq :remote-hangup (sip:call-cause call)))
        (is (eq :terminated (sip:call-state (funcall incoming))))
        (is (eq :local-hangup (sip:call-cause (funcall incoming))))
        (let ((bye (first (wire-messages world "BYE"))))
          (is (string= "sip:alice@client.atlanta.example.com:5060"
                       (sip:uri-string (sip:request-uri bye))))
          (is (string= "sip:bob@client.biloxi.example.com"
                       (sip:uri-string (sip:name-addr-uri (sip:msg-from bye))))))
        (is (equal '(:ringing :established :terminated) (event-names (funcall events))))
        ;; All dialog state is gone.
        (is (zerop (hash-table-count (sip::ua-dialog-owners alice))))
        (is (zerop (hash-table-count (sip::ua-dialog-owners bob))))))))

(test session-over-reliable-transport
  (multiple-value-bind (world alice bob incoming) (call-pair :on-incoming :answer :reliable t)
    (declare (ignorable bob))
    (let ((call (sip:invite alice (bob-target t) :sdp *offer*)))
      (settle world)
      (is (equal '("INVITE" "180/INVITE" "200/INVITE" "ACK") (wire world)))
      (is (eq :established (sip:call-state call)))
      (let ((invite (first (wire-messages world "INVITE"))))
        (is (string= "TCP" (sip:via-transport (sip:msg-top-via invite))))
        (is (string= "tcp" (sip:uri-param (sip:msg-contact-uri invite) "transport"))))
      (sip:hangup call)
      (settle world)
      (is (eq :terminated (sip:call-state (funcall incoming))))
      (tick world 40)
      (is (= 6 (length (wire world)))))))

(test unsuccessful-no-answer-rfc3665-3.8
  (multiple-value-bind (world alice bob incoming) (call-pair)
    (declare (ignorable bob))
    (let ((call (sip:invite alice *bob* :sdp *offer*)))
      (settle world)
      (sip:hangup call)
      (settle world)
      (is (equal '("INVITE" "180/INVITE" "CANCEL" "200/CANCEL" "487/INVITE" "ACK")
                 (wire world)))
      (is (eq :terminated (sip:call-state call)))
      (is (eq :cancelled (sip:call-cause call)))
      (is (eq :remote-cancel (sip:call-cause (funcall incoming))))
      (let ((invite (first (wire-messages world "INVITE")))
            (cancel (first (wire-messages world "CANCEL"))))
        ;; Section 9.1: same Request-URI, Call-ID, To, From, CSeq number and
        ;; top Via (including the branch) as the INVITE.
        (is (sip:uri-equal (sip:request-uri invite) (sip:request-uri cancel)))
        (is (string= (sip:header invite "Via") (sip:header cancel "Via")))
        (is (string= (sip:header invite "To") (sip:header cancel "To")))
        (is (string= (sip:header invite "From") (sip:header cancel "From")))
        (is (string= (sip:msg-call-id invite) (sip:msg-call-id cancel)))
        (is (= (sip:cseq-number (sip:msg-cseq invite)) (sip:cseq-number (sip:msg-cseq cancel)))))
      (is (zerop (hash-table-count (sip::ua-dialog-owners alice)))))))

(test cancel-waits-for-a-provisional-response-9.1
  (multiple-value-bind (world alice bob incoming) (call-pair :on-incoming :ignore)
    (declare (ignorable bob))
    (let ((call (sip:invite alice *bob* :sdp *offer*)))
      (settle world)
      (sip:hangup call)
      (settle world)
      ;; Nothing but the INVITE so far: no CANCEL before a response.
      (is (equal '("INVITE") (wire world)))
      (is (eq :terminating (sip:call-state call)))
      ;; Bob's transaction sends 100 Trying after 200 ms, which releases it.
      (tick world 1/4)
      (is (equal '("INVITE" "100/INVITE" "CANCEL" "200/CANCEL" "487/INVITE" "ACK")
                 (wire world)))
      (is (eq :cancelled (sip:call-cause call)))
      (is (eq :terminated (sip:call-state (funcall incoming)))))))

(test unsuccessful-busy-rfc3665-3.9
  (multiple-value-bind (world alice) (call-pair :on-incoming :busy)
    (multiple-value-bind (on-event events) (recorder)
      (let ((call (sip:invite alice *bob* :sdp *offer* :on-event on-event)))
        (settle world)
        (is (equal '("INVITE" "486/INVITE" "ACK") (wire world)))
        (is (eq :terminated (sip:call-state call)))
        (is (eq :rejected (sip:call-cause call)))
        (is (= 486 (sip:call-status call)))
        (let ((event (first (funcall events))))
          (is (eq :terminated (car event)))
          (is (= 486 (getf (cdr event) :status))))))))

(test unanswered-incoming-call-without-handler
  (multiple-value-bind (world alice bob) (call-pair)
    (setf (sip:ua-on-incoming-call bob) nil)
    (let ((call (sip:invite alice *bob* :sdp *offer*)))
      (settle world)
      (is (equal '("INVITE" "480/INVITE" "ACK") (wire world)))
      (is (= 480 (sip:call-status call))))))

(test no-response-rfc3665-3.10
  (let* ((world (new-world))
         (alice (agent world "sip:alice@atlanta.example.com" "client.atlanta.example.com"))
         (call (sip:invite alice "sip:bob@nowhere.example.com" :sdp *offer*)))
    (tick world 33)
    (is (= 7 (length (wire-messages world "INVITE"))))
    (is (eq :terminated (sip:call-state call)))
    (is (eq :timeout (sip:call-cause call)))))

(test lost-200-is-retransmitted-13.3.1.4
  (multiple-value-bind (world alice bob incoming) (call-pair)
    (declare (ignorable bob))
    (let ((dropped 0))
      (set-filter world (lambda (m from to)
                          (declare (ignore from to))
                          (if (and (sip:response-p m) (= 200 (sip:response-status m))
                                   (< dropped 2))
                              (progn (incf dropped) :drop)
                              t)))
      (let ((call (sip:invite alice *bob* :sdp *offer*)))
        (settle world)
        (sip:answer (funcall incoming) :sdp *answer*)
        (settle world)
        (is (eq :ringing (sip:call-state call)))
        (is (eq :answered (sip:call-state (funcall incoming))))
        (tick world 2)
        ;; The UAS core retransmits the 2xx at T1, then 2*T1.
        (is (equal '(0 1/2 3/2) (send-times world 200)))
        (is (= 1 (length (wire-messages world "ACK"))))
        (is (eq :established (sip:call-state call)))
        (is (eq :established (sip:call-state (funcall incoming))))
        (tick world 40)
        (is (= 3 (length (send-times world 200))))))))

(test duplicate-200-is-acknowledged-again
  (multiple-value-bind (world alice bob incoming) (call-pair)
    (declare (ignorable bob))
    (set-filter world (lambda (m from to)
                        (declare (ignore from to))
                        (if (and (sip:response-p m) (= 200 (sip:response-status m))) 2 t)))
    (multiple-value-bind (on-event events) (recorder)
      (let ((call (sip:invite alice *bob* :sdp *offer* :on-event on-event)))
        (settle world)
        (sip:answer (funcall incoming) :sdp *answer*)
        (settle world)
        (let ((acks (wire-messages world "ACK")))
          (is (= 2 (length acks)))
          ;; A retransmitted ACK is the same message, branch included.
          (is (string= (sip:header (first acks) "Via") (sip:header (second acks) "Via"))))
        (is (eq :established (sip:call-state call)))
        (is (= 1 (count :established (event-names (funcall events)))))))))

(test lost-ack-ends-the-session-13.3.1.4
  (multiple-value-bind (world alice bob incoming) (call-pair)
    (declare (ignorable bob))
    (set-filter world (lambda (m from to)
                        (declare (ignore from to))
                        (if (and (sip:request-p m) (string= "ACK" (sip:request-method m)))
                            :drop
                            t)))
    (let ((call (sip:invite alice *bob* :sdp *offer*)))
      (settle world)
      (sip:answer (funcall incoming) :sdp *answer*)
      (tick world 127/4)
      (is (equal '(0 1/2 3/2 7/2 15/2 23/2 31/2 39/2 47/2 55/2 63/2) (send-times world 200)))
      (is (eq :answered (sip:call-state (funcall incoming))))
      (is (null (wire-messages world "BYE")))
      ;; After 64*T1 without an ACK the UAS gives up and sends BYE.
      (tick world 2)
      (is (= 1 (length (wire-messages world "BYE"))))
      (is (eq :terminated (sip:call-state (funcall incoming))))
      (is (eq :no-ack (sip:call-cause (funcall incoming))))
      (is (eq :terminated (sip:call-state call)))
      (is (eq :remote-hangup (sip:call-cause call))))))

(test cancel-crosses-200-rfc5407-3.1.2
  (multiple-value-bind (world alice bob incoming) (call-pair)
    (declare (ignorable bob))
    (let ((call (sip:invite alice *bob* :sdp *offer*)))
      (settle world)
      ;; Both act before either message is delivered.
      (sip:hangup call)
      (sip:answer (funcall incoming) :sdp *answer*)
      (settle world)
      ;; The CANCEL is answered 200 but has no effect; the 200 to the INVITE
      ;; establishes the session, which Alice then ends with BYE.
      (is (equal '("INVITE" "180/INVITE" "CANCEL" "200/INVITE" "200/CANCEL" "ACK" "BYE" "200/BYE")
                 (wire world)))
      (is (null (wire-messages world 487)))
      (is (eq :terminated (sip:call-state call)))
      (is (eq :cancelled (sip:call-cause call)))
      (is (eq :terminated (sip:call-state (funcall incoming))))
      (is (eq :remote-hangup (sip:call-cause (funcall incoming)))))))

(test forked-2xx-is-acknowledged-and-released-13.2.2.4
  (multiple-value-bind (world alice bob incoming) (call-pair)
    (declare (ignorable bob))
    (let ((call (sip:invite alice *bob* :sdp *offer*)))
      (settle world)
      (sip:answer (funcall incoming) :sdp *answer*)
      (settle world)
      (clear-wire world)
      ;; A second 2xx with another To tag, as if a proxy had forked the INVITE.
      (let* ((stack (sip:ua-stack alice))
             (ok (sip:make-response
                  200 :headers `(("Via" ,(sip:header (sip:call-invite call) "Via"))
                                 ("From" ,(sip:header (sip:call-invite call) "From"))
                                 ("To" "<sip:bob@biloxi.example.com>;tag=otherfork")
                                 ("Call-ID" ,(sip:msg-call-id (sip:call-invite call)))
                                 ("CSeq" ,(sip:header (sip:call-invite call) "CSeq"))
                                 ("Contact" "<sip:bob@client.biloxi.example.com>")))))
        (sip::stack-deliver stack (first (sip:stack-transports stack))
                            (sip:serialize-message ok) "client.biloxi.example.com" 5060 nil))
      (settle world)
      (is (equal '("ACK" "BYE") (subseq (wire world) 0 2)))
      (is (string= "otherfork" (sip:msg-to-tag (first (wire-messages world "ACK")))))
      (is (string= "otherfork" (sip:msg-to-tag (first (wire-messages world "BYE")))))
      ;; The original session is untouched.
      (is (eq :established (sip:call-state call))))))

(test invite-with-proxy-authentication-22.3
  ;; A server that challenges the INVITE with 407 and then accepts it.
  (let* ((world (new-world))
         (alice (agent world "sip:alice@atlanta.example.com" "client.atlanta.example.com"
                       :credentials '(("atlanta.example.com" "alice" "wonderland"))))
         (server (raw-stack world "ss1.atlanta.example.com"))
         (challenge (sip:generate-challenge "atlanta.example.com" :algorithm "SHA-256")))
    (setf (sip:stack-request-handler server)
          (lambda (request tx)
            (let ((raw (sip:header request "Proxy-Authorization")))
              (cond ((string/= (sip:request-method request) "INVITE")
                     (sip:send-response tx (sip:make-response-for request 200)))
                    ((and raw (sip:verify-credentials (sip:parse-credentials raw)
                                                      :password "wonderland" :method "INVITE"
                                                      :body (sip:message-body request)))
                     (sip:send-response
                      tx (sip:make-response-for
                          request 200 :to-tag "srv"
                          :headers '(("Contact" "<sip:bob@ss1.atlanta.example.com>")))))
                    (t
                     (sip:send-response
                      tx (sip:make-response-for
                          request 407 :to-tag "chal"
                          :headers `(("Proxy-Authenticate"
                                      ,(sip:format-challenge challenge))))))))))
    (let ((call (sip:invite alice "sip:bob@ss1.atlanta.example.com" :sdp *offer*)))
      (settle world)
      (is (equal '("INVITE" "407/INVITE" "ACK" "INVITE" "200/INVITE" "ACK") (wire world)))
      (is (eq :established (sip:call-state call)))
      (destructuring-bind (first second) (wire-messages world "INVITE")
        (is (= (1+ (sip:cseq-number (sip:msg-cseq first))) (sip:cseq-number (sip:msg-cseq second))))
        (is (string= (sip:msg-call-id first) (sip:msg-call-id second)))
        (is (string= (sip:msg-from-tag first) (sip:msg-from-tag second)))
        (is (string= *offer* (sip:message-body-string second)))
        (is (string= "SHA-256" (sip:credentials-algorithm
                                (sip:parse-credentials (sip:header second "Proxy-Authorization"))))))
      (destructuring-bind (first second) (wire-messages world "ACK")
        ;; The ACK for the 407 belongs to the first transaction; the ACK for
        ;; the 2xx carries the credentials of the accepted INVITE.
        (is (null (sip:header first "Proxy-Authorization")))
        (is (sip:header second "Proxy-Authorization"))
        (is (= 2 (sip:cseq-number (sip:msg-cseq second)))))
      ;; The BYE continues the dialog's sequence.
      (sip:hangup call)
      (settle world)
      (is (= 3 (sip:cseq-number (sip:msg-cseq (first (wire-messages world "BYE"))))))
      (is (eq :terminated (sip:call-state call))))))

(test reinvite-14
  (multiple-value-bind (world alice bob incoming bob-events) (call-pair :on-incoming :answer)
    (declare (ignorable bob))
    (multiple-value-bind (on-event events) (recorder)
      (let ((call (sip:invite alice *bob* :sdp *offer* :on-event on-event))
            (new-offer (substitute #\9 #\1 *offer*)))
        (settle world)
        (clear-wire world)
        ;; Accepted automatically with the current description.
        (sip:reinvite call :sdp new-offer)
        (settle world)
        (is (equal '("INVITE" "200/INVITE" "ACK") (wire world)))
        (is (equal '(:ringing :established :reinvite-accepted) (event-names (funcall events))))
        (is (equal '(:established :reinvite) (event-names (funcall bob-events))))
        (is (string= new-offer (sip:call-remote-sdp (funcall incoming))))
        (is (string= new-offer (sip:call-local-sdp call)))
        (is (string= *answer* (sip:call-remote-sdp call)))
        (let ((reinvite (first (wire-messages world "INVITE"))))
          (is (sip:msg-to-tag reinvite))
          (is (sip:msg-contact-uri reinvite)))
        ;; No retransmissions afterwards.
        (tick world 40)
        (is (= 3 (length (wire world))))
        ;; Refused by the application: the session carries on unchanged.
        (clear-wire world)
        (setf (sip:call-auto-answer-reinvite (funcall incoming)) nil)
        (sip:reinvite (funcall incoming) :sdp *answer*)
        (settle world)
        (is (equal '("INVITE" "200/INVITE" "ACK") (wire world)))
        (clear-wire world)
        (setf (sip:call-auto-answer-reinvite call) nil
              (sip:call-on-event call)
              (lambda (call event &rest args)
                (declare (ignore args))
                (when (eq event :reinvite) (sip:reject call))))
        (sip:reinvite (funcall incoming) :sdp new-offer)
        (settle world)
        (is (equal '("INVITE" "488/INVITE" "ACK") (wire world)))
        (is (eq :reinvite-rejected (car (first (last (funcall bob-events))))))
        (is (eq :established (sip:call-state call)))
        (is (eq :established (sip:call-state (funcall incoming))))
        (is (string= *answer* (sip:call-local-sdp (funcall incoming))))))))

(test update-rfc3311
  (multiple-value-bind (world alice bob incoming bob-events) (call-pair :on-incoming :answer)
    (declare (ignorable bob))
    (multiple-value-bind (on-event events) (recorder)
      (let ((call (sip:invite alice *bob* :sdp *offer* :on-event on-event))
            (new-offer (substitute #\9 #\1 *offer*)))
        (settle world)
        (clear-wire world)
        ;; Without a description: a refresh, answered at once.
        (sip:update call)
        (settle world)
        (is (equal '("UPDATE" "200/UPDATE") (wire world)))
        (is (member "UPDATE" (sip:header-values (first (wire-messages world 200)) "Allow")
                    :test #'string=))
        (is (eq :update-accepted (car (first (last (funcall events))))))
        (is (null (getf (cdr (first (last (funcall events)))) :sdp)))
        ;; With one: an offer, answered in the 200, with no ACK to follow.
        (clear-wire world)
        (sip:update call :sdp new-offer)
        (settle world)
        (tick world 40)
        (is (equal '("UPDATE" "200/UPDATE") (wire world)))
        (is (eq :reinvite (car (first (last (funcall bob-events))))))
        (is (string= new-offer (sip:call-remote-sdp (funcall incoming))))
        (is (string= new-offer (sip:call-local-sdp call)))
        (is (string= *answer* (getf (cdr (first (last (funcall events)))) :sdp)))
        ;; The other side may do the same.
        (clear-wire world)
        (sip:update (funcall incoming) :sdp *answer*)
        (settle world)
        (is (equal '("UPDATE" "200/UPDATE") (wire world)))
        ;; An offer while one of ours is outstanding is refused (section 5.2).
        (clear-wire world)
        (sip:reinvite call :sdp *offer*)
        (sip:update (funcall incoming) :sdp *answer*)
        (settle world)
        (is (equal '(:established) (remove-duplicates
                                    (list (sip:call-state call)
                                          (sip:call-state (funcall incoming))))))
        (is-true (wire-messages world 491))))))

(test reinvite-glare-14.1
  (multiple-value-bind (world alice bob incoming) (call-pair :on-incoming :answer)
    (declare (ignorable bob))
    (let ((call (sip:invite alice *bob* :sdp *offer*)))
      (settle world)
      (clear-wire world)
      ;; Both sides send a re-INVITE before seeing the other's.
      (sip:reinvite call :sdp *offer*)
      (sip:reinvite (funcall incoming) :sdp *answer*)
      (settle world)
      (is (equal '("INVITE" "INVITE" "491/INVITE" "491/INVITE" "ACK" "ACK") (wire world)))
      ;; Each retries after its random interval and the second attempts
      ;; succeed one after the other.
      (tick world 5)
      (is (= 2 (length (wire-messages world 200))))
      (is (= 4 (length (wire-messages world "INVITE"))))
      (is (eq :established (sip:call-state call)))
      (is (eq :established (sip:call-state (funcall incoming))))
      (is (null (sip::call-reinvite-creq call)))
      (is (null (sip::call-reinvite-creq (funcall incoming)))))))

(test bye-for-unknown-dialog-after-hangup
  (multiple-value-bind (world alice bob incoming) (call-pair :on-incoming :answer)
    (declare (ignorable bob))
    (let ((call (sip:invite alice *bob* :sdp *offer*)))
      (settle world)
      (let ((stale-bye (sip:dialog-make-request (sip:call-dialog (funcall incoming)) "BYE")))
        (sip:hangup call)
        (settle world)
        (is (eq :terminated (sip:call-state (funcall incoming))))
        ;; A second BYE for the dialog that no longer exists.
        (is (= 481 (sip:response-status (final-response world bob stale-bye))))))))

(test out-of-order-cseq-in-dialog-12.2.2
  (multiple-value-bind (world alice bob incoming) (call-pair :on-incoming :answer)
    (declare (ignorable alice))
    (let ((call (sip:invite alice *bob* :sdp *offer*)))
      (settle world)
      (let ((dialog (sip:call-dialog (funcall incoming))))
        ;; Bob sends OPTIONS in the dialog, then a request with a lower CSeq.
        (is (= 200 (sip:response-status
                    (final-response world bob (sip:dialog-make-request dialog "OPTIONS")))))
        (is (= 200 (sip:response-status
                    (final-response world bob (sip:dialog-make-request dialog "OPTIONS")))))
        (is (= 500 (sip:response-status
                    (final-response world bob
                                    (sip:dialog-make-request
                                     dialog "OPTIONS"
                                     :cseq (1- (sip:dialog-local-seq dialog)))))))
        (is (eq :established (sip:call-state call)))))))

;;;; NAT (RFC 3581) and request screening

(test registration-learns-public-address-behind-nat
  (let* ((world (new-world))
         (registrar (sip:make-registrar))
         (server (agent world "sip:registrar@pop.example.net" "pop.example.net"
                        :registrar registrar))
         ;; Bob's requests appear to come from 203.0.113.7:40000.
         (bob (sip:make-user-agent
               :aor "sip:bob@pop.example.net"
               :scheduler (world-scheduler world)
               :transports (list (sip:make-memory-transport
                                  (world-network world) "192.168.1.20"
                                  :nat '("203.0.113.7" . 40000))))))
    (sip:start bob)
    (multiple-value-bind (on-event events) (recorder)
      (let ((registration (sip:register bob :expires 300 :keepalive 25 :on-event on-event)))
        (settle world)
        ;; First REGISTER carries the private contact; the reply reveals the
        ;; public address, so a second one swaps the binding.
        (destructuring-bind (first second) (wire-messages world "REGISTER")
          (is (equal '("sip:bob@192.168.1.20:5060")
                     (mapcar (lambda (c) (sip:uri-string (sip:name-addr-uri c)))
                             (sip:msg-contacts first))))
          (is (equal '(("sip:bob@192.168.1.20:5060" . "0") ("sip:bob@203.0.113.7:40000" . "300"))
                     (mapcar (lambda (c)
                               (cons (sip:uri-string (sip:name-addr-uri c))
                                     (sip:param (sip:name-addr-params c) "expires")))
                             (sip:msg-contacts second)))))
        (let ((ok (first (wire-messages world 200))))
          (is (string= "203.0.113.7" (sip:via-param (sip:msg-top-via ok) "received")))
          (is (string= "40000" (sip:via-param (sip:msg-top-via ok) "rport"))))
        (is (equal '("sip:bob@203.0.113.7:40000")
                   (mapcar #'sip:uri-string
                           (sip:registrar-lookup registrar server "sip:bob@pop.example.net"))))
        (is (eq :registered (sip:registration-state registration)))
        ;; Reported once, after the address was settled.
        (is (equal '(:registered) (event-names (funcall events))))
        (let ((transport (first (sip:stack-transports (sip:ua-stack bob)))))
          (is (string= "203.0.113.7" (sip:transport-public-host transport)))
          (is (= 40000 (sip:transport-public-port transport))))
        ;; Keep-alives every 25 s; the refresh at 150 s is a single REGISTER.
        (clear-wire world)
        (tick world 140)
        (is (= 5 (sip:network-keepalives (world-network world))))
        (is (null (wire world)))
        (tick world 15)
        (is (equal '("REGISTER" "200/REGISTER") (wire world)))
        ;; The server can reach Bob at his public address, and Bob answers
        ;; with a public Contact.
        (let (response)
          (sip:options server "sip:bob@203.0.113.7:40000"
                       :on-response (lambda (r) (setf response r)))
          (settle world)
          (is (= 200 (sip:response-status response)))
          (is (string= "sip:bob@203.0.113.7:40000"
                       (sip:uri-string (sip:msg-contact-uri response)))))
        ;; No keep-alives once unregistered.
        (sip:unregister registration)
        (settle world)
        (let ((count (sip:network-keepalives (world-network world))))
          (tick world 100)
          (is (= count (sip:network-keepalives (world-network world)))))))))

(test request-filter
  (multiple-value-bind (world alice bob) (pair)
    (let ((handled 0))
      (setf (sip:ua-request-filter bob)
            (lambda (request)
              (incf handled)
              (cond ((string= (sip:request-method request) "OPTIONS") t)
                    ((string= (sip:request-method request) "INVITE") 403)
                    (t nil))))
      (is (= 200 (sip:response-status
                  (final-response world alice (sip:ua-make-request alice "OPTIONS" *bob*)))))
      (is (= 403 (sip:response-status
                  (final-response world alice (sip:ua-make-request alice "INVITE" *bob*)))))
      ;; Dropped requests get no answer at all, and leave no state behind.
      (clear-wire world)
      (let ((response :none))
        (sip:ua-send alice (sip:ua-make-request alice "BYE" *bob*)
                     (lambda (creq event argument)
                       (declare (ignore creq argument))
                       (setf response event)))
        (tick world 33)
        (is (eq :timeout response))
        (is (null (remove-if #'sip:request-p (sip:network-messages (world-network world)))))
        (is (zerop (hash-table-count (sip::stack-server-transactions (sip:ua-stack bob)))))))))

(test address-classification
  (is (sip:cidr-match-p "34.210.91.120" "34.210.91.112/28"))
  (is (not (sip:cidr-match-p "34.210.91.128" "34.210.91.112/28")))
  (is (sip:cidr-match-p "10.1.2.3" "10.1.2.3"))
  (is (not (sip:cidr-match-p "host.example.com" "0.0.0.0/0")))
  (is (sip:cidr-match-p "8.8.8.8" "0.0.0.0/0"))
  (dolist (host '("127.0.0.1" "10.0.0.5" "172.16.16.195" "172.31.255.1" "192.168.1.1" "::1"))
    (is (sip:private-address-p host) "~a is private" host))
  (dolist (host '("172.32.0.1" "34.210.91.112" "203.0.113.7" "sip.example.com"))
    (is (not (sip:private-address-p host)) "~a is public" host)))
