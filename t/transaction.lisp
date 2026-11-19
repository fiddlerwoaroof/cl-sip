(in-package :cl-sip.test)

(def-suite transaction :in cl-sip)
(in-suite transaction)

(defun raw-stack (world host &key reliable handler)
  (let ((stack (sip:make-stack :scheduler (world-scheduler world))))
    (sip:transport-start
     (sip:add-transport stack (sip:make-memory-transport (world-network world) host
                                                         :reliable reliable)))
    (setf (sip:stack-request-handler stack) handler)
    stack))

(defun tx-request (method &key (target "sip:bob@server.example.com") (call-id "tx-test"))
  (sip:make-request method target
                    :headers `(("Max-Forwards" "70")
                               ("From" "<sip:alice@client.example.com>;tag=a1")
                               ("To" "<sip:bob@server.example.com>")
                               ("Call-ID" ,call-id)
                               ("CSeq" ,(format nil "1 ~a" method)))))

(defun tu-recorder ()
  (let ((events '()))
    (values (lambda (tx event argument)
              (declare (ignore tx))
              (push (if (eq event :response)
                        (sip:response-status argument)
                        event)
                    events))
            (lambda () (reverse events)))))

(defun responder (&rest statuses)
  "A request handler answering each new request with STATUSES in turn."
  (lambda (request tx)
    (dolist (status statuses)
      (sip:send-response tx (sip:make-response-for request status :to-tag "srv")))))

(test invite-client-retransmits-and-times-out
  ;; Section 17.1.1.2: timer A doubles from T1; timer B fires at 64*T1.
  (let* ((world (new-world))
         (client (raw-stack world "client.example.com")))
    (multiple-value-bind (tu events) (tu-recorder)
      (let ((tx (sip:send-request client (tx-request "INVITE") tu)))
        (is (eq :calling (sip:transaction-state tx)))
        (tick world 31)
        (is (eq :calling (sip:transaction-state tx)))
        (tick world 2)
        (is (equal '(0 1/2 3/2 7/2 15/2 31/2 63/2) (send-times world "INVITE")))
        (is (equal '(:timeout :terminated) (funcall events)))
        (is (eq :terminated (sip:transaction-state tx)))
        (is (zerop (hash-table-count (sip::stack-client-transactions client))))))))

(test non-invite-client-retransmits-and-times-out
  ;; Section 17.1.2.2: timer E doubles up to T2; timer F fires at 64*T1.
  (let* ((world (new-world))
         (client (raw-stack world "client.example.com")))
    (multiple-value-bind (tu events) (tu-recorder)
      (sip:send-request client (tx-request "OPTIONS") tu)
      (tick world 40)
      (is (equal '(0 1/2 3/2 7/2 15/2 23/2 31/2 39/2 47/2 55/2 63/2)
                 (send-times world "OPTIONS")))
      (is (equal '(:timeout :terminated) (funcall events))))))

(test non-invite-client-proceeding-retransmits-at-t2
  (let* ((world (new-world))
         (server (raw-stack world "server.example.com" :handler (responder 100)))
         (client (raw-stack world "client.example.com")))
    (declare (ignorable server))
    (multiple-value-bind (tu events) (tu-recorder)
      (let ((tx (sip:send-request client (tx-request "OPTIONS") tu)))
        (settle world)
        (is (eq :proceeding (sip:transaction-state tx)))
        (tick world 13)
        ;; Timer E was pending at T1 when the 1xx arrived; from then on T2.
        (is (equal '(0 1/2 9/2 17/2 25/2) (send-times world "OPTIONS")))
        ;; The server answers each retransmission with its last provisional.
        (is (= 5 (length (wire-messages world 100))))
                 ;; Every provisional is passed up, retransmitted or not.
        (is (equal '(100 100 100 100 100) (funcall events)))))))

(test invite-non-2xx-is-acknowledged-by-the-transaction
  ;; Sections 17.1.1.2, 17.1.1.3, 17.2.1.
  (let* ((world (new-world))
         (server-tx nil)
         (server (raw-stack world "server.example.com"
                            :handler (lambda (request tx)
                                       (setf server-tx tx)
                                       (funcall (responder 180 486) request tx))))
         (client (raw-stack world "client.example.com")))
    (declare (ignorable server))
    (multiple-value-bind (tu events) (tu-recorder)
      (let ((tx (sip:send-request client (tx-request "INVITE") tu)))
        (settle world)
        (is (equal '("INVITE" "180/INVITE" "486/INVITE" "ACK") (wire world)))
        (is (equal '(180 486) (funcall events)))
        (is (eq :completed (sip:transaction-state tx)))
        (is (eq :confirmed (sip:transaction-state server-tx)))
        (let ((invite (first (wire-messages world "INVITE")))
              (ack (first (wire-messages world "ACK"))))
          (is (string= (sip:header invite "Via") (sip:header ack "Via")))
          (is (= 1 (length (sip:header-values ack "Via"))))
          (is (string= "srv" (sip:msg-to-tag ack)))
          (is (string= "1 ACK" (sip:header ack "CSeq")))
          (is (sip:uri-equal (sip:request-uri invite) (sip:request-uri ack))))
        ;; Timer I (T4) ends the server side, timer D (32 s) the client side.
        (tick world 6)
        (is (eq :terminated (sip:transaction-state server-tx)))
        (is (eq :completed (sip:transaction-state tx)))
        (tick world 27)
        (is (eq :terminated (sip:transaction-state tx)))
        ;; No retransmissions of anything once acknowledged.
        (is (= 4 (length (wire world))))))))

(test invite-server-retransmits-final-response-until-ack
  ;; Section 17.2.1: timer G doubles up to T2, timer H gives up at 64*T1.
  (let* ((world (new-world))
         (server-tx nil)
         (server-events nil)
         (server (raw-stack world "server.example.com"
                            :handler (lambda (request tx)
                                       (setf server-tx tx)
                                       (multiple-value-bind (tu events) (tu-recorder)
                                         (setf (sip:transaction-tu tx) tu
                                               server-events events))
                                       (funcall (responder 486) request tx))))
         (client (raw-stack world "client.example.com")))
    (declare (ignorable server))
    ;; Lose every ACK.
    (set-filter world (lambda (m from to)
                        (declare (ignore from to))
                        (if (and (sip:request-p m) (string= "ACK" (sip:request-method m)))
                            :drop
                            t)))
    (multiple-value-bind (tu events) (tu-recorder)
      (sip:send-request client (tx-request "INVITE") tu)
      (tick world 33)
      (is (equal '(0 1/2 3/2 7/2 15/2 23/2 31/2 39/2 47/2 55/2 63/2)
                 (send-times world 486)))
      ;; Each retransmitted response is acknowledged again, but passed to the
      ;; TU only once.
      (is (= 11 (length (send-times world "ACK"))))
      (is (equal '(486) (remove :terminated (funcall events))))
      (is (equal '(:timeout :terminated) (funcall server-events)))
      (is (eq :terminated (sip:transaction-state server-tx))))))

(test invite-server-sends-100-after-200ms
  (let* ((world (new-world))
         (server-tx nil)
         (server (raw-stack world "server.example.com"
                            :handler (lambda (request tx)
                                       (declare (ignore request))
                                       (setf server-tx tx))))
         (client (raw-stack world "client.example.com")))
    (declare (ignorable server))
    (let ((tx (sip:send-request client (tx-request "INVITE") nil)))
      (tick world 1/10)
      (is (equal '("INVITE") (wire world)))
      (tick world 2/10)
      (is (equal '("INVITE" "100/INVITE") (wire world)))
      (is (null (sip:msg-to-tag (first (wire-messages world 100)))))
      (is (eq :proceeding (sip:transaction-state tx)))
      ;; The 100 stopped the retransmissions.
      (tick world 20)
      (is (equal '("INVITE" "100/INVITE") (wire world))))))

(test invite-2xx-accepted-state-rfc6026
  (let* ((world (new-world))
         (server-tx nil)
         (response nil)
         (server (raw-stack world "server.example.com"
                            :handler (lambda (request tx)
                                       (setf server-tx tx
                                             response (sip:make-response-for
                                                       request 200 :to-tag "srv"))
                                       (sip:send-response tx response))))
         (client (raw-stack world "client.example.com")))
    (declare (ignorable server))
    (multiple-value-bind (tu events) (tu-recorder)
      (let ((tx (sip:send-request client (tx-request "INVITE") tu)))
        (settle world)
        ;; Neither side destroys its transaction on the 2xx.
        (is (eq :accepted (sip:transaction-state tx)))
        (is (eq :accepted (sip:transaction-state server-tx)))
        ;; The TU retransmits the 2xx through the transaction; each copy
        ;; reaches the client's TU.
        (sip:send-response server-tx response)
        (settle world)
        (is (equal '(200 200) (funcall events)))
        ;; The transaction itself never sends an ACK for a 2xx.
        (is (null (wire-messages world "ACK")))
        ;; A retransmitted INVITE is absorbed, not handed to the TU again.
        (let ((invite (first (wire-messages world "INVITE"))))
          (sip::stack-deliver server (first (sip:stack-transports server))
                              (sip:serialize-message invite) "client.example.com" 5060 nil))
        (is (= 2 (length (wire-messages world 200))))
        ;; Timers L and M (64*T1).
        (tick world 33)
        (is (eq :terminated (sip:transaction-state tx)))
        (is (eq :terminated (sip:transaction-state server-tx)))))))

(test non-invite-server-absorbs-retransmissions
  ;; Section 17.2.2.
  (let* ((world (new-world))
         (calls 0)
         (server-tx nil)
         (server (raw-stack world "server.example.com"
                            :handler (lambda (request tx)
                                       (incf calls)
                                       (setf server-tx tx)
                                       (funcall (responder 200) request tx))))
         (client (raw-stack world "client.example.com"))
         (dropped nil))
    (declare (ignorable server))
    ;; Lose the first 200.
    (set-filter world (lambda (m from to)
                        (declare (ignore from to))
                        (if (and (sip:response-p m) (not dropped))
                            (progn (setf dropped t) :drop)
                            t)))
    (multiple-value-bind (tu events) (tu-recorder)
      (let ((tx (sip:send-request client (tx-request "OPTIONS") tu)))
        (tick world 1)
        (is (equal '("OPTIONS" "200/OPTIONS" "OPTIONS" "200/OPTIONS") (wire world)))
        (is (= 1 calls))
        (is (equal '(200) (funcall events)))
        (is (eq :completed (sip:transaction-state tx)))
        (is (eq :completed (sip:transaction-state server-tx)))
        ;; Timer K (T4) then timer J (64*T1).
        (tick world 5)
        (is (eq :terminated (sip:transaction-state tx)))
        (tick world 27)
        (is (eq :terminated (sip:transaction-state server-tx)))))))

(test reliable-transport-does-not-retransmit
  (let* ((world (new-world))
         (server-tx nil)
         (answer nil)
         (server (raw-stack world "server.example.com" :reliable t
                            :handler (lambda (request tx)
                                       (setf server-tx tx)
                                       (when answer
                                         (funcall (responder answer) request tx)))))
         (client (raw-stack world "client.example.com" :reliable t)))
    (declare (ignorable server))
    ;; Unanswered: one transmission only, then timer F.
    (multiple-value-bind (tu events) (tu-recorder)
      (sip:send-request client (tx-request "OPTIONS") tu)
      (tick world 33)
      (is (equal '("OPTIONS") (wire world)))
      (is (string= "TCP" (sip:via-transport (sip:msg-top-via (first (wire-messages world))))))
      (is (equal '(:timeout :terminated) (funcall events))))
    ;; Answered: timers K and J are zero, so both sides finish at once.
    (clear-wire world)
    (setf answer 200)
    (let ((tx (sip:send-request client (tx-request "OPTIONS" :call-id "second") nil)))
      (settle world)
      (is (eq :terminated (sip:transaction-state tx)))
      (is (eq :terminated (sip:transaction-state server-tx))))
    ;; INVITE rejected: no timer G, timers D and I are zero.
    (clear-wire world)
    (setf answer 486)
    (let ((tx (sip:send-request client (tx-request "INVITE" :call-id "third") nil)))
      (tick world 10)
      (is (equal '("INVITE" "486/INVITE" "ACK") (wire world)))
      (is (eq :terminated (sip:transaction-state tx)))
      (is (eq :terminated (sip:transaction-state server-tx))))
    ;; Nobody listening: a transport error, reported to the TU.
    (multiple-value-bind (tu events) (tu-recorder)
      (sip:send-request client (tx-request "OPTIONS" :target "sip:x@nowhere.example.com") tu)
      (is (equal '(:transport-error :terminated) (funcall events))))))

(test via-and-rport-handling-rfc3581
  (let* ((world (new-world))
         (seen nil)
         (server (raw-stack world "server.example.com"
                            :handler (lambda (request tx)
                                       (setf seen request)
                                       (funcall (responder 200) request tx))))
         (client (raw-stack world "client.example.com")))
    (declare (ignorable client))
    ;; The client asks for rport; the server fills in received and rport.
    (sip:send-request client (tx-request "OPTIONS") nil)
    (settle world)
    (let ((sent (sip:msg-top-via (first (wire-messages world "OPTIONS"))))
          (received (sip:msg-top-via seen)))
      (is (equal '(nil t) (multiple-value-list (sip:via-param sent "rport"))))
      (is (alexandria:starts-with-subseq "z9hG4bK" (sip:via-branch sent)))
      (is (string= "5060" (sip:via-param received "rport")))
      (is (string= "client.example.com" (sip:via-param received "received"))))
    ;; A request whose Via names another host gets `received`, and the
    ;; response goes back to the source address (section 18.2.2).
    (clear-wire world)
    (sip::stack-deliver
     server (first (sip:stack-transports server))
     (sip:serialize-message
      (sip:make-request "OPTIONS" "sip:bob@server.example.com"
                        :headers '(("Via" "SIP/2.0/UDP nat.internal:5099;branch=z9hG4bKnat")
                                   ("From" "<sip:a@b>;tag=1") ("To" "<sip:c@d>")
                                   ("Call-ID" "nat") ("CSeq" "1 OPTIONS"))))
     "client.example.com" 5060 nil)
    (is (string= "client.example.com" (sip:via-param (sip:msg-top-via seen) "received")))
    (is (null (nth-value 1 (sip:via-param (sip:msg-top-via seen) "rport"))))
    (destructuring-bind (message from-host from-port to-host to-port)
        (first (sip::network-log (world-network world)))
      (declare (ignore from-host from-port))
      (is (= 200 (sip:response-status message)))
      (is (string= "client.example.com" to-host))
      ;; Without rport the port comes from the sent-by value.
      (is (= 5099 to-port)))))

(test malformed-requests-get-400
  (let* ((world (new-world))
         (calls 0)
         (server (raw-stack world "server.example.com"
                            :handler (lambda (request tx)
                                       (declare (ignore request tx))
                                       (incf calls)))))
    (sip::stack-deliver
     server (first (sip:stack-transports server))
     (sip::string-to-octets
      (crlf "OPTIONS sip:bob@server.example.com SIP/2.0"
            "Via: SIP/2.0/UDP client.example.com;branch=z9hG4bKbad"
            "From: <sip:a@b>;tag=1" "To: <sip:c@d>" "Call-ID: bad" "CSeq: 1 INVITE"))
     "client.example.com" 5060 nil)
    (is (zerop calls))
    (is (equal '("400/INVITE") (wire world)))
    ;; Garbage is dropped silently.
    (sip::stack-deliver server (first (sip:stack-transports server))
                        (sip::string-to-octets "hello") "client.example.com" 5060 nil)
    (is (= 1 (length (wire world))))))

(test rfc2543-branch-matching
  ;; Section 17.2.3: without the magic cookie, a retransmission is still
  ;; recognized from the other header fields.
  (let* ((world (new-world))
         (calls 0)
         (server (raw-stack world "server.example.com"
                            :handler (lambda (request tx)
                                       (incf calls)
                                       (funcall (responder 200) request tx))))
         (octets (sip:serialize-message
                  (sip:make-request "OPTIONS" "sip:bob@server.example.com"
                                    :headers '(("Via" "SIP/2.0/UDP client.example.com;branch=old")
                                               ("From" "<sip:a@b>;tag=1") ("To" "<sip:c@d>")
                                               ("Call-ID" "old-style") ("CSeq" "1 OPTIONS"))))))
    (dotimes (i 2)
      (sip::stack-deliver server (first (sip:stack-transports server)) octets
                          "client.example.com" 5060 nil))
    (is (= 1 calls))
    (is (equal '("200/OPTIONS" "200/OPTIONS") (wire world)))))
