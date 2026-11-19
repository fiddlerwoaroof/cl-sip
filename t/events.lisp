(in-package :cl-sip.test)

(def-suite events :in cl-sip)
(in-suite events)

;;;; SUBSCRIBE and NOTIFY (RFC 6665), in the virtual world: Bob offers an
;;;; event package whose state is a string, Alice subscribes to it.

(defun event-pair ()
  (let* ((world (new-world))
         (alice (agent world "sip:alice@atlanta.example.com" "client.atlanta.example.com"))
         (bob (agent world "sip:bob@biloxi.example.com" "client.biloxi.example.com"))
         (state (list "idle")))
    (sip:ua-add-event-package
     bob "lamp"
     :content-type "text/plain"
     :max-expires 600
     :body (lambda (subscription)
             (and (string= "bob" (sip:sip-uri-user (sip:subscription-resource subscription)))
                  (format nil "~a ~d" (first state) (sip:subscription-version subscription))))
     :authorize (lambda (subscription request)
                  (declare (ignore subscription))
                  (or (null (sip:header request "X-Stranger"))
                      (values nil 403))))
    (values world alice bob state)))

(defun notify-bodies (events)
  (loop for (name . args) in (funcall events)
        when (eq name :notify) collect (getf args :body)))

(test subscribe-and-notify-rfc6665-4
  (multiple-value-bind (world alice bob state) (event-pair)
    (multiple-value-bind (on-event events) (recorder)
      (let ((subscription (sip:subscribe alice *bob* "lamp" :expires 900 :on-event on-event)))
        (settle world)
        ;; Accepted, with the state following at once (section 4.2.1.2).
        (is (equal '("SUBSCRIBE" "200/SUBSCRIBE" "NOTIFY" "200/NOTIFY") (wire world)))
        (is (equal '(:active :notify) (event-names (funcall events))))
        (is (equal '("idle 0") (notify-bodies events)))
        (is (eq :active (sip:subscription-state subscription)))
        ;; The notifier may shorten the interval (section 4.2.1.1).
        (is (= 600 (sip:msg-expires (first (wire-messages world 200)))))
        (let ((notify (first (wire-messages world "NOTIFY"))))
          (is (string= "lamp" (sip:header notify "Event")))
          (is (search "active;expires=600" (sip:header notify "Subscription-State")))
          (is (string= "text/plain" (sip:msg-content-type notify)))
          ;; Same dialog as the SUBSCRIBE created.
          (is (string= (sip:msg-call-id notify)
                       (sip:msg-call-id (first (wire-messages world "SUBSCRIBE"))))))
        (is (= 1 (length (sip:ua-event-subscriptions bob "lamp"))))
        ;; A change of state.
        (clear-wire world)
        (setf (first state) "busy")
        (sip:subscription-notify (first (sip:ua-event-subscriptions bob)))
        (settle world)
        (is (equal '("NOTIFY" "200/NOTIFY") (wire world)))
        (is (equal '("idle 0" "busy 1") (notify-bodies events)))
        ;; A refresh inside the dialog brings the state again.
        (clear-wire world)
        (tick world 300)
        (sip:resubscribe subscription :expires 300)
        (settle world)
        (is (equal '("SUBSCRIBE" "200/SUBSCRIBE" "NOTIFY" "200/NOTIFY") (wire world)))
        (is (sip:msg-to-tag (first (wire-messages world "SUBSCRIBE"))))
        (is (search "expires=300" (sip:header (first (wire-messages world "NOTIFY"))
                                              "Subscription-State")))
        ;; Left alone, the subscription lapses and the subscriber is told.
        (clear-wire world)
        (tick world 301)
        (is (equal '("NOTIFY" "200/NOTIFY") (wire world)))
        (is (search "terminated;reason=timeout"
                    (sip:header (first (wire-messages world "NOTIFY")) "Subscription-State")))
        (is (eq :terminated (car (first (last (funcall events))))))
        (is (string= "timeout" (getf (cdr (first (last (funcall events)))) :reason)))
        (is (eq :terminated (sip:subscription-state subscription)))
        (is (null (sip:ua-event-subscriptions bob)))
        ;; Nothing is left of the dialog on either side.
        (is (zerop (hash-table-count (sip::ua-dialog-owners alice))))
        (is (zerop (hash-table-count (sip::ua-dialog-owners bob))))))))

(test unsubscribe-and-fetch
  (multiple-value-bind (world alice bob) (event-pair)
    (multiple-value-bind (on-event events) (recorder)
      (let ((subscription (sip:subscribe alice *bob* "lamp" :on-event on-event)))
        (settle world)
        (clear-wire world)
        (sip:unsubscribe subscription)
        (settle world)
        (is (equal '("SUBSCRIBE" "200/SUBSCRIBE" "NOTIFY" "200/NOTIFY") (wire world)))
        (is (= 0 (sip:msg-expires (first (wire-messages world "SUBSCRIBE")))))
        (is (search "terminated" (sip:header (first (wire-messages world "NOTIFY"))
                                             "Subscription-State")))
        (is (eq :terminated (sip:subscription-state subscription)))
        (is (null (sip:ua-event-subscriptions bob)))))
    ;; Expires of 0 from the start asks for the state once (section 4.4.3).
    (clear-wire world)
    (multiple-value-bind (on-event events) (recorder)
      (sip:subscribe alice *bob* "lamp" :expires 0 :on-event on-event)
      (settle world)
      (is (equal '("SUBSCRIBE" "200/SUBSCRIBE" "NOTIFY" "200/NOTIFY") (wire world)))
      (is (equal '("idle 0") (notify-bodies events)))
      (is (null (sip:ua-event-subscriptions bob))))))

(test subscriptions-refused
  (multiple-value-bind (world alice bob) (event-pair)
    (declare (ignorable bob))
    (flet ((outcome (target event &rest headers)
             (clear-wire world)
             (multiple-value-bind (on-event events) (recorder)
               (declare (ignorable on-event))
               (let ((request (sip:ua-make-request
                               alice "SUBSCRIBE" target
                               :headers `(("Event" ,event) ("Expires" 600) ,@headers))))
                 (sip:ua-send alice request (lambda (&rest args) (declare (ignore args)))))
               (settle world)
               (list (wire world) events))))
      ;; An event package we do not have (section 4.2.1.1), with what we do.
      (is (equal '("SUBSCRIBE" "489/SUBSCRIBE") (first (outcome *bob* "weather"))))
      (is (string= "lamp" (sip:header (first (wire-messages world 489)) "Allow-Events")))
      ;; No such resource.
      (is (equal '("SUBSCRIBE" "404/SUBSCRIBE")
                 (first (outcome "sip:carol@client.biloxi.example.com" "lamp"))))
      ;; Not allowed.
      (is (equal '("SUBSCRIBE" "403/SUBSCRIBE")
                 (first (outcome *bob* "lamp" '("X-Stranger" "yes"))))))
    ;; Through the API the refusal is an event.
    (multiple-value-bind (on-event events) (recorder)
      (let ((subscription (sip:subscribe alice *bob* "weather" :on-event on-event)))
        (settle world)
        (is (equal '(:rejected) (event-names (funcall events))))
        (is (= 489 (getf (cdr (first (funcall events))) :status)))
        (is (eq :terminated (sip:subscription-state subscription)))))
    ;; An agent with no packages does not take SUBSCRIBE at all.
    (clear-wire world)
    (sip:subscribe bob "sip:alice@client.atlanta.example.com" "lamp")
    (settle world)
    (is (equal '("SUBSCRIBE" "405/SUBSCRIBE") (wire world)))))

(test notify-before-the-200-rfc6665-4.4.1
  (multiple-value-bind (world alice bob) (event-pair)
    (declare (ignorable bob))
    ;; The first 200 to the SUBSCRIBE is lost, so the NOTIFY gets there first
    ;; and has to set the dialog up by itself.
    (let ((dropped nil))
      (set-filter world (lambda (message from to)
                          (declare (ignore from to))
                          (cond ((and (not dropped)
                                      (sip:response-p message)
                                      (= 200 (sip:response-status message))
                                      (string= "SUBSCRIBE"
                                               (sip:cseq-method (sip:msg-cseq message))))
                                 (setf dropped t)
                                 :drop)
                                (t t))))
      (multiple-value-bind (on-event events) (recorder)
        (let ((subscription (sip:subscribe alice *bob* "lamp" :on-event on-event)))
          (settle world)
          (is (equal '(:notify) (event-names (funcall events))))
          (is (eq :active (sip:subscription-state subscription)))
          ;; The SUBSCRIBE is retransmitted and answered again.
          (tick world 2)
          (is (equal '(:notify :active) (event-names (funcall events))))
          ;; The dialog works in both directions afterwards.
          (clear-wire world)
          (sip:unsubscribe subscription)
          (settle world)
          (is (equal '("SUBSCRIBE" "200/SUBSCRIBE" "NOTIFY" "200/NOTIFY") (wire world))))))))
