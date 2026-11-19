(in-package :cl-sip)

;;;; Event notification: SUBSCRIBE and NOTIFY (RFC 6665)
;;;;
;;;; A notifier registers event packages with UA-ADD-EVENT-PACKAGE; each
;;;; SUBSCRIBE for one creates a SUBSCRIPTION, which the package describes
;;;; the state of on demand.  A subscriber uses SUBSCRIBE.

(defclass subscription ()
  ((ua :initarg :ua :reader subscription-ua)
   (role :initarg :role :reader subscription-role :documentation ":notifier or :subscriber.")
   (event :initarg :event :reader subscription-event
          :documentation "The event package name, e.g. \"dialog\".")
   (resource :initarg :resource :initform nil :reader subscription-resource
             :documentation "The URI subscribed to.")
   (request :initarg :request :initform nil :accessor subscription-request
            :documentation "The SUBSCRIBE that created the subscription.")
   (dialog :initarg :dialog :initform nil :accessor subscription-dialog)
   (state :initform :active :accessor subscription-state
          :documentation ":pending (subscriber, before any answer), :active or :terminated.")
   (expires-at :initform nil :accessor subscription-expires-at)
   (timer :initform nil :accessor subscription-timer)
   (version :initform -1 :accessor subscription-version
            :documentation "Counts the notifications sent; packages whose
documents carry a version number use it.")
   (on-event :initarg :on-event :initform nil :accessor subscription-on-event
             :documentation "Subscriber: (lambda (subscription event &key ...)).
Events: :active (:expires), :notify (:body :content-type :state),
:rejected (:status), :terminated (:reason).")
   (user-data :initarg :user-data :initform nil :accessor subscription-user-data)))

(defstruct event-package
  name
  content-type
  body                 ; (lambda (subscription)) -> document, or NIL: no such resource
  authorize            ; (lambda (subscription request)) -> true, or status and headers
  (default-expires 3600)
  (max-expires 3600))

(defun ua-add-event-package (ua name &key content-type body authorize
                                       (default-expires 3600) (max-expires 3600))
  "Offer the event package NAME to subscribers.  BODY is called with a
subscription and returns the current state of what it subscribes to as a
document of CONTENT-TYPE, or NIL when there is no such resource (404).
AUTHORIZE, when given, is called with the subscription and its SUBSCRIBE
request first; unless it returns true, the request is refused with the
status it returns as second value (403 by default) and the headers it
returns as third."
  (setf (ua-event-packages ua)
        (cons (make-event-package :name name :content-type content-type :body body
                                  :authorize authorize
                                  :default-expires default-expires
                                  :max-expires max-expires)
              (remove name (ua-event-packages ua) :key #'event-package-name
                                                  :test #'string-equal)))
  (pushnew "SUBSCRIBE" (ua-methods ua) :test #'string=)
  name)

(defun ua-find-event-package (ua name)
  (find name (ua-event-packages ua) :key #'event-package-name :test #'string-equal))

(defun ua-event-subscriptions (ua &optional event)
  "The live subscriptions UA is notifier for, to EVENT or to anything."
  (remove-if-not (lambda (s)
                   (and (eq (subscription-role s) :notifier)
                        (or (null event) (string-equal event (subscription-event s)))))
                 (ua-subscriptions ua)))

(defun event-name (request)
  "The event package named by REQUEST's Event header, without parameters."
  (let ((raw (header request "Event")))
    (and raw (string-downcase (trim-ws (subseq raw 0 (position #\; raw)))))))

(defun subscription-fire (subscription event &rest args)
  (when (subscription-on-event subscription)
    (apply (subscription-on-event subscription) subscription event args)))

(defun subscription-remove (subscription)
  (let ((ua (subscription-ua subscription)))
    (cancel-timer (subscription-timer subscription))
    (setf (subscription-state subscription) :terminated
          (ua-subscriptions ua) (remove subscription (ua-subscriptions ua)))
    (when (subscription-dialog subscription)
      (ua-unregister-dialog ua (subscription-dialog subscription)))))

;;;; Notifier (RFC 6665 section 4.2)

(defun subscription-send-notify (subscription &key terminate (reason "timeout"))
  "Send the current state to the subscriber, or with TERMINATE the news
that the subscription is over."
  (let* ((ua (subscription-ua subscription))
         (package (ua-find-event-package ua (subscription-event subscription)))
         (scheduler (ua-scheduler ua))
         (body (progn
                 (incf (subscription-version subscription))
                 (and package (funcall (event-package-body package) subscription))))
         (request
           (dialog-make-request
            (subscription-dialog subscription) "NOTIFY"
            :headers `(("Event" ,(subscription-event subscription))
                       ("Subscription-State"
                        ,(if terminate
                             (format nil "terminated;reason=~a" reason)
                             (format nil "active;expires=~d"
                                     (max 0 (ceiling (- (subscription-expires-at subscription)
                                                        (now scheduler)))))))
                       ("Contact" ,(ua-contact ua))
                       ("User-Agent" ,(ua-user-agent-name ua))
                       ,@(when body
                           `(("Content-Type" ,(event-package-content-type package)))))
            :body body)))
    (when terminate (subscription-remove subscription))
    (ua-send ua request
             (lambda (creq event argument)
               (declare (ignore creq))
               ;; Section 4.2.2: a subscriber that has gone, or does not know
               ;; the subscription, ends it.
               (when (or (member event '(:timeout :transport-error))
                         (and (eq event :response)
                              (member (response-status argument) '(481 404 410 489))))
                 (subscription-remove subscription)))
             :dialog (subscription-dialog subscription))
    subscription))

(defun subscription-notify (subscription)
  "Tell the subscriber the state has changed."
  (on-loop ((ua-scheduler (subscription-ua subscription)))
    (when (eq (subscription-state subscription) :active)
      (subscription-send-notify subscription))
    subscription))

(defun subscription-terminate (subscription &key (reason "noresource"))
  "End a subscription from the notifier's side."
  (on-loop ((ua-scheduler (subscription-ua subscription)))
    (when (eq (subscription-state subscription) :active)
      (subscription-send-notify subscription :terminate t :reason reason))
    subscription))

(defun subscription-arm (subscription seconds)
  (let ((scheduler (ua-scheduler (subscription-ua subscription))))
    (cancel-timer (subscription-timer subscription))
    (setf (subscription-expires-at subscription) (+ (now scheduler) seconds)
          (subscription-timer subscription)
          (schedule scheduler seconds
                    (lambda ()
                      (when (eq (subscription-state subscription) :active)
                        (subscription-send-notify subscription :terminate t)))))))

(defun subscribe-expires (package request)
  (let ((asked (msg-expires request)))
    (min (or asked (event-package-default-expires package))
         (event-package-max-expires package))))

(defun ua-handle-subscribe (ua request tx)
  "A SUBSCRIBE outside any dialog: a new subscription (section 4.2.1)."
  (let* ((name (event-name request))
         (package (and name (ua-find-event-package ua name))))
    (cond
      ((null package)
       (ua-respond ua tx request 489
                   :headers `(("Allow-Events"
                               ,(format nil "~{~a~^, ~}"
                                        (mapcar #'event-package-name
                                                (ua-event-packages ua)))))))
      (t
       (let* ((tag (make-tag))
              (subscription
                (make-instance 'subscription
                               :ua ua :role :notifier :event name
                               :resource (request-uri request) :request request
                               :dialog (make-uas-dialog request tag :state :confirmed))))
         (multiple-value-bind (allowed status headers)
             (if (event-package-authorize package)
                 (funcall (event-package-authorize package) subscription request)
                 t)
           (cond
             ((not allowed)
              (ua-respond ua tx request (or status 403) :headers headers))
             ((null (funcall (event-package-body package) subscription))
              (ua-respond ua tx request 404))
             (t
              (let ((expires (subscribe-expires package request)))
                (ua-respond ua tx request 200 :to-tag tag
                            :headers `(("Expires" ,expires)
                                       ("Contact" ,(ua-contact-for ua request))))
                (cond
                  ((zerop expires)
                   ;; A one-off fetch of the state (section 4.4.3).
                   (setf (subscription-expires-at subscription) (now (ua-scheduler ua)))
                   (ua-register-dialog ua (subscription-dialog subscription) subscription)
                   (subscription-send-notify subscription :terminate t))
                  (t
                   (push subscription (ua-subscriptions ua))
                   (ua-register-dialog ua (subscription-dialog subscription) subscription)
                   (subscription-arm subscription expires)
                   ;; Section 4.2.1.2: the state follows at once.
                   (subscription-send-notify subscription))))))))))))

(defun subscription-handle-resubscribe (subscription request tx)
  "A SUBSCRIBE inside the subscription's dialog: a refresh, or with Expires
of 0 the end (section 4.2.1.4)."
  (let* ((ua (subscription-ua subscription))
         (package (ua-find-event-package ua (subscription-event subscription)))
         (expires (subscribe-expires package request)))
    (dialog-refresh-target (subscription-dialog subscription) request)
    (ua-respond ua tx request 200
                :headers `(("Expires" ,expires)
                           ("Contact" ,(ua-contact-for ua request))))
    (cond ((zerop expires)
           (setf (subscription-expires-at subscription) (now (ua-scheduler ua)))
           (subscription-send-notify subscription :terminate t))
          (t
           (subscription-arm subscription expires)
           (subscription-send-notify subscription)))))

;;;; Subscriber (section 4.1)

(defun subscribe (ua target event &key (expires 3600) accept on-event user-data)
  "Subscribe to EVENT at TARGET (a URI or string).  Returns a SUBSCRIPTION;
the state arrives through ON-EVENT as :notify events.  The subscription is
not renewed automatically: call RESUBSCRIBE before it lapses."
  (on-loop ((ua-scheduler ua))
    (let* ((request (ua-make-request
                     ua "SUBSCRIBE" target
                     :headers `(("Event" ,event)
                                ("Expires" ,expires)
                                ,@(when accept `(("Accept" ,accept))))))
           (subscription (make-instance 'subscription
                                        :ua ua :role :subscriber :event event
                                        :resource (request-uri request)
                                        :on-event on-event :user-data user-data)))
      (add-header request "Contact" (ua-contact-for ua request))
      (setf (subscription-state subscription) :pending)
      (push subscription (ua-subscriptions ua))
      (subscription-send-subscribe subscription request)
      subscription)))

(defun subscription-send-subscribe (subscription request)
  (let ((ua (subscription-ua subscription)))
    (setf (subscription-request subscription) request)
    (ua-send ua request
             (lambda (creq event argument)
               (setf (subscription-request subscription) (creq-request creq))
               (let ((status (and (eq event :response) (response-status argument))))
                 (cond
                   ((eq (subscription-state subscription) :terminated))
                   ((and status (< status 200)))
                   ((and status (< status 300))
                    ;; The NOTIFY may have set the dialog up already.
                    (unless (subscription-dialog subscription)
                      (setf (subscription-dialog subscription)
                            (make-uac-dialog (creq-request creq) argument))
                      (ua-register-dialog ua (subscription-dialog subscription)
                                          subscription))
                    (when (eq (subscription-state subscription) :pending)
                      (setf (subscription-state subscription) :active))
                    (subscription-fire subscription :active
                                       :expires (msg-expires argument)
                                       :response argument))
                   (t
                    (subscription-remove subscription)
                    (subscription-fire subscription :rejected
                                       :status status :response argument)))))
             :dialog (subscription-dialog subscription))))

(defun resubscribe (subscription &key (expires 3600))
  "Renew a subscription made with SUBSCRIBE, or with EXPIRES of 0 end it."
  (on-loop ((ua-scheduler (subscription-ua subscription)))
    (when (and (eq (subscription-role subscription) :subscriber)
               (eq (subscription-state subscription) :active)
               (subscription-dialog subscription))
      (subscription-send-subscribe
       subscription
       (dialog-make-request (subscription-dialog subscription) "SUBSCRIBE"
                            :headers `(("Event" ,(subscription-event subscription))
                                       ("Expires" ,expires)
                                       ("Contact" ,(ua-contact (subscription-ua
                                                                subscription)))))))
    subscription))

(defun unsubscribe (subscription)
  (resubscribe subscription :expires 0))

(defun subscription-handle-notify (subscription request tx)
  (let* ((ua (subscription-ua subscription))
         (state (string-downcase (or (header request "Subscription-State") "")))
         (terminated (alexandria:starts-with-subseq "terminated" (trim-ws state))))
    (ua-respond ua tx request 200)
    (when (eq (subscription-state subscription) :pending)
      (setf (subscription-state subscription) :active))
    (subscription-fire subscription :notify
                       :body (and (message-body request) (message-body-string request))
                       :content-type (msg-content-type request)
                       :state (if terminated :terminated :active)
                       :request request)
    (when terminated
      (subscription-remove subscription)
      (subscription-fire subscription :terminated
                         :reason (let ((at (search "reason=" state)))
                                   (and at (subseq state (+ at 7)
                                                   (position #\; state :start at))))))))

(defun ua-adopt-notify (ua request)
  "A NOTIFY for a subscription whose SUBSCRIBE has not been answered yet:
the NOTIFY itself establishes the dialog (section 4.4.1).  Returns the
subscription."
  (let ((subscription
          (find-if (lambda (s)
                     (and (eq (subscription-role s) :subscriber)
                          (null (subscription-dialog s))
                          (subscription-request s)
                          (equal (msg-call-id (subscription-request s)) (msg-call-id request))
                          (equal (msg-from-tag (subscription-request s)) (msg-to-tag request))
                          (equal (event-name request) (string-downcase
                                                      (subscription-event s)))))
                   (ua-subscriptions ua))))
    (when subscription
      (let ((dialog (make-uas-dialog request (msg-to-tag request) :state :confirmed)))
        (setf (dialog-local-seq dialog)
              (cseq-number (msg-cseq (subscription-request subscription)))
              (subscription-dialog subscription) dialog)
        (ua-register-dialog ua dialog subscription)))
    subscription))

;;;; Requests inside a subscription's dialog

(defmethod handle-dialog-request ((subscription subscription) request tx)
  (let ((ua (subscription-ua subscription))
        (method (request-method request)))
    (cond
      ((not (dialog-check-remote-cseq (subscription-dialog subscription) request))
       (ua-respond ua tx request 500 :reason "CSeq Out Of Order"))
      ((and (string= method "SUBSCRIBE") (eq (subscription-role subscription) :notifier))
       (subscription-handle-resubscribe subscription request tx))
      ((and (string= method "NOTIFY") (eq (subscription-role subscription) :subscriber))
       (subscription-handle-notify subscription request tx))
      (t (ua-respond ua tx request 405)))))
