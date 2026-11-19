(in-package :cl-sip)

;;;; Sessions: INVITE, ACK, CANCEL, BYE, re-INVITE
;;;; (RFC 3261 sections 9, 13, 14, 15; race handling per RFC 5407)

(defclass call ()
  ((ua :initarg :ua :reader call-ua)
   (direction :initarg :direction :reader call-direction
              :documentation ":outgoing or :incoming.")
   (state :initarg :state :accessor call-state
          :documentation "Outgoing: :calling, :proceeding, :ringing.  Incoming:
:incoming, :ringing, :answered (2xx sent, ACK awaited).  Both: :established,
:terminating, :terminated.")
   (dialog :initform nil :accessor call-dialog)
   (invite :initarg :invite :initform nil :accessor call-invite
           :documentation "The initial INVITE request.")
   (on-event :initarg :on-event :initform nil :accessor call-on-event
             :documentation "(lambda (call event &key ...)).  Events:
:progress (:status :sdp :response), :ringing (:response),
:established (:sdp), :reinvite (:sdp), :reinvite-accepted (:sdp),
:reinvite-rejected (:status), :terminated (:cause :status :response).")
   (local-sdp :initarg :local-sdp :initform nil :accessor call-local-sdp)
   (remote-sdp :initarg :remote-sdp :initform nil :accessor call-remote-sdp)
   (cause :initform nil :accessor call-cause)
   (status :initform nil :accessor call-status
           :documentation "Final status of a failed outgoing INVITE.")
   (user-data :initarg :user-data :initform nil :accessor call-user-data)
   ;; Outgoing INVITE
   (creq :initform nil :accessor call-creq)
   (got-provisional :initform nil :accessor call-got-provisional)
   (cancel-requested :initform nil :accessor call-cancel-requested)
   (cancel-sent :initform nil :accessor call-cancel-sent)
   (ack :initform nil :accessor call-ack
        :documentation "(request transport host port) of the last ACK sent for a 2xx.")
   (answer-function :initarg :answer-function :initform nil
                    :accessor call-answer-function
                    :documentation "For an INVITE sent without SDP: called with
the offer from the 2xx, returns the answer to put in the ACK.")
   ;; Incoming INVITE
   (server-tx :initarg :server-tx :initform nil :accessor call-server-tx)
   (local-tag :initarg :local-tag :initform nil :accessor call-local-tag)
   ;; 2xx retransmission by the UAS core (section 13.3.1.4)
   (pending-2xx :initform nil :accessor call-pending-2xx)
   (pending-2xx-timer :initform nil :accessor call-pending-2xx-timer)
   ;; re-INVITE
   (auto-answer-reinvite :initarg :auto-answer-reinvite :initform t
                         :accessor call-auto-answer-reinvite)
   (reinvite-tx :initform nil :accessor call-reinvite-tx)
   (reinvite-request :initform nil :accessor call-reinvite-request)
   (reinvite-creq :initform nil :accessor call-reinvite-creq)))

(defmethod print-object ((call call) stream)
  (print-unreadable-object (call stream :type t :identity t)
    (format stream "~a ~a" (call-direction call) (call-state call))))

(defun call-stack (call) (ua-stack (call-ua call)))
(defun call-scheduler (call) (ua-scheduler (call-ua call)))

(defun call-fire (call event &rest args)
  (when (call-on-event call)
    (apply (call-on-event call) call event args)))

(defun call-terminated-p (call)
  (eq (call-state call) :terminated))

(defun call-terminate (call cause &rest args)
  (unless (call-terminated-p call)
    (setf (call-state call) :terminated
          (call-cause call) cause
          (call-pending-2xx call) nil)
    (cancel-timer (call-pending-2xx-timer call))
    (when (call-dialog call)
      (setf (dialog-state (call-dialog call)) :terminated)
      (ua-unregister-dialog (call-ua call) (call-dialog call)))
    (when (call-server-tx call)
      (remhash (call-server-tx call) (ua-incoming-invites (call-ua call))))
    (apply #'call-fire call :terminated :cause cause args)))

(defun sdp-body (message)
  "Body of MESSAGE as a string when it is (or is assumed to be) SDP."
  (and (message-body message)
       (member (msg-content-type message) '(nil "application/sdp") :test #'equal)
       (message-body-string message)))

(defun session-headers (ua message body)
  "Headers common to INVITE requests and their 2xx responses."
  `(("Contact" ,(ua-contact-for ua message))
    ("Allow" ,(ua-allow-header ua))
    ,@(when body '(("Content-Type" "application/sdp")))))

;;;; UAC side (section 13.2)

(defun invite (ua target &key to sdp headers on-event answer-function user-data)
  "Call TARGET (a SIP URI or string), offering the session description SDP
(a string).  TO is the logical recipient when it differs from TARGET, as it
does when calling a registered contact directly.  Returns a CALL immediately;
progress arrives through ON-EVENT."
  (on-loop ((ua-scheduler ua))
    (let* ((request (ua-make-request ua "INVITE" target :to to :headers headers :body sdp))
           (call (make-instance 'call :ua ua :direction :outgoing :state :calling
                                      :on-event on-event :local-sdp sdp
                                      :answer-function answer-function
                                      :user-data user-data :invite request)))
      (loop for (name value) in (session-headers ua request sdp)
            do (add-header request name value))
      (setf (call-creq call)
            (ua-send ua request
                     (lambda (creq event argument)
                       (call-uac-event call creq event argument))))
      call)))

(defun call-uac-event (call creq event argument)
  (unless (call-terminated-p call)
    (setf (call-invite call) (creq-request creq))
    (case event
      (:response
       (let ((status (response-status argument)))
         (cond ((< status 200) (call-uac-provisional call argument))
               ((< status 300) (call-uac-2xx call argument))
               (t (call-uac-failure call argument)))))
      (:timeout (call-drop-early-dialog call) (call-terminate call :timeout))
      (:transport-error (call-drop-early-dialog call) (call-terminate call :transport-error)))))

(defun call-drop-early-dialog (call)
  (let ((dialog (call-dialog call)))
    (when (and dialog (eq (dialog-state dialog) :early))
      (ua-unregister-dialog (call-ua call) dialog)
      (setf (call-dialog call) nil))))

(defun call-adopt-dialog (call dialog)
  (when (call-dialog call)
    (ua-unregister-dialog (call-ua call) (call-dialog call)))
  (setf (call-dialog call) dialog)
  (ua-register-dialog (call-ua call) dialog call))

(defun call-uac-provisional (call response)
  (let ((status (response-status response))
        (tag (msg-to-tag response)))
    (setf (call-got-provisional call) t)
    (when (and tag (> status 100)
               (or (null (call-dialog call))
                   (not (equal tag (dialog-remote-tag (call-dialog call))))))
      (call-adopt-dialog call (make-uac-dialog (call-invite call) response)))
    (cond ((call-cancel-requested call)
           (call-send-cancel call))
          ((= status 100))
          ((= status 180)
           (setf (call-state call) :ringing)
           (call-fire call :ringing :response response))
          (t
           (when (eq (call-state call) :calling)
             (setf (call-state call) :proceeding))
           (call-fire call :progress :status status :sdp (sdp-body response)
                                     :response response)))))

(defun call-send-ack (call dialog invite body)
  "Section 13.2.2.4: the UA core, not the transaction, acknowledges a 2xx.
Returns (request transport host port) for retransmission."
  (let ((ack (dialog-make-request dialog "ACK"
                                  :cseq (cseq-number (msg-cseq invite))
                                  :body body)))
    (dolist (name '("Authorization" "Proxy-Authorization"))
      (dolist (value (header-values invite name))
        (add-header ack name value)))
    (when body (add-header ack "Content-Type" "application/sdp"))
    (handler-case
        (multiple-value-bind (transport host port)
            (stack-send-stateless (call-stack call) ack)
          (list ack transport host port))
      (transport-error () nil))))

(defun call-resend-ack (call)
  (when (call-ack call)
    (destructuring-bind (ack transport host port) (call-ack call)
      (ignore-errors (stack-send-message (call-stack call) ack transport host port)))))

(defun call-uac-2xx (call response)
  (let ((dialog (call-dialog call))
        (invite (call-invite call))
        (tag (msg-to-tag response)))
    (cond
      ;; The 2xx we already acknowledged, retransmitted.
      ((and dialog (eq (dialog-state dialog) :confirmed)
            (equal tag (dialog-remote-tag dialog)))
       (call-resend-ack call))
      ;; A 2xx from another branch of a forked INVITE: acknowledge it and
      ;; release that dialog straight away.
      ((and dialog (eq (dialog-state dialog) :confirmed))
       (let ((fork (make-uac-dialog invite response)))
         (call-send-ack call fork invite nil)
         (ignore-errors
          (send-request (call-stack call)
                        (dialog-make-request
                         fork "BYE"
                         :headers `(("User-Agent" ,(ua-user-agent-name (call-ua call)))))
                        nil))))
      (t
       (call-adopt-dialog call (make-uac-dialog invite response))
       (let ((offer-or-answer (sdp-body response)))
         (when offer-or-answer
           (setf (call-remote-sdp call) offer-or-answer))
         (setf (call-ack call)
               (call-send-ack call (call-dialog call) invite
                              (and (null (message-body invite))
                                   offer-or-answer
                                   (call-answer-function call)
                                   (setf (call-local-sdp call)
                                         (funcall (call-answer-function call)
                                                  offer-or-answer))))))
       (cond ((or (call-cancel-requested call) (eq (call-state call) :terminating))
              ;; The CANCEL lost the race with the 2xx (RFC 5407 3.1.2): the
              ;; session exists, so end it with BYE.
              (setf (call-state call) :established)
              (call-send-bye call :cancelled))
             (t
              (setf (call-state call) :established)
              (call-fire call :established :sdp (call-remote-sdp call)
                                           :response response)))))))

(defun call-handle-2xx-retransmission (call response)
  "A 2xx arriving after the INVITE client transaction has gone."
  (unless (call-terminated-p call)
    (when (and (call-ack call)
               (= (cseq-number (msg-cseq response))
                  (cseq-number (msg-cseq (first (call-ack call))))))
      (call-resend-ack call))))

(defun call-uac-failure (call response)
  (let ((status (response-status response)))
    (call-drop-early-dialog call)
    (setf (call-status call) status)
    (if (and (= status 487)
             (or (call-cancel-requested call) (eq (call-state call) :terminating)))
        (call-terminate call :cancelled :status status :response response)
        (call-terminate call :rejected :status status :response response))))

;;; CANCEL (section 9.1)

(defun call-send-cancel (call)
  (unless (call-cancel-sent call)
    (setf (call-cancel-sent call) t)
    (let* ((invite (call-invite call))
           (cancel (make-request "CANCEL" (request-uri invite))))
      (dolist (route (header-values invite "Route"))
        (add-header cancel "Route" route))
      (add-header cancel "Max-Forwards" "70")
      (add-header cancel "From" (header invite "From"))
      (add-header cancel "To" (header invite "To"))
      (add-header cancel "Call-ID" (header invite "Call-ID"))
      (add-header cancel "CSeq" (make-cseq :number (cseq-number (msg-cseq invite))
                                           :method "CANCEL"))
      (dolist (name '("Authorization" "Proxy-Authorization"))
        (dolist (value (header-values invite name))
          (add-header cancel name value)))
      (ignore-errors
       (send-request (call-stack call) cancel nil
                     :branch (via-branch (msg-top-via invite))))
      ;; Section 9.1: if no final response to the INVITE turns up within
      ;; 64*T1, consider the original transaction cancelled.
      (schedule (call-scheduler call) (* 64 (stack-t1 (call-stack call)))
                (lambda ()
                  (when (eq (call-state call) :terminating)
                    (call-drop-early-dialog call)
                    (call-terminate call :cancelled)))))))

;;;; UAS side (section 13.3)

(defun call-handle-new-invite (ua request tx)
  (cond
    ((and (message-body request)
          (not (equal (msg-content-type request) "application/sdp")))
     (ua-respond ua tx request 415 :headers '(("Accept" "application/sdp"))))
    (t
     (let ((call (make-instance 'call :ua ua :direction :incoming :state :incoming
                                      :invite request :server-tx tx
                                      :local-tag (make-tag)
                                      :remote-sdp (sdp-body request))))
       (setf (gethash tx (ua-incoming-invites ua)) call
             (transaction-tu tx)
             (lambda (tx event argument)
               (declare (ignore tx))
               (when (eq event :ack) (call-handle-ack call argument))))
       (if (ua-on-incoming-call ua)
           (funcall (ua-on-incoming-call ua) call)
           (reject call 480))
       call))))

(defun call-ensure-uas-dialog (call state)
  (let ((dialog (call-dialog call)))
    (cond (dialog (setf (dialog-state dialog) state))
          (t (setf dialog (make-uas-dialog (call-invite call) (call-local-tag call)
                                           :state state))
             (call-adopt-dialog call dialog)))
    dialog))

(defun record-route-headers (request)
  "Section 12.1.1: Record-Route values are copied into dialog-creating
responses."
  (loop for value in (header-values request "Record-Route")
        collect (list "Record-Route" value)))

(defun ring (call &key (status 180) sdp)
  "Send a provisional response (180 Ringing by default) for an incoming call."
  (on-loop ((call-scheduler call))
    (when (and (eq (call-direction call) :incoming)
               (member (call-state call) '(:incoming :ringing)))
      (call-ensure-uas-dialog call :early)
      (setf (call-state call) :ringing)
      (when sdp (setf (call-local-sdp call) sdp))
      (ua-respond (call-ua call) (call-server-tx call) (call-invite call) status
                  :to-tag (call-local-tag call)
                  :headers (append (record-route-headers (call-invite call))
                                   (session-headers (call-ua call) (call-invite call) sdp))
                  :body sdp))
    call))

(defun call-send-2xx (call tx request sdp)
  "Send the 2xx and keep retransmitting it until the ACK arrives."
  (let* ((ua (call-ua call))
         (stack (ua-stack ua))
         (scheduler (ua-scheduler ua))
         (response (ua-respond ua tx request 200
                               :to-tag (call-local-tag call)
                               :headers (append (record-route-headers request)
                                                (session-headers ua request sdp))
                               :body sdp))
         (interval (stack-t1 stack))
         (deadline (+ (now scheduler) (* 64 (stack-t1 stack)))))
    (cancel-timer (call-pending-2xx-timer call))
    (setf (call-pending-2xx call) response)
    (labels ((retransmit ()
               (when (eq (call-pending-2xx call) response)
                 (cond ((>= (now scheduler) deadline)
                        ;; No ACK: the session is unusable (section 13.3.1.4).
                        (setf (call-pending-2xx call) nil)
                        (call-send-bye call :no-ack))
                       (t
                        (send-response tx response)
                        (setf interval (min (* 2 interval) (stack-t2 stack)))
                        (arm)))))
             (arm ()
               (setf (call-pending-2xx-timer call)
                     (schedule scheduler
                               (min interval (max 0 (- deadline (now scheduler))))
                               #'retransmit))))
      (arm))
    response))

(defun answer (call &key sdp)
  "Accept an incoming call, or a pending re-INVITE, with the session
description SDP.  For a re-INVITE, SDP defaults to the current local
description."
  (on-loop ((call-scheduler call))
    (cond
      ((call-reinvite-tx call)
       (let ((tx (call-reinvite-tx call))
             (request (call-reinvite-request call))
             (sdp (or sdp (call-local-sdp call))))
         (setf (call-reinvite-tx call) nil
               (call-reinvite-request call) nil
               (call-local-sdp call) sdp)
         (when (sdp-body request)
           (setf (call-remote-sdp call) (sdp-body request)))
         (call-send-2xx call tx request sdp)))
      ((and (eq (call-direction call) :incoming)
            (member (call-state call) '(:incoming :ringing)))
       (call-ensure-uas-dialog call :confirmed)
       (setf (call-state call) :answered
             (call-local-sdp call) sdp)
       (remhash (call-server-tx call) (ua-incoming-invites (call-ua call)))
       (call-send-2xx call (call-server-tx call) (call-invite call) sdp)))
    call))

(defun reject (call &optional status headers)
  "Refuse an incoming call (486 Busy Here by default) or a pending re-INVITE
(488 Not Acceptable Here by default).  HEADERS, a list of (name value), go
into the response: a challenge with 401, a Reason, a Retry-After."
  (on-loop ((call-scheduler call))
    (cond
      ((call-reinvite-tx call)
       (let ((tx (call-reinvite-tx call))
             (request (call-reinvite-request call)))
         (setf (call-reinvite-tx call) nil
               (call-reinvite-request call) nil)
         (ua-respond (call-ua call) tx request (or status 488) :headers headers)))
      ((and (eq (call-direction call) :incoming)
            (member (call-state call) '(:incoming :ringing)))
       (ua-respond (call-ua call) (call-server-tx call) (call-invite call)
                   (or status 486) :to-tag (call-local-tag call) :headers headers)
       (call-terminate call :local-reject :status (or status 486))))
    call))

(defun call-handle-ack (call request)
  (when (call-pending-2xx call)
    (setf (call-pending-2xx call) nil)
    (cancel-timer (call-pending-2xx-timer call))
    (when (sdp-body request)
      ;; INVITE without offer: the answer arrives in the ACK.
      (setf (call-remote-sdp call) (sdp-body request)))
    (when (eq (call-state call) :answered)
      (setf (call-state call) :established)
      (call-fire call :established :sdp (call-remote-sdp call)))))

(defun call-cancelled (call)
  "The caller cancelled before we answered (section 9.2)."
  (when (member (call-state call) '(:incoming :ringing))
    (ua-respond (call-ua call) (call-server-tx call) (call-invite call) 487
                :to-tag (call-local-tag call))
    (call-terminate call :remote-cancel)))

;;;; Requests inside the dialog

(defun call-make-request (call method &key headers body)
  (dialog-make-request (call-dialog call) method
                       :headers (append `(("User-Agent" ,(ua-user-agent-name (call-ua call))))
                                        headers)
                       :body body))

(defun call-handle-request (call request tx)
  (let ((ua (call-ua call))
        (dialog (call-dialog call))
        (method (request-method request)))
    (cond
      ((not (dialog-check-remote-cseq dialog request))
       (ua-respond ua tx request 500 :reason "CSeq Out Of Order"))
      ((string= method "BYE")
       (ua-respond ua tx request 200)
       ;; Section 15.1.2: pending requests in the dialog are answered 487.
       (when (call-reinvite-tx call)
         (ua-respond ua (call-reinvite-tx call) (call-reinvite-request call) 487)
         (setf (call-reinvite-tx call) nil))
       (when (member (call-state call) '(:incoming :ringing))
         (ua-respond ua (call-server-tx call) (call-invite call) 487
                     :to-tag (call-local-tag call)))
       (call-terminate call :remote-hangup))
      ((string= method "INVITE")
       (call-handle-reinvite call request tx))
      ((string= method "OPTIONS")
       (ua-handle-options ua request tx))
      (t
       (ua-respond ua tx request 501)))))

;;; BYE (section 15)

(defun call-send-bye (call cause)
  (setf (call-state call) :terminating
        (call-pending-2xx call) nil)
  (cancel-timer (call-pending-2xx-timer call))
  (ua-send (call-ua call) (call-make-request call "BYE")
           (lambda (creq event argument)
             (declare (ignore creq))
             (when (or (not (eq event :response))
                       (>= (response-status argument) 200))
               (call-terminate call cause)))
           :dialog (call-dialog call)))

(defun hangup (call)
  "End CALL by whatever the protocol requires in its current state: CANCEL
for an outgoing call not yet answered, a rejection for an incoming one, BYE
for an established session."
  (on-loop ((call-scheduler call))
    (case (call-state call)
      ((:calling :proceeding)
       (setf (call-cancel-requested call) t
             (call-state call) :terminating)
       ;; A CANCEL may only follow a provisional response (section 9.1).
       (when (call-got-provisional call)
         (call-send-cancel call)))
      (:ringing
       (cond ((eq (call-direction call) :outgoing)
              (setf (call-cancel-requested call) t
                    (call-state call) :terminating)
              (call-send-cancel call))
             (t (reject call 486))))
      (:incoming (reject call 486))
      ((:answered :established)
       (call-send-bye call :local-hangup)))
    call))

;;; re-INVITE (section 14)

(defun call-handle-reinvite (call request tx)
  (let ((ua (call-ua call)))
    (cond
      ((call-reinvite-creq call)
       ;; Glare: we have our own re-INVITE outstanding (section 14.2).
       (ua-respond ua tx request 491))
      ((or (call-reinvite-tx call) (not (eq (call-state call) :established)))
       (ua-respond ua tx request 500
                   :headers `(("Retry-After" ,(random 10)))))
      (t
       (dialog-refresh-target (call-dialog call) request)
       (setf (call-reinvite-tx call) tx
             (call-reinvite-request call) request)
       (call-fire call :reinvite :sdp (sdp-body request) :request request)
       (when (and (eq (call-reinvite-tx call) tx)
                  (call-auto-answer-reinvite call))
         (answer call))))))

(defun reinvite (call &key sdp)
  "Offer a changed session description inside an established call.  The
outcome arrives as a :reinvite-accepted or :reinvite-rejected event."
  (on-loop ((call-scheduler call))
    (when (and (eq (call-state call) :established)
               (null (call-reinvite-creq call))
               (null (call-reinvite-tx call)))
      (let ((request (call-make-request call "INVITE" :body sdp)))
        (loop for (name value) in (session-headers (call-ua call) request sdp)
              do (add-header request name value))
        (setf (call-reinvite-creq call)
              (ua-send (call-ua call) request
                       (lambda (creq event argument)
                         (call-reinvite-event call creq sdp event argument))
                       :dialog (call-dialog call)))))
    call))

(defun call-reinvite-event (call creq sdp event argument)
  (let ((current (eq creq (call-reinvite-creq call)))
        (status (and (eq event :response) (response-status argument))))
    (cond
      ((call-terminated-p call))
      ((and status (< status 200)))
      ((and status (< status 300))
       (cond (current
              (setf (call-reinvite-creq call) nil)
              (dialog-refresh-target (call-dialog call) argument)
              (setf (call-ack call)
                    (call-send-ack call (call-dialog call) (creq-request creq) nil)
                    (call-local-sdp call) sdp)
              (when (sdp-body argument)
                (setf (call-remote-sdp call) (sdp-body argument)))
              (call-fire call :reinvite-accepted :sdp (call-remote-sdp call)
                                                 :response argument))
             (t (call-handle-2xx-retransmission call argument))))
      ((not current))
      ((eql status 491)
       ;; Section 14.1: retry after a random interval; the owner of the
       ;; Call-ID waits longer so the two sides do not collide again.
       (setf (call-reinvite-creq call) nil)
       (schedule (call-scheduler call)
                 (if (eq (call-direction call) :outgoing)
                     (+ 21/10 (random 1.9))
                     (random 2.0))
                 (lambda () (reinvite call :sdp sdp))))
      ((or (member status '(481 408)) (member event '(:timeout :transport-error)))
       ;; Section 14.1 / 12.2.1.2: the dialog is gone.
       (setf (call-reinvite-creq call) nil)
       (if (eq (call-state call) :established)
           (call-send-bye call :dialog-lost)
           (call-terminate call :dialog-lost)))
      (t
       ;; The session continues as it was.
       (setf (call-reinvite-creq call) nil)
       (call-fire call :reinvite-rejected :status status :response argument)))))
