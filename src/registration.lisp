(in-package :cl-sip)

;;;; REGISTER client (RFC 3261 section 10.2)

(defclass registration ()
  ((ua :initarg :ua :reader registration-ua)
   (registrar :initarg :registrar :reader registration-registrar)
   (contact :initarg :contact :accessor registration-contact)
   (explicit-contact :initarg :explicit-contact :initform nil
                     :reader registration-explicit-contact-p)
   (nat :initarg :nat :initform t :reader registration-nat-p
        :documentation "Learn the public address from the registrar's replies
and register that instead of the local one.")
   (keepalive :initarg :keepalive :initform nil :reader registration-keepalive
              :documentation "Seconds between keep-alive packets to the
registrar, which hold a NAT mapping open; NIL sends none.")
   (keepalive-timer :initform nil :accessor registration-keepalive-timer)
   (transaction :initform nil :accessor registration-transaction
                :documentation "The client transaction of the last REGISTER.")
   (requested-expires :initarg :expires :accessor registration-requested-expires)
   (expires :initform nil :accessor registration-expires
            :documentation "Lifetime granted by the registrar, in seconds.")
   (bindings :initform '() :accessor registration-bindings
             :documentation "Contacts (NAME-ADDRs) the registrar reported.")
   (call-id :initarg :call-id :reader registration-call-id)
   (cseq :initform 0 :accessor registration-cseq)
   (state :initform :idle :accessor registration-state
          :documentation ":idle, :registering, :registered, :unregistered or :failed.")
   (last-response :initform nil :accessor registration-last-response)
   (refresh :initarg :refresh :initform t :accessor registration-refresh-p)
   (refresh-timer :initform nil :accessor registration-refresh-timer)
   (on-event :initarg :on-event :initform nil :accessor registration-on-event
             :documentation "(lambda (registration event &key ...)); events are
:registered (:expires :bindings), :unregistered, :bindings (:bindings) and
:failed (:status :response :reason).")))

(defmethod print-object ((r registration) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~a ~a" (registration-state r)
            (uri-string (registration-registrar r)))))

(defun registration-fire (registration event &rest args)
  (when (registration-on-event registration)
    (apply (registration-on-event registration) registration event args)))

(defun contact-expires (contact)
  (let ((value (param (name-addr-params contact) "expires")))
    (and value (all-digits-p value) (parse-integer value))))

(defun registration-send (registration mode &key retried remove)
  "MODE is :register, :unregister, :unregister-all or :fetch.  REMOVE is a
contact to delete in the same request."
  (let* ((ua (registration-ua registration))
         (expires (if (eq mode :register) (registration-requested-expires registration) 0))
         (request (ua-make-request ua "REGISTER" (registration-registrar registration)
                                   :to (strip-tag (ua-aor ua))
                                   :call-id (registration-call-id registration)
                                   :cseq (incf (registration-cseq registration)))))
    (unless (registration-explicit-contact-p registration)
      ;; The right contact depends on where the registrar is and on what has
      ;; been learned about our public address since the last attempt.
      (setf (registration-contact registration) (ua-contact-for ua request)))
    (when remove
      (let ((old (copy-name-addr remove)))
        (setf (name-addr-params old) (set-param (name-addr-params old) "expires" "0"))
        (add-header request "Contact" old)))
    (ecase mode
      ((:register :unregister)
       (let ((contact (copy-name-addr (registration-contact registration))))
         (setf (name-addr-params contact)
               (set-param (name-addr-params contact) "expires" (princ-to-string expires)))
         (add-header request "Contact" contact)))
      (:unregister-all
       (add-header request "Contact" "*")
       (add-header request "Expires" "0"))
      (:fetch))
    (unless (eq mode :fetch)
      (cancel-timer (registration-refresh-timer registration))
      (setf (registration-state registration) :registering))
    (ua-send
     ua request
     (lambda (creq event argument)
       ;; An authentication retry consumed a sequence number.
       (setf (registration-cseq registration)
             (cseq-number (msg-cseq (creq-request creq)))
             (registration-transaction registration) (creq-transaction creq))
       (case event
         (:response
          (let ((status (response-status argument)))
            (setf (registration-last-response registration) argument)
            (cond
              ((< status 200))
              ((< status 300)
               (registration-succeeded registration mode argument))
              ((and (= status 423) (eq mode :register) (not retried)
                    (msg-integer-header argument "Min-Expires"))
               ;; Section 10.2.8: retry with the interval the registrar asks for.
               (setf (registration-requested-expires registration)
                     (msg-integer-header argument "Min-Expires"))
               (registration-send registration mode :retried t))
              (t
               (setf (registration-state registration) :failed)
               (registration-fire registration :failed :status status
                                                        :response argument)))))
         (t
          (setf (registration-state registration) :failed)
          (registration-fire registration :failed :reason event)))))
    registration))

(defun registration-succeeded (registration mode response)
  (let* ((ua (registration-ua registration))
         (bindings (remove :star (msg-contacts response)))
         (mine (find (name-addr-uri (registration-contact registration)) bindings
                     :key #'name-addr-uri :test #'uri-equal)))
    (setf (registration-bindings registration) bindings)
    (when (and (eq mode :register) (registration-learn-public-address registration response))
      ;; We registered an address the registrar cannot reach us at: replace it.
      (return-from registration-succeeded
        (registration-send registration :register
                           :remove (registration-contact registration))))
    (ecase mode
      (:register
       (let ((granted (or (and mine (contact-expires mine))
                          (msg-expires response)
                          (registration-requested-expires registration))))
         (setf (registration-expires registration) granted
               (registration-state registration) :registered)
         (when (and (registration-refresh-p registration) (plusp granted))
           (setf (registration-refresh-timer registration)
                 (schedule (ua-scheduler ua) (/ granted 2)
                           (lambda ()
                             (when (eq (registration-state registration) :registered)
                               (registration-send registration :register))))))
         (registration-arm-keepalive registration)
         (registration-fire registration :registered :expires granted :bindings bindings)))
      ((:unregister :unregister-all)
       (cancel-timer (registration-keepalive-timer registration))
       (setf (registration-expires registration) nil
             (registration-state registration) :unregistered)
       (registration-fire registration :unregistered :bindings bindings))
      (:fetch
       (registration-fire registration :bindings :bindings bindings)))))

(defun registration-learn-public-address (registration response)
  "RFC 3581: the registrar reports the address our request came from in the
Via's `received` and `rport`.  When that differs from what the request said,
we are behind a NAT; record the public address on the transport.  Returns
true when the public address changed."
  (let* ((transaction (registration-transaction registration))
         (transport (and transaction (transaction-transport transaction)))
         (via (msg-top-via response))
         (received (via-param via "received"))
         (rport (let ((value (via-param via "rport")))
                  (and value (all-digits-p value) (parse-integer value)))))
    (when (and (registration-nat-p registration)
               (not (registration-explicit-contact-p registration))
               transport received rport
               (or (not (host-equal received (via-host via)))
                   (/= rport (or (via-port via) 5060)))
               (not (and (equal received (transport-public-host transport))
                         (eql rport (transport-public-port transport)))))
      (setf (transport-public-host transport) received
            (transport-public-port transport) rport)
      t)))

(defun registration-arm-keepalive (registration)
  "Send a double CRLF to the registrar at intervals (the keep-alive of RFC
5626 section 3.5.1, used here over UDP as well, as is common practice)."
  (cancel-timer (registration-keepalive-timer registration))
  (let ((interval (registration-keepalive registration))
        (transaction (registration-transaction registration)))
    (when (and interval transaction)
      (labels ((ping ()
                 (when (eq (registration-state registration) :registered)
                   (ignore-errors
                    (transport-send (transaction-transport transaction)
                                    (coerce #(13 10 13 10) 'octets)
                                    (transaction-host transaction)
                                    (transaction-port transaction)
                                    nil))
                   (arm)))
               (arm ()
                 (setf (registration-keepalive-timer registration)
                       (schedule (ua-scheduler (registration-ua registration))
                                 interval #'ping))))
        (arm)))))

(defun register (ua &key registrar (expires 3600) on-event (refresh t) contact
                      (nat t) keepalive)
  "Register the agent's contact with REGISTRAR (default: the domain of the
address-of-record).  Returns a REGISTRATION; progress is reported through
ON-EVENT.  With REFRESH, the binding is renewed at half its lifetime.

Unless NAT is NIL or CONTACT is given, a registrar that reports seeing the
request from another address (the agent is behind a NAT) is sent a second
REGISTER with that public address.  KEEPALIVE is the number of seconds
between keep-alive packets to the registrar."
  (on-loop ((ua-scheduler ua))
    (let* ((aor (name-addr-uri (ua-aor ua)))
           (registrar (if registrar
                          (ensure-uri registrar)
                          (make-sip-uri :scheme (sip-uri-scheme aor)
                                        :host (sip-uri-host aor)
                                        :port (sip-uri-port aor))))
           (registration
             (make-instance 'registration
                            :ua ua :registrar registrar :expires expires
                            :contact (or (and contact (ensure-name-addr contact))
                                         (ua-contact ua))
                            :explicit-contact (and contact t)
                            :nat nat :keepalive keepalive
                            :call-id (make-call-id) :on-event on-event :refresh refresh)))
      (registration-send registration :register))))

(defun refresh-registration (registration)
  "Send REGISTER again now, for instance to retry after a failure."
  (on-loop ((ua-scheduler (registration-ua registration)))
    (registration-send registration :register)))

(defun unregister (registration &key all)
  "Remove this agent's binding, or with ALL every binding of the
address-of-record (Contact: *)."
  (on-loop ((ua-scheduler (registration-ua registration)))
    (registration-send registration (if all :unregister-all :unregister))))

(defun fetch-bindings (registration)
  "Ask the registrar for the current bindings (section 10.2.3); the answer
arrives as a :bindings event."
  (on-loop ((ua-scheduler (registration-ua registration)))
    (registration-send registration :fetch)))

;;;; A small in-memory registrar (section 10.3)

(defstruct binding contact expires-at call-id cseq)

(defclass registrar ()
  ((bindings :initform (make-hash-table :test #'equal) :reader registrar-bindings)
   (realm :initarg :realm :initform "cl-sip" :accessor registrar-realm)
   (users :initarg :users :initform nil :accessor registrar-users
          :documentation "Alist (username . password).  NIL disables authentication.")
   (algorithm :initarg :algorithm :initform "MD5" :accessor registrar-algorithm)
   (strict :initarg :strict :initform nil :accessor registrar-strict
           :documentation "When true, a user may only register its own name: the
user part of the To header must be the authenticated user name (section 10.3,
step 4).")
   (min-expires :initarg :min-expires :initform 60 :accessor registrar-min-expires)
   (default-expires :initarg :default-expires :initform 3600
                    :accessor registrar-default-expires)
   (max-expires :initarg :max-expires :initform 7200 :accessor registrar-max-expires)
   (on-change :initarg :on-change :initform nil :accessor registrar-on-change
              :documentation "(lambda (user contacts)) called when the addresses
registered for USER change; CONTACTS are the URIs now registered.")
   (nonces :initform (make-hash-table :test #'equal) :reader registrar-nonces)))

(defun make-registrar (&rest args &key realm users algorithm min-expires
                                    default-expires max-expires strict on-change)
  (declare (ignore realm users algorithm min-expires default-expires max-expires strict
                   on-change))
  (apply #'make-instance 'registrar args))

(defun aor-key (uri)
  (format nil "~(~a~)@~(~a~)" (or (sip-uri-user uri) "") (sip-uri-host uri)))

(defun registrar-live-bindings (registrar key now)
  (setf (gethash key (registrar-bindings registrar))
        (remove-if (lambda (b) (<= (binding-expires-at b) now))
                   (gethash key (registrar-bindings registrar)))))

(defun registrar-lookup (registrar ua aor)
  "Contact URIs currently registered for AOR."
  (mapcar (lambda (b) (name-addr-uri (binding-contact b)))
          (registrar-live-bindings registrar (aor-key (ensure-uri aor))
                                   (now (ua-scheduler ua)))))

(defun registrar-lookup-user (registrar ua user)
  "Contact URIs registered for USER under any domain: a phone may name the
server by address or by any of its host names."
  (let ((prefix (format nil "~(~a~)@" user))
        (now (now (ua-scheduler ua)))
        (contacts '()))
    (loop for key in (alexandria:hash-table-keys (registrar-bindings registrar))
          when (alexandria:starts-with-subseq prefix key)
            do (dolist (b (registrar-live-bindings registrar key now))
                 (pushnew (name-addr-uri (binding-contact b)) contacts :test #'uri-equal)))
    (nreverse contacts)))

(defun registrar-make-challenge (registrar &key stale)
  "A fresh digest challenge, as a WWW-Authenticate header value."
  (let ((challenge (generate-challenge (registrar-realm registrar)
                                       :algorithm (registrar-algorithm registrar)
                                       :stale stale)))
    (setf (gethash (challenge-nonce challenge) (registrar-nonces registrar)) t)
    (format-challenge challenge)))

(defun registrar-authenticate (registrar request)
  "The user name REQUEST proves it holds the password of, by digest
credentials for this registrar's realm; or NIL and why: :MISSING, :STALE
(the nonce is not one of ours any more) or :BAD."
  (let* ((credentials
           (find (registrar-realm registrar)
                 (loop for raw in (header-values request "Authorization")
                       for c = (ignore-errors (parse-credentials raw))
                       when c collect c)
                 :key #'credentials-realm :test #'equal))
         (password (and credentials
                        (cdr (assoc (credentials-username credentials)
                                    (registrar-users registrar) :test #'equal)))))
    (cond ((null credentials) (values nil :missing))
          ((not (gethash (credentials-nonce credentials) (registrar-nonces registrar)))
           (values nil :stale))
          ((and password
                (verify-credentials credentials :password password
                                                :method (request-method request)
                                                :body (message-body request)))
           (credentials-username credentials))
          (t (values nil :bad)))))

(defun registrar-authorized-p (registrar ua request tx)
  "True (the user name, when there are users) when REQUEST may proceed;
otherwise a 401 has been sent."
  (or (null (registrar-users registrar))
      (multiple-value-bind (user problem) (registrar-authenticate registrar request)
        (or user
            (progn
              (ua-respond ua tx request 401
                          :headers `(("WWW-Authenticate"
                                      ,(registrar-make-challenge
                                        registrar :stale (eq problem :stale)))))
              nil)))))

(defun registrar-handle-register (registrar ua request tx)
  (let ((user (registrar-authorized-p registrar ua request tx)))
    (cond ((null user))
          ((and (registrar-strict registrar) (stringp user)
                (not (equalp user (sip-uri-user (name-addr-uri (msg-to request))))))
           (ua-respond ua tx request 403))
          (t (registrar-update-bindings registrar ua request tx)))))

(defun registrar-update-bindings (registrar ua request tx)
  (progn
    (let* ((now (now (ua-scheduler ua)))
           (key (aor-key (name-addr-uri (msg-to request))))
           (current (registrar-live-bindings registrar key now))
           (contacts (msg-contacts request))
           (header-expires (msg-expires request))
           (call-id (msg-call-id request))
           (cseq (cseq-number (msg-cseq request))))
      (flet ((fail (status &rest headers)
               (return-from registrar-update-bindings
                 (ua-respond ua tx request status :headers headers)))
             (out-of-order-p (binding)
               (and (equal (binding-call-id binding) call-id)
                    (>= (binding-cseq binding) cseq))))
        (cond
          ((member :star contacts)
           (unless (and (null (rest contacts)) (eql header-expires 0))
             (fail 400))
           (when (some #'out-of-order-p current) (fail 500))
           (setf current '()))
          (t
           ;; First pass: validate everything, so a failure changes nothing.
           (let ((updates
                   (loop for contact in contacts
                         for expires = (or (contact-expires contact) header-expires
                                           (registrar-default-expires registrar))
                         for existing = (find (name-addr-uri contact) current
                                              :key (lambda (b) (name-addr-uri (binding-contact b)))
                                              :test #'uri-equal)
                         do (when (and existing (out-of-order-p existing)) (fail 500))
                            (when (and (plusp expires)
                                       (< expires (registrar-min-expires registrar)))
                              (fail 423 (list "Min-Expires"
                                              (registrar-min-expires registrar))))
                         collect (list contact
                                       (min expires (registrar-max-expires registrar))
                                       existing))))
             (loop for (contact expires existing) in updates
                   do (setf current (remove existing current))
                      (when (plusp expires)
                        (setf current
                              (append current
                                      (list (make-binding
                                             :contact (let ((c (copy-name-addr contact)))
                                                        (setf (name-addr-params c)
                                                              (remove-param (name-addr-params c)
                                                                            "expires"))
                                                        c)
                                             :expires-at (+ now expires)
                                             :call-id call-id :cseq cseq)))))))))
        (let ((before (mapcar (lambda (b) (uri-string (name-addr-uri (binding-contact b))))
                              (gethash key (registrar-bindings registrar))))
              (after (mapcar (lambda (b) (uri-string (name-addr-uri (binding-contact b))))
                             current)))
          (setf (gethash key (registrar-bindings registrar)) current)
          (when (and (registrar-on-change registrar)
                     (set-exclusive-or before after :test #'string=))
            (ignore-errors
             (funcall (registrar-on-change registrar)
                      (sip-uri-user (name-addr-uri (msg-to request))) after))))
        (ua-respond
         ua tx request 200
         :headers (append
                   (loop for b in current
                         collect (let ((c (copy-name-addr (binding-contact b))))
                                   (setf (name-addr-params c)
                                         (set-param (name-addr-params c) "expires"
                                                    (princ-to-string
                                                     (ceiling (- (binding-expires-at b) now)))))
                                   (list "Contact" c)))
                   (list (list "Date" (format-sip-date)))))))))
