(in-package :cl-sip)

;;;; REGISTER client (RFC 3261 section 10.2)

(defclass registration ()
  ((ua :initarg :ua :reader registration-ua)
   (registrar :initarg :registrar :reader registration-registrar)
   (contact :initarg :contact :reader registration-contact)
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

(defun registration-send (registration mode &key retried)
  "MODE is :register, :unregister, :unregister-all or :fetch."
  (let* ((ua (registration-ua registration))
         (expires (if (eq mode :register) (registration-requested-expires registration) 0))
         (request (ua-make-request ua "REGISTER" (registration-registrar registration)
                                   :to (strip-tag (ua-aor ua))
                                   :call-id (registration-call-id registration)
                                   :cseq (incf (registration-cseq registration)))))
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
             (cseq-number (msg-cseq (creq-request creq))))
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
         (registration-fire registration :registered :expires granted :bindings bindings)))
      ((:unregister :unregister-all)
       (setf (registration-expires registration) nil
             (registration-state registration) :unregistered)
       (registration-fire registration :unregistered :bindings bindings))
      (:fetch
       (registration-fire registration :bindings :bindings bindings)))))

(defun register (ua &key registrar (expires 3600) on-event (refresh t) contact)
  "Register the agent's contact with REGISTRAR (default: the domain of the
address-of-record).  Returns a REGISTRATION; progress is reported through
ON-EVENT.  With REFRESH, the binding is renewed at half its lifetime."
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
                            :call-id (make-call-id) :on-event on-event :refresh refresh)))
      (registration-send registration :register))))

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
   (min-expires :initarg :min-expires :initform 60 :accessor registrar-min-expires)
   (default-expires :initarg :default-expires :initform 3600
                    :accessor registrar-default-expires)
   (max-expires :initarg :max-expires :initform 7200 :accessor registrar-max-expires)
   (nonces :initform (make-hash-table :test #'equal) :reader registrar-nonces)))

(defun make-registrar (&rest args &key realm users algorithm min-expires
                                    default-expires max-expires)
  (declare (ignore realm users algorithm min-expires default-expires max-expires))
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

(defun registrar-challenge (registrar ua request tx &key stale)
  (let ((challenge (generate-challenge (registrar-realm registrar)
                                       :algorithm (registrar-algorithm registrar)
                                       :stale stale)))
    (setf (gethash (challenge-nonce challenge) (registrar-nonces registrar)) t)
    (ua-respond ua tx request 401
                :headers `(("WWW-Authenticate" ,(format-challenge challenge))))))

(defun registrar-authorized-p (registrar ua request tx)
  "True when REQUEST may proceed; otherwise a 401 has been sent."
  (or (null (registrar-users registrar))
      (let* ((credentials
               (find (registrar-realm registrar)
                     (loop for raw in (header-values request "Authorization")
                           for c = (ignore-errors (parse-credentials raw))
                           when c collect c)
                     :key #'credentials-realm :test #'equal))
             (password (and credentials
                            (cdr (assoc (credentials-username credentials)
                                        (registrar-users registrar) :test #'equal)))))
        (cond ((null credentials)
               (registrar-challenge registrar ua request tx) nil)
              ((not (gethash (credentials-nonce credentials) (registrar-nonces registrar)))
               (registrar-challenge registrar ua request tx :stale t) nil)
              ((and password
                    (verify-credentials credentials :password password
                                                    :method (request-method request)
                                                    :body (message-body request)))
               t)
              (t (registrar-challenge registrar ua request tx) nil)))))

(defun registrar-handle-register (registrar ua request tx)
  (when (registrar-authorized-p registrar ua request tx)
    (let* ((now (now (ua-scheduler ua)))
           (key (aor-key (name-addr-uri (msg-to request))))
           (current (registrar-live-bindings registrar key now))
           (contacts (msg-contacts request))
           (header-expires (msg-expires request))
           (call-id (msg-call-id request))
           (cseq (cseq-number (msg-cseq request))))
      (flet ((fail (status &rest headers)
               (return-from registrar-handle-register
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
        (setf (gethash key (registrar-bindings registrar)) current)
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
