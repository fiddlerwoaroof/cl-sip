(in-package :cl-sip)

;;;; User agent core (RFC 3261 section 8)

(defclass user-agent ()
  ((stack :initarg :stack :reader ua-stack)
   (aor :initarg :aor :accessor ua-aor
        :documentation "Address-of-record as a NAME-ADDR (display name + URI).")
   (credentials :initarg :credentials :initform '() :accessor ua-credentials
                :documentation "List of (realm username password); a realm of
NIL matches any realm.")
   (outbound-proxy :initarg :outbound-proxy :initform nil :accessor ua-outbound-proxy)
   (user-agent-name :initarg :user-agent-name :initform "cl-sip/0.1"
                    :accessor ua-user-agent-name)
   (methods :initform (list "INVITE" "ACK" "BYE" "CANCEL" "OPTIONS" "UPDATE")
            :accessor ua-methods)
   (dialog-owners :initform (make-hash-table :test #'equal) :reader ua-dialog-owners
                  :documentation "Dialog id -> the call that owns the dialog.")
   (incoming-invites :initform (make-hash-table :test #'eq) :reader ua-incoming-invites
                     :documentation "INVITE server transaction -> call, until answered.")
   (initial-requests :initform (make-hash-table :test #'equal) :reader ua-initial-requests
                     :documentation "For merged-request detection (section 8.2.2.2).")
   (nonce-counts :initform (make-hash-table :test #'equal) :reader ua-nonce-counts)
   (on-incoming-call :initarg :on-incoming-call :initform nil
                     :accessor ua-on-incoming-call
                     :documentation "(lambda (call)) called for each new INVITE.")
   (registrar :initarg :registrar :initform nil :accessor ua-registrar)
   (request-filter :initarg :request-filter :initform nil :accessor ua-request-filter
                   :documentation "(lambda (request)) consulted before anything
else for each new request: true accepts it, a status code refuses it with that
response, NIL drops it without answering.")
   (media :initarg :media :initform nil :accessor ua-media
          :documentation "Free for the application (e.g. SDP capabilities).")))

(defun make-transport-from-spec (spec)
  (etypecase spec
    (transport spec)
    (cons (ecase (first spec)
            (:udp (apply #'make-udp-transport (rest spec)))
            (:tcp (apply #'make-tcp-transport (rest spec)))))))

(defun make-user-agent (&key aor display-name credentials outbound-proxy
                          transports scheduler stack on-incoming-call registrar
                          request-filter (user-agent-name "cl-sip/0.1") trace)
  "Create a user agent.  AOR is the address-of-record (string or URI).
TRANSPORTS is a list of transport objects or specs such as
(:udp :host \"0.0.0.0\" :port 5060).  SCHEDULER lets several agents share one
event loop (the tests use a virtual one)."
  (let* ((stack (or stack (make-stack :scheduler scheduler :trace trace)))
         (aor (ensure-name-addr aor))
         (ua (make-instance 'user-agent
                            :stack stack
                            :aor (if display-name
                                     (make-name-addr :display-name display-name
                                                     :uri (name-addr-uri aor))
                                     aor)
                            :credentials credentials
                            :outbound-proxy (and outbound-proxy (ensure-uri outbound-proxy))
                            :on-incoming-call on-incoming-call
                            :registrar registrar
                            :request-filter request-filter
                            :user-agent-name user-agent-name)))
    (dolist (spec transports)
      (add-transport stack (make-transport-from-spec spec)))
    (setf (stack-request-handler stack)
          (lambda (request tx) (ua-handle-request ua request tx))
          (stack-ack-handler stack)
          (lambda (request) (ua-handle-ack ua request))
          (stack-stray-response-handler stack)
          (lambda (response) (ua-handle-stray-2xx ua response)))
    ua))

(defun ua-scheduler (ua)
  (stack-scheduler (ua-stack ua)))

(defun start (ua)
  "Open the transports and, unless the scheduler is virtual, start the event
loop thread."
  (let ((stack (ua-stack ua)))
    (mapc #'transport-start (stack-transports stack))
    (unless (scheduler-virtual-p (stack-scheduler stack))
      (start-scheduler (stack-scheduler stack))))
  ua)

(defun stop (ua)
  (let ((stack (ua-stack ua)))
    (unless (scheduler-virtual-p (stack-scheduler stack))
      (stop-scheduler (stack-scheduler stack)))
    (mapc #'transport-stop (stack-transports stack)))
  ua)

(defun ua-contact (ua &optional transport external)
  "Contact NAME-ADDR for this agent on TRANSPORT (default: the first).  With
EXTERNAL, the transport's public address is used when one is known."
  (let* ((transport (or transport (first (stack-transports (ua-stack ua)))))
         (aor (name-addr-uri (ua-aor ua)))
         (public (and external (transport-public-host transport))))
    (make-name-addr
     :uri (make-sip-uri :user (and (sip-uri-p aor) (sip-uri-user aor))
                        :host (or public (transport-advertised-host transport))
                        :port (or (and public (transport-public-port transport))
                                  (transport-port transport))
                        :params (unless (string= (transport-protocol transport) "UDP")
                                  (list (cons "transport"
                                              (string-downcase
                                               (transport-protocol transport)))))))))

(defun ua-contact-for (ua request)
  "Contact matching the transport REQUEST will use or arrived on.  A peer on
the public side of a NAT is given the transport's public address."
  (if (message-transport request)
      (ua-contact ua (message-transport request)
                  (not (private-address-p (message-remote-host request))))
      (multiple-value-bind (host port protocol)
          (ignore-errors (uri-destination (next-hop-uri request)))
        (declare (ignore port))
        (ua-contact ua
                    (ignore-errors (stack-find-transport (ua-stack ua) protocol))
                    (and host (not (private-address-p host)))))))

;;;; Building requests outside a dialog (section 8.1.1)

(defun ua-make-request (ua method target &key to from from-tag call-id cseq headers body)
  (let* ((target (ensure-uri target))
         (request (make-request method target)))
    (when (ua-outbound-proxy ua)
      (add-header request "Route"
                  (make-name-addr :uri (uri-with-param (ua-outbound-proxy ua) "lr"))))
    (add-header request "Max-Forwards" "70")
    ;; FROM is for an agent that calls on someone else's behalf, as a
    ;; back-to-back user agent does.
    (add-header request "From" (name-addr-with-tag (if from (ensure-name-addr from) (ua-aor ua))
                                                   (or from-tag (make-tag))))
    (add-header request "To" (if to (ensure-name-addr to) (make-name-addr :uri target)))
    (add-header request "Call-ID"
                (or call-id
                    (make-call-id (let ((transport (first (stack-transports (ua-stack ua)))))
                                    (and transport (transport-advertised-host transport))))))
    (add-header request "CSeq" (make-cseq :number (or cseq 1) :method method))
    (add-header request "User-Agent" (ua-user-agent-name ua))
    (loop for (name . value) in (normalize-headers headers)
          do (add-header request name value))
    (setf (message-body request) (ensure-octets body))
    request))

;;;; Sending requests with automatic digest authentication (section 22)

(defstruct (client-request (:conc-name creq-))
  ua request transaction callback dialog tu
  (auth-attempts 0))

(defun ua-credentials-for (ua realm)
  "Username and password for REALM, or NIL."
  (let ((entry (or (find realm (ua-credentials ua) :key #'first :test #'equal)
                   (find nil (ua-credentials ua) :key #'first))))
    (and entry (values (second entry) (third entry)))))

(defun ua-next-nonce-count (ua nonce)
  (incf (gethash nonce (ua-nonce-counts ua) 0)))

(defun ua-authenticate (ua creq response)
  "Answer the challenge in RESPONSE (401 or 407) by resending the request
with credentials.  Returns true when a new attempt was sent."
  (multiple-value-bind (challenge-header credentials-header)
      (if (= (response-status response) 401)
          (values "WWW-Authenticate" "Authorization")
          (values "Proxy-Authenticate" "Proxy-Authorization"))
    (let* ((request (creq-request creq))
           (challenge (select-challenge
                       (loop for raw in (header-values response challenge-header)
                             for c = (ignore-errors (parse-challenge raw))
                             when c collect c)))
           (realm (and challenge (challenge-realm challenge)))
           (previous (and challenge
                          (find realm (header-values request credentials-header)
                                :key (lambda (raw)
                                       (ignore-errors
                                        (credentials-realm (parse-credentials raw))))
                                :test #'equal))))
      (multiple-value-bind (username password)
          (and challenge (ua-credentials-for ua realm))
        (when (and challenge username
                   (< (creq-auth-attempts creq) 3)
                   ;; Credentials we already sent were refused: give up
                   ;; unless the server only says the nonce went stale.
                   (or (null previous) (challenge-stale challenge)))
          (let ((retry (copy-message request)))
            (remove-first-header retry "Via")
            (setf (header retry "CSeq")
                  (make-cseq :number (if (creq-dialog creq)
                                         (dialog-next-cseq (creq-dialog creq))
                                         (1+ (cseq-number (msg-cseq request))))
                             :method (request-method request)))
            (when previous
              (setf (message-headers retry)
                    (remove-if (lambda (cell)
                                 (and (string= (car cell) credentials-header)
                                      (string= (cdr cell) previous)))
                               (message-headers retry))))
            (add-header retry credentials-header
                        (format-credentials
                         (answer-challenge
                          challenge
                          :username username :password password
                          :method (request-method retry)
                          :uri (uri-string (request-uri retry))
                          :body (message-body retry)
                          :nonce-count (ua-next-nonce-count ua (challenge-nonce challenge)))))
            (incf (creq-auth-attempts creq))
            (setf (creq-request creq) retry
                  (creq-transaction creq) nil)
            (setf (creq-transaction creq)
                  (send-request (ua-stack ua) retry (creq-tu creq)))
            t))))))

(defun ua-send (ua request callback &key dialog branch)
  "Send REQUEST in a new client transaction.  CALLBACK is called as
(funcall callback client-request event argument) with EVENT :response (for
every response not consumed by an authentication retry), :timeout or
:transport-error.  Returns the CLIENT-REQUEST, whose request and transaction
slots always describe the latest attempt."
  (let ((creq (make-client-request :ua ua :request request :callback callback
                                   :dialog dialog)))
    (setf (creq-tu creq)
          (lambda (tx event argument)
            (when (or (null (creq-transaction creq)) (eq tx (creq-transaction creq)))
              (case event
                (:response
                 (unless (and (member (response-status argument) '(401 407))
                              (ua-authenticate ua creq argument))
                   (funcall callback creq :response argument)))
                ((:timeout :transport-error)
                 (funcall callback creq event argument))))))
    (handler-case
        (setf (creq-transaction creq)
              (send-request (ua-stack ua) request (creq-tu creq) :branch branch))
      (transport-error (e)
        ;; Could not even pick a destination; report asynchronously so the
        ;; caller always gets its client-request back first.
        (schedule (ua-scheduler ua) 0
                  (lambda () (funcall callback creq :transport-error e)))))
    creq))

;;;; Answering requests (section 8.2)

(defun ua-allowed-methods (ua)
  (if (ua-registrar ua)
      (append (ua-methods ua) (list "REGISTER"))
      (ua-methods ua)))

(defun ua-allow-header (ua)
  (format nil "~{~a~^, ~}" (ua-allowed-methods ua)))

(defun ua-respond (ua tx request status &key reason headers body (to-tag (make-tag)))
  "Send a response to REQUEST through its server transaction TX."
  (let ((response (make-response-for request status
                                     :reason reason
                                     :to-tag (and (> status 100) to-tag)
                                     :headers headers :body body)))
    (add-header response "Server" (ua-user-agent-name ua))
    (send-response tx response)
    response))

(defun dialog-id-of-request (request)
  "Dialog id as seen by the UAS: (call-id local-tag remote-tag)."
  (list (msg-call-id request) (msg-to-tag request) (msg-from-tag request)))

(defun ua-register-dialog (ua dialog owner)
  (setf (gethash (dialog-id dialog) (ua-dialog-owners ua)) owner))

(defun ua-unregister-dialog (ua dialog)
  (remhash (dialog-id dialog) (ua-dialog-owners ua)))

(defun ua-merged-request-p (ua request tx)
  "Section 8.2.2.2: a request without To tag that matches an ongoing
transaction's From tag, Call-ID and CSeq, but arrived by another path."
  (let* ((table (ua-initial-requests ua))
         (key (list (msg-from-tag request) (msg-call-id request)
                    (header request "CSeq")))
         (existing (gethash key table)))
    (cond ((and existing (not (transaction-terminated-p existing))) t)
          (t
           (when (> (hash-table-count table) 500)
             (maphash (lambda (k v) (when (transaction-terminated-p v) (remhash k table)))
                      table))
           (setf (gethash key table) tx)
           nil))))

(defun ua-handle-request (ua request tx)
  (let ((verdict (or (null (ua-request-filter ua))
                     (funcall (ua-request-filter ua) request))))
    (cond ((null verdict)
           ;; Unwanted source: say nothing, and forget the transaction.
           (tx-terminate tx))
          ((integerp verdict)
           (ua-respond ua tx request verdict))
          (t (ua-dispatch-request ua request tx)))))

(defun ua-dispatch-request (ua request tx)
  (let ((method (request-method request)))
    (cond
      ((string= method "CANCEL")
       (ua-handle-cancel ua request tx))
      ((not (member method (ua-allowed-methods ua) :test #'string=))
       (ua-respond ua tx request 405 :headers `(("Allow" ,(ua-allow-header ua)))))
      ((header-values request "Require")
       ;; No extensions are implemented, so every required one is unsupported.
       (ua-respond ua tx request 420
                   :headers `(("Unsupported"
                               ,(format nil "~{~a~^, ~}" (msg-option-tags request "Require"))))))
      ((not (sip-uri-p (request-uri request)))
       (ua-respond ua tx request 416))
      ((msg-to-tag request)
       (let ((owner (gethash (dialog-id-of-request request) (ua-dialog-owners ua))))
         (if owner
             (call-handle-request owner request tx)
             (ua-respond ua tx request 481))))
      ((ua-merged-request-p ua request tx)
       (ua-respond ua tx request 482))
      ((string= method "OPTIONS")
       (ua-handle-options ua request tx))
      ((string= method "INVITE")
       (call-handle-new-invite ua request tx))
      ((string= method "REGISTER")
       (registrar-handle-register (ua-registrar ua) ua request tx))
      (t
       ;; BYE (or anything else) outside a dialog.
       (ua-respond ua tx request 481)))))

(defun ua-handle-cancel (ua request tx)
  "Section 9.2."
  (let ((invite-tx (find-server-transaction (ua-stack ua) request "INVITE")))
    (cond ((null invite-tx)
           (ua-respond ua tx request 481))
          (t
           (ua-respond ua tx request 200
                       :to-tag (or (let ((last (transaction-last-response invite-tx)))
                                     (and last (msg-to-tag last)))
                                   (make-tag)))
           (let ((call (gethash invite-tx (ua-incoming-invites ua))))
             (when (and call (eq (transaction-state invite-tx) :proceeding))
               (call-cancelled call)))))))

(defun ua-handle-options (ua request tx)
  "Section 11.2: answer as an INVITE would be answered, without a session."
  (ua-respond ua tx request 200
              :headers `(("Allow" ,(ua-allow-header ua))
                         ("Accept" "application/sdp")
                         ("Contact" ,(ua-contact-for ua request)))))

(defun ua-handle-ack (ua request)
  "An ACK that matched no transaction: the ACK to a 2xx."
  (let ((owner (gethash (dialog-id-of-request request) (ua-dialog-owners ua))))
    (when owner (call-handle-ack owner request))))

(defun ua-handle-stray-2xx (ua response)
  "A 2xx to an INVITE whose client transaction is gone."
  (let ((owner (gethash (list (msg-call-id response) (msg-from-tag response)
                              (msg-to-tag response))
                        (ua-dialog-owners ua))))
    (when owner (call-handle-2xx-retransmission owner response))))

;;;; OPTIONS as a client (section 11.1)

(defun options (ua target &key on-response)
  "Query TARGET's capabilities.  ON-RESPONSE is called with the final
response, or with NIL on timeout or transport failure."
  (on-loop ((ua-scheduler ua))
    (let ((request (ua-make-request ua "OPTIONS" target
                                    :headers '(("Accept" "application/sdp")))))
      (ua-send ua request
               (lambda (creq event argument)
                 (declare (ignore creq))
                 (when on-response
                   (case event
                     (:response (when (>= (response-status argument) 200)
                                  (funcall on-response argument)))
                     (t (funcall on-response nil)))))))))

;;;; Waiting from other threads

(defun wait-until (predicate &key (timeout 10) (interval 1/50))
  "Poll PREDICATE until it returns true (returning that value) or TIMEOUT
seconds pass (returning NIL).  For application threads, not the event loop."
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        for value = (funcall predicate)
        do (cond (value (return value))
                 ((> (get-internal-real-time) deadline) (return nil))
                 (t (sleep interval)))))
