(in-package :cl-sip)

;;;; Transactions (RFC 3261 section 17, with the corrections of RFC 6026
;;;; and RFC 4320)
;;;;
;;;; The transaction user (TU) is a function called as
;;;;   (funcall tu transaction event &optional argument)
;;;; with EVENT one of
;;;;   :response         client: a response to pass up (argument: the response)
;;;;   :ack              server: an ACK for a 2xx reached the transaction
;;;;   :timeout          timer B, F or H fired
;;;;   :transport-error  argument: the condition
;;;;   :terminated       the transaction has been destroyed

(defclass transaction ()
  ((stack :initarg :stack :reader transaction-stack)
   (request :initarg :request :reader transaction-request)
   (state :initarg :state :accessor transaction-state)
   (tu :initarg :tu :initform nil :accessor transaction-tu)
   (key :initarg :key :reader transaction-key)
   (timers :initform '() :accessor transaction-timers)
   (reliable :initarg :reliable :initform nil :reader transaction-reliable-p)))

(defmethod print-object ((tx transaction) stream)
  (print-unreadable-object (tx stream :type t)
    (format stream "~a ~a" (request-method (transaction-request tx))
            (transaction-state tx))))

(defun transaction-terminated-p (tx)
  (eq (transaction-state tx) :terminated))

(defun tx-notify (tx event &optional argument)
  (when (transaction-tu tx)
    (funcall (transaction-tu tx) tx event argument)))

(defun tx-start-timer (tx name delay function)
  "Start (or restart) the timer NAME."
  (tx-cancel-timer tx name)
  (let ((timer (schedule (stack-scheduler (transaction-stack tx)) delay
                         (lambda ()
                           (setf (transaction-timers tx)
                                 (remove name (transaction-timers tx) :key #'car))
                           (unless (transaction-terminated-p tx)
                             (funcall function)))
                         name)))
    (push (cons name timer) (transaction-timers tx))
    timer))

(defun tx-cancel-timer (tx name)
  (let ((cell (assoc name (transaction-timers tx))))
    (when cell
      (cancel-timer (cdr cell))
      (setf (transaction-timers tx) (remove cell (transaction-timers tx))))))

(defgeneric transaction-table (tx))

(defun tx-terminate (tx)
  (unless (transaction-terminated-p tx)
    (loop for (nil . timer) in (transaction-timers tx) do (cancel-timer timer))
    (setf (transaction-timers tx) '()
          (transaction-state tx) :terminated)
    (remhash (transaction-key tx) (transaction-table tx))
    (tx-notify tx :terminated)))

(defun tx-t1 (tx) (stack-t1 (transaction-stack tx)))
(defun tx-t2 (tx) (stack-t2 (transaction-stack tx)))
(defun tx-t4 (tx) (stack-t4 (transaction-stack tx)))

;;;; Client transactions (section 17.1)

(defclass client-transaction (transaction)
  ((transport :initarg :transport :reader transaction-transport)
   (host :initarg :host :reader transaction-host)
   (port :initarg :port :reader transaction-port)
   (interval :initform nil :accessor transaction-interval)))

(defclass invite-client-transaction (client-transaction)
  ((ack :initform nil :accessor transaction-ack)))

(defclass non-invite-client-transaction (client-transaction) ())

(defmethod transaction-table ((tx client-transaction))
  (stack-client-transactions (transaction-stack tx)))

(defun client-transaction-key (branch method)
  (list branch method))

(defun tx-transmit (tx message)
  "Pass MESSAGE to the transport.  On failure the TU is informed and the
transaction terminated (section 17.1.4); returns NIL in that case."
  (handler-case
      (progn (stack-send-message (transaction-stack tx) message
                                 (transaction-transport tx)
                                 (transaction-host tx) (transaction-port tx))
             t)
    (transport-error (e)
      (tx-notify tx :transport-error e)
      (tx-terminate tx)
      nil)))

(defgeneric client-transaction-start (tx))
(defgeneric client-transaction-receive (tx response))

(defun send-request (stack request tu &key branch)
  "Create a client transaction for REQUEST and send it.  The stack chooses
the transport and pushes the Via header (with BRANCH, or a fresh one).
Returns the transaction."
  (multiple-value-bind (transport host port)
      (stack-prepare-request stack request :branch branch)
    (let* ((method (request-method request))
           (tx (make-instance (if (string= method "INVITE")
                                  'invite-client-transaction
                                  'non-invite-client-transaction)
                              :stack stack :request request :tu tu
                              :key (client-transaction-key
                                    (via-branch (msg-top-via request)) method)
                              :transport transport :host host :port port
                              :reliable (transport-reliable-p transport))))
      (setf (gethash (transaction-key tx) (stack-client-transactions stack)) tx)
      (client-transaction-start tx)
      tx)))

;;; INVITE client transaction (section 17.1.1, RFC 6026 section 7.2)

(defmethod client-transaction-start ((tx invite-client-transaction))
  (setf (transaction-state tx) :calling)
  (when (tx-transmit tx (transaction-request tx))
    (unless (transaction-reliable-p tx)
      (setf (transaction-interval tx) (tx-t1 tx))
      (labels ((timer-a ()
                 (when (and (eq (transaction-state tx) :calling)
                            (tx-transmit tx (transaction-request tx)))
                   (setf (transaction-interval tx) (* 2 (transaction-interval tx)))
                   (tx-start-timer tx :a (transaction-interval tx) #'timer-a))))
        (tx-start-timer tx :a (transaction-interval tx) #'timer-a)))
    (tx-start-timer tx :b (* 64 (tx-t1 tx))
                    (lambda ()
                      (when (eq (transaction-state tx) :calling)
                        (tx-notify tx :timeout)
                        (tx-terminate tx))))))

(defun make-ack-for-non-2xx (request response)
  "Section 17.1.1.3."
  (let ((ack (make-request "ACK" (request-uri request))))
    (add-header ack "Via" (header request "Via"))
    (dolist (route (header-values request "Route"))
      (add-header ack "Route" route))
    (add-header ack "Max-Forwards" "70")
    (add-header ack "From" (header request "From"))
    (add-header ack "To" (header response "To"))
    (add-header ack "Call-ID" (header request "Call-ID"))
    (add-header ack "CSeq" (make-cseq :number (cseq-number (msg-cseq request))
                                      :method "ACK"))
    ack))

(defmethod client-transaction-receive ((tx invite-client-transaction) response)
  (let ((status (response-status response)))
    (ecase (transaction-state tx)
      ((:calling :proceeding)
       (tx-cancel-timer tx :a)
       (tx-cancel-timer tx :b)
       (cond ((< status 200)
              (setf (transaction-state tx) :proceeding)
              (tx-notify tx :response response))
             ((< status 300)
              (setf (transaction-state tx) :accepted)
              (tx-start-timer tx :m (* 64 (tx-t1 tx)) (lambda () (tx-terminate tx)))
              (tx-notify tx :response response))
             (t
              (setf (transaction-state tx) :completed
                    (transaction-ack tx)
                    (make-ack-for-non-2xx (transaction-request tx) response))
              (when (tx-transmit tx (transaction-ack tx))
                (tx-start-timer tx :d (if (transaction-reliable-p tx) 0 32)
                                (lambda () (tx-terminate tx)))
                (tx-notify tx :response response)))))
      (:accepted
       (when (<= 200 status 299)
         (tx-notify tx :response response)))
      (:completed
       (when (>= status 300)
         (tx-transmit tx (transaction-ack tx))))
      (:terminated nil))))

;;; Non-INVITE client transaction (section 17.1.2)

(defmethod client-transaction-start ((tx non-invite-client-transaction))
  (setf (transaction-state tx) :trying)
  (when (tx-transmit tx (transaction-request tx))
    (unless (transaction-reliable-p tx)
      (setf (transaction-interval tx) (tx-t1 tx))
      (labels ((timer-e ()
                 (when (and (member (transaction-state tx) '(:trying :proceeding))
                            (tx-transmit tx (transaction-request tx)))
                   (setf (transaction-interval tx)
                         (if (eq (transaction-state tx) :trying)
                             (min (* 2 (transaction-interval tx)) (tx-t2 tx))
                             (tx-t2 tx)))
                   (tx-start-timer tx :e (transaction-interval tx) #'timer-e))))
        (tx-start-timer tx :e (transaction-interval tx) #'timer-e)))
    (tx-start-timer tx :f (* 64 (tx-t1 tx))
                    (lambda ()
                      (when (member (transaction-state tx) '(:trying :proceeding))
                        (tx-notify tx :timeout)
                        (tx-terminate tx))))))

(defmethod client-transaction-receive ((tx non-invite-client-transaction) response)
  (let ((status (response-status response)))
    (case (transaction-state tx)
      ((:trying :proceeding)
       (cond ((< status 200)
              (setf (transaction-state tx) :proceeding)
              (tx-notify tx :response response))
             (t
              (tx-cancel-timer tx :e)
              (tx-cancel-timer tx :f)
              (setf (transaction-state tx) :completed)
              (tx-start-timer tx :k (if (transaction-reliable-p tx) 0 (tx-t4 tx))
                              (lambda () (tx-terminate tx)))
              (tx-notify tx :response response)))))))

;;;; Server transactions (section 17.2)

(defclass server-transaction (transaction)
  ((last-response :initform nil :accessor transaction-last-response)
   (interval :initform nil :accessor transaction-interval)))

(defclass invite-server-transaction (server-transaction) ())
(defclass non-invite-server-transaction (server-transaction) ())

(defmethod transaction-table ((tx server-transaction))
  (stack-server-transactions (transaction-stack tx)))

(defun server-transaction-key (request &optional method)
  "Section 17.2.3.  ACK maps onto the INVITE transaction.  METHOD overrides
the request's method (used to find the INVITE a CANCEL refers to)."
  (let* ((via (msg-top-via request))
         (branch (via-branch via))
         (method (or method
                     (if (string= (request-method request) "ACK")
                         "INVITE"
                         (request-method request)))))
    (if (and branch (alexandria:starts-with-subseq +branch-cookie+ branch))
        (list branch (string-downcase (via-host via)) (or (via-port via) 5060) method)
        ;; RFC 2543 peers: no unique branch, so identify the transaction by
        ;; the fields the old matching rules compare.
        (list :rfc2543
              (uri-string (request-uri request))
              (msg-from-tag request)
              (msg-call-id request)
              (cseq-number (msg-cseq request))
              (string-downcase (via-host via)) (or (via-port via) 5060) branch
              method))))

(defun tx-send-to-transport (tx response)
  "Send RESPONSE; on failure tell the TU but keep the transaction, so that
request retransmissions are still absorbed (RFC 6026 section 7.1)."
  (handler-case
      (progn (stack-send-response (transaction-stack tx) response) t)
    (transport-error (e)
      (tx-notify tx :transport-error e)
      nil)))

(defgeneric server-transaction-receive (tx request)
  (:documentation "A retransmitted request, or an ACK, matched TX."))

(defgeneric send-response (tx response)
  (:documentation "The TU hands RESPONSE to the server transaction TX."))

(defun make-server-transaction (stack request)
  (let* ((invite (string= (request-method request) "INVITE"))
         (tx (make-instance (if invite
                                'invite-server-transaction
                                'non-invite-server-transaction)
                            :stack stack :request request
                            :key (server-transaction-key request)
                            :state (if invite :proceeding :trying)
                            :reliable (let ((transport (message-transport request)))
                                        (and transport (transport-reliable-p transport))))))
    (setf (gethash (transaction-key tx) (stack-server-transactions stack)) tx)
    (when invite
      ;; Send 100 Trying unless the TU answers within 200 ms.
      (tx-start-timer tx :trying 1/5
                      (lambda ()
                        (when (and (eq (transaction-state tx) :proceeding)
                                   (null (transaction-last-response tx)))
                          (send-response tx (make-response-for request 100))))))
    tx))

;;; INVITE server transaction (section 17.2.1, RFC 6026 section 7.1)

(defmethod send-response ((tx invite-server-transaction) response)
  (let ((status (response-status response)))
    (case (transaction-state tx)
      (:proceeding
       (tx-cancel-timer tx :trying)
       (setf (transaction-last-response tx) response)
       (cond ((< status 200)
              (tx-send-to-transport tx response))
             ((< status 300)
              (setf (transaction-state tx) :accepted)
              (tx-start-timer tx :l (* 64 (tx-t1 tx)) (lambda () (tx-terminate tx)))
              (tx-send-to-transport tx response))
             (t
              (setf (transaction-state tx) :completed)
              (unless (transaction-reliable-p tx)
                (setf (transaction-interval tx) (tx-t1 tx))
                (labels ((timer-g ()
                           (when (eq (transaction-state tx) :completed)
                             (tx-send-to-transport tx (transaction-last-response tx))
                             (setf (transaction-interval tx)
                                   (min (* 2 (transaction-interval tx)) (tx-t2 tx)))
                             (tx-start-timer tx :g (transaction-interval tx) #'timer-g))))
                  (tx-start-timer tx :g (transaction-interval tx) #'timer-g)))
              (tx-start-timer tx :h (* 64 (tx-t1 tx))
                              (lambda ()
                                (when (eq (transaction-state tx) :completed)
                                  (tx-notify tx :timeout)
                                  (tx-terminate tx))))
              (tx-send-to-transport tx response))))
      (:accepted
       ;; 2xx retransmissions are the TU's job; just pass them down.
       (when (<= 200 status 299)
         (tx-send-to-transport tx response))))
    tx))

(defmethod server-transaction-receive ((tx invite-server-transaction) request)
  (if (string= (request-method request) "ACK")
      (case (transaction-state tx)
        (:completed
         (tx-cancel-timer tx :g)
         (tx-cancel-timer tx :h)
         (setf (transaction-state tx) :confirmed)
         (tx-start-timer tx :i (if (transaction-reliable-p tx) 0 (tx-t4 tx))
                         (lambda () (tx-terminate tx))))
        (:accepted (tx-notify tx :ack request)))
      (case (transaction-state tx)
        ((:proceeding :completed)
         (when (transaction-last-response tx)
           (tx-send-to-transport tx (transaction-last-response tx)))))))

;;; Non-INVITE server transaction (section 17.2.2)

(defmethod send-response ((tx non-invite-server-transaction) response)
  (let ((status (response-status response)))
    (case (transaction-state tx)
      ((:trying :proceeding)
       (setf (transaction-last-response tx) response)
       (cond ((< status 200)
              (setf (transaction-state tx) :proceeding))
             (t
              (setf (transaction-state tx) :completed)
              (tx-start-timer tx :j (if (transaction-reliable-p tx) 0 (* 64 (tx-t1 tx)))
                              (lambda () (tx-terminate tx)))))
       (tx-send-to-transport tx response)))
    tx))

(defmethod server-transaction-receive ((tx non-invite-server-transaction) request)
  (declare (ignore request))
  (case (transaction-state tx)
    ((:proceeding :completed)
     (tx-send-to-transport tx (transaction-last-response tx)))))

;;;; Dispatch from the transport layer

(defun find-server-transaction (stack request &optional method)
  (gethash (server-transaction-key request method) (stack-server-transactions stack)))

(defun stack-dispatch-request (stack request)
  (let ((tx (find-server-transaction stack request)))
    (cond (tx (server-transaction-receive tx request))
          ((string= (request-method request) "ACK")
           (when (stack-ack-handler stack)
             (funcall (stack-ack-handler stack) request)))
          (t
           (let ((tx (make-server-transaction stack request)))
             (if (stack-request-handler stack)
                 (funcall (stack-request-handler stack) request tx)
                 (send-response tx (make-response-for request 501 :to-tag (make-tag)))))))))

(defun stack-dispatch-response (stack response)
  (let* ((via (msg-top-via response))
         (cseq (msg-cseq response))
         (tx (gethash (client-transaction-key (via-branch via) (cseq-method cseq))
                      (stack-client-transactions stack))))
    (cond (tx (client-transaction-receive tx response))
          ((and (<= 200 (response-status response) 299)
                (string= (cseq-method cseq) "INVITE")
                (stack-stray-response-handler stack))
           (funcall (stack-stray-response-handler stack) response)))))
