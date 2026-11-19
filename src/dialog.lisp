(in-package :cl-sip)

;;;; Dialogs (RFC 3261 section 12)

(defclass dialog ()
  ((call-id :initarg :call-id :reader dialog-call-id)
   (local-tag :initarg :local-tag :reader dialog-local-tag)
   (remote-tag :initarg :remote-tag :accessor dialog-remote-tag)
   (local-seq :initarg :local-seq :initform nil :accessor dialog-local-seq)
   (remote-seq :initarg :remote-seq :initform nil :accessor dialog-remote-seq)
   (local-uri :initarg :local-uri :reader dialog-local-uri
              :documentation "NAME-ADDR without tag.")
   (remote-uri :initarg :remote-uri :reader dialog-remote-uri
               :documentation "NAME-ADDR without tag.")
   (remote-target :initarg :remote-target :accessor dialog-remote-target)
   (route-set :initarg :route-set :initform '() :accessor dialog-route-set
              :documentation "List of NAME-ADDRs, first hop first.")
   (secure :initarg :secure :initform nil :reader dialog-secure-p)
   (state :initarg :state :initform :early :accessor dialog-state
          :documentation ":early, :confirmed or :terminated.")
   (role :initarg :role :reader dialog-role :documentation ":uac or :uas.")))

(defmethod print-object ((dialog dialog) stream)
  (print-unreadable-object (dialog stream :type t)
    (format stream "~a ~a local=~a remote=~a" (dialog-state dialog)
            (dialog-call-id dialog) (dialog-local-tag dialog) (dialog-remote-tag dialog))))

(defun dialog-id (dialog)
  (list (dialog-call-id dialog) (dialog-local-tag dialog) (dialog-remote-tag dialog)))

(defun strip-tag (name-addr)
  (let ((copy (copy-name-addr name-addr)))
    (setf (name-addr-params copy) (remove-param (name-addr-params name-addr) "tag"))
    copy))

(defun request-secure-p (request)
  (let ((uri (request-uri request)))
    (and (sip-uri-p uri) (eq (sip-uri-scheme uri) :sips))))

(defun make-uac-dialog (request response)
  "Section 12.1.2: dialog state for the UAC from the request it sent and a
1xx or 2xx RESPONSE carrying a To tag."
  (let ((from (msg-from request))
        (to (msg-to response)))
    (make-instance 'dialog
                   :role :uac
                   :call-id (msg-call-id request)
                   :local-tag (name-addr-tag from)
                   :remote-tag (name-addr-tag to)
                   :local-seq (cseq-number (msg-cseq request))
                   :local-uri (strip-tag from)
                   :remote-uri (strip-tag to)
                   :remote-target (or (msg-contact-uri response) (request-uri request))
                   :route-set (reverse (msg-record-routes response))
                   :secure (request-secure-p request)
                   :state (if (< (response-status response) 200) :early :confirmed))))

(defun make-uas-dialog (request local-tag &key (state :early))
  "Section 12.1.1: dialog state for the UAS answering REQUEST with a response
whose To tag is LOCAL-TAG."
  (let ((from (msg-from request))
        (to (msg-to request)))
    (make-instance 'dialog
                   :role :uas
                   :call-id (msg-call-id request)
                   :local-tag local-tag
                   :remote-tag (name-addr-tag from)
                   :remote-seq (cseq-number (msg-cseq request))
                   :local-uri (strip-tag to)
                   :remote-uri (strip-tag from)
                   :remote-target (or (msg-contact-uri request)
                                      (name-addr-uri from))
                   :route-set (msg-record-routes request)
                   :secure (request-secure-p request)
                   :state state)))

(defun dialog-next-cseq (dialog)
  (setf (dialog-local-seq dialog)
        (if (dialog-local-seq dialog)
            (1+ (dialog-local-seq dialog))
            (1+ (random 10000)))))

(defun dialog-make-request (dialog method &key cseq headers body)
  "Section 12.2.1.1: build a request inside DIALOG.  CSEQ overrides the
sequence number (ACK and CANCEL reuse the INVITE's); otherwise the local
sequence number is incremented."
  (let* ((route-set (dialog-route-set dialog))
         (target (dialog-remote-target dialog))
         (first-route (and route-set (name-addr-uri (first route-set))))
         (strict (and first-route (not (uri-loose-route-p first-route))))
         (request (make-request method (if strict first-route target))))
    (add-header request "Max-Forwards" "70")
    (cond (strict
           (setf (request-next-hop request) first-route)
           ;; Strict router: it becomes the Request-URI and the remote target
           ;; travels as the last Route.
           (dolist (route (rest route-set))
             (add-header request "Route" route))
           (add-header request "Route" (make-name-addr :uri target)))
          (t
           (dolist (route route-set)
             (add-header request "Route" route))))
    (add-header request "From"
                (name-addr-with-tag (dialog-local-uri dialog) (dialog-local-tag dialog)))
    (add-header request "To"
                (if (dialog-remote-tag dialog)
                    (name-addr-with-tag (dialog-remote-uri dialog) (dialog-remote-tag dialog))
                    (dialog-remote-uri dialog)))
    (add-header request "Call-ID" (dialog-call-id dialog))
    (add-header request "CSeq"
                (make-cseq :number (or cseq (dialog-next-cseq dialog)) :method method))
    (loop for (name . value) in (normalize-headers headers)
          do (add-header request name value))
    (setf (message-body request) (ensure-octets body))
    request))

(defun dialog-refresh-target (dialog message)
  "Target refresh (sections 12.2.1.2, 12.2.2): adopt the Contact of a
re-INVITE or of its 2xx."
  (let ((uri (msg-contact-uri message)))
    (when uri (setf (dialog-remote-target dialog) uri))))

(defun dialog-check-remote-cseq (dialog request)
  "Section 12.2.2: true when REQUEST is in order, updating the remote
sequence number.  ACK is exempt."
  (let ((number (cseq-number (msg-cseq request))))
    (cond ((string= (request-method request) "ACK") t)
          ((and (dialog-remote-seq dialog) (< number (dialog-remote-seq dialog))) nil)
          (t (setf (dialog-remote-seq dialog) number) t))))
