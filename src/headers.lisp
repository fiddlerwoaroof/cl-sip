(in-package :cl-sip)

;;;; Header names (RFC 3261 sections 7.3 and 20)

(defparameter *header-names*
  '("Accept" "Accept-Encoding" "Accept-Language" "Alert-Info" "Allow"
    "Authentication-Info" "Authorization" "Call-ID" "Call-Info" "Contact"
    "Content-Disposition" "Content-Encoding" "Content-Language"
    "Content-Length" "Content-Type" "CSeq" "Date" "Error-Info" "Expires"
    "From" "In-Reply-To" "Max-Forwards" "MIME-Version" "Min-Expires"
    "Organization" "Priority" "Proxy-Authenticate" "Proxy-Authorization"
    "Proxy-Require" "Record-Route" "Reply-To" "Require" "Retry-After" "Route"
    "Server" "Subject" "Supported" "Timestamp" "To" "Unsupported"
    "User-Agent" "Via" "Warning" "WWW-Authenticate"))

(defparameter *compact-forms*
  '(("i" . "Call-ID") ("m" . "Contact") ("e" . "Content-Encoding")
    ("l" . "Content-Length") ("c" . "Content-Type") ("f" . "From")
    ("s" . "Subject") ("k" . "Supported") ("t" . "To") ("v" . "Via")))

(defparameter *list-headers*
  '("Accept" "Accept-Encoding" "Accept-Language" "Alert-Info" "Allow"
    "Call-Info" "Contact" "Content-Encoding" "Content-Language" "Error-Info"
    "In-Reply-To" "Proxy-Require" "Record-Route" "Require" "Route"
    "Supported" "Unsupported" "Via" "Warning")
  "Headers whose grammar is a comma-separated list, so that several values on
one line are equivalent to several header lines (section 7.3.1).")

(defvar *header-name-table*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (name *header-names*)
      (setf (gethash (string-downcase name) table) name))
    (loop for (compact . name) in *compact-forms*
          do (setf (gethash compact table) name))
    table))

(defun canonical-header-name (name)
  "Canonical spelling of header NAME, expanding compact forms.  Unknown names
are returned unchanged."
  (or (gethash (string-downcase name) *header-name-table*) name))

(defun list-header-p (canonical-name)
  (member canonical-name *list-headers* :test #'string=))

;;;; name-addr / addr-spec with parameters (From, To, Contact, Route, ...)

(defstruct name-addr
  (display-name nil :type (or null string))
  (uri nil)
  (params '() :type list))

(defun parse-name-addr (string)
  "Parse `[display-name] <uri> *(;param)` or `addr-spec *(;param)`."
  (let* ((s (trim-ws string))
         (n (length s)))
    (when (zerop n) (parse-fail "empty address"))
    (let ((display nil)
          (i 0))
      ;; Quoted display name.
      (when (char= (char s 0) #\")
        (let ((close (loop with j = 1
                           while (< j n)
                           do (cond ((char= (char s j) #\\) (incf j 2))
                                    ((char= (char s j) #\") (return j))
                                    (t (incf j)))
                           finally (parse-fail "unterminated quoted string in ~s" s))))
          (setf display (unquote-string (subseq s 0 (1+ close)))
                i (1+ close))))
      (let ((open (position #\< s :start i)))
        (cond
          (open
           (let ((close (position #\> s :start open))
                 (before (trim-ws (subseq s i open))))
             (unless close (parse-fail "missing '>' in ~s" s))
             (cond ((and display (string/= before ""))
                    (parse-fail "junk before '<' in ~s" s))
                   ((string/= before "") (setf display before)))
             (make-name-addr :display-name display
                             :uri (parse-uri (subseq s (1+ open) close))
                             :params (parse-params (subseq s (1+ close))))))
          (display (parse-fail "display name without <uri> in ~s" s))
          (t
           ;; Bare addr-spec: parameters after the first ';' belong to the
           ;; header, not the URI (section 20.10).
           (let ((semi (position #\; s)))
             (make-name-addr :uri (parse-uri (subseq s 0 semi))
                             :params (and semi (parse-params (subseq s semi)))))))))))

(defun format-name-addr (na)
  (with-output-to-string (out)
    (let ((display (name-addr-display-name na)))
      (when (and display (string/= display ""))
        (write-string (if (every (lambda (c) (or (token-char-p c) (char= c #\Space))) display)
                          display
                          (quote-string display))
                      out)
        (write-char #\Space out)))
    (format out "<~a>" (uri-string (name-addr-uri na)))
    (format-params (name-addr-params na) out)))

(defun ensure-name-addr (thing)
  (etypecase thing
    (name-addr thing)
    (string (parse-name-addr thing))
    ((or sip-uri generic-uri) (make-name-addr :uri thing))))

(defun name-addr-tag (na)
  (values (param (name-addr-params na) "tag")))

(defun name-addr-with-tag (na tag)
  (let ((copy (copy-name-addr na)))
    (setf (name-addr-params copy) (set-param (name-addr-params na) "tag" tag))
    copy))

;;;; Via (section 20.42)

(defstruct via
  (protocol-name "SIP" :type string)
  (protocol-version "2.0" :type string)
  (transport "UDP" :type string)
  (host "" :type string)
  (port nil :type (or null integer))
  (params '() :type list))

(defun parse-via (string)
  (let* ((s (trim-ws string))
         (semi (position #\; s))
         (head (subseq s 0 semi))
         (params (and semi (parse-params (subseq s semi))))
         (pos 0)
         (n (length head)))
    (labels ((skip-ws ()
               (loop while (and (< pos n) (ws-char-p (char head pos))) do (incf pos)))
             (token ()
               (skip-ws)
               (let ((start pos))
                 (loop while (and (< pos n) (token-char-p (char head pos))) do (incf pos))
                 (when (= start pos) (parse-fail "malformed Via: ~s" string))
                 (subseq head start pos)))
             (slash ()
               (skip-ws)
               (unless (and (< pos n) (char= (char head pos) #\/))
                 (parse-fail "malformed Via: ~s" string))
               (incf pos)))
      (let* ((name (token))
             (version (progn (slash) (token)))
             (transport (progn (slash) (token))))
        (unless (and (< pos n) (ws-char-p (char head pos)))
          (parse-fail "malformed Via: ~s" string))
        (multiple-value-bind (host port)
            (split-hostport (remove-if #'ws-char-p (subseq head pos)) :what "Via")
          (make-via :protocol-name (string-upcase name)
                    :protocol-version version
                    :transport (string-upcase transport)
                    :host host :port port :params params))))))

(defun format-via (via)
  (with-output-to-string (out)
    (format out "~a/~a/~a ~a" (via-protocol-name via) (via-protocol-version via)
            (via-transport via) (via-host via))
    (when (via-port via) (format out ":~d" (via-port via)))
    (format-params (via-params via) out)))

(defun via-param (via name)
  (param (via-params via) name))

(defun via-branch (via)
  (values (via-param via "branch")))

;;;; CSeq (section 20.16)

(defstruct cseq
  (number 0 :type integer)
  (method "" :type string))

(defun parse-cseq (string)
  (let* ((s (trim-ws string))
         (space (position-if #'ws-char-p s)))
    (unless space (parse-fail "malformed CSeq: ~s" string))
    (let ((method (trim-ws (subseq s space))))
      (unless (token-p method) (parse-fail "malformed CSeq method: ~s" string))
      (make-cseq :number (parse-decimal (subseq s 0 space) :what "CSeq" :max (1- (expt 2 32)))
                 :method method))))

(defun format-cseq (cseq)
  (format nil "~d ~a" (cseq-number cseq) (cseq-method cseq)))
