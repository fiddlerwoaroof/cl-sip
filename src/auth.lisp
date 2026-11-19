(in-package :cl-sip)

;;;; Digest authentication (RFC 3261 section 22, RFC 7616, RFC 8760)

(defstruct challenge
  (scheme "Digest" :type string)
  realm nonce opaque
  (algorithm "MD5")
  (qop '() :type list)                  ; offered qop values, e.g. ("auth" "auth-int")
  (stale nil)
  domain
  (params '() :type list))              ; anything else

(defstruct credentials
  (scheme "Digest" :type string)
  username realm nonce uri response
  algorithm cnonce opaque qop nc
  (params '() :type list))

(defun parse-auth-params (string)
  "Parse `name=value *(, name=value)` into an alist with downcased names."
  (loop for piece in (split-top-level string #\,)
        for trimmed = (trim-ws piece)
        unless (string= trimmed "")
          collect (let ((eq (position-top-level #\= trimmed)))
                    (unless eq (parse-fail "malformed auth parameter: ~s" trimmed))
                    (cons (string-downcase (trim-ws (subseq trimmed 0 eq)))
                          (unquote-string (subseq trimmed (1+ eq)))))))

(defun split-auth-scheme (string)
  (let* ((s (trim-ws string))
         (space (position-if #'ws-char-p s)))
    (unless space (parse-fail "malformed authentication header: ~s" string))
    (values (subseq s 0 space) (subseq s space))))

(defun parse-challenge (string)
  "Parse a WWW-Authenticate or Proxy-Authenticate header value."
  (multiple-value-bind (scheme rest) (split-auth-scheme string)
    (let ((params (parse-auth-params rest))
          (known '("realm" "nonce" "opaque" "algorithm" "qop" "stale" "domain")))
      (flet ((p (name) (cdr (assoc name params :test #'string=))))
        (make-challenge
         :scheme scheme
         :realm (p "realm") :nonce (p "nonce") :opaque (p "opaque")
         :algorithm (or (p "algorithm") "MD5")
         :qop (and (p "qop")
                   (mapcar (lambda (q) (string-downcase (trim-ws q)))
                           (split-top-level (p "qop") #\,)))
         :stale (string-equal (or (p "stale") "") "true")
         :domain (p "domain")
         :params (remove-if (lambda (cell) (member (car cell) known :test #'string=))
                            params))))))

(defun format-challenge (challenge)
  (with-output-to-string (out)
    (format out "~a realm=~a, nonce=~a" (challenge-scheme challenge)
            (quote-string (challenge-realm challenge))
            (quote-string (challenge-nonce challenge)))
    (when (challenge-opaque challenge)
      (format out ", opaque=~a" (quote-string (challenge-opaque challenge))))
    (when (challenge-algorithm challenge)
      (format out ", algorithm=~a" (challenge-algorithm challenge)))
    (when (challenge-qop challenge)
      (format out ", qop=\"~{~a~^,~}\"" (challenge-qop challenge)))
    (when (challenge-stale challenge)
      (write-string ", stale=true" out))
    (when (challenge-domain challenge)
      (format out ", domain=~a" (quote-string (challenge-domain challenge))))))

(defun parse-credentials (string)
  "Parse an Authorization or Proxy-Authorization header value."
  (multiple-value-bind (scheme rest) (split-auth-scheme string)
    (let ((params (parse-auth-params rest))
          (known '("username" "realm" "nonce" "uri" "response" "algorithm"
                   "cnonce" "opaque" "qop" "nc")))
      (flet ((p (name) (cdr (assoc name params :test #'string=))))
        (make-credentials
         :scheme scheme
         :username (p "username") :realm (p "realm") :nonce (p "nonce")
         :uri (p "uri") :response (p "response")
         :algorithm (p "algorithm") :cnonce (p "cnonce") :opaque (p "opaque")
         :qop (and (p "qop") (string-downcase (p "qop")))
         :nc (p "nc")
         :params (remove-if (lambda (cell) (member (car cell) known :test #'string=))
                            params))))))

(defun format-credentials (c)
  (with-output-to-string (out)
    (format out "~a username=~a, realm=~a, nonce=~a, uri=~a, response=~a"
            (credentials-scheme c)
            (quote-string (credentials-username c))
            (quote-string (credentials-realm c))
            (quote-string (credentials-nonce c))
            (quote-string (credentials-uri c))
            (quote-string (credentials-response c)))
    (when (credentials-algorithm c)
      (format out ", algorithm=~a" (credentials-algorithm c)))
    (when (credentials-cnonce c)
      (format out ", cnonce=~a" (quote-string (credentials-cnonce c))))
    (when (credentials-opaque c)
      (format out ", opaque=~a" (quote-string (credentials-opaque c))))
    (when (credentials-qop c)
      (format out ", qop=~a" (credentials-qop c)))
    (when (credentials-nc c)
      (format out ", nc=~a" (credentials-nc c)))))

;;; Computation (RFC 7616 section 3.4)

(defun digest-algorithm (name)
  "Returns the ironclad digest for algorithm NAME and whether it is a -sess
variant, or NIL when unsupported."
  (let* ((name (string-upcase (or name "MD5")))
         (sess (and (> (length name) 5)
                    (string= "-SESS" name :start2 (- (length name) 5))))
         (base (if sess (subseq name 0 (- (length name) 5)) name))
         (digest (cond ((string= base "MD5") :md5)
                       ((string= base "SHA-256") :sha256))))
    (and digest (values digest sess))))

(defun algorithm-supported-p (name)
  (and (digest-algorithm name) t))

(defun hex-digest (digest data)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence digest (if (stringp data) (string-to-octets data) data))))

(defun digest-response (&key (algorithm "MD5") username realm password
                          method uri nonce nc cnonce qop body)
  "Compute the request-digest as a lowercase hex string."
  (multiple-value-bind (digest sess) (digest-algorithm algorithm)
    (unless digest (error "Unsupported digest algorithm ~a" algorithm))
    (flet ((h (data) (hex-digest digest data)))
      (let* ((ha1 (let ((base (h (format nil "~a:~a:~a" username realm password))))
                    (if sess
                        (h (format nil "~a:~a:~a" base nonce cnonce))
                        base)))
             (ha2 (if (equal qop "auth-int")
                      (h (format nil "~a:~a:~a" method uri
                                 (h (or body (make-array 0 :element-type '(unsigned-byte 8))))))
                      (h (format nil "~a:~a" method uri)))))
        (if qop
            (h (format nil "~a:~a:~a:~a:~a:~a" ha1 nonce nc cnonce qop ha2))
            (h (format nil "~a:~a:~a" ha1 nonce ha2)))))))

;;; Client side

(defun choose-qop (challenge)
  (cond ((member "auth" (challenge-qop challenge) :test #'string=) "auth")
        ((member "auth-int" (challenge-qop challenge) :test #'string=) "auth-int")
        (t nil)))

(defun answer-challenge (challenge &key username password method uri body
                                     (nonce-count 1) cnonce)
  "Build CREDENTIALS answering CHALLENGE for a request with METHOD and URI
(the Request-URI as a string)."
  (let* ((qop (choose-qop challenge))
         (algorithm (challenge-algorithm challenge))
         (sess (nth-value 1 (digest-algorithm algorithm)))
         (cnonce (or cnonce (and (or qop sess) (random-hex 8))))
         (nc (and qop (format nil "~8,'0x" nonce-count))))
    (make-credentials
     :username username
     :realm (challenge-realm challenge)
     :nonce (challenge-nonce challenge)
     :uri uri
     :algorithm algorithm
     :cnonce cnonce
     :opaque (challenge-opaque challenge)
     :qop qop
     :nc nc
     :response (digest-response :algorithm algorithm :username username
                                :realm (challenge-realm challenge)
                                :password password :method method :uri uri
                                :nonce (challenge-nonce challenge)
                                :nc nc :cnonce cnonce :qop qop :body body))))

(defun select-challenge (challenges)
  "Pick the first challenge with a supported scheme and algorithm; servers
list them in order of preference (RFC 8760 section 2.4, RFC 7616 3.7)."
  (find-if (lambda (c)
             (and (string-equal (challenge-scheme c) "Digest")
                  (algorithm-supported-p (challenge-algorithm c))))
           challenges))

;;; Server side

(defun generate-challenge (realm &key (algorithm "MD5") (qop '("auth")) stale opaque)
  (make-challenge :realm realm :nonce (random-hex 16) :algorithm algorithm
                  :qop qop :stale stale :opaque opaque))

(defun verify-credentials (credentials &key password method body)
  "True when CREDENTIALS carry the right response for PASSWORD and METHOD."
  (and (string-equal (credentials-scheme credentials) "Digest")
       (credentials-response credentials)
       (algorithm-supported-p (credentials-algorithm credentials))
       (string-equal
        (credentials-response credentials)
        (digest-response :algorithm (or (credentials-algorithm credentials) "MD5")
                         :username (credentials-username credentials)
                         :realm (credentials-realm credentials)
                         :password password
                         :method method
                         :uri (credentials-uri credentials)
                         :nonce (credentials-nonce credentials)
                         :nc (credentials-nc credentials)
                         :cnonce (credentials-cnonce credentials)
                         :qop (credentials-qop credentials)
                         :body body))))
