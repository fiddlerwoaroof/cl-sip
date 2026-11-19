(in-package :cl-sip)

;;;; SIP and SIPS URIs (RFC 3261 section 19.1)

(defstruct sip-uri
  (scheme :sip :type (member :sip :sips))
  (user nil :type (or null string))
  (password nil :type (or null string))
  (host "" :type string)
  (port nil :type (or null (integer 0 65535)))
  (params '() :type list)                ; alist (downcased-name . value-or-nil)
  (headers '() :type list))              ; alist (name . value)

(defstruct generic-uri
  "Any URI whose scheme is not sip or sips (tel:, http:, ...)."
  (scheme "" :type string)
  (rest "" :type string))

(defmethod print-object ((uri sip-uri) stream)
  (print-unreadable-object (uri stream :type t)
    (write-string (uri-string uri) stream)))

(defmethod print-object ((uri generic-uri) stream)
  (print-unreadable-object (uri stream :type t)
    (write-string (uri-string uri) stream)))

;;; Escaping (section 19.1.2)

(defun unescape (string)
  "Resolve %HH escapes.  The escaped octets are interpreted as UTF-8."
  (if (not (find #\% string))
      string
      (let ((octets (make-array (length string) :element-type '(unsigned-byte 8)
                                                :fill-pointer 0 :adjustable t))
            (i 0)
            (n (length string)))
        (loop while (< i n)
              do (let ((c (char string i)))
                   (cond ((and (char= c #\%)
                               (< (+ i 2) n)
                               (hex-digit-p (char string (+ i 1)))
                               (hex-digit-p (char string (+ i 2))))
                          (vector-push-extend
                           (parse-integer string :start (+ i 1) :end (+ i 3) :radix 16)
                           octets)
                          (incf i 3))
                         (t
                          (loop for o across (string-to-octets (string c))
                                do (vector-push-extend o octets))
                          (incf i)))))
        (octets-to-string (coerce octets 'octets)))))

(defun escape (string allowed)
  "Percent-escape every character of STRING that is neither unreserved nor
in the string ALLOWED."
  (if (every (lambda (c) (or (unreserved-char-p c) (find c allowed))) string)
      string
      (with-output-to-string (out)
        (loop for c across string
              do (if (or (unreserved-char-p c) (find c allowed))
                     (write-char c out)
                     (loop for o across (string-to-octets (string c))
                           do (format out "%~2,'0X" o)))))))

(defparameter +user-unreserved+ "&=+$,;?/")
(defparameter +password-unreserved+ "&=+$,")
(defparameter +param-unreserved+ "[]/:&+$")
(defparameter +header-unreserved+ "[]/?:+$")

;;; Parsing

(defun valid-host-p (host)
  (and (plusp (length host))
       (if (char= (char host 0) #\[)
           (and (char= (char host (1- (length host))) #\])
                (> (length host) 2)
                (every (lambda (c) (or (hex-digit-p c) (find c ":.")))
                       (subseq host 1 (1- (length host)))))
           (every (lambda (c) (or (alphanum-p c) (find c "-."))) host))))

(defun split-hostport (string &key (what "URI"))
  "Split \"host[:port]\" (host may be an IPv6 reference).  Returns host, port."
  (let* ((string (trim-ws string))
         (host-end (if (and (plusp (length string)) (char= (char string 0) #\[))
                       (let ((close (position #\] string)))
                         (unless close (parse-fail "unterminated IPv6 reference in ~a" what))
                         (1+ close))
                       (or (position #\: string) (length string))))
         (host (trim-ws (subseq string 0 host-end)))
         (rest (trim-ws (subseq string host-end))))
    (unless (valid-host-p host)
      (parse-fail "invalid host ~s in ~a" host what))
    (cond ((string= rest "") (values host nil))
          ((char= (char rest 0) #\:)
           (values host (parse-decimal (subseq rest 1) :what "port" :max 65535)))
          (t (parse-fail "junk after host in ~a: ~s" what rest)))))

(defun parse-sip-uri (scheme rest)
  (when (find-if (lambda (c) (or (ws-char-p c) (char= c #\Return) (char= c #\Newline)
                                 (char= c #\<) (char= c #\>)))
                 rest)
    (parse-fail "illegal character in URI ~s" rest))
  (let* ((at (position #\@ rest))
         (userinfo (and at (subseq rest 0 at)))
         (after-user (if at (subseq rest (1+ at)) rest))
         (qmark (position #\? after-user))
         (headers-part (and qmark (subseq after-user (1+ qmark))))
         (before-headers (subseq after-user 0 qmark))
         (semi (if (and (plusp (length before-headers))
                        (char= (char before-headers 0) #\[))
                   (position #\; before-headers
                             :start (or (position #\] before-headers) 0))
                   (position #\; before-headers)))
         (hostport (subseq before-headers 0 semi))
         (params-part (and semi (subseq before-headers (1+ semi))))
         user password)
    (when userinfo
      (let ((colon (position #\: userinfo)))
        (setf user (unescape (subseq userinfo 0 colon))
              password (and colon (unescape (subseq userinfo (1+ colon)))))
        (when (string= user "")
          (parse-fail "empty user part in URI"))))
    (multiple-value-bind (host port) (split-hostport hostport)
      (make-sip-uri
       :scheme scheme
       :user user
       :password password
       :host host
       :port port
       :params (when params-part
                 (loop for piece in (split-top-level params-part #\;)
                       unless (string= piece "")
                         collect (let ((eq (position #\= piece)))
                                   (if eq
                                       (cons (string-downcase (unescape (subseq piece 0 eq)))
                                             (unescape (subseq piece (1+ eq))))
                                       (cons (string-downcase (unescape piece)) nil)))))
       :headers (when headers-part
                  (loop for piece in (split-top-level headers-part #\&)
                        unless (string= piece "")
                          collect (let ((eq (position #\= piece)))
                                    (cons (unescape (subseq piece 0 eq))
                                          (if eq (unescape (subseq piece (1+ eq))) "")))))))))

(defun parse-uri (string)
  "Parse STRING into a SIP-URI or, for other schemes, a GENERIC-URI."
  (let* ((string (trim-ws string))
         (colon (position #\: string)))
    (unless (and colon (plusp colon)
                 (ascii-alpha-p (char string 0))
                 (every (lambda (c) (or (alphanum-p c) (find c "+-.")))
                        (subseq string 0 colon)))
      (parse-fail "not a URI: ~s" string))
    (let ((scheme (string-downcase (subseq string 0 colon)))
          (rest (subseq string (1+ colon))))
      (cond ((string= scheme "sip") (parse-sip-uri :sip rest))
            ((string= scheme "sips") (parse-sip-uri :sips rest))
            (t
             (when (or (string= rest "")
                       (find-if (lambda (c) (or (ws-char-p c) (find c "<>\""))) rest))
               (parse-fail "malformed URI: ~s" string))
             (make-generic-uri :scheme scheme :rest rest))))))

(defun ensure-uri (thing)
  (etypecase thing
    ((or sip-uri generic-uri) thing)
    (string (parse-uri thing))))

;;; Serializing

(defgeneric uri-string (uri))

(defmethod uri-string ((uri generic-uri))
  (format nil "~a:~a" (generic-uri-scheme uri) (generic-uri-rest uri)))

(defmethod uri-string ((uri string)) uri)

(defmethod uri-string ((uri sip-uri))
  (with-output-to-string (out)
    (write-string (if (eq (sip-uri-scheme uri) :sips) "sips:" "sip:") out)
    (when (sip-uri-user uri)
      (write-string (escape (sip-uri-user uri) +user-unreserved+) out)
      (when (sip-uri-password uri)
        (write-char #\: out)
        (write-string (escape (sip-uri-password uri) +password-unreserved+) out))
      (write-char #\@ out))
    (write-string (sip-uri-host uri) out)
    (when (sip-uri-port uri)
      (format out ":~d" (sip-uri-port uri)))
    (loop for (name . value) in (sip-uri-params uri)
          do (write-char #\; out)
             (write-string (escape name +param-unreserved+) out)
             (when value
               (write-char #\= out)
               (write-string (escape value +param-unreserved+) out)))
    (loop for (name . value) in (sip-uri-headers uri)
          for sep = #\? then #\&
          do (write-char sep out)
             (write-string (escape name +header-unreserved+) out)
             (write-char #\= out)
             (write-string (escape value +header-unreserved+) out))))

;;; Accessors

(defun uri-param (uri name)
  "Value of URI parameter NAME; second value true when present."
  (if (sip-uri-p uri)
      (param (sip-uri-params uri) name)
      (values nil nil)))

(defun uri-with-param (uri name &optional value)
  (let ((copy (copy-sip-uri uri)))
    (setf (sip-uri-params copy) (set-param (sip-uri-params uri) name value))
    copy))

(defun uri-without-params (uri &rest names)
  (let ((copy (copy-sip-uri uri)))
    (dolist (name names)
      (setf (sip-uri-params copy) (remove-param (sip-uri-params copy) name)))
    copy))

(defun uri-loose-route-p (uri)
  (and (sip-uri-p uri) (nth-value 1 (uri-param uri "lr"))))

(defun uri-address-of-record (uri)
  "URI reduced to scheme, user and host, as used for an address-of-record."
  (make-sip-uri :scheme (sip-uri-scheme uri) :user (sip-uri-user uri)
                :host (sip-uri-host uri)))

;;; Comparison (section 19.1.4)

(defun uri-equal (a b)
  (let ((a (ensure-uri a))
        (b (ensure-uri b)))
    (cond
      ((and (generic-uri-p a) (generic-uri-p b))
       (and (string-equal (generic-uri-scheme a) (generic-uri-scheme b))
            (string= (generic-uri-rest a) (generic-uri-rest b))))
      ((or (generic-uri-p a) (generic-uri-p b)) nil)
      (t
       (and (eq (sip-uri-scheme a) (sip-uri-scheme b))
            (equal (sip-uri-user a) (sip-uri-user b))
            (equal (sip-uri-password a) (sip-uri-password b))
            (string-equal (sip-uri-host a) (sip-uri-host b))
            (eql (sip-uri-port a) (sip-uri-port b))
            ;; Any parameter appearing in both must match.
            (loop for (name . value) in (sip-uri-params a)
                  always (multiple-value-bind (other present)
                             (param (sip-uri-params b) name)
                           (or (not present)
                               (string-equal (or value "") (or other "")))))
            ;; These must match when present in either.
            (loop for name in '("user" "ttl" "method" "maddr" "transport")
                  always (multiple-value-bind (va pa) (param (sip-uri-params a) name)
                           (multiple-value-bind (vb pb) (param (sip-uri-params b) name)
                             (and (eq pa pb)
                                  (string-equal (or va "") (or vb ""))))))
            ;; Header components are never ignored.
            (= (length (sip-uri-headers a)) (length (sip-uri-headers b)))
            (loop for (name . value) in (sip-uri-headers a)
                  always (let ((cell (assoc name (sip-uri-headers b) :test #'string-equal)))
                           (and cell (string= value (cdr cell))))))))))
