(in-package :cl-sip)

;;;; Conditions

(define-condition sip-error (error) ())

(define-condition sip-parse-error (sip-error)
  ((message :initarg :message :reader sip-parse-error-message)
   (status :initarg :status :initform 400 :reader sip-parse-error-status
           :documentation "The response status a UAS should answer with."))
  (:report (lambda (c stream)
             (format stream "SIP parse error: ~a" (sip-parse-error-message c)))))

(defun parse-fail (control &rest args)
  (error 'sip-parse-error :message (apply #'format nil control args)))

(define-condition transport-error (sip-error)
  ((cause :initarg :cause :initform nil :reader transport-error-cause))
  (:report (lambda (c stream)
             (format stream "SIP transport error: ~a" (transport-error-cause c)))))

;;;; Character classes (RFC 3261 section 25.1)

(declaim (inline ascii-alpha-p ascii-digit-p alphanum-p ws-char-p))

(defun ascii-alpha-p (c)
  (or (char<= #\a c #\z) (char<= #\A c #\Z)))

(defun ascii-digit-p (c)
  (char<= #\0 c #\9))

(defun alphanum-p (c)
  (or (ascii-alpha-p c) (ascii-digit-p c)))

(defun ws-char-p (c)
  (or (char= c #\Space) (char= c #\Tab)))

(defun token-char-p (c)
  (or (alphanum-p c) (find c "-.!%*_+`'~")))

(defun token-p (string)
  (and (stringp string) (plusp (length string)) (every #'token-char-p string)))

(defun mark-char-p (c)
  (find c "-_.!~*'()"))

(defun unreserved-char-p (c)
  (or (alphanum-p c) (mark-char-p c)))

(defun hex-digit-p (c)
  (digit-char-p c 16))

(defun all-digits-p (string)
  (and (plusp (length string)) (every #'ascii-digit-p string)))

(defun trim-ws (string)
  (string-trim '(#\Space #\Tab #\Return #\Newline) string))

(defun parse-decimal (string &key (what "number") max)
  "Parse STRING as an unsigned decimal integer, signalling SIP-PARSE-ERROR."
  (let ((string (trim-ws string)))
    (unless (all-digits-p string)
      (parse-fail "invalid ~a: ~s" what string))
    (let ((n (parse-integer string)))
      (when (and max (> n max))
        (parse-fail "~a out of range: ~a" what string))
      n)))

;;;; Quote-aware splitting

(defun split-top-level (string delimiter &key (start 0) end)
  "Split STRING on the character DELIMITER, ignoring delimiters inside
double-quoted strings and angle brackets.  Returns a list of substrings,
untrimmed."
  (let ((end (or end (length string)))
        (pieces '())
        (piece-start start)
        (in-quote nil)
        (in-angle nil)
        (i start))
    (loop while (< i end)
          do (let ((c (char string i)))
               (cond (in-quote
                      (cond ((char= c #\\) (incf i))
                            ((char= c #\") (setf in-quote nil))))
                     ((char= c #\") (setf in-quote t))
                     ((and in-angle (char= c #\>)) (setf in-angle nil))
                     (in-angle)
                     ((char= c #\<) (setf in-angle t))
                     ((char= c delimiter)
                      (push (subseq string piece-start i) pieces)
                      (setf piece-start (1+ i)))))
             (incf i))
    (push (subseq string piece-start end) pieces)
    (nreverse pieces)))

(defun position-top-level (char string &key (start 0) end)
  "Position of CHAR in STRING outside quoted strings and angle brackets."
  (let ((end (or end (length string)))
        (in-quote nil)
        (in-angle nil)
        (i start))
    (loop while (< i end)
          do (let ((c (char string i)))
               (cond (in-quote
                      (cond ((char= c #\\) (incf i))
                            ((char= c #\") (setf in-quote nil))))
                     ((char= c #\") (setf in-quote t))
                     ((and in-angle (char= c #\>)) (setf in-angle nil))
                     (in-angle)
                     ((char= c char) (return-from position-top-level i))
                     ((char= c #\<) (setf in-angle t))))
             (incf i))
    nil))

;;;; Quoted strings

(defun unquote-string (string)
  "Strip surrounding double quotes and resolve backslash escapes.  A string
that is not quoted is returned trimmed."
  (let ((s (trim-ws string)))
    (if (and (>= (length s) 2)
             (char= (char s 0) #\")
             (char= (char s (1- (length s))) #\"))
        (with-output-to-string (out)
          (loop with i = 1
                with end = (1- (length s))
                while (< i end)
                do (let ((c (char s i)))
                     (when (and (char= c #\\) (< (1+ i) end))
                       (incf i)
                       (setf c (char s i)))
                     (write-char c out)
                     (incf i))))
        s)))

(defun quote-string (string)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for c across string
          do (when (or (char= c #\") (char= c #\\))
               (write-char #\\ out))
             (write-char c out))
    (write-char #\" out)))

;;;; Generic parameters:  *( SEMI token [ EQUAL gen-value ] )

(defun parse-params (string)
  "Parse \";a=b;c;d=\\\"e\\\"\" (leading semicolon optional) into an alist of
(downcased-name . value-or-nil)."
  (loop for piece in (split-top-level string #\;)
        for trimmed = (trim-ws piece)
        unless (string= trimmed "")
          collect (let ((eq (position-top-level #\= trimmed)))
                    (if eq
                        (cons (string-downcase (trim-ws (subseq trimmed 0 eq)))
                              (unquote-string (subseq trimmed (1+ eq))))
                        (cons (string-downcase trimmed) nil)))))

(defun param-value-needs-quoting-p (value)
  (or (string= value "")
      (notevery (lambda (c) (or (token-char-p c) (find c ":[]"))) value)))

(defun format-params (params stream)
  (loop for (name . value) in params
        do (write-char #\; stream)
           (write-string name stream)
           (when value
             (write-char #\= stream)
             (write-string (if (param-value-needs-quoting-p value)
                               (quote-string value)
                               value)
                           stream))))

(defun param (params name)
  "Value of parameter NAME in the alist PARAMS.  Second value is true when the
parameter is present (it may be present without a value)."
  (let ((cell (assoc name params :test #'string-equal)))
    (values (cdr cell) (and cell t))))

(defun set-param (params name value)
  "Return PARAMS with NAME set to VALUE, preserving order."
  (let ((cell (assoc name params :test #'string-equal)))
    (if cell
        (mapcar (lambda (c) (if (eq c cell) (cons (car c) value) c)) params)
        (append params (list (cons (string-downcase name) value))))))

(defun remove-param (params name)
  (remove name params :key #'car :test #'string-equal))

;;;; Octets and strings

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun string-to-octets (string)
  (coerce (babel:string-to-octets string :encoding :utf-8) 'octets))

(defun octets-to-string (octets &key (start 0) end)
  (handler-case
      (babel:octets-to-string octets :start start :end end :encoding :utf-8)
    (error ()
      (babel:octets-to-string octets :start start :end end :encoding :latin-1))))

(defun ensure-octets (thing)
  (etypecase thing
    (null nil)
    (string (string-to-octets thing))
    (vector (coerce thing 'octets))))

;;;; Identifiers

(defun random-hex (n-bytes)
  (ironclad:byte-array-to-hex-string (ironclad:random-data n-bytes)))

(defparameter +branch-cookie+ "z9hG4bK")

(defun make-branch ()
  (concatenate 'string +branch-cookie+ (random-hex 8)))

(defun make-tag ()
  (random-hex 5))

(defun make-call-id (&optional host)
  (if host
      (format nil "~a@~a" (random-hex 12) host)
      (random-hex 12)))

;;;; Dates (RFC 3261 section 20.17, always GMT)

(defun format-sip-date (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (sec min hour day month year dow)
      (decode-universal-time universal-time 0)
    (format nil "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT"
            (nth dow '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            day
            (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                              "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            year hour min sec)))
