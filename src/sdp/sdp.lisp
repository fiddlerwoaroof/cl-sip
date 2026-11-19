(defpackage :cl-sip.sdp
  (:use :cl)
  (:export
   #:sdp-parse-error
   #:session #:make-session #:copy-session #:session-p
   #:session-version #:session-origin #:session-name #:session-info #:session-uri
   #:session-emails #:session-phones #:session-connection #:session-bandwidths
   #:session-times #:session-zone #:session-key #:session-attributes #:session-media
   #:origin #:make-origin #:copy-origin #:origin-username #:origin-session-id
   #:origin-session-version #:origin-nettype #:origin-addrtype #:origin-address
   #:connection #:make-connection #:connection-nettype #:connection-addrtype
   #:connection-address
   #:media #:make-media #:copy-media #:media-type #:media-port #:media-port-count
   #:media-protocol #:media-formats #:media-title #:media-connection
   #:media-bandwidths #:media-key #:media-attributes
   #:parse-sdp #:sdp-string
   #:attribute #:attribute-values #:media-direction #:media-address
   #:codec #:make-codec #:copy-codec #:codec-payload-type #:codec-name
   #:codec-clock-rate #:codec-channels #:codec-fmtp
   #:media-codecs #:media-rejected-p
   ;; offer/answer
   #:*default-codecs* #:make-audio-offer #:make-answer #:hold #:resume
   #:bump-version #:negotiated-audio))

(in-package :cl-sip.sdp)

;;;; Session Description Protocol (RFC 8866)

(define-condition sdp-parse-error (error)
  ((message :initarg :message :reader sdp-parse-error-message))
  (:report (lambda (c s) (format s "SDP parse error: ~a" (sdp-parse-error-message c)))))

(defun sdp-fail (control &rest args)
  (error 'sdp-parse-error :message (apply #'format nil control args)))

(defstruct origin
  (username "-") (session-id "0") (session-version "0")
  (nettype "IN") (addrtype "IP4") (address "127.0.0.1"))

(defstruct connection
  (nettype "IN") (addrtype "IP4") (address "127.0.0.1"))

(defstruct media
  (type "audio") (port 0) (port-count nil) (protocol "RTP/AVP")
  (formats '())                         ; list of strings
  title connection (bandwidths '()) key
  (attributes '()))                     ; alist (name . value-or-nil)

(defstruct session
  (version 0) (origin (make-origin)) (name "-") info uri
  (emails '()) (phones '()) connection (bandwidths '())
  (times '(("0 0")))                    ; list of (t-value . r-values)
  zone key (attributes '()) (media '()))

(defun split-spaces (string)
  (loop with start = 0
        for space = (position #\Space string :start start)
        for piece = (subseq string start space)
        unless (string= piece "") collect piece
        while space
        do (setf start (1+ space))))

(defun parse-attribute (value)
  (let ((colon (position #\: value)))
    (if colon
        (cons (subseq value 0 colon) (subseq value (1+ colon)))
        (cons value nil))))

(defun parse-origin (value)
  (let ((fields (split-spaces value)))
    (unless (= (length fields) 6) (sdp-fail "malformed o= line: ~s" value))
    (destructuring-bind (username id version nettype addrtype address) fields
      (make-origin :username username :session-id id :session-version version
                   :nettype nettype :addrtype addrtype :address address))))

(defun parse-connection (value)
  (let ((fields (split-spaces value)))
    (unless (= (length fields) 3) (sdp-fail "malformed c= line: ~s" value))
    (make-connection :nettype (first fields) :addrtype (second fields)
                     :address (third fields))))

(defun parse-media-line (value)
  (let ((fields (split-spaces value)))
    (unless (>= (length fields) 4) (sdp-fail "malformed m= line: ~s" value))
    (let* ((port-field (second fields))
           (slash (position #\/ port-field))
           (port (parse-integer port-field :end slash :junk-allowed t))
           (count (and slash (parse-integer port-field :start (1+ slash) :junk-allowed t))))
      (unless port (sdp-fail "malformed port in m= line: ~s" value))
      (make-media :type (first fields) :port port :port-count count
                  :protocol (third fields) :formats (cdddr fields)))))

(defun parse-sdp (text)
  "Parse an SDP session description from a string or octet vector."
  (let* ((text (if (stringp text) text (babel:octets-to-string text :encoding :utf-8)))
         (lines (loop with start = 0
                      for nl = (position #\Newline text :start start)
                      for line = (string-right-trim '(#\Return)
                                                    (subseq text start nl))
                      unless (string= line "") collect line
                      while nl
                      do (setf start (1+ nl))))
         (session (make-session :times '()))
         (media nil)
         (seen-version nil))
    (dolist (line lines)
      (unless (and (>= (length line) 2) (char= (char line 1) #\=))
        (sdp-fail "malformed line: ~s" line))
      (let ((type (char line 0))
            (value (subseq line 2)))
        (if media
            ;; Media section.
            (case type
              (#\m (setf media (parse-media-line value))
               (push media (session-media session)))
              (#\i (setf (media-title media) value))
              (#\c (setf (media-connection media) (parse-connection value)))
              (#\b (push value (media-bandwidths media)))
              (#\k (setf (media-key media) value))
              (#\a (push (parse-attribute value) (media-attributes media)))
              (t (sdp-fail "unexpected ~a= line in media section" type)))
            (case type
              (#\v (unless (string= value "0") (sdp-fail "unsupported SDP version ~s" value))
               (setf seen-version t))
              (#\o (setf (session-origin session) (parse-origin value)))
              (#\s (setf (session-name session) value))
              (#\i (setf (session-info session) value))
              (#\u (setf (session-uri session) value))
              (#\e (push value (session-emails session)))
              (#\p (push value (session-phones session)))
              (#\c (setf (session-connection session) (parse-connection value)))
              (#\b (push value (session-bandwidths session)))
              (#\t (push (list value) (session-times session)))
              (#\r (unless (session-times session) (sdp-fail "r= before t="))
               (setf (first (session-times session))
                     (append (first (session-times session)) (list value))))
              (#\z (setf (session-zone session) value))
              (#\k (setf (session-key session) value))
              (#\a (push (parse-attribute value) (session-attributes session)))
              (#\m (setf media (parse-media-line value))
               (push media (session-media session)))
              (t nil)))))                ; unknown types are ignored (section 5)
    (unless seen-version (sdp-fail "missing v= line"))
    (macrolet ((fix (place) `(setf ,place (nreverse ,place))))
      (fix (session-emails session)) (fix (session-phones session))
      (fix (session-bandwidths session)) (fix (session-times session))
      (fix (session-attributes session)) (fix (session-media session))
      (dolist (m (session-media session))
        (fix (media-bandwidths m)) (fix (media-attributes m))))
    session))

(defun sdp-string (session)
  "Serialize SESSION with CRLF line endings."
  (with-output-to-string (out)
    (flet ((line (type value)
             (format out "~a=~a~c~c" type value #\Return #\Newline))
           (connection-value (c)
             (format nil "~a ~a ~a" (connection-nettype c) (connection-addrtype c)
                     (connection-address c)))
           (attribute-value (a)
             (if (cdr a) (format nil "~a:~a" (car a) (cdr a)) (car a))))
      (line #\v (session-version session))
      (let ((o (session-origin session)))
        (line #\o (format nil "~a ~a ~a ~a ~a ~a" (origin-username o) (origin-session-id o)
                          (origin-session-version o) (origin-nettype o)
                          (origin-addrtype o) (origin-address o))))
      (line #\s (session-name session))
      (when (session-info session) (line #\i (session-info session)))
      (when (session-uri session) (line #\u (session-uri session)))
      (dolist (e (session-emails session)) (line #\e e))
      (dolist (p (session-phones session)) (line #\p p))
      (when (session-connection session)
        (line #\c (connection-value (session-connection session))))
      (dolist (b (session-bandwidths session)) (line #\b b))
      (dolist (time (or (session-times session) '(("0 0"))))
        (line #\t (first time))
        (dolist (r (rest time)) (line #\r r)))
      (when (session-zone session) (line #\z (session-zone session)))
      (when (session-key session) (line #\k (session-key session)))
      (dolist (a (session-attributes session)) (line #\a (attribute-value a)))
      (dolist (m (session-media session))
        (line #\m (format nil "~a ~d~@[/~d~] ~a~{ ~a~}" (media-type m) (media-port m)
                          (media-port-count m) (media-protocol m) (media-formats m)))
        (when (media-title m) (line #\i (media-title m)))
        (when (media-connection m) (line #\c (connection-value (media-connection m))))
        (dolist (b (media-bandwidths m)) (line #\b b))
        (when (media-key m) (line #\k (media-key m)))
        (dolist (a (media-attributes m)) (line #\a (attribute-value a)))))))

;;;; Attribute helpers

(defun attributes-of (thing)
  (etypecase thing
    (media (media-attributes thing))
    (session (session-attributes thing))))

(defun attribute (thing name)
  "Value of the first attribute NAME of a MEDIA or SESSION.  Second value is
true when the attribute is present (property attributes have no value)."
  (let ((cell (assoc name (attributes-of thing) :test #'string-equal)))
    (values (cdr cell) (and cell t))))

(defun attribute-values (thing name)
  (loop for (n . v) in (attributes-of thing)
        when (string-equal n name) collect v))

(defparameter +directions+ '("sendrecv" "sendonly" "recvonly" "inactive"))

(defun media-direction (media &optional session)
  "Direction of MEDIA as a keyword, falling back to the session-level
attribute and then to :SENDRECV (RFC 8866 section 6.7)."
  (flet ((find-in (thing)
           (and thing
                (loop for d in +directions+
                      when (nth-value 1 (attribute thing d))
                        return (intern (string-upcase d) :keyword)))))
    (or (find-in media) (find-in session) :sendrecv)))

(defun media-address (media &optional session)
  "Connection address for MEDIA, falling back to the session-level c= line."
  (let ((c (or (media-connection media) (and session (session-connection session)))))
    (and c (connection-address c))))

(defun media-rejected-p (media)
  (zerop (media-port media)))

;;;; Codecs (rtpmap / fmtp, with the static table of RFC 3551 section 6)

(defstruct codec
  payload-type (name "") (clock-rate 8000) (channels 1) fmtp)

(defparameter +static-payload-types+
  '((0 "PCMU" 8000 1) (3 "GSM" 8000 1) (4 "G723" 8000 1) (5 "DVI4" 8000 1)
    (6 "DVI4" 16000 1) (7 "LPC" 8000 1) (8 "PCMA" 8000 1) (9 "G722" 8000 1)
    (10 "L16" 44100 2) (11 "L16" 44100 1) (12 "QCELP" 8000 1) (13 "CN" 8000 1)
    (14 "MPA" 90000 1) (15 "G728" 8000 1) (16 "DVI4" 11025 1) (17 "DVI4" 22050 1)
    (18 "G729" 8000 1) (25 "CelB" 90000 1) (26 "JPEG" 90000 1) (28 "nv" 90000 1)
    (31 "H261" 90000 1) (32 "MPV" 90000 1) (33 "MP2T" 90000 1) (34 "H263" 90000 1)))

(defun parse-rtpmap (value)
  "\"96 opus/48000/2\" -> payload type, name, clock rate, channels."
  (let* ((space (position #\Space value))
         (pt (and space (parse-integer value :end space :junk-allowed t)))
         (parts (and space
                     (loop with rest = (string-left-trim " " (subseq value space))
                           with start = 0
                           for slash = (position #\/ rest :start start)
                           collect (subseq rest start slash)
                           while slash
                           do (setf start (1+ slash))))))
    (when (and pt parts)
      (values pt (first parts)
              (or (and (second parts) (parse-integer (second parts) :junk-allowed t)) 8000)
              (or (and (third parts) (parse-integer (third parts) :junk-allowed t)) 1)))))

(defun media-codecs (media)
  "The formats of an RTP MEDIA description as CODEC structures, in order of
preference."
  (let ((rtpmaps (loop for value in (attribute-values media "rtpmap")
                       collect (multiple-value-list (parse-rtpmap value))))
        (fmtps (loop for value in (attribute-values media "fmtp")
                     for space = (position #\Space value)
                     when space
                       collect (cons (parse-integer value :end space :junk-allowed t)
                                     (string-left-trim " " (subseq value space))))))
    (loop for format in (media-formats media)
          for pt = (parse-integer format :junk-allowed t)
          for map = (and pt (or (assoc pt rtpmaps) (assoc pt +static-payload-types+)))
          when pt
            collect (make-codec :payload-type pt
                                :name (if map (second map) "")
                                :clock-rate (if map (third map) 8000)
                                :channels (if map (fourth map) 1)
                                :fmtp (cdr (assoc pt fmtps))))))
