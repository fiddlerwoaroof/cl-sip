(in-package :cl-sip)

;;;; Octets -> message (RFC 3261 sections 7 and 25)

(defconstant +cr+ 13)
(defconstant +lf+ 10)
(defparameter +ws-bag+ (list #\Space #\Tab))

(defun skip-leading-crlf (octets start end)
  (loop while (and (< start end)
                   (or (= (aref octets start) +cr+) (= (aref octets start) +lf+)))
        do (incf start))
  start)

(defun find-head-end (octets start end)
  "Locate the blank line ending the header block.  Returns the index of the
first octet of the terminator and the index just past it, or NIL."
  (loop for i from start below end
        do (when (= (aref octets i) +lf+)
             (cond ((and (< (+ i 2) end)
                         (= (aref octets (+ i 1)) +cr+)
                         (= (aref octets (+ i 2)) +lf+))
                    (return (values (1+ i) (+ i 3))))
                   ((and (< (+ i 1) end) (= (aref octets (+ i 1)) +lf+))
                    (return (values (1+ i) (+ i 2))))))
        finally (return nil)))

(defun split-lines (string)
  "Split on CRLF or bare LF and unfold continuation lines (section 7.3.1)."
  (let ((lines '()))
    (loop with start = 0
          for nl = (position #\Newline string :start start)
          do (let* ((end (or nl (length string)))
                    (line (string-right-trim '(#\Return) (subseq string start end))))
               (cond ((and lines (plusp (length line)) (ws-char-p (char line 0)))
                      (setf (first lines)
                            (concatenate 'string (string-right-trim +ws-bag+ (first lines))
                                         " " (string-left-trim +ws-bag+ line))))
                     ((or nl (plusp (length line)))
                      (push line lines))))
             (if nl (setf start (1+ nl)) (return)))
    (nreverse lines)))

(defun parse-start-line (line)
  (cond
    ((and (>= (length line) 4) (string-equal "SIP/" line :end2 4))
     ;; Status-Line = SIP-Version SP Status-Code SP Reason-Phrase
     (let ((sp1 (position #\Space line)))
       (unless sp1 (parse-fail "malformed status line: ~s" line))
       (unless (string-equal (subseq line 0 sp1) "SIP/2.0")
         (error 'sip-parse-error :status 505
                                 :message (format nil "unsupported version in ~s" line)))
       (let* ((sp2 (position #\Space line :start (1+ sp1)))
              (code (subseq line (1+ sp1) sp2)))
         (unless (and (= (length code) 3) (all-digits-p code))
           (parse-fail "malformed status code: ~s" line))
         (make-instance 'response :status (parse-integer code)
                                  :reason (if sp2 (subseq line (1+ sp2)) "")))))
    (t
     ;; Request-Line = Method SP Request-URI SP SIP-Version
     (let* ((sp1 (position #\Space line))
            (sp2 (and sp1 (position #\Space line :start (1+ sp1)))))
       (unless (and sp1 sp2 (not (position #\Space line :start (1+ sp2))))
         (parse-fail "malformed request line: ~s" line))
       (let ((method (subseq line 0 sp1))
             (uri (subseq line (1+ sp1) sp2))
             (version (subseq line (1+ sp2))))
         (unless (token-p method) (parse-fail "invalid method: ~s" method))
         (unless (and (> (length version) 4) (string-equal "SIP/" version :end2 4))
           (parse-fail "malformed request line: ~s" line))
         (unless (string-equal version "SIP/2.0")
           (error 'sip-parse-error :status 505
                                   :message (format nil "unsupported version ~s" version)))
         (make-instance 'request :method method :uri (parse-uri uri)))))))

(defun parse-header-line (line)
  "Returns a list of (canonical-name . value) entries."
  (let ((colon (position #\: line)))
    (unless colon (parse-fail "header line without colon: ~s" line))
    (let ((name (string-right-trim +ws-bag+ (subseq line 0 colon)))
          (value (trim-ws (subseq line (1+ colon)))))
      (unless (token-p name) (parse-fail "invalid header name: ~s" name))
      (let ((name (canonical-header-name name)))
        (if (list-header-p name)
            (or (loop for piece in (split-top-level value #\,)
                      for trimmed = (trim-ws piece)
                      unless (string= trimmed "")
                        collect (cons name trimmed))
                (list (cons name "")))
            (list (cons name value)))))))

(defun parse-message (octets &key (start 0) end stream-p)
  "Parse one SIP message from OCTETS.  Returns the message and the index just
past it.  With STREAM-P, returns NIL when the buffer does not yet hold a
complete message; without it the input is treated as one datagram: a missing
Content-Length means the body runs to the end, and trailing octets beyond the
declared length are ignored."
  (let* ((octets (if (stringp octets) (string-to-octets octets) octets))
         (end (or end (length octets)))
         (start (skip-leading-crlf octets start end)))
    (when (and stream-p (= start end))
      (return-from parse-message (values nil start)))
    (multiple-value-bind (head-end body-start) (find-head-end octets start end)
      (unless head-end
        (if stream-p
            (return-from parse-message (values nil start))
            (setf head-end end body-start end)))
      (let* ((lines (split-lines (octets-to-string octets :start start :end head-end)))
             (message (if lines
                          (parse-start-line (first lines))
                          (parse-fail "empty message")))
             (headers (loop for line in (rest lines)
                            append (parse-header-line line)))
             (lengths (remove-duplicates
                       (loop for (name . value) in headers
                             when (string= name "Content-Length")
                               collect (parse-decimal value :what "Content-Length"))))
             (available (- end body-start))
             (body-length (cond ((rest lengths) (parse-fail "conflicting Content-Length values"))
                                (lengths (first lengths))
                                (stream-p 0)
                                (t available))))
        (when (> body-length available)
          (if stream-p
              (return-from parse-message (values nil start))
              (parse-fail "Content-Length ~d exceeds the ~d octets received"
                          body-length available)))
        (setf (message-headers message)
              (remove "Content-Length" headers :key #'car :test #'string=)
              (message-body message)
              (and (plusp body-length)
                   (subseq octets body-start (+ body-start body-length))))
        (values message (+ body-start body-length))))))

;;;; Semantic validation

(defun validate-message (message)
  "Check the header fields every message must carry (section 8.1.1) and that
they parse.  Returns NIL when acceptable, else a reason string."
  (handler-case
      (progn
        (dolist (name '("To" "From" "CSeq" "Call-ID"))
          (let ((values (header-values message name)))
            (cond ((null values) (parse-fail "missing ~a header" name))
                  ((rest values) (parse-fail "multiple ~a headers" name)))))
        (unless (header message "Via") (parse-fail "missing Via header"))
        (msg-vias message)
        (msg-from message)
        (msg-to message)
        (when (string= (trim-ws (msg-call-id message)) "")
          (parse-fail "empty Call-ID"))
        (let ((cseq (msg-cseq message)))
          (when (and (request-p message)
                     (string/= (cseq-method cseq) (request-method message)))
            (parse-fail "CSeq method ~a does not match request method ~a"
                        (cseq-method cseq) (request-method message))))
        (dolist (name '("Max-Forwards" "Expires"))
          (let ((raw (header message name)))
            (when raw (parse-decimal raw :what name :max (1- (expt 2 32))))))
        (let ((mf (msg-max-forwards message)))
          (when (and mf (> mf 255)) (parse-fail "Max-Forwards out of range")))
        (msg-contacts message)
        (msg-routes message)
        (msg-record-routes message)
        nil)
    (sip-parse-error (e) (sip-parse-error-message e))))

;;;; Stream framing (section 18.3)

(defclass framer ()
  ((buffer :initform (make-array 0 :element-type '(unsigned-byte 8))
           :accessor framer-buffer))
  (:documentation "Accumulates octets from a stream transport and yields
complete messages delimited by Content-Length."))

(defun framer-push (framer octets &key (start 0) (end (length octets)))
  (let* ((old (framer-buffer framer))
         (new (make-array (+ (length old) (- end start)) :element-type '(unsigned-byte 8))))
    (replace new old)
    (replace new octets :start1 (length old) :start2 start :end2 end)
    (setf (framer-buffer framer) new)
    framer))

(defun framer-next (framer)
  "Next complete message in the buffer, or NIL.  May signal SIP-PARSE-ERROR,
after which the stream is unusable."
  (let ((buffer (framer-buffer framer)))
    (multiple-value-bind (message consumed) (parse-message buffer :stream-p t)
      (setf (framer-buffer framer) (subseq buffer consumed))
      message)))
