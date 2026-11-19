(in-package :cl-sip.test)

(def-suite message :in cl-sip)
(in-suite message)

(defparameter *sdp-body*
  (format nil "v=0~c~co=alice 2890844526 2890844526 IN IP4 client.atlanta.example.com~c~cs=-~c~cc=IN IP4 192.0.2.101~c~ct=0 0~c~cm=audio 49172 RTP/AVP 0~c~ca=rtpmap:0 PCMU/8000~c~c"
          #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline
          #\Return #\Newline #\Return #\Newline #\Return #\Newline))

(defun rfc3665-invite ()
  "RFC 3665 section 3.1, message F1."
  (concatenate
   'string
   (crlf "INVITE sip:bob@biloxi.example.com SIP/2.0"
         "Via: SIP/2.0/TCP client.atlanta.example.com:5060;branch=z9hG4bK74bf9"
         "Max-Forwards: 70"
         "From: Alice <sip:alice@atlanta.example.com>;tag=9fxced76sl"
         "To: Bob <sip:bob@biloxi.example.com>"
         "Call-ID: 3848276298220188511@atlanta.example.com"
         "CSeq: 1 INVITE"
         "Contact: <sip:alice@client.atlanta.example.com;transport=tcp>"
         "Content-Type: application/sdp"
         (format nil "Content-Length: ~d" (length *sdp-body*)))
   *sdp-body*))

(test parse-request-rfc3665-3.1
  (let ((m (sip:parse-message (rfc3665-invite))))
    (is (sip:request-p m))
    (is (string= "INVITE" (sip:request-method m)))
    (is (string= "sip:bob@biloxi.example.com" (sip:uri-string (sip:request-uri m))))
    (is (null (sip:validate-message m)))
    (let ((via (sip:msg-top-via m)))
      (is (string= "TCP" (sip:via-transport via)))
      (is (string= "client.atlanta.example.com" (sip:via-host via)))
      (is (= 5060 (sip:via-port via)))
      (is (string= "z9hG4bK74bf9" (sip:via-branch via))))
    (is (string= "Alice" (sip:name-addr-display-name (sip:msg-from m))))
    (is (string= "9fxced76sl" (sip:msg-from-tag m)))
    (is (null (sip:msg-to-tag m)))
    (is (= 1 (sip:cseq-number (sip:msg-cseq m))))
    (is (= 70 (sip:msg-max-forwards m)))
    (is (string= "application/sdp" (sip:msg-content-type m)))
    (is (string= *sdp-body* (sip:message-body-string m)))
    ;; Serializing and parsing again gives the same message.
    (let ((again (sip:parse-message (sip:serialize-message m))))
      (is (equal (sip:message-headers m) (sip:message-headers again)))
      (is (equalp (sip:message-body m) (sip:message-body again))))))

(test parse-response-rfc3665-2.1
  (let ((m (sip:parse-message
            (crlf "SIP/2.0 401 Unauthorized"
                  "Via: SIP/2.0/TLS client.biloxi.example.com:5061;branch=z9hG4bKnashds7"
                  " ;received=192.0.2.201"
                  "From: Bob <sips:bob@biloxi.example.com>;tag=a73kszlfl"
                  "To: Bob <sips:bob@biloxi.example.com>;tag=1410948204"
                  "Call-ID: 1j9FpLxk3uxtm8tn@biloxi.example.com"
                  "CSeq: 1 REGISTER"
                  "WWW-Authenticate: Digest realm=\"atlanta.example.com\", qop=\"auth\","
                  " nonce=\"ea9c8e88df84f1cec4341ae6cbe5a359\","
                  " opaque=\"\", stale=FALSE, algorithm=MD5"
                  "Content-Length: 0"))))
    (is (sip:response-p m))
    (is (= 401 (sip:response-status m)))
    (is (string= "Unauthorized" (sip:response-reason m)))
    (is (string= "192.0.2.201" (sip:via-param (sip:msg-top-via m) "received")))
    (is (string= "1410948204" (sip:msg-to-tag m)))
    ;; The challenge is one header value even though it contains commas.
    (is (= 1 (length (sip:header-values m "WWW-Authenticate"))))
    (let ((c (sip:parse-challenge (sip:header m "WWW-Authenticate"))))
      (is (string= "atlanta.example.com" (sip:challenge-realm c)))
      (is (string= "ea9c8e88df84f1cec4341ae6cbe5a359" (sip:challenge-nonce c)))
      (is (equal '("auth") (sip:challenge-qop c)))
      (is (string= "" (sip:challenge-opaque c)))
      (is (not (sip:challenge-stale c))))))

(test compact-forms-and-lists
  (let ((m (sip:parse-message
            (crlf "OPTIONS sip:carol@chicago.example.com SIP/2.0"
                  "v: SIP/2.0/UDP a.example.com;branch=z9hG4bK1, SIP/2.0/UDP b.example.com;branch=z9hG4bK2"
                  "f: <sip:a@example.com>;tag=1"
                  "t: sip:carol@chicago.example.com"
                  "i: abc"
                  "CSeq: 7 OPTIONS"
                  "m: \"Smith, J\" <sip:j@example.com>;q=0.7, <sip:k@example.com>"
                  "k: 100rel, timer"
                  "l: 0"))))
    (is (equal '("a.example.com" "b.example.com") (mapcar #'sip:via-host (sip:msg-vias m))))
    (is (string= "abc" (sip:msg-call-id m)))
    (is (= 2 (length (sip:msg-contacts m))))
    (is (string= "Smith, J" (sip:name-addr-display-name (first (sip:msg-contacts m)))))
    (is (string= "0.7" (sip:param (sip:name-addr-params (first (sip:msg-contacts m))) "q")))
    (is (equal '("100rel" "timer") (sip:header-values m "Supported")))
    ;; Long names on output.
    (is (search "Call-ID: abc" (sip:message-string m)))))

(test torture-short-tortuous-invite
  ;; RFC 4475 section 3.1.1.1, with a body of our own.
  (let* ((body "0123456789")
         (m (sip:parse-message
             (concatenate
              'string
              (crlf "INVITE sip:vivekg@chair-dnrc.example.com;unknownparam SIP/2.0"
                    "TO :"
                    " sip:vivekg@chair-dnrc.example.com ;   tag    = 1918181833n"
                    "from   : \"J Rosenberg \\\\\\\"\"       <sip:jdrosen@example.com>"
                    "  ;"
                    "  tag = 98asjd8"
                    "MaX-fOrWaRdS: 0068"
                    "Call-ID: wsinv.ndaksdj@192.0.2.1"
                    "Content-Length   : 10"
                    "cseq: 0009"
                    "  INVITE"
                    "Via  : SIP  /   2.0"
                    " /UDP"
                    "    192.0.2.2;branch=390skdjuw"
                    "s :"
                    "NewFangledHeader:   newfangled value"
                    " continued newfangled value"
                    "UnknownHeaderWithUnusualValue: ;;,,;;,;"
                    "Content-Type: application/sdp"
                    "Route:"
                    " <sip:services.example.com;lr;unknownwith=value;unknown-no-value>"
                    "v:  SIP  / 2.0  / TCP     spindle.example.com   ;"
                    "  branch  =   z9hG4bK9ikj8  ,"
                    " SIP  /    2.0   / UDP  192.168.255.111   ; branch="
                    " z9hG4bK30239"
                    "m:\"Quoted string \\\"\\\"\" <sip:jdrosen@example.com> ; newparam ="
                    "      newvalue ;"
                    "  secondparam ; q = 0.33")
              body))))
    (is (null (sip:validate-message m)))
    (is-true (nth-value 1 (sip:uri-param (sip:request-uri m) "unknownparam")))
    (is (string= "1918181833n" (sip:msg-to-tag m)))
    (is (string= "J Rosenberg \\\"" (sip:name-addr-display-name (sip:msg-from m))))
    (is (string= "98asjd8" (sip:msg-from-tag m)))
    (is (= 68 (sip:msg-max-forwards m)))
    (is (= 9 (sip:cseq-number (sip:msg-cseq m))))
    (is (equal '("192.0.2.2" "spindle.example.com" "192.168.255.111")
               (mapcar #'sip:via-host (sip:msg-vias m))))
    (is (equal '("390skdjuw" "z9hG4bK9ikj8" "z9hG4bK30239")
               (mapcar #'sip:via-branch (sip:msg-vias m))))
    (is (equal '("UDP" "TCP" "UDP") (mapcar #'sip:via-transport (sip:msg-vias m))))
    (is (string= "" (sip:header m "Subject")))
    (is (string= "newfangled value continued newfangled value"
                 (sip:header m "NewFangledHeader")))
    (is (string= ";;,,;;,;" (sip:header m "UnknownHeaderWithUnusualValue")))
    (let ((route (sip:name-addr-uri (first (sip:msg-routes m)))))
      (is (string= "value" (sip:uri-param route "unknownwith")))
      (is-true (nth-value 1 (sip:uri-param route "lr"))))
    (let ((contact (first (sip:msg-contacts m))))
      (is (string= "Quoted string \"\"" (sip:name-addr-display-name contact)))
      (is (string= "newvalue" (sip:param (sip:name-addr-params contact) "newparam")))
      (is (string= "0.33" (sip:param (sip:name-addr-params contact) "q")))
      (is-true (nth-value 1 (sip:param (sip:name-addr-params contact) "secondparam"))))
    (is (string= body (sip:message-body-string m)))))

(defun minimal-request (&key (start "OPTIONS sip:user@example.com SIP/2.0") drop extra
                          (cseq "CSeq: 1 OPTIONS") (body ""))
  (concatenate
   'string
   (apply #'crlf
          (append
           (list start)
           (remove-if (lambda (line) (and drop (alexandria:starts-with-subseq drop line)))
                      (list "Via: SIP/2.0/UDP host.example.com;branch=z9hG4bKkdjuw"
                            "To: sip:user@example.com"
                            "From: sip:caller@example.net;tag=323"
                            "Max-Forwards: 70"
                            "Call-ID: test.0ha0isndaksdj@192.0.2.1"
                            cseq))
           extra))
   body))

(test torture-valid-variants
  ;; 3.1.1.8: octets beyond Content-Length in a datagram are ignored.
  (let ((m (sip:parse-message (minimal-request :extra '("Content-Length: 3") :body "abcdef"))))
    (is (string= "abc" (sip:message-body-string m))))
  ;; No Content-Length in a datagram: the body runs to the end.
  (is (string= "abcdef" (sip:message-body-string
                         (sip:parse-message (minimal-request :body "abcdef")))))
  ;; 3.1.1.6: no LWS between display name and '<'.
  (is (string= "caller"
               (sip:name-addr-display-name
                (sip:parse-name-addr "caller<sip:caller@example.com>;tag=323"))))
  ;; 3.1.1.12 / 3.1.1.13: unusual and empty reason phrases.
  (is (string= "= 2**3 * 5**2 "
               (sip:response-reason
                (sip:parse-message (crlf "SIP/2.0 200 = 2**3 * 5**2 " "Via: SIP/2.0/UDP h")))))
  (is (string= "" (sip:response-reason
                   (sip:parse-message (crlf "SIP/2.0 100 " "Via: SIP/2.0/UDP h")))))
  ;; Leading CRLFs before the start line are skipped (section 7.5).
  (is (sip:request-p (sip:parse-message
                      (concatenate 'string (crlf) (minimal-request))))))

(test torture-invalid-framing
  (flet ((bad (text &optional (status 400))
           (handler-case (progn (sip:parse-message text) nil)
             (sip:sip-parse-error (e) (= status (sip:sip-parse-error-status e))))))
    ;; 3.1.2.2: Content-Length larger than the message.
    (is (bad (minimal-request :extra '("Content-Length: 9999") :body "abc")))
    ;; 3.1.2.3: negative Content-Length.
    (is (bad (minimal-request :extra '("Content-Length: -999") :body "abc")))
    ;; 3.3.9: conflicting Content-Length values.
    (is (bad (minimal-request :extra '("Content-Length: 3" "Content-Length: 5") :body "abcde")))
    ;; 3.1.2.7: <> around the Request-URI.
    (is (bad (minimal-request :start "OPTIONS <sip:user@example.com> SIP/2.0")))
    ;; 3.1.2.8: embedded LWS in the Request-URI.
    (is (bad (minimal-request :start "OPTIONS sip:user@example.com; lr SIP/2.0")))
    ;; 3.1.2.9: multiple SP between request-line elements.
    (is (bad (minimal-request :start "OPTIONS  sip:user@example.com SIP/2.0")))
    ;; 3.1.2.10: trailing SP on the request line.
    (is (bad (minimal-request :start "OPTIONS sip:user@example.com SIP/2.0 ")))
    ;; 3.1.2.16: unknown protocol version.
    (is (bad (minimal-request :start "OPTIONS sip:user@example.com SIP/7.0") 505))
    ;; 3.1.2.19: overlarge response code.
    (is (bad (crlf "SIP/2.0 4294967301 better not break the receiver" "Via: SIP/2.0/UDP h")))
    ;; Header line without a colon.
    (is (bad (minimal-request :extra '("this is not a header"))))))

(test torture-invalid-semantics
  (flet ((problem (&rest args) (sip:validate-message (sip:parse-message (apply #'minimal-request args)))))
    (is (null (problem)))
    ;; 3.3.1: missing required header fields.
    (is (problem :drop "Call-ID"))
    (is (problem :drop "From"))
    (is (problem :drop "To"))
    (is (problem :drop "Via"))
    (is (problem :drop "CSeq" :cseq "X-Nothing: 1"))
    ;; 3.1.2.17: start line and CSeq method mismatch.
    (is (problem :cseq "CSeq: 1 INVITE"))
    ;; 3.1.2.4: overlarge scalar values.
    (is (problem :cseq "CSeq: 36893488147419103232 OPTIONS"))
    (is (problem :extra '("Expires: 1000000000000000000000")))
    (is (problem :drop "Max-Forwards" :extra '("Max-Forwards: 300")))
    ;; 3.3.8: multiple values in single-value fields.
    (is (problem :extra '("To: sip:other@example.com")))
    ;; 3.1.2.6: unterminated quoted string in a display name.
    (is (problem :drop "To" :extra '("To: \"Mr. J. User <sip:j.user@example.com>")))
    ;; 3.1.2.14: spaces within addr-spec.
    (is (problem :drop "To" :extra '("To: \"Joe\" <sip:joe@example.org   <sip:x@y>")))))

(test header-editing
  (let ((m (sip:make-request "OPTIONS" "sip:a@b.example"
                             :headers '(("f" "<sip:x@y>;tag=1") ("X-One" . "1")))))
    (is (string= "<sip:x@y>;tag=1" (sip:header m "From")))
    (sip:add-header m "Via" "SIP/2.0/UDP one;branch=z9hG4bK1")
    (sip:prepend-header m "Via" "SIP/2.0/UDP two;branch=z9hG4bK2")
    (is (string= "two" (sip:via-host (sip:msg-top-via m))))
    (setf (sip:header m "x-one") "2")
    (is (equal '("2") (sip:header-values m "X-One")))
    (setf (sip:header m "X-One") nil)
    (is (null (sip:header m "X-One")))
    (sip:remove-headers m "Via")
    (is (null (sip:msg-vias m)))))

(test response-construction-8.2.6.2
  (let* ((request (sip:parse-message (rfc3665-invite)))
         (response (sip:make-response-for request 180 :to-tag "8321234356")))
    (is (equal (sip:header-values request "Via") (sip:header-values response "Via")))
    (is (string= (sip:header request "From") (sip:header response "From")))
    (is (string= (sip:header request "Call-ID") (sip:header response "Call-ID")))
    (is (string= (sip:header request "CSeq") (sip:header response "CSeq")))
    (is (string= "8321234356" (sip:msg-to-tag response)))
    (is (string= "Ringing" (sip:response-reason response)))
    ;; A tag already present is kept.
    (let ((again (sip:make-response-for
                  (sip:parse-message (minimal-request :drop "To"
                                                      :extra '("To: <sip:u@example.com>;tag=abc")))
                  200 :to-tag "other")))
      (is (string= "abc" (sip:msg-to-tag again))))))

(test stream-framing-18.3
  (let* ((one (sip:serialize-message (sip:parse-message (rfc3665-invite))))
         (two (sip:serialize-message (sip:parse-message (minimal-request))))
         (framer (make-instance 'sip:framer)))
    ;; A message arriving in pieces.
    (sip:framer-push framer one :end 40)
    (is (null (sip:framer-next framer)))
    (sip:framer-push framer one :start 40 :end (- (length one) 5))
    (is (null (sip:framer-next framer)))
    ;; The rest, a keep-alive CRLF and a second message in one segment.
    (sip:framer-push framer one :start (- (length one) 5))
    (sip:framer-push framer (coerce #(13 10 13 10) '(vector (unsigned-byte 8))))
    (sip:framer-push framer two)
    (let ((first (sip:framer-next framer))
          (second (sip:framer-next framer)))
      (is (string= "INVITE" (sip:request-method first)))
      (is (string= *sdp-body* (sip:message-body-string first)))
      (is (string= "OPTIONS" (sip:request-method second)))
      (is (null (sip:framer-next framer))))))
