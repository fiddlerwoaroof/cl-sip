(in-package :cl-sip.test)

(def-suite dialog :in cl-sip)
(in-suite dialog)

(defun dialog-invite ()
  (sip:parse-message
   (crlf "INVITE sip:bob@biloxi.example.com SIP/2.0"
         "Via: SIP/2.0/UDP client.atlanta.example.com:5060;branch=z9hG4bK74bf9"
         "Max-Forwards: 70"
         "From: Alice <sip:alice@atlanta.example.com>;tag=9fxced76sl"
         "To: Bob <sip:bob@biloxi.example.com>"
         "Call-ID: 2xTb9vxSit55XU7p8@atlanta.example.com"
         "CSeq: 314159 INVITE"
         "Contact: <sip:alice@client.atlanta.example.com>")))

(defun dialog-200 (&rest record-routes)
  (sip:parse-message
   (apply #'crlf
          (append
           (list "SIP/2.0 200 OK"
                 "Via: SIP/2.0/UDP client.atlanta.example.com:5060;branch=z9hG4bK74bf9")
           (mapcar (lambda (rr) (format nil "Record-Route: ~a" rr)) record-routes)
           (list "From: Alice <sip:alice@atlanta.example.com>;tag=9fxced76sl"
                 "To: Bob <sip:bob@biloxi.example.com>;tag=314159"
                 "Call-ID: 2xTb9vxSit55XU7p8@atlanta.example.com"
                 "CSeq: 314159 INVITE"
                 "Contact: <sip:bob@client.biloxi.example.com>")))))

(test uac-dialog-state-12.1.2
  (let ((dialog (sip:make-uac-dialog
                 (dialog-invite)
                 (dialog-200 "<sip:ss2.biloxi.example.com;lr>" "<sip:ss1.atlanta.example.com;lr>"))))
    (is (equal '("2xTb9vxSit55XU7p8@atlanta.example.com" "9fxced76sl" "314159")
               (sip:dialog-id dialog)))
    (is (eq :confirmed (sip:dialog-state dialog)))
    (is (= 314159 (sip:dialog-local-seq dialog)))
    (is (null (sip:dialog-remote-seq dialog)))
    (is (string= "sip:bob@client.biloxi.example.com"
                 (sip:uri-string (sip:dialog-remote-target dialog))))
    ;; The route set is the Record-Route list reversed.
    (is (equal '("sip:ss1.atlanta.example.com;lr" "sip:ss2.biloxi.example.com;lr")
               (mapcar (lambda (r) (sip:uri-string (sip:name-addr-uri r)))
                       (sip:dialog-route-set dialog))))
    ;; A provisional response gives an early dialog.
    (let ((ringing (dialog-200)))
      (setf (sip:response-status ringing) 180)
      (is (eq :early (sip:dialog-state (sip:make-uac-dialog (dialog-invite) ringing)))))))

(test in-dialog-request-loose-routing-12.2.1.1
  (let* ((dialog (sip:make-uac-dialog
                  (dialog-invite)
                  (dialog-200 "<sip:ss2.biloxi.example.com;lr>" "<sip:ss1.atlanta.example.com;lr>")))
         (bye (sip:dialog-make-request dialog "BYE")))
    (is (string= "sip:bob@client.biloxi.example.com" (sip:uri-string (sip:request-uri bye))))
    (is (equal '("<sip:ss1.atlanta.example.com;lr>" "<sip:ss2.biloxi.example.com;lr>")
               (sip:header-values bye "Route")))
    (is (string= "9fxced76sl" (sip:msg-from-tag bye)))
    (is (string= "314159" (sip:msg-to-tag bye)))
    (is (string= "2xTb9vxSit55XU7p8@atlanta.example.com" (sip:msg-call-id bye)))
    (is (string= "314160 BYE" (sip:header bye "CSeq")))
    ;; The next hop is the first route, not the remote target.
    (is (string= "sip:ss1.atlanta.example.com;lr" (sip:uri-string (sip::next-hop-uri bye))))
    ;; An ACK reuses the INVITE's sequence number and does not advance it.
    (is (string= "314159 ACK"
                 (sip:header (sip:dialog-make-request dialog "ACK" :cseq 314159) "CSeq")))
    (is (string= "314161 INFO" (sip:header (sip:dialog-make-request dialog "INFO") "CSeq")))))

(test in-dialog-request-strict-routing-12.2.1.1
  ;; First route without ;lr: it becomes the Request-URI and the remote
  ;; target moves to the end of the Route list (RFC 3261 section 16.12.1.2).
  (let* ((dialog (sip:make-uac-dialog
                  (dialog-invite)
                  (dialog-200 "<sip:p2.example.com;lr>" "<sip:p1.example.com>")))
         (bye (sip:dialog-make-request dialog "BYE")))
    (is (string= "sip:p1.example.com" (sip:uri-string (sip:request-uri bye))))
    (is (equal '("<sip:p2.example.com;lr>" "<sip:bob@client.biloxi.example.com>")
               (sip:header-values bye "Route")))
    (is (string= "sip:p1.example.com" (sip:uri-string (sip::next-hop-uri bye))))))

(test uas-dialog-state-12.1.1
  (let* ((invite (dialog-invite))
         (dialog (progn
                   (sip:add-header invite "Record-Route" "<sip:ss1.atlanta.example.com;lr>")
                   (sip:add-header invite "Record-Route" "<sip:ss2.biloxi.example.com;lr>")
                   (sip:make-uas-dialog invite "314159"))))
    (is (equal '("2xTb9vxSit55XU7p8@atlanta.example.com" "314159" "9fxced76sl")
               (sip:dialog-id dialog)))
    (is (eq :early (sip:dialog-state dialog)))
    (is (= 314159 (sip:dialog-remote-seq dialog)))
    (is (null (sip:dialog-local-seq dialog)))
    (is (string= "sip:alice@client.atlanta.example.com"
                 (sip:uri-string (sip:dialog-remote-target dialog))))
    ;; For the UAS the route set keeps the Record-Route order.
    (is (equal '("sip:ss1.atlanta.example.com;lr" "sip:ss2.biloxi.example.com;lr")
               (mapcar (lambda (r) (sip:uri-string (sip:name-addr-uri r)))
                       (sip:dialog-route-set dialog))))
    (let ((bye (sip:dialog-make-request dialog "BYE")))
      (is (string= "sip:alice@client.atlanta.example.com" (sip:uri-string (sip:request-uri bye))))
      (is (string= "314159" (sip:msg-from-tag bye)))
      (is (string= "9fxced76sl" (sip:msg-to-tag bye)))
      (is (string= "Bob" (sip:name-addr-display-name (sip:msg-from bye)))))
    ;; Remote sequence numbers must not go backwards (section 12.2.2).
    (flet ((in-dialog (n method)
             (sip:make-request method "sip:bob@biloxi.example.com"
                               :headers `(("CSeq" ,(format nil "~d ~a" n method))))))
      (is (sip::dialog-check-remote-cseq dialog (in-dialog 314160 "BYE")))
      (is (not (sip::dialog-check-remote-cseq dialog (in-dialog 314159 "INFO"))))
      (is (sip::dialog-check-remote-cseq dialog (in-dialog 314159 "ACK")))
      (is (= 314160 (sip:dialog-remote-seq dialog))))))
