(in-package :cl-sip.test)

(def-suite auth :in cl-sip)
(in-suite auth)

(test digest-rfc2617-vector
  ;; RFC 2617 section 3.5.
  (is (string= "6629fae49393a05397450978507c4ef1"
               (sip:digest-response :algorithm "MD5" :username "Mufasa"
                                    :realm "testrealm@host.com" :password "Circle Of Life"
                                    :method "GET" :uri "/dir/index.html"
                                    :nonce "dcd98b7102dd2f0e8b11d0f600bfb0c093"
                                    :nc "00000001" :cnonce "0a4f113b" :qop "auth"))))

(test digest-rfc7616-vectors
  ;; RFC 7616 section 3.9.1.
  (flet ((response (algorithm)
           (sip:digest-response :algorithm algorithm :username "Mufasa"
                                :realm "http-auth@example.org" :password "Circle of Life"
                                :method "GET" :uri "/dir/index.html"
                                :nonce "7ypf/xlj9XXwfDPEoM4URrv/xwf94BcCAzFZH4GiTo0v"
                                :nc "00000001"
                                :cnonce "f2/wE4q74E6zIJEtWaHKaf5wv/H5QzzpXusqGemxURZJ"
                                :qop "auth")))
    (is (string= "8ca523f5e9506fed4657c9700eebdbec" (response "MD5")))
    (is (string= "753927fa0e85d155564e2e272a28d1802ca10daf4496794697cf8db5856cb6c1"
                 (response "SHA-256")))))

(test challenge-parsing-and-formatting
  (let ((c (sip:parse-challenge
            "Digest realm=\"biloxi.com\", qop=\"auth,auth-int\", nonce=\"dcd98b\", opaque=\"5ccc\", algorithm=SHA-256, stale=TRUE")))
    (is (string= "biloxi.com" (sip:challenge-realm c)))
    (is (equal '("auth" "auth-int") (sip:challenge-qop c)))
    (is (string= "SHA-256" (sip:challenge-algorithm c)))
    (is (sip:challenge-stale c))
    (let ((again (sip:parse-challenge (sip:format-challenge c))))
      (is (equalp c again))))
  (signals sip:sip-parse-error (sip:parse-challenge "Digest")))

(test client-answers-verify-on-server
  (dolist (algorithm '("MD5" "MD5-sess" "SHA-256" "SHA-256-sess"))
    (dolist (qop '(nil ("auth") ("auth-int") ("auth-int" "auth")))
      (let* ((challenge (sip:generate-challenge "example.com" :algorithm algorithm :qop qop))
             (body (sip::string-to-octets "v=0"))
             (credentials (sip:parse-credentials
                           (sip:format-credentials
                            (sip:answer-challenge challenge :username "bob" :password "zanzibar"
                                                            :method "INVITE"
                                                            :uri "sip:bob@biloxi.com"
                                                            :body body)))))
        (is (sip:verify-credentials credentials :password "zanzibar" :method "INVITE" :body body)
            "~a with qop ~a" algorithm qop)
        (is (not (sip:verify-credentials credentials :password "wrong" :method "INVITE"
                                                     :body body)))
        (is (not (sip:verify-credentials credentials :password "zanzibar" :method "BYE"
                                                     :body body)))
        ;; "auth" is preferred when both are offered.
        (when (member "auth" qop :test #'string=)
          (is (string= "auth" (sip:credentials-qop credentials))))
        (when (equal qop '("auth-int"))
          (is (not (sip:verify-credentials credentials :password "zanzibar" :method "INVITE"
                                                       :body (sip::string-to-octets "v=1")))))))))

(test challenge-selection-rfc8760
  (let ((challenges (mapcar #'sip:parse-challenge
                            '("Digest realm=\"r\", nonce=\"n\", algorithm=SHA-512-256"
                              "Digest realm=\"r\", nonce=\"n\", algorithm=SHA-256"
                              "Digest realm=\"r\", nonce=\"n\", algorithm=MD5"))))
    ;; The first algorithm we implement wins; unsupported ones are skipped.
    (is (string= "SHA-256" (sip:challenge-algorithm (sip:select-challenge challenges))))
    (is (null (sip:select-challenge (list (first challenges)))))
    (is (null (sip:select-challenge (list (sip:parse-challenge "Basic realm=\"r\"")))))))
