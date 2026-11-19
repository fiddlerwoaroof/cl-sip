(in-package :cl-sip.test)

(def-suite uri :in cl-sip)
(in-suite uri)

(test uri-examples-19.1.3
  (dolist (text '("sip:alice@atlanta.com"
                  "sip:alice:secretword@atlanta.com;transport=tcp"
                  "sips:alice@atlanta.com?subject=project%20x&priority=urgent"
                  "sip:+1-212-555-1212:1234@gateway.com;user=phone"
                  "sips:1212@gateway.com"
                  "sip:alice@192.0.2.4"
                  "sip:atlanta.com;method=REGISTER?to=alice%40atlanta.com"
                  "sip:alice;day=tuesday@atlanta.com"
                  "sip:alice@[2001:db8::1]:5070;transport=tcp"))
    (is (string= text (sip:uri-string (sip:parse-uri text))) "round trip of ~a" text)))

(test uri-components
  (let ((uri (sip:parse-uri "sips:alice:pw@Example.COM:5061;Transport=TCP;lr?Subject=hi")))
    (is (eq :sips (sip:sip-uri-scheme uri)))
    (is (string= "alice" (sip:sip-uri-user uri)))
    (is (string= "pw" (sip:sip-uri-password uri)))
    (is (string= "Example.COM" (sip:sip-uri-host uri)))
    (is (= 5061 (sip:sip-uri-port uri)))
    (is (string= "TCP" (sip:uri-param uri "transport")))
    (is (equal '(nil t) (multiple-value-list (sip:uri-param uri "lr"))))
    (is (equal '(("Subject" . "hi")) (sip:sip-uri-headers uri))))
  ;; ';' in the user part belongs to the user, not to the parameters.
  (let ((uri (sip:parse-uri "sip:alice;day=tuesday@atlanta.com")))
    (is (string= "alice;day=tuesday" (sip:sip-uri-user uri)))
    (is (null (sip:sip-uri-params uri)))))

(test uri-escaping
  (let ((uri (sip:parse-uri "sip:j%40s0n@example.com;x=a%20b")))
    (is (string= "j@s0n" (sip:sip-uri-user uri)))
    (is (string= "a b" (sip:uri-param uri "x")))
    (is (string= "sip:j%40s0n@example.com;x=a%20b" (sip:uri-string uri))))
  ;; RFC 4475 3.1.1.4: escaped nulls survive.
  (is (= 0 (char-code (char (sip:sip-uri-user (sip:parse-uri "sip:null-%00-null@example.com"))
                            5)))))

(test uri-other-schemes
  (let ((uri (sip:parse-uri "tel:+1-212-555-1212")))
    (is (sip:generic-uri-p uri))
    (is (string= "tel:+1-212-555-1212" (sip:uri-string uri)))))

(test uri-malformed
  (dolist (text '("" "alice@atlanta.com" "sip:" "sip:alice@" "sip:alice@host:port"
                  "sip:alice@atlanta.com; lr" "<sip:alice@atlanta.com>" "sip:@atlanta.com"
                  "sip:alice@[::1"))
    (signals sip:sip-parse-error (sip:parse-uri text) "~s should not parse" text)))

(test uri-equivalence-19.1.4
  (flet ((same (a b) (and (sip:uri-equal a b) (sip:uri-equal b a))))
    (is (same "sip:%61lice@atlanta.com;transport=TCP" "sip:alice@AtLanTa.CoM;Transport=tcp"))
    (is (same "sip:carol@chicago.com" "sip:carol@chicago.com;newparam=5"))
    (is (same "sip:carol@chicago.com;newparam=5" "sip:carol@chicago.com;security=on"))
    (is (same "sip:biloxi.com;transport=tcp;method=REGISTER?to=sip:bob%40biloxi.com"
              "sip:biloxi.com;method=REGISTER;transport=tcp?to=sip:bob%40biloxi.com"))
    (is (same "sip:alice@atlanta.com?subject=project%20x&priority=urgent"
              "sip:alice@atlanta.com?priority=urgent&subject=project%20x"))
    ;; Not equivalent.
    (is (not (same "SIP:ALICE@AtLanTa.CoM;Transport=udp" "sip:alice@AtLanTa.CoM;Transport=UDP")))
    (is (not (same "sip:bob@biloxi.com" "sip:bob@biloxi.com:5060")))
    (is (not (same "sip:bob@biloxi.com" "sip:bob@biloxi.com;transport=udp")))
    (is (not (same "sip:bob@biloxi.com" "sip:bob@biloxi.com:6000;transport=tcp")))
    (is (not (same "sip:carol@chicago.com" "sip:carol@chicago.com?Subject=next%20meeting")))
    (is (not (same "sip:bob@phone21.boxesbybob.com" "sip:bob@192.0.2.4")))
    (is (not (same "sip:alice@atlanta.com" "sips:alice@atlanta.com")))))
