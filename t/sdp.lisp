(in-package :cl-sip.test)

(def-suite sdp :in cl-sip)
(in-suite sdp)

(defun sdp-lines (&rest lines)
  (format nil "~{~a~c~c~}" (loop for l in lines append (list l #\Return #\Newline))))

(defparameter *rfc4317-2.1-offer*
  (sdp-lines "v=0"
             "o=alice 2890844526 2890844526 IN IP4 host.atlanta.example.com"
             "s="
             "c=IN IP4 host.atlanta.example.com"
             "t=0 0"
             "m=audio 49170 RTP/AVP 0 8 97"
             "a=rtpmap:0 PCMU/8000"
             "a=rtpmap:8 PCMA/8000"
             "a=rtpmap:97 iLBC/8000"
             "m=video 51372 RTP/AVP 31 32"
             "a=rtpmap:31 H261/90000"
             "a=rtpmap:32 MPV/90000"))

(test sdp-parse-and-serialize
  (let ((session (sdp:parse-sdp *rfc4317-2.1-offer*)))
    (is (string= "alice" (sdp:origin-username (sdp:session-origin session))))
    (is (string= "2890844526" (sdp:origin-session-id (sdp:session-origin session))))
    (is (string= "" (sdp:session-name session)))
    (is (string= "host.atlanta.example.com"
                 (sdp:connection-address (sdp:session-connection session))))
    (is (equal '(("0 0")) (sdp:session-times session)))
    (destructuring-bind (audio video) (sdp:session-media session)
      (is (string= "audio" (sdp:media-type audio)))
      (is (= 49170 (sdp:media-port audio)))
      (is (string= "RTP/AVP" (sdp:media-protocol audio)))
      (is (equal '("0" "8" "97") (sdp:media-formats audio)))
      (is (equal '("PCMU" "PCMA" "iLBC") (mapcar #'sdp:codec-name (sdp:media-codecs audio))))
      (is (equal '(0 8 97) (mapcar #'sdp:codec-payload-type (sdp:media-codecs audio))))
      (is (= 90000 (sdp:codec-clock-rate (first (sdp:media-codecs video)))))
      (is (eq :sendrecv (sdp:media-direction audio session)))
      (is (string= "host.atlanta.example.com" (sdp:media-address audio session))))
    ;; Exact round trip.
    (is (string= *rfc4317-2.1-offer* (sdp:sdp-string session)))))

(test sdp-all-line-types
  (let* ((text (sdp-lines "v=0"
                          "o=jdoe 3724394400 3724394405 IN IP4 198.51.100.1"
                          "s=Call to John Smith"
                          "i=SDP Offer #1"
                          "u=http://www.jdoe.example.com/home.html"
                          "e=Jane Doe <jane@jdoe.example.com>"
                          "p=+1 617 555-6011"
                          "c=IN IP4 198.51.100.1"
                          "b=AS:128"
                          "t=3034423619 3042462419"
                          "r=604800 3600 0 90000"
                          "t=0 0"
                          "z=2882844526 -1h"
                          "a=recvonly"
                          "m=audio 49170/2 RTP/AVP 0"
                          "i=the audio"
                          "c=IN IP6 2001:db8::1"
                          "b=AS:64"
                          "a=sendonly"
                          "a=ptime:20"
                          "m=video 51372 RTP/AVP 99"
                          "a=rtpmap:99 h263-1998/90000"
                          "a=fmtp:99 profile=0"))
         (session (sdp:parse-sdp text)))
    (is (string= text (sdp:sdp-string session)))
    (is (equal '(("3034423619 3042462419" "604800 3600 0 90000") ("0 0"))
               (sdp:session-times session)))
    (destructuring-bind (audio video) (sdp:session-media session)
      (is (= 2 (sdp:media-port-count audio)))
      ;; Media-level values override session-level ones.
      (is (eq :sendonly (sdp:media-direction audio session)))
      (is (eq :recvonly (sdp:media-direction video session)))
      (is (string= "2001:db8::1" (sdp:media-address audio session)))
      (is (string= "198.51.100.1" (sdp:media-address video session)))
      (is (string= "20" (sdp:attribute audio "ptime")))
      (is (string= "profile=0" (sdp:codec-fmtp (first (sdp:media-codecs video)))))
      ;; Static payload type without rtpmap (RFC 3551).
      (is (string= "PCMU" (sdp:codec-name (first (sdp:media-codecs audio)))))))
  ;; Bare LF line endings are tolerated; a missing v= is not.
  (is (= 1 (length (sdp:session-media
                    (sdp:parse-sdp (format nil "v=0~%o=- 1 1 IN IP4 h~%s=-~%t=0 0~%m=audio 9 RTP/AVP 0~%"))))))
  (signals sdp:sdp-parse-error (sdp:parse-sdp (sdp-lines "o=- 1 1 IN IP4 h" "s=-")))
  (signals sdp:sdp-parse-error (sdp:parse-sdp (sdp-lines "v=0" "m=audio"))))

(test answer-selects-codecs-rfc3264-6
  ;; RFC 4317 section 2.2 shape: one audio codec set accepted, video rejected.
  (multiple-value-bind (answer accepted)
      (sdp:make-answer (sdp:parse-sdp *rfc4317-2.1-offer*)
                       :address "192.0.2.201" :port 49174
                       :codecs (list (sdp:make-codec :payload-type 8 :name "PCMA")
                                     (sdp:make-codec :payload-type 0 :name "PCMU")))
    (is-true accepted)
    (destructuring-bind (audio video) (sdp:session-media answer)
      ;; One m= line per offered m= line, in order.
      (is (string= "audio" (sdp:media-type audio)))
      (is (= 49174 (sdp:media-port audio)))
      ;; Common codecs in the offerer's order of preference.
      (is (equal '("0" "8") (sdp:media-formats audio)))
      (is (string= "video" (sdp:media-type video)))
      (is (sdp:media-rejected-p video))
      (is (equal '("31" "32") (sdp:media-formats video))))
    (is (string= "192.0.2.201" (sdp:connection-address (sdp:session-connection answer))))
    (multiple-value-bind (codecs direction address port)
        (sdp:negotiated-audio answer (sdp:parse-sdp *rfc4317-2.1-offer*))
      (is (equal '("PCMU" "PCMA") (mapcar #'sdp:codec-name codecs)))
      (is (eq :sendrecv direction))
      (is (string= "host.atlanta.example.com" address))
      (is (= 49170 port)))))

(test answer-dynamic-payload-types-and-telephone-events
  ;; RFC 4317 section 2.4 shape: the answer uses the offerer's dynamic
  ;; payload numbers, whatever numbers we would have chosen ourselves.
  (let* ((offer (sdp:parse-sdp
                 (sdp-lines "v=0" "o=alice 1 1 IN IP4 host.atlanta.example.com" "s=-"
                            "c=IN IP4 host.atlanta.example.com" "t=0 0"
                            "m=audio 49170 RTP/AVP 97 0 98"
                            "a=rtpmap:97 iLBC/8000"
                            "a=rtpmap:0 PCMU/8000"
                            "a=rtpmap:98 telephone-event/8000"
                            "a=fmtp:98 0-11")))
         (answer (sdp:make-answer offer :address "192.0.2.201" :port 49174))
         (audio (first (sdp:session-media answer))))
    (is (equal '("0" "98") (sdp:media-formats audio)))
    (is (equal '("0 PCMU/8000" "98 telephone-event/8000") (sdp:attribute-values audio "rtpmap")))
    (is (equal '("98 0-15") (sdp:attribute-values audio "fmtp"))))
  ;; Nothing but telephone-event in common is not a usable stream.
  (multiple-value-bind (answer accepted)
      (sdp:make-answer (sdp:parse-sdp
                        (sdp-lines "v=0" "o=- 1 1 IN IP4 h" "s=-" "c=IN IP4 h" "t=0 0"
                                   "m=audio 49170 RTP/AVP 18 101"
                                   "a=rtpmap:18 G729/8000"
                                   "a=rtpmap:101 telephone-event/8000")))
    (is-false accepted)
    (is (sdp:media-rejected-p (first (sdp:session-media answer))))))

(test answer-direction-rfc3264-6.1
  (flet ((answered (offered wanted)
           (let* ((offer (sdp:make-audio-offer :direction offered))
                  (answer (sdp:make-answer (sdp:parse-sdp (sdp:sdp-string offer))
                                           :direction wanted)))
             (sdp:media-direction (first (sdp:session-media answer)) answer))))
    (is (eq :sendrecv (answered :sendrecv :sendrecv)))
    (is (eq :recvonly (answered :sendonly :sendrecv)))
    (is (eq :sendonly (answered :recvonly :sendrecv)))
    (is (eq :inactive (answered :inactive :sendrecv)))
    (is (eq :sendonly (answered :sendrecv :sendonly)))
    (is (eq :inactive (answered :sendonly :sendonly)))
    (is (eq :inactive (answered :sendrecv :inactive)))))

(test offer-hold-and-resume-rfc3264-8
  (let* ((offer (sdp:make-audio-offer :address "192.0.2.101" :port 49170 :username "alice"
                                      :session-id "2890844526" :session-version "2890844526"))
         (held (sdp:hold offer))
         (resumed (sdp:resume held)))
    (is (search "m=audio 49170 RTP/AVP 0 8 101" (sdp:sdp-string offer)))
    (is (search "a=fmtp:101 0-15" (sdp:sdp-string offer)))
    (is (eq :sendrecv (sdp:media-direction (first (sdp:session-media offer)) offer)))
    ;; Each new offer increments the version and keeps the session id.
    (is (string= "2890844527" (sdp:origin-session-version (sdp:session-origin held))))
    (is (string= "2890844528" (sdp:origin-session-version (sdp:session-origin resumed))))
    (is (string= "2890844526" (sdp:origin-session-id (sdp:session-origin resumed))))
    (is (eq :sendonly (sdp:media-direction (first (sdp:session-media held)) held)))
    (is (eq :sendrecv (sdp:media-direction (first (sdp:session-media resumed)) resumed)))
    ;; The original is not modified.
    (is (string= "2890844526" (sdp:origin-session-version (sdp:session-origin offer))))
    ;; The peer answers a hold with recvonly.
    (let ((answer (sdp:make-answer held)))
      (is (eq :recvonly (sdp:media-direction (first (sdp:session-media answer)) answer))))))

(test sdp-in-a-call
  ;; Offer/answer carried by INVITE and 200 (RFC 3264 with RFC 3261 13.2.1).
  (let* ((world (new-world))
         (alice (agent world "sip:alice@atlanta.example.com" "client.atlanta.example.com"))
         (bob (agent world "sip:bob@biloxi.example.com" "client.biloxi.example.com"
                     :on-incoming-call
                     (lambda (call)
                       (let ((offer (sdp:parse-sdp (sip:call-remote-sdp call))))
                         (sip:answer call :sdp (sdp:sdp-string
                                                (sdp:make-answer offer :address "192.0.2.201"
                                                                       :port 3456)))))))
         (call (sip:invite alice *bob*
                           :sdp (sdp:sdp-string (sdp:make-audio-offer :address "192.0.2.101"
                                                                      :port 49172)))))
    (declare (ignorable bob))
    (settle world)
    (is (eq :established (sip:call-state call)))
    (multiple-value-bind (codecs direction address port)
        (sdp:negotiated-audio (sdp:parse-sdp (sip:call-remote-sdp call)))
      (is (equal '("PCMU" "PCMA" "telephone-event") (mapcar #'sdp:codec-name codecs)))
      (is (eq :sendrecv direction))
      (is (string= "192.0.2.201" address))
      (is (= 3456 port)))))
