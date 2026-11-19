(defsystem "cl-sip"
  :description "A basic SIP (RFC 3261) user-agent stack with SDP and RTP helpers."
  :version "0.1.0"
  :license "MIT"
  :depends-on ("alexandria" "babel" "bordeaux-threads" "ironclad" "usocket")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "util")
     (:file "uri")
     (:file "headers")
     (:file "message")
     (:file "parser")
     (:file "serializer")
     (:file "auth")
     (:file "scheduler")
     (:file "transport")
     (:file "transaction")
     (:file "transport-memory")
     (:file "transport-udp")
     (:file "transport-tcp")
     (:file "dialog")
     (:file "ua")
     (:file "registration")
     (:file "call")
     (:module "sdp"
      :serial t
      :components ((:file "sdp") (:file "offer-answer")))
     (:module "rtp"
      :serial t
      :components ((:file "packet") (:file "g711") (:file "dtmf") (:file "session"))))))
  :in-order-to ((test-op (test-op "cl-sip/test"))))

(defsystem "cl-sip/test"
  :depends-on ("cl-sip" "fiveam")
  :serial t
  :components
  ((:module "t"
    :serial t
    :components
    ((:file "package")
     (:file "harness")
     (:file "uri")
     (:file "message")
     (:file "auth")
     (:file "transaction")
     (:file "dialog")
     (:file "ua")
     (:file "sdp")
     (:file "rtp")
     (:file "sockets"))))
  :perform (test-op (o c) (unless (symbol-call :cl-sip.test :run-all)
                            (error "cl-sip tests failed"))))
