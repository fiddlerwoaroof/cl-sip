(defpackage :cl-sip.examples
  (:use :cl)
  (:local-nicknames (:sip :cl-sip) (:sdp :cl-sip.sdp) (:rtp :cl-sip.rtp))
  (:export #:register-example #:start-echo-uas #:stop-echo-uas #:call-example))
