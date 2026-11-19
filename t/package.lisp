(defpackage :cl-sip.test
  (:use :cl :fiveam)
  (:local-nicknames (:sip :cl-sip) (:sdp :cl-sip.sdp) (:rtp :cl-sip.rtp))
  (:export #:run-all))

(in-package :cl-sip.test)

(def-suite cl-sip :description "All cl-sip tests.")

(defun run-all ()
  "Run every test; true when all pass."
  (run! 'cl-sip))
