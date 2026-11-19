(in-package :cl-sip.sdp)

;;;; Offer/answer for audio (RFC 3264)

(defparameter *default-codecs*
  (list (make-codec :payload-type 0 :name "PCMU")
        (make-codec :payload-type 8 :name "PCMA")
        (make-codec :payload-type 101 :name "telephone-event" :fmtp "0-15"))
  "Codecs offered and accepted when none are given.")

(defun telephone-event-p (codec)
  (string-equal (codec-name codec) "telephone-event"))

(defun codec-match-p (a b)
  (and (string-equal (codec-name a) (codec-name b))
       (= (codec-clock-rate a) (codec-clock-rate b))
       (= (codec-channels a) (codec-channels b))))

(defun codec-attributes (codecs)
  (loop for c in codecs
        collect (cons "rtpmap"
                      (format nil "~d ~a/~d~:[~;/~d~]" (codec-payload-type c) (codec-name c)
                              (codec-clock-rate c) (> (codec-channels c) 1)
                              (codec-channels c)))
        when (codec-fmtp c)
          collect (cons "fmtp" (format nil "~d ~a" (codec-payload-type c) (codec-fmtp c)))))

(defun direction-attribute (direction)
  ;; sendrecv is the default and is left implicit.
  (unless (eq direction :sendrecv)
    (list (cons (string-downcase (symbol-name direction)) nil))))

(defun addrtype-of (address)
  (if (find #\: address) "IP6" "IP4"))

(defun new-session-id ()
  (princ-to-string (+ (* (get-universal-time) 1000) (random 1000))))

(defun make-audio-offer (&key (address "127.0.0.1") (port 49170)
                           (codecs *default-codecs*) (direction :sendrecv)
                           (username "-") session-id (session-version "1")
                           (name "-") ptime)
  "A session description offering one RTP audio stream (RFC 3264 section 5)."
  (let ((id (or session-id (new-session-id))))
    (make-session
     :origin (make-origin :username username :session-id id
                          :session-version session-version
                          :addrtype (addrtype-of address) :address address)
     :name name
     :connection (make-connection :addrtype (addrtype-of address) :address address)
     :media (list (make-media
                   :type "audio" :port port :protocol "RTP/AVP"
                   :formats (mapcar (lambda (c) (princ-to-string (codec-payload-type c)))
                                    codecs)
                   :attributes (append (codec-attributes codecs)
                                       (when ptime
                                         (list (cons "ptime" (princ-to-string ptime))))
                                       (direction-attribute direction)))))))

(defun answer-direction (offered wanted)
  "RFC 3264 section 6.1: we may send only if the offerer receives, and
receive only if the offerer sends."
  (let ((send (and (member offered '(:sendrecv :recvonly))
                   (member wanted '(:sendrecv :sendonly))))
        (recv (and (member offered '(:sendrecv :sendonly))
                   (member wanted '(:sendrecv :recvonly)))))
    (cond ((and send recv) :sendrecv)
          (send :sendonly)
          (recv :recvonly)
          (t :inactive))))

(defun common-codecs (offered ours)
  "Codecs both sides support, in the offerer's order and with the offerer's
payload type numbers (section 6.1)."
  (loop for o in offered
        for mine = (find o ours :test #'codec-match-p)
        when mine
          collect (make-codec :payload-type (codec-payload-type o)
                              :name (codec-name o)
                              :clock-rate (codec-clock-rate o)
                              :channels (codec-channels o)
                              :fmtp (or (codec-fmtp mine) (codec-fmtp o)))))

(defun rejected-media (offered)
  (make-media :type (media-type offered) :port 0 :protocol (media-protocol offered)
              :formats (media-formats offered)))

(defun make-answer (offer &key (address "127.0.0.1") (port 49172)
                            (codecs *default-codecs*) (direction :sendrecv)
                            (username "-") session-id (session-version "1"))
  "Answer OFFER (RFC 3264 section 6).  The answer has one m= line per offered
m= line, in the same order; the first acceptable audio stream is accepted and
every other stream is rejected with port 0.  Returns the answer and, as a
second value, true when a stream was accepted."
  (let* ((accepted nil)
         (media
           (loop for m in (session-media offer)
                 for common = (and (not accepted)
                                   (string-equal (media-type m) "audio")
                                   (string-equal (media-protocol m) "RTP/AVP")
                                   (not (media-rejected-p m))
                                   (common-codecs (media-codecs m) codecs))
                 collect (cond
                           ((and common (notevery #'telephone-event-p common))
                            (setf accepted t)
                            (make-media
                             :type "audio" :port port :protocol "RTP/AVP"
                             :formats (mapcar (lambda (c)
                                                (princ-to-string (codec-payload-type c)))
                                              common)
                             :attributes
                             (append (codec-attributes common)
                                     (direction-attribute
                                      (answer-direction (media-direction m offer)
                                                        direction)))))
                           (t (rejected-media m))))))
    (values
     (make-session
      :origin (make-origin :username username
                           :session-id (or session-id (new-session-id))
                           :session-version session-version
                           :addrtype (addrtype-of address) :address address)
      :name (session-name offer)
      :connection (make-connection :addrtype (addrtype-of address) :address address)
      :media media)
     accepted)))

(defun negotiated-audio (answer &optional (remote answer))
  "From an ANSWER, the agreed codecs and the direction of its audio stream.
REMOTE is the description whose address and port media should be sent to
(the answer itself when we made the offer, the offer when we answered).
Returns codecs, direction (from the answerer's point of view), remote
address, remote port."
  (let ((mine (find-if (lambda (m) (and (string-equal (media-type m) "audio")
                                        (not (media-rejected-p m))))
                       (session-media answer)))
        (theirs (find-if (lambda (m) (and (string-equal (media-type m) "audio")
                                          (not (media-rejected-p m))))
                         (session-media remote))))
    (when (and mine theirs)
      (values (media-codecs mine)
              (media-direction mine answer)
              (media-address theirs remote)
              (media-port theirs)))))

;;;; Modifying a session (section 8)

(defun bump-version (session)
  "Copy of SESSION with the o= version incremented, as every new offer
requires (section 8)."
  (let ((copy (copy-session session))
        (origin (copy-origin (session-origin session))))
    (setf (origin-session-version origin)
          (princ-to-string
           (1+ (or (parse-integer (origin-session-version origin) :junk-allowed t) 0)))
          (session-origin copy) origin
          (session-media copy) (mapcar #'copy-media (session-media session)))
    copy))

(defun set-direction (media direction)
  (setf (media-attributes media)
        (append (remove-if (lambda (a) (member (car a) +directions+ :test #'string-equal))
                           (media-attributes media))
                (direction-attribute direction)))
  media)

(defun hold (session)
  "A new offer placing every active stream of SESSION on hold (section
8.4): sendrecv becomes sendonly, recvonly becomes inactive."
  (let ((copy (bump-version session)))
    (dolist (m (session-media copy) copy)
      (unless (media-rejected-p m)
        (set-direction m (ecase (media-direction m session)
                           ((:sendrecv :sendonly) :sendonly)
                           ((:recvonly :inactive) :inactive)))))))

(defun resume (session)
  "A new offer taking the streams of SESSION off hold."
  (let ((copy (bump-version session)))
    (dolist (m (session-media copy) copy)
      (unless (media-rejected-p m)
        (set-direction m (ecase (media-direction m session)
                           ((:sendrecv :sendonly) :sendrecv)
                           ((:recvonly :inactive) :recvonly)))))))
