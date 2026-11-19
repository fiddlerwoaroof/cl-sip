(in-package :cl-sip.voice)

;;;; Bridged calls: the server as a back-to-back user agent that relays
;;;; media (RFC 7092).  A call from A to B is two calls, each with its own
;;;; dialog, its own offer/answer exchange with the server (RFC 6337) and its
;;;; own RTP session here; packets are forwarded between the two sessions.
;;;;
;;;; Everything that changes a bridge runs on the SIP event loop.  The RTP
;;;; receive threads only read what was settled before they were wired up.

(defclass leg ()
  ((call :initarg :call :initform nil :accessor leg-call)
   (session :initarg :session :reader leg-session)
   (trunk :initarg :trunk :initform nil :reader leg-trunk-p
          :documentation "True when the far end is across the NAT.")
   (target :initarg :target :initform nil :reader leg-target
           :documentation "For an outgoing leg, the plist it was called from.")
   (codec :initform nil :accessor leg-codec
          :documentation "Name of the audio codec to send this leg, PCMU or PCMA.")
   (audio-type :initform nil :accessor leg-audio-type)
   (event-type :initform nil :accessor leg-event-type
               :documentation "Payload type of telephone events on this leg, if any.")
   (local-sdp :initform nil :accessor leg-local-sdp
              :documentation "The session description last sent on this leg.")
   (sdp-text :initform nil :accessor leg-sdp-text)
   (held :initform nil :accessor leg-held
         :documentation "True while this leg's phone has the call on hold.")
   (told-held :initform nil :accessor leg-told-held
              :documentation "True while this leg has been offered sendonly because
the other party is holding.")
   (id :initform (format nil "~(~36r~)" (random (expt 36 10))) :reader leg-id
       :documentation "Names this leg in dialog state documents (RFC 4235).")
   (extension :initarg :extension :initform nil :accessor leg-extension
              :documentation "The extension at the far end, when it is one.")
   (party :initarg :party :initform nil :accessor leg-party
          :documentation "Who is at the far end, as a name-addr the server vouches for.")))

(defun server-open-leg (server &key call trunk target)
  "A leg with its RTP session bound and receiving."
  (make-instance
   'leg :call call :trunk trunk :target target
        :session (rtp:rtp-session-start
                  (rtp:make-rtp-session :local-host (server-host server) :latch t
                                        :port-range (server-rtp-port-range server)
                                        ;; Only the trunk is across a NAT.
                                        :stun (and trunk (server-stun server))))))

(defun leg-address (server leg)
  "The address and port the far end of LEG should send media to: public
for the trunk, local for a phone."
  (let ((session (leg-session leg))
        (transport (server-transport server)))
    (values (or (and (leg-trunk-p leg)
                     (or (rtp:rtp-session-public-host session)
                         (sip:transport-public-host transport)))
                (sip:transport-advertised-host transport))
            (or (and (leg-trunk-p leg) (rtp:rtp-session-public-port session))
                (rtp:rtp-session-local-port session)))))

(defun leg-label (leg)
  "Who is on LEG, for the log."
  (let ((party (leg-party leg)))
    (cond ((null party) "someone")
          ((stringp party) party)
          (t (sip:uri-string (sip:name-addr-uri party))))))

(defun leg-disconnect (leg)
  (setf (rtp:rtp-session-on-packet (leg-session leg)) nil))

(defun leg-close (leg)
  (leg-disconnect leg)
  (let ((session (leg-session leg)))
    (in-background (lambda () (ignore-errors (rtp:rtp-session-stop session))))))

(defun leg-set-codecs (leg codecs)
  "Note what was agreed for LEG.  Returns the audio codec, if there is one."
  (let ((audio (find-if-not (lambda (c) (string-equal (sdp:codec-name c) "telephone-event"))
                            codecs))
        (events (find "telephone-event" codecs :key #'sdp:codec-name :test #'string-equal)))
    (when audio
      (setf (leg-codec leg) (string-upcase (sdp:codec-name audio))
            (leg-audio-type leg) (sdp:codec-payload-type audio)
            (leg-event-type leg) (and events (sdp:codec-payload-type events))))
    audio))

(defun leg-set-remote (leg host port)
  (unless (or (null host) (string= host "0.0.0.0") (null port) (zerop port))
    (setf (rtp:rtp-session-remote-host (leg-session leg)) host
          (rtp:rtp-session-remote-port (leg-session leg)) port)))

(defun leg-next-version (leg)
  "The o= version for the next description sent on LEG: each one that
differs from the last must count up (RFC 3264 section 8)."
  (let ((last (leg-local-sdp leg)))
    (if last
        (princ-to-string
         (1+ (or (parse-integer (sdp:origin-session-version (sdp:session-origin last))
                                :junk-allowed t)
                 0)))
        "1")))

(defun offered-direction (offer)
  "The direction of the audio stream in OFFER, from the offerer's side."
  (let ((audio (find-if (lambda (m) (and (string-equal (sdp:media-type m) "audio")
                                         (not (sdp:media-rejected-p m))))
                        (sdp:session-media offer))))
    (if audio (sdp:media-direction audio offer) :inactive)))

(defun leg-answer-offer (server leg offer-text &key (direction :sendrecv))
  "Answer the far end's offer on LEG (RFC 3264 section 6); DIRECTION is
what the server would like, cut down to what the offer allows.  Returns the
answer as text, or NIL when the offer has no audio we can take.  As a second
value, the direction the far end offered."
  (let ((offer (ignore-errors (sdp:parse-sdp (or offer-text "")))))
    (when offer
      (multiple-value-bind (address port) (leg-address server leg)
        (multiple-value-bind (answer accepted)
            (sdp:make-answer offer :address address :port port :direction direction
                                   :session-id (and (leg-local-sdp leg)
                                                    (sdp:origin-session-id
                                                     (sdp:session-origin (leg-local-sdp leg))))
                                   :session-version (leg-next-version leg))
          (when accepted
            (multiple-value-bind (codecs answered-direction host remote-port)
                (sdp:negotiated-audio answer offer)
              (declare (ignore answered-direction))
              (when (leg-set-codecs leg codecs)
                (leg-set-remote leg host remote-port)
                (setf (leg-local-sdp leg) answer
                      (leg-sdp-text leg) (sdp:sdp-string answer))
                (values (leg-sdp-text leg) (offered-direction offer))))))))))

(defun leg-make-offer (server leg)
  "The server's own offer for an outgoing LEG, as text."
  (multiple-value-bind (address port) (leg-address server leg)
    (let ((offer (sdp:make-audio-offer :address address :port port)))
      (setf (leg-local-sdp leg) offer
            (leg-sdp-text leg) (sdp:sdp-string offer)))))

(defun leg-accept-answer (leg answer-text)
  "Take the far end's answer to our offer.  True when it leaves us audio."
  (let ((answer (ignore-errors (sdp:parse-sdp (or answer-text "")))))
    (when answer
      (multiple-value-bind (codecs direction host port) (sdp:negotiated-audio answer)
        (declare (ignore direction))
        (when (and codecs (leg-set-codecs leg codecs))
          (leg-set-remote leg host port)
          t)))))

;;; Media

(defun transcode (payload from to)
  "G.711 PAYLOAD from one companding law to the other."
  (cond ((string= from to) payload)
        ((string= from "PCMU") (rtp:alaw-encode (rtp:ulaw-decode payload)))
        (t (rtp:ulaw-encode (rtp:alaw-decode payload)))))

(defun leg-relay-to (from to)
  "Forward what arrives on FROM out of TO.  Audio is recognised by its
static payload type, since a phone that agreed to both laws may send either;
telephone events (RFC 4733) keep their meaning but take the number the
other leg negotiated."
  (setf (rtp:rtp-session-on-packet (leg-session from))
        (lambda (session packet)
          (declare (ignore session))
          (unless (or (leg-held from) (leg-held to))
            (let ((type (rtp:rtp-packet-payload-type packet)))
              (ignore-errors
               (cond ((member type '(0 8))
                      (rtp:rtp-forward
                       (leg-session to) packet
                       :payload-type (leg-audio-type to)
                       :payload (transcode (rtp:rtp-packet-payload packet)
                                           (if (= type 0) "PCMU" "PCMA")
                                           (leg-codec to))))
                     ((and (eql type (leg-event-type from)) (leg-event-type to))
                      (rtp:rtp-forward (leg-session to) packet
                                       :payload-type (leg-event-type to))))))))))

(defun legs-connect (a b)
  (leg-relay-to a b)
  (leg-relay-to b a))

;;; A bridge

(defparameter *completed-elsewhere*
  '(("Reason" "SIP;cause=200;text=\"Call completed elsewhere\""))
  "RFC 3326: why the other phones of a forked call stop ringing.")

(defclass bridge ()
  ((server :initarg :server :reader bridge-server)
   (a :initarg :a :accessor bridge-a :documentation "The leg of the call that came in.")
   (b :initform nil :accessor bridge-b :documentation "The leg that answered.")
   (forks :initform '() :accessor bridge-forks
          :documentation "Outgoing legs still ringing.")
   (state :initform :calling :accessor bridge-state
          :documentation ":calling, :connected, :failed (nobody answered) or :ended.")
   (early :initform nil :accessor bridge-early
          :documentation "The ringing leg whose early media A is hearing.")
   (rang :initform nil :accessor bridge-rang)
   (outcomes :initform '() :accessor bridge-outcomes
             :documentation "How the legs that did not answer ended: a status or a cause.")
   (timer :initform nil :accessor bridge-timer)
   (label :initarg :label :initform "call" :reader bridge-label)
   (on-failed :initarg :on-failed :initform nil :reader bridge-on-failed
              :documentation "(lambda (status reason)) called instead of refusing
A when nobody answers; REASON is :busy, :declined, :no-answer or :unavailable.")
   (established :initarg :established :initform nil :reader bridge-established-p
                :documentation "True when A is a call already in progress, for whom a
new party is being rung (a transfer).  A is then left alone until someone
answers, and ON-ANSWERED decides what happens.")
   (on-answered :initarg :on-answered :initform nil :reader bridge-on-answered
                :documentation "(lambda (leg)) for an ESTABLISHED bridge.")
   (pending :initform '() :accessor bridge-pending
            :documentation "Transfers being attempted on behalf of this bridge.")))

(defun bridge-log (bridge control &rest args)
  (server-log (bridge-server bridge) "~a: ~?" (bridge-label bridge) control args))

(defun bridge-legs (bridge)
  (remove nil (list* (bridge-a bridge) (bridge-b bridge) (bridge-forks bridge))))

(defun bridge-forget (bridge)
  (sip:cancel-timer (bridge-timer bridge))
  (setf (server-bridges (bridge-server bridge))
        (remove bridge (server-bridges (bridge-server bridge))))
  (server-publish-dialogs (bridge-server bridge)))

(defun bridge-drop-forks (bridge &key headers except)
  "Stop ringing every outgoing leg but EXCEPT."
  (dolist (leg (bridge-forks bridge))
    (unless (eq leg except)
      (sip:hangup (leg-call leg) :headers headers)
      (leg-close leg)))
  (setf (bridge-forks bridge) '())
  (when (and (bridge-early bridge) (not (eq (bridge-early bridge) except)))
    (leg-disconnect (bridge-a bridge))
    (setf (bridge-early bridge) nil)))

(defun bridge-fail (bridge status reason)
  "Nobody answered.  The incoming call is refused with STATUS unless the
bridge was given something else to do about it."
  (when (eq (bridge-state bridge) :calling)
    (setf (bridge-state bridge) :failed)
    (bridge-drop-forks bridge)
    (bridge-forget bridge)
    (bridge-log bridge "not answered (~(~a~))" reason)
    (cond ((bridge-on-failed bridge)
           (funcall (bridge-on-failed bridge) status reason))
          ((not (bridge-established-p bridge))
           (let ((a (bridge-a bridge)))
             (sip:reject (leg-call a) status)
             (leg-close a))))))

(defun bridge-abandon (bridge)
  "Give up ringing for an established call, which carries on as it was."
  (when (eq (bridge-state bridge) :calling)
    (setf (bridge-state bridge) :ended)
    (bridge-drop-forks bridge)
    (bridge-forget bridge)))

(defun bridge-end (bridge cause)
  "One side has gone: release the other and the media."
  (unless (member (bridge-state bridge) '(:failed :ended))
    (setf (bridge-state bridge) :ended)
    (bridge-forget bridge)
    (mapc #'bridge-abandon (bridge-pending bridge))
    (let ((legs (bridge-legs bridge)))
      (setf (bridge-forks bridge) '())
      (dolist (leg legs)
        (sip:hangup (leg-call leg))
        (leg-close leg)))
    (bridge-log bridge "ended (~(~a~))" cause)))

(defun bridge-ring (bridge)
  "RFC 3960 section 3.2: tell the caller the far end is ringing and let its
phone make the ringback."
  (when (and (eq (bridge-state bridge) :calling) (not (bridge-rang bridge)))
    (setf (bridge-rang bridge) t)
    (unless (bridge-established-p bridge)
      (sip:ring (leg-call (bridge-a bridge))))))

(defun bridge-early-media (bridge leg sdp)
  "A ringing leg is sending media before answering (announcements or
ringback from a telephone network): let the caller hear it."
  (when (and (eq (bridge-state bridge) :calling)
             (not (bridge-established-p bridge))
             (null (bridge-early bridge))
             (leg-accept-answer leg sdp))
    (let ((a (bridge-a bridge)))
      (setf (bridge-early bridge) leg
            (bridge-rang bridge) t)
      (sip:ring (leg-call a) :status 183 :sdp (leg-sdp-text a))
      (legs-connect a leg))))

(defun bridge-answered (bridge leg sdp)
  (cond
    ((not (eq (bridge-state bridge) :calling))
     ;; Answered just as the call was settled some other way.
     (sip:hangup (leg-call leg)))
    ((not (leg-accept-answer leg sdp))
     (sip:hangup (leg-call leg)))
    ((bridge-established-p bridge)
     ;; A transfer target answered: this bridge has done its work, and the
     ;; leg is handed to whoever asked for it.
     (setf (bridge-state bridge) :ended)
     (bridge-drop-forks bridge :headers *completed-elsewhere* :except leg)
     (bridge-forget bridge)
     (funcall (bridge-on-answered bridge) leg))
    (t
     (let ((a (bridge-a bridge)))
       (setf (bridge-state bridge) :connected
             (bridge-b bridge) leg)
       (sip:cancel-timer (bridge-timer bridge))
       (bridge-drop-forks bridge :headers *completed-elsewhere* :except leg)
       (setf (bridge-early bridge) nil)
       (sip:answer (leg-call a) :sdp (leg-sdp-text a))
       (legs-connect a leg)
       (bridge-log bridge "answered")
       (server-publish-dialogs (bridge-server bridge))))))

(defun bridge-failure (outcomes)
  "The status and reason to give the caller when every leg ended as in
OUTCOMES.  One busy phone means busy; an outright refusal is passed on;
anything else means nobody is available."
  (cond ((some (lambda (o) (member o '(486 600))) outcomes) (values 486 :busy))
        ((member 603 outcomes) (values 603 :declined))
        (t (values 480 :unavailable))))

(defun bridge-fork-ended (bridge leg status cause)
  (cond
    ((eq leg (bridge-b bridge))
     (bridge-end bridge cause))
    ((member leg (bridge-forks bridge))
     (setf (bridge-forks bridge) (remove leg (bridge-forks bridge)))
     (push (or status cause) (bridge-outcomes bridge))
     (leg-close leg)
     (when (eq leg (bridge-early bridge))
       (leg-disconnect (bridge-a bridge))
       (setf (bridge-early bridge) nil))
     (if (and (eq (bridge-state bridge) :calling) (null (bridge-forks bridge)))
         (multiple-value-call #'bridge-fail bridge
           (bridge-failure (bridge-outcomes bridge)))
         (server-publish-dialogs (bridge-server bridge))))))

;;; Changes during the call (RFC 3264 section 8, RFC 6337, RFC 5359 section 2.1)

(defun bridge-peer (bridge leg)
  (if (eq leg (bridge-a bridge)) (bridge-b bridge) (bridge-a bridge)))

(defun leg-reoffer (leg held)
  "Send LEG a new offer that says whether the other party has it on hold:
sendonly for held, so that its phone shows the call as held (RFC 5359
section 2.1), sendrecv again afterwards."
  (let ((offer (if held
                   (sdp:hold (leg-local-sdp leg))
                   (sdp:resume (leg-local-sdp leg)))))
    (setf (leg-local-sdp leg) offer
          (leg-sdp-text leg) (sdp:sdp-string offer)
          (leg-told-held leg) (and held t))
    (sip:reinvite (leg-call leg) :sdp (leg-sdp-text leg))))

(defun bridge-leg-offer (bridge leg sdp)
  "LEG's phone sent a new offer in a re-INVITE or UPDATE: answer it from
what it asks for, and pass a hold or resume on to the other party."
  (let* ((server (bridge-server bridge))
         (call (leg-call leg))
         (peer (bridge-peer bridge leg)))
    (cond
      ((null sdp)
       ;; A re-INVITE without an offer asks for ours: the current one.
       (sip:answer call :sdp (leg-sdp-text leg)))
      (t
       (multiple-value-bind (answer offered)
           (leg-answer-offer server leg sdp
                             ;; Held by the other party: we only send.
                             :direction (if (and peer (leg-held peer)) :sendonly :sendrecv))
         (cond
           ((null answer) (sip:reject call 488))
           (t
            (sip:answer call :sdp answer)
            (let ((held (and (member offered '(:sendonly :inactive)) t)))
              (unless (eq held (leg-held leg))
                (setf (leg-held leg) held)
                (bridge-log bridge "~:[resumed~;on hold~]" held)
                (when (and peer (eq (bridge-state bridge) :connected))
                  (leg-reoffer peer held)))))))))))

(defun bridge-leg-event (bridge leg event &key sdp status refer-to replaces notify
                        &allow-other-keys)
  "Events of an established leg, either side."
  (case event
    (:refer (bridge-transfer bridge leg refer-to replaces notify))
    (:reinvite (bridge-leg-offer bridge leg sdp))
    (:reinvite-accepted (when sdp (leg-accept-answer leg sdp)))
    (:reinvite-rejected
     (bridge-log bridge "a phone refused a change to the call (~a)" status))))

(defun bridge-fork-event (bridge leg event &rest args &key sdp status cause
                          &allow-other-keys)
  (case event
    (:ringing (bridge-ring bridge))
    (:progress (if sdp
                   (bridge-early-media bridge leg sdp)
                   (bridge-ring bridge)))
    (:established (bridge-answered bridge leg sdp))
    (:terminated (bridge-fork-ended bridge leg status cause))
    (t (apply #'bridge-leg-event bridge leg event args))))

(defun bridge-a-event (bridge event &rest args &key cause &allow-other-keys)
  (case event
    (:terminated (bridge-end bridge cause))
    (t (apply #'bridge-leg-event bridge (bridge-a bridge) event args))))

(defun bridge-call (server a targets &key from headers (label "call") on-failed
                                       established on-answered
                                       (ring-seconds (server-ring-seconds server)))
  "Ring TARGETS for the incoming call on leg A, whose offer has been
answered already (LEG-ANSWER-OFFER), and join A to the first that answers.
Each target is a plist: :URI where to send the INVITE, :TO the party being
called, :TRUNK true when it is reached through the trunk.  FROM is who the
call is from, as the called phones will see it.

With ESTABLISHED, A is a call already in progress (see the BRIDGE class):
it is not touched, and ON-ANSWERED is called with the leg that answers."
  (let ((bridge (make-instance 'bridge :server server :a a :label label
                                       :on-failed on-failed
                                       :established established
                                       :on-answered on-answered))
        (ua (server-ua server)))
    (push bridge (server-bridges server))
    (unless established
      (setf (leg-party a) from
            (leg-extension a) (call-caller-extension (leg-call a)))
      (bridge-take-leg bridge a))
    (dolist (target targets)
      (let ((leg (server-open-leg server :trunk (getf target :trunk) :target target)))
        (push leg (bridge-forks bridge))
        (setf (leg-party leg) (getf target :to)
              (leg-extension leg) (getf target :extension))
        (setf (leg-call leg)
              (sip:invite ua (getf target :uri)
                          :auto-answer-reinvite nil
                          :to (getf target :to) :from from
                          :sdp (leg-make-offer server leg)
                          :headers (append headers (getf target :headers))
                          :on-event (lambda (call event &rest args)
                                      (declare (ignore call))
                                      (apply #'bridge-fork-event bridge leg event args))))
        (setf (sip:call-accept-refer (leg-call leg)) t)))
    (setf (bridge-timer bridge)
          (sip:schedule (sip:ua-scheduler ua) ring-seconds
                        (lambda () (bridge-fail bridge 480 :no-answer))))
    (bridge-log bridge "ringing ~d phone~:p" (length targets))
    (server-publish-dialogs server)
    bridge))

;;; Transfer (RFC 5589; RFC 5359 sections 2.4 and 2.5)

(defun bridge-take-leg (bridge leg)
  "Make BRIDGE the owner of LEG's call: its events come here, and the
bridge, not the SIP core, answers its re-INVITEs and REFERs."
  (let ((call (leg-call leg)))
    (setf (sip:call-auto-answer-reinvite call) nil
          (sip:call-accept-refer call) t
          (sip:call-on-event call)
          (if (eq leg (bridge-a bridge))
              (lambda (call event &rest args)
                (declare (ignore call))
                (apply #'bridge-a-event bridge event args))
              (lambda (call event &rest args)
                (declare (ignore call))
                (apply #'bridge-fork-event bridge leg event args))))))

(defun server-find-leg (server call)
  "The bridge and leg CALL belongs to."
  (dolist (bridge (server-bridges server))
    (let ((leg (find call (bridge-legs bridge) :key #'leg-call)))
      (when leg (return (values bridge leg))))))

(defun bridge-replace-leg (bridge old new)
  "Put NEW in OLD's place in a connected BRIDGE and release OLD: the party
that stays is now talking to NEW."
  (let ((keep (bridge-peer bridge old)))
    (if (eq old (bridge-a bridge))
        (setf (bridge-a bridge) new)
        (setf (bridge-b bridge) new))
    (bridge-take-leg bridge new)
    (leg-disconnect old)
    (setf (leg-held new) nil)
    (legs-connect keep new)
    ;; Whoever was told they were on hold by a party now gone is not any more.
    (dolist (leg (list keep new))
      (when (leg-told-held leg)
        (leg-reoffer leg nil)))
    ;; OLD's call is no longer this bridge's business.
    (setf (sip:call-on-event (leg-call old)) nil)
    (sip:hangup (leg-call old))
    (leg-close old)
    (bridge-log bridge "transferred: now ~a and ~a" (leg-label keep) (leg-label new))
    (server-publish-dialogs (bridge-server bridge))))

(defun bridge-detach-leg (bridge leg)
  "Take LEG out of a connected BRIDGE for use elsewhere; the bridge is
dissolved and its other party released."
  (let ((other (bridge-peer bridge leg)))
    (setf (bridge-state bridge) :ended)
    (bridge-forget bridge)
    (mapc #'bridge-abandon (bridge-pending bridge))
    (leg-disconnect leg)
    (when other
      (sip:hangup (leg-call other))
      (leg-close other))
    leg))

(defun bridge-transfer (bridge leg refer-to replaces notify)
  "The phone on LEG asked, with REFER, that the party it is talking to be
connected to REFER-TO instead.  NOTIFY reports progress to that phone as SIP
statuses (RFC 3515): the last report is final."
  (let* ((server (bridge-server bridge))
         (keep (bridge-peer bridge leg))
         (uri (sip:name-addr-uri refer-to))
         (number (normalize-number (uri-user uri))))
    (cond
      ((not (and keep (eq (bridge-state bridge) :connected)))
       (funcall notify 481 t))
      (replaces
       ;; Attended: the Replaces names the transferring phone's other call
       ;; with this server.  Join the two far parties (RFC 5589 section 7).
       (multiple-value-bind (other-bridge other-leg)
           (let ((call (sip:find-replaced-call (server-ua server) replaces)))
             (and call (server-find-leg server call)))
         (cond
           ((and other-bridge (not (eq other-bridge bridge))
                 (eq (bridge-state other-bridge) :connected))
            (bridge-log bridge "attended transfer by ~a"
                        (leg-label leg))
            (funcall notify 200 t)
            (bridge-replace-leg
             bridge leg
             (bridge-detach-leg other-bridge (bridge-peer other-bridge other-leg))))
           (t (funcall notify 481 t)))))
      (t
       (let ((action (server-route server number)))
         (bridge-log bridge "transfer by ~a to ~a" (leg-label leg)
                     number)
         (cond
           ((and (consp action) (eq (first action) :extension))
            (let ((targets (server-extension-targets server (second action))))
              (cond
                ((null targets) (funcall notify 480 t))
                (t
                 (push (bridge-call
                        server keep targets
                        :from (leg-party keep)
                        :headers `(("Referred-By" ,(leg-party leg)))
                        :label (format nil "transfer to ~a"
                                       (extension-label (second action)))
                        :ring-seconds (or (extension-ring-seconds (second action))
                                          (server-ring-seconds server))
                        :established t
                        ;; The call being transferred carries on meanwhile,
                        ;; and stays as it is if nobody answers.
                        :on-failed (lambda (status reason)
                                     (declare (ignore reason))
                                     (funcall notify status t))
                        :on-answered (lambda (new)
                                       (funcall notify 200 t)
                                       (if (eq (bridge-state bridge) :connected)
                                           (bridge-replace-leg bridge leg new)
                                           (progn (sip:hangup (leg-call new))
                                                  (leg-close new)))))
                       (bridge-pending bridge))))))
           ((eq action :trunk)
            (cond
              ((not (and (leg-extension leg) (extension-outbound (leg-extension leg))))
               (funcall notify 403 t))
              (t
               (push (bridge-call
                      server keep (list (server-trunk-target server number))
                      :from (server-trunk-identity server (leg-extension leg))
                      :label (format nil "transfer to ~a" number)
                      :ring-seconds *outbound-ring-seconds*
                      :established t
                      :on-failed (lambda (status reason)
                                   (declare (ignore reason))
                                   (funcall notify status t))
                      :on-answered (lambda (new)
                                     (funcall notify 200 t)
                                     (if (eq (bridge-state bridge) :connected)
                                         (bridge-replace-leg bridge leg new)
                                         (progn (sip:hangup (leg-call new))
                                                (leg-close new)))))
                     (bridge-pending bridge)))))
           ((or (eq action :agent) (eq action :echo)
                (and (consp action) (member (first action) '(:say :play))))
            ;; Handed to the server itself: the agent, or an announcement.
            (funcall notify 200 t)
            (bridge-detach-leg bridge keep)
            (server-attach-action server keep action))
           (t (funcall notify 404 t))))))))

;;; Calls to extensions

(defun server-identity (server number &optional name)
  "NUMBER at this server as a name-addr: how a party is shown to another."
  (sip:make-name-addr
   :display-name name
   :uri (sip:make-sip-uri :user number
                          :host (sip:transport-advertised-host (server-transport server)))))

(defun call-identity (server call)
  "Who CALL is from, as the server vouches for it: the extension that
authenticated, or for a trunk call the number the provider gave."
  (let ((extension (call-caller-extension call))
        (from (sip:msg-from (sip:call-invite call))))
    (if extension
        (server-identity server (extension-number extension) (extension-name extension))
        (server-identity server (normalize-number (call-caller-number call))
                         (sip:name-addr-display-name from)))))

(defun server-extension-targets (server extension &optional route)
  "Every phone that should ring for EXTENSION: its registered contacts, or
for a group those of each member.  With ROUTE, each target carries the
History-Info of the call so far."
  (let ((ua (server-ua server)))
    (loop for member in (if (extension-group-p extension)
                            (mapcar (lambda (number)
                                      (find-extension (server-extensions server) number))
                                    (extension-ring extension))
                            (list extension))
          append (loop for uri in (sip:registrar-lookup-user
                                   (sip:ua-registrar ua) ua (extension-number member))
                       collect (list :uri uri
                                     :to (server-identity server (extension-number member)
                                                          (extension-name member))
                                     :extension member
                                     :headers (and route (route-headers route uri)))))))

;;; Where a call has been (RFC 7044)

(defstruct route
  "The targets an incoming call has been sent to, oldest first.  Each entry
is a plist: :URI, :INDEX, :CAUSE (why the call left that target, a SIP
status) and :MP (the index of the entry it was retargeted from)."
  (entries '())
  (counter 0))

(defparameter *max-forwards* 5
  "How many times one call may be forwarded before it is taken for a loop.")

(defun route-current (route)
  (first (last (route-entries route))))

(defun route-forwards (route)
  (max 0 (1- (length (route-entries route)))))

(defun route-next-index (route)
  ;; Everything this server adds hangs off the entry for the number dialled
  ;; (RFC 7044 section 10.3).
  (format nil "1.~d" (incf (route-counter route))))

(defun route-start (server number)
  (make-route :entries (list (list :uri (sip:uri-string
                                         (sip:name-addr-uri (server-identity server number)))
                                   :index "1"))))

(defun route-retarget (route server number cause)
  "Record that the call is leaving its current target for NUMBER, a
different user, because of CAUSE."
  (let ((from (route-current route)))
    (setf (route-entries route)
          (append (butlast (route-entries route))
                  (list (list :uri (getf from :uri) :index (getf from :index)
                              :mp (getf from :mp) :cause cause)
                        (list :uri (sip:uri-string
                                    (sip:name-addr-uri (server-identity server number)))
                              :index (route-next-index route)
                              :mp (getf from :index)))))
    route))

(defun route-headers (route contact)
  "History-Info for an INVITE sent to CONTACT, a registered address of the
route's current target: the entries so far, then one for this hop (\"rc\":
the same user at another address)."
  (append
   (loop for entry in (route-entries route)
         collect (list "History-Info"
                       (format nil "<~a~@[?Reason=SIP%3Bcause%3D~d~]>;index=~a~@[;mp=~a~]"
                               (getf entry :uri) (getf entry :cause)
                               (getf entry :index) (getf entry :mp))))
   (list (list "History-Info"
               (format nil "<~a>;index=~a;rc=~a" (sip:uri-string contact)
                       (route-next-index route)
                       (getf (route-current route) :index))))))

;;; Delivering a call: ring, forward, or answer here

(defun server-answer-with (server a action)
  "Answer the incoming call on leg A here and run the dialplan ACTION on it:
the voice agent, the echo, an announcement."
  (let ((call (leg-call a))
        (agent (eq action :agent)))
    (cond
      ((and (consp action) (eq (first action) :reject))
       (sip:reject call (or (second action) 486))
       (leg-close a))
      ((or (null action) (functionp action))
       (sip:reject call 404)
       (leg-close a))
      ((and agent (>= (server-active-calls server) (server-max-calls server)))
       (server-log server "  too many calls in progress")
       (sip:reject call 486)
       (leg-close a))
      (t
       (server-attach-action server a action)
       (sip:answer call :sdp (leg-sdp-text a))))))

(defparameter *digit-pause* 3
  "Seconds without a key press after which the digits keyed so far are
taken to be the whole number.")

(defstruct handoff
  "A call the agent is on, whose caller can be put through to an extension
(see below: handing a call from the agent to a phone)."
  server leg agent release
  (digits "")
  (timer nil)
  (bridge nil))                         ; ringing for the caller, if any

(defun server-attach-action (server a action)
  "Run the dialplan ACTION on leg A: when its call is established, or at
once if it already is (a call transferred to the agent)."
  (let* ((call (leg-call a))
         (agent (eq action :agent))
         (stop nil)
         (released nil)
         (handoff (and agent (server-extensions server)
                       (make-handoff :server server :leg a))))
    (when agent (incf (server-active-calls server)))
    (labels ((begin ()
               (setf (leg-held a) nil
                     stop (server-start-media
                           server call action (leg-session a) (leg-codec a)
                           :agent-options (and handoff (handoff-agent-options handoff a))
                           :on-agent (and handoff
                                          (lambda (agent)
                                            (setf (handoff-agent handoff) agent))))))
             (release ()
               ;; The server's own part in the call is over.
               (unless released
                 (setf released t)
                 (when agent (decf (server-active-calls server)))
                 (when stop (funcall stop)))))
      (when handoff (setf (handoff-release handoff) #'release))
      (setf (sip:call-auto-answer-reinvite call) nil
            (sip:call-accept-refer call) nil
            (sip:call-on-event call)
            (lambda (call event &key cause sdp &allow-other-keys)
              (case event
                (:established (begin))
                (:reinvite
                 ;; A hold, a resume or a change of address from the phone:
                 ;; answered from what it offers (RFC 3264 section 8).
                 (cond ((null sdp) (sip:answer call :sdp (leg-sdp-text a)))
                       ((leg-answer-offer server a sdp) (sip:answer call :sdp (leg-sdp-text a)))
                       (t (sip:reject call 488))))
                (:terminated
                 (server-log server "call from ~a ended (~(~a~))" (call-caller call) cause)
                 (when handoff (handoff-cancel handoff))
                 (release)
                 (leg-close a)))))
      (when (eq (sip:call-state call) :established)
        (begin)
        (when (leg-told-held a)
          (leg-reoffer a nil))))))

;;; Handing a call from the agent to a phone
;;;
;;; A caller talking to the agent can be put through to an extension: by
;;; keying its number, or by asking, which the model acts on with a tool.
;;; The extension rings while the caller stays with the agent; when a phone
;;; answers the agent leaves and the two are bridged, and when none does the
;;; agent carries on.  Only listed extensions and groups can be reached this
;;; way: the agent never dials out.

(defun bridge-join (server a b label)
  "Bridge two legs whose calls are both established."
  (let ((bridge (make-instance 'bridge :server server :a a :label label)))
    (setf (bridge-state bridge) :connected
          (bridge-b bridge) b
          (leg-held b) nil)
    (push bridge (server-bridges server))
    (bridge-take-leg bridge a)
    (bridge-take-leg bridge b)
    (legs-connect a b)
    (bridge-log bridge "connected")
    (server-publish-dialogs server)
    bridge))

(defun handoff-cancel (handoff)
  "The caller has gone: stop ringing on their behalf."
  (sip:cancel-timer (handoff-timer handoff))
  (when (handoff-bridge handoff)
    (bridge-abandon (shiftf (handoff-bridge handoff) nil))))

(defun handoff-start (handoff number on-result)
  "Ring the extension NUMBER for the caller who is talking to the agent.
Returns NIL when ringing has started; ON-RESULT is then called with
:ANSWERED, after which the two are talking and the agent has left, or with
why not (:BUSY, :DECLINED, :NO-ANSWER, :UNAVAILABLE).  When ringing cannot
start, returns the reason instead (:UNKNOWN, :UNAVAILABLE, :RINGING, :GONE)
and, as a second value, the extension if there is one.  Runs on the SIP
event loop."
  (let* ((server (handoff-server handoff))
         (a (handoff-leg handoff))
         (call (leg-call a))
         (action (server-route server number))
         (extension (and (consp action) (eq (first action) :extension) (second action))))
    (cond
      ((handoff-bridge handoff) (values :ringing extension))
      ((not (eq (sip:call-state call) :established)) :gone)
      ((null extension) :unknown)
      (t
       (let ((targets (server-extension-targets server extension))
             (label (format nil "~a -> ~a"
                            (sip:uri-string (sip:name-addr-uri (call-identity server call)))
                            (extension-label extension))))
         (cond
           ((null targets) (values :unavailable extension))
           (t
            (server-log server "  putting the caller through to ~a"
                        (extension-label extension))
            (setf (leg-party a) (call-identity server call)
                  (leg-extension a) (call-caller-extension call)
                  (handoff-bridge handoff)
                  (bridge-call
                   server a targets
                   :from (call-identity server call)
                   :label label
                   :ring-seconds (or (extension-ring-seconds extension)
                                     (server-ring-seconds server))
                   :established t
                   :on-failed (lambda (status reason)
                                (declare (ignore status))
                                (setf (handoff-bridge handoff) nil)
                                (funcall on-result reason))
                   :on-answered (lambda (new)
                                  (setf (handoff-bridge handoff) nil)
                                  (cond
                                    ((eq (sip:call-state call) :established)
                                     (funcall on-result :answered)
                                     (funcall (handoff-release handoff))
                                     (bridge-join server a new label))
                                    (t (sip:hangup (leg-call new))
                                       (leg-close new))))))
            (values nil extension))))))))

(defun spoken-digits (number)
  "NUMBER as it should be read out: digit by digit."
  (format nil "~{~a~^ ~}" (coerce number 'list)))

(defun handoff-wording (reason number extension)
  "What the caller is told when they cannot be put through."
  (let ((who (if (and extension (extension-name extension))
                 (extension-name extension)
                 (format nil "extension ~a" (spoken-digits number)))))
    (ecase reason
      (:unknown (format nil "There is no extension ~a." (spoken-digits number)))
      (:unavailable (format nil "~a is not available right now." who))
      (:busy (format nil "~a is on another call." who))
      (:declined (format nil "~a did not take the call." who))
      (:no-answer (format nil "~a is not answering." who))
      (:ringing "I am already putting you through.")
      (:gone "The call has ended."))))

(defun handoff-ring-seconds (handoff extension)
  (ceiling (or (and extension (extension-ring-seconds extension))
               (server-ring-seconds (handoff-server handoff)))))

;; By keypad

(defun handoff-dial (handoff)
  "Ring the number keyed so far."
  (let ((agent (handoff-agent handoff))
        (number (shiftf (handoff-digits handoff) "")))
    (sip:cancel-timer (handoff-timer handoff))
    (when (and agent (plusp (length number)))
      (let (extension)
        (flet ((result (reason)
                 (unless (eq reason :answered)
                   (agent-resume agent)
                   (agent-say agent (format nil "~a Is there anything else I can do?"
                                            (handoff-wording reason number extension))))))
          (multiple-value-bind (problem found) (handoff-start handoff number #'result)
            (setf extension found)
            (cond
              ((eq problem :ringing))
              (problem
               (agent-say agent (handoff-wording problem number extension)))
              (t
               (agent-wait agent)
               (agent-say agent (format nil "Calling ~a."
                                        (or (extension-name extension)
                                            (format nil "extension ~a"
                                                    (spoken-digits number)))))
               (agent-post agent (list :ringback
                                       (handoff-ring-seconds handoff extension)))))))))))

(defun handoff-key (handoff key)
  "The caller pressed KEY.  Digits collect into a number, which is rung as
soon as it can only be one extension, at #, or after a pause; * starts over.
Runs on the SIP event loop."
  (let ((server (handoff-server handoff)))
    (sip:cancel-timer (handoff-timer handoff))
    (cond
      ((handoff-bridge handoff))          ; already ringing someone
      ((char= key #\*) (setf (handoff-digits handoff) ""))
      ((char= key #\#) (handoff-dial handoff))
      ((digit-char-p key)
       (let* ((digits (setf (handoff-digits handoff)
                            (concatenate 'string (handoff-digits handoff) (string key))))
              (possible (remove-if-not
                         (lambda (extension)
                           (alexandria:starts-with-subseq digits (extension-number extension)))
                         (server-extensions server))))
         (if (and possible
                  (or (rest possible)
                      (string/= digits (extension-number (first possible)))))
             ;; It may yet become one of several numbers: wait for more.
             (setf (handoff-timer handoff)
                   (sip:schedule (sip:ua-scheduler (server-ua server)) *digit-pause*
                                 (lambda () (handoff-dial handoff))))
             ;; One extension exactly, or none at all, which is said at once.
             (handoff-dial handoff)))))))

;; By asking

(defun handoff-directory (server)
  "The extensions a caller can be put through to, for the model."
  (format nil "You can put the caller through to one of these extensions with ~
the transfer_call tool, when they ask for a person, a group or an extension ~
number.  Say that you are putting them through before you call the tool, and ~
nothing after it succeeds.  If it fails, tell them and offer to help ~
otherwise.  You cannot call any other number.  Extensions: ~
~{~a~^; ~}."
          (loop for extension in (server-extensions server)
                collect (format nil "~a~@[ (~a)~]" (extension-number extension)
                                (extension-name extension)))))

(defun handoff-tool (handoff)
  "The tool with which the model puts the caller through."
  (make-function-tool
   "transfer_call"
   "Put the caller through to an extension.  It rings while the caller waits, and returns when someone answers or nobody does."
   (lambda (input)
     (let* ((server (handoff-server handoff))
            (agent (handoff-agent handoff))
            (number (normalize-number (princ-to-string (gethash "extension" input ""))))
            (outcome nil))
       (multiple-value-bind (problem extension)
           (sip:call-on-loop (sip:ua-scheduler (server-ua server))
                             (lambda ()
                               (handoff-start handoff number
                                              (lambda (reason) (setf outcome reason)))))
         (cond
           (problem (handoff-wording problem number extension))
           (t
            (agent-wait agent)
            (agent-ringback agent (handoff-ring-seconds handoff extension))
            (loop until (or outcome (not (agent-running agent)))
                  do (sleep 1/20))
            (cond ((eq outcome :answered) "The caller has been put through.")
                  ((null outcome) "The call has ended.")
                  (t
                   (agent-resume agent)
                   (handoff-wording outcome number extension))))))))
   :schema (json-object
            "type" "object"
            "properties" (json-object
                          "extension" (json-object
                                       "type" "string"
                                       "description" "The extension number, such as 101."))
            "required" (vector "extension"))
   :instructions (handoff-directory (handoff-server handoff))))

(defun handoff-agent-options (handoff leg)
  "START-AGENT arguments that let the caller on LEG be put through."
  (let ((scheduler (sip:ua-scheduler (server-ua (handoff-server handoff)))))
    (list :tools (list (handoff-tool handoff))
          :event-type (leg-event-type leg)
          :on-digit (lambda (key)
                      ;; From the RTP thread to the SIP event loop.
                      (sip:post scheduler (lambda () (handoff-key handoff key)))))))

(defun server-deliver (server a number route &optional on-behalf-of)
  "Send the incoming call on leg A to NUMBER, wherever the dialplan and the
extension list say that is.  ON-BEHALF-OF is the extension whose forwarding
sent it there."
  (let ((action (server-route server number)))
    (cond
      ((> (route-forwards route) *max-forwards*)
       (server-log server "  forwarded too many times; giving up")
       (sip:reject (leg-call a) 482)
       (leg-close a))
      ((eq action :trunk)
       ;; Forwarding to an outside number spends the forwarding extension's
       ;; right to dial out, not the caller's.
       (server-ring-trunk server a number on-behalf-of))
      ((and (consp action) (eq (first action) :extension))
       (server-ring-extension server a (second action) route))
      (t (server-answer-with server a action)))))

(defun server-forward (server a extension target cause route)
  "The call for EXTENSION goes to TARGET instead: a number, or :AGENT."
  (server-log server "  ~a -> ~(~a~) (~a)" (extension-label extension) target
              (sip:reason-phrase cause))
  (cond ((eq target :agent)
         (server-answer-with server a :agent))
        (t
         (let ((number (normalize-number target)))
           (server-deliver server a number
                           (route-retarget route server number cause)
                           extension)))))

(defun server-ring-extension (server a extension &optional
                                                   (route (route-start
                                                           server (extension-number extension))))
  "Ring EXTENSION for the incoming call on leg A, or send the call on as
the extension's forwarding says (RFC 5359 sections 2.7 to 2.9)."
  (let ((call (leg-call a)))
    (flet ((failed (status reason)
             (let ((target (ecase reason
                             ((:busy :declined) (extension-busy extension))
                             ((:no-answer :unavailable) (extension-no-answer extension)))))
               (cond (target
                      (server-forward server a extension target
                                      (ecase reason
                                        (:busy 486) (:declined 603)
                                        (:no-answer 408) (:unavailable 480))
                                      route))
                     (t
                      (sip:reject call status)
                      (leg-close a))))))
      (cond
        ((extension-forward extension)
         (server-forward server a extension (extension-forward extension) 302 route))
        (t
         (let ((targets (server-extension-targets server extension route)))
           (cond
             ((null targets)
              (server-log server "  ~a has no phone registered" (extension-label extension))
              (failed 480 :unavailable))
             (t
              (bridge-call server a targets
                           :from (call-identity server call)
                           :label (format nil "~a -> ~a"
                                          (sip:uri-string
                                           (sip:name-addr-uri (call-identity server call)))
                                          (extension-label extension))
                           :ring-seconds (or (extension-ring-seconds extension)
                                             (server-ring-seconds server))
                           :on-failed #'failed)))))))))

(defun server-call-extension (server call extension)
  "Dialplan action for a call to EXTENSION."
  (let ((a (server-open-leg server :call call :trunk (eq (call-kind server call) :trunk))))
    (cond ((leg-answer-offer server a (sip:call-remote-sdp call))
           (server-ring-extension server a extension))
          (t
           ;; No offer, or no audio in it that we can relay.
           (leg-close a)
           (sip:reject call 488)))))

;;; Calls out through the trunk

(defparameter *outbound-ring-seconds* 60
  "How long a call placed through the trunk may ring.  The telephone
network gives up by itself; this is the backstop.")

(defun e164-number (number)
  "NUMBER as the provider wants it: eleven digits for North America."
  (if (= (length number) 10)
      (concatenate 'string "1" number)
      number))

(defun server-trunk-uri (server number)
  (let ((registrar (sip:ensure-uri (getf (server-trunk server) :registrar))))
    (sip:make-sip-uri :user (e164-number number)
                      :host (sip:sip-uri-host registrar)
                      :port (sip:sip-uri-port registrar))))

(defun server-trunk-target (server number)
  (let ((uri (server-trunk-uri server number)))
    (list :uri uri :to (sip:make-name-addr :uri uri) :trunk t)))

(defun server-trunk-identity (server &optional extension)
  "Who a call through the trunk is from: the server's number there, since a
provider only carries calls from numbers it knows, under the caller's name."
  (sip:make-name-addr
   :display-name (and extension (extension-name extension))
   :uri (server-trunk-uri server (server-outbound-caller-id server))))

(defun server-ring-trunk (server a number extension)
  "Place the incoming call on leg A to the outside NUMBER, on the authority
of EXTENSION, which must be allowed to dial out.  Calls that came in from
the trunk never go back out by themselves: that is how toll fraud works."
  (let ((call (leg-call a)))
    (cond
      ((not (and extension (extension-outbound extension)))
       (server-log server "  ~a may not dial out"
                   (if extension (extension-label extension) "the caller"))
       (sip:reject call 403)
       (leg-close a))
      (t
       (bridge-call server a (list (server-trunk-target server number))
                    :from (server-trunk-identity server extension)
                    :label (format nil "~a -> ~a" (extension-label extension)
                                   (e164-number number))
                    :ring-seconds *outbound-ring-seconds*)))))

(defun server-call-trunk (server call number)
  "Dialplan action for a telephone number dialled by a phone."
  (let ((a (server-open-leg server :call call :trunk (eq (call-kind server call) :trunk))))
    (cond ((leg-answer-offer server a (sip:call-remote-sdp call))
           (server-ring-trunk server a number (call-caller-extension call)))
          (t
           (leg-close a)
           (sip:reject call 488)))))

;;; Busy lamps: the state of an extension's calls (RFC 4235, over RFC 6665)

(defun extension-dialogs (server extension)
  "The bridged calls EXTENSION's phones are in, each as (id direction state
remote): what a busy lamp for the extension shows."
  (loop for bridge in (server-bridges server)
        append (loop for leg in (bridge-legs bridge)
                     for calling = (eq leg (bridge-a bridge))
                     when (and (eq (leg-extension leg) extension)
                               ;; The party a transfer is being rung for is
                               ;; counted in the call it is still in.
                               (not (and calling (bridge-established-p bridge))))
                       collect (list (leg-id leg)
                                     (if calling "initiator" "recipient")
                                     (cond ((eq (bridge-state bridge) :connected) "confirmed")
                                           (calling "trying")
                                           (t "early"))
                                     (cond ((eq (bridge-state bridge) :connected)
                                            (leg-label (bridge-peer bridge leg)))
                                           (calling nil)
                                           (t (leg-label (bridge-a bridge))))))))

(defun xml-escape (string)
  (with-output-to-string (out)
    (loop for c across string
          do (case c
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char c out))))))

(defun dialog-info-document (entity version dialogs)
  "RFC 4235 section 4: the full state of ENTITY's dialogs."
  (with-output-to-string (out)
    (format out "<?xml version=\"1.0\"?>~%")
    (format out "<dialog-info xmlns=\"urn:ietf:params:xml:ns:dialog-info\" ~
                 version=\"~d\" state=\"full\" entity=\"~a\">~%"
            version (xml-escape entity))
    (loop for (id direction state remote) in dialogs
          do (format out "  <dialog id=\"~a\" direction=\"~a\">~%    <state>~a</state>~%"
                     id direction state)
             (when remote
               (format out "    <remote><identity>~a</identity></remote>~%"
                       (xml-escape remote)))
             (format out "  </dialog>~%"))
    (format out "</dialog-info>~%")))

(defun subscription-extension (server subscription)
  (let ((uri (sip:subscription-resource subscription)))
    (find-extension (server-extensions server) (normalize-number (uri-user uri)))))

(defun server-dialog-document (server subscription)
  "The document for SUBSCRIPTION, or NIL when it names no extension."
  (let ((extension (subscription-extension server subscription)))
    (when extension
      (let ((dialogs (extension-dialogs server extension)))
        ;; Remembered, so that only changes are published.
        (setf (sip:subscription-user-data subscription) (list dialogs))
        (dialog-info-document
         (sip:uri-string (sip:name-addr-uri
                          (server-identity server (extension-number extension))))
         (sip:subscription-version subscription)
         dialogs)))))

(defun server-publish-dialogs (server)
  "Notify whoever watches an extension whose calls have changed."
  (dolist (subscription (sip:ua-event-subscriptions (server-ua server) "dialog"))
    (let ((extension (subscription-extension server subscription)))
      (when (and extension
                 (not (equal (list (extension-dialogs server extension))
                             (sip:subscription-user-data subscription))))
        (sip:subscription-notify subscription)))))

(defun server-offer-dialog-events (server)
  "Let authenticated phones subscribe to the dialog state of extensions."
  (let* ((ua (server-ua server))
         (registrar (sip:ua-registrar ua)))
    (sip:ua-add-event-package
     ua "dialog"
     :content-type "application/dialog-info+xml"
     :authorize (lambda (subscription request)
                  (declare (ignore subscription))
                  (multiple-value-bind (user problem)
                      (sip:registrar-authenticate registrar request)
                    (if (and user (find-extension (server-extensions server) user))
                        t
                        (values nil 401
                                `(("WWW-Authenticate"
                                   ,(sip:registrar-make-challenge
                                     registrar :stale (eq problem :stale))))))))
     :body (lambda (subscription) (server-dialog-document server subscription)))))
