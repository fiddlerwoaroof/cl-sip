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
        (remove bridge (server-bridges (bridge-server bridge)))))

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
       (bridge-log bridge "answered")))))

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
     (when (and (eq (bridge-state bridge) :calling) (null (bridge-forks bridge)))
       (multiple-value-call #'bridge-fail bridge
         (bridge-failure (bridge-outcomes bridge)))))))

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
    (bridge-log bridge "transferred: now ~a and ~a" (leg-label keep) (leg-label new))))

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

(defun server-attach-action (server a action)
  "Run the dialplan ACTION on leg A: when its call is established, or at
once if it already is (a call transferred to the agent)."
  (let ((call (leg-call a))
        (agent (eq action :agent))
        (stop nil))
    (when agent (incf (server-active-calls server)))
    (flet ((begin ()
             (setf (leg-held a) nil
                   stop (server-start-media server call action
                                            (leg-session a) (leg-codec a)))))
      (setf (sip:call-auto-answer-reinvite call) t
            (sip:call-accept-refer call) nil
            (sip:call-on-event call)
            (lambda (call event &key cause &allow-other-keys)
              (case event
                (:established (begin))
                (:terminated
                 (server-log server "call from ~a ended (~(~a~))" (call-caller call) cause)
                 (when agent (decf (server-active-calls server)))
                 (when stop (funcall stop))
                 (leg-close a)))))
      (when (eq (sip:call-state call) :established)
        (begin)
        (when (leg-told-held a)
          (leg-reoffer a nil))))))

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
