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
         :documentation "True while this leg's phone has the call on hold.")))

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

(defun leg-answer-offer (server leg offer-text &key (direction :sendrecv) version)
  "Answer the far end's offer on LEG (RFC 3264 section 6).  Returns the
answer as text, or NIL when the offer has no audio we can take."
  (let ((offer (ignore-errors (sdp:parse-sdp (or offer-text "")))))
    (when offer
      (multiple-value-bind (address port) (leg-address server leg)
        (multiple-value-bind (answer accepted)
            (sdp:make-answer offer :address address :port port :direction direction
                                   :session-id (and (leg-local-sdp leg)
                                                    (sdp:origin-session-id
                                                     (sdp:session-origin (leg-local-sdp leg))))
                                   :session-version (or version "1"))
          (when accepted
            (multiple-value-bind (codecs answered-direction host remote-port)
                (sdp:negotiated-audio answer offer)
              (declare (ignore answered-direction))
              (when (leg-set-codecs leg codecs)
                (leg-set-remote leg host remote-port)
                (setf (leg-local-sdp leg) answer
                      (leg-sdp-text leg) (sdp:sdp-string answer))))))))))

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
   (a :initarg :a :reader bridge-a :documentation "The leg of the call that came in.")
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
A when nobody answers; REASON is :busy, :declined, :no-answer or :unavailable.")))

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
    (if (bridge-on-failed bridge)
        (funcall (bridge-on-failed bridge) status reason)
        (let ((a (bridge-a bridge)))
          (sip:reject (leg-call a) status)
          (leg-close a)))))

(defun bridge-end (bridge cause)
  "One side has gone: release the other and the media."
  (unless (member (bridge-state bridge) '(:failed :ended))
    (setf (bridge-state bridge) :ended)
    (bridge-forget bridge)
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
    (sip:ring (leg-call (bridge-a bridge)))))

(defun bridge-early-media (bridge leg sdp)
  "A ringing leg is sending media before answering (announcements or
ringback from a telephone network): let the caller hear it."
  (when (and (eq (bridge-state bridge) :calling)
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

(defun bridge-fork-event (bridge leg event &key sdp status cause &allow-other-keys)
  (case event
    (:ringing (bridge-ring bridge))
    (:progress (if sdp
                   (bridge-early-media bridge leg sdp)
                   (bridge-ring bridge)))
    (:established (bridge-answered bridge leg sdp))
    (:terminated (bridge-fork-ended bridge leg status cause))))

(defun bridge-a-event (bridge event &key cause &allow-other-keys)
  (case event
    (:terminated (bridge-end bridge cause))))

(defun bridge-call (server a targets &key from headers (label "call") on-failed
                                       (ring-seconds (server-ring-seconds server)))
  "Ring TARGETS for the incoming call on leg A, whose offer has been
answered already (LEG-ANSWER-OFFER), and join A to the first that answers.
Each target is a plist: :URI where to send the INVITE, :TO the party being
called, :TRUNK true when it is reached through the trunk.  FROM is who the
call is from, as the called phones will see it."
  (let ((bridge (make-instance 'bridge :server server :a a :label label
                                       :on-failed on-failed))
        (ua (server-ua server)))
    (push bridge (server-bridges server))
    (setf (sip:call-on-event (leg-call a))
          (lambda (call event &rest args)
            (declare (ignore call))
            (apply #'bridge-a-event bridge event args)))
    (dolist (target targets)
      (let ((leg (server-open-leg server :trunk (getf target :trunk) :target target)))
        (push leg (bridge-forks bridge))
        (setf (leg-call leg)
              (sip:invite ua (getf target :uri)
                          :to (getf target :to) :from from
                          :sdp (leg-make-offer server leg)
                          :headers headers
                          :on-event (lambda (call event &rest args)
                                      (declare (ignore call))
                                      (apply #'bridge-fork-event bridge leg event args))))))
    (setf (bridge-timer bridge)
          (sip:schedule (sip:ua-scheduler ua) ring-seconds
                        (lambda () (bridge-fail bridge 480 :no-answer))))
    (bridge-log bridge "ringing ~d phone~:p" (length targets))
    bridge))

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

(defun server-extension-targets (server extension)
  "Every phone that should ring for EXTENSION: its registered contacts, or
for a group those of each member."
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
                                     :extension member)))))

(defun server-ring-extension (server a extension)
  "Ring EXTENSION for the incoming call on leg A."
  (let ((targets (server-extension-targets server extension))
        (call (leg-call a)))
    (cond ((null targets)
           (server-log server "  ~a has no phone registered" (extension-label extension))
           (sip:reject call 480)
           (leg-close a))
          (t
           (bridge-call server a targets
                        :from (call-identity server call)
                        :label (format nil "~a -> ~a"
                                       (sip:uri-string
                                        (sip:name-addr-uri (call-identity server call)))
                                       (extension-label extension))
                        :ring-seconds (or (extension-ring-seconds extension)
                                          (server-ring-seconds server)))))))

(defun server-call-extension (server call extension)
  "Dialplan action for a call to EXTENSION."
  (let ((a (server-open-leg server :call call :trunk (eq (call-kind server call) :trunk))))
    (cond ((leg-answer-offer server a (sip:call-remote-sdp call))
           (server-ring-extension server a extension))
          (t
           ;; No offer, or no audio in it that we can relay.
           (leg-close a)
           (sip:reject call 488)))))
