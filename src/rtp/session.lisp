(in-package :cl-sip.rtp)

;;;; A minimal RTP session over UDP: send with sequence/timestamp
;;;; bookkeeping, receive on a thread.  No RTCP, no jitter buffer.

(defclass rtp-session ()
  ((local-host :initarg :local-host :initform "127.0.0.1" :reader rtp-session-local-host)
   (local-port :initarg :local-port :initform 0 :accessor rtp-session-local-port)
   (remote-host :initarg :remote-host :initform nil :accessor rtp-session-remote-host)
   (remote-port :initarg :remote-port :initform nil :accessor rtp-session-remote-port)
   (payload-type :initarg :payload-type :initform 0 :accessor rtp-session-payload-type)
   (ssrc :initarg :ssrc :initform (random (expt 2 32)) :reader rtp-session-ssrc)
   (sequence :initform (random (expt 2 16)) :accessor rtp-session-sequence)
   (timestamp :initform (random (expt 2 32)) :accessor rtp-session-timestamp)
   (on-packet :initarg :on-packet :initform nil :accessor rtp-session-on-packet
              :documentation "(lambda (session packet)) called on the receive thread.")
   (packets-sent :initform 0 :accessor rtp-session-packets-sent)
   (packets-received :initform 0 :accessor rtp-session-packets-received)
   (remote-ssrc :initform nil :accessor rtp-session-remote-ssrc)
   (port-range :initarg :port-range :initform nil
               :documentation "(low . high): bind to the first free port in
this range instead of an ephemeral one, e.g. a range forwarded by a router.")
   (stun :initarg :stun :initform nil
         :documentation "(host . port) of a STUN server to ask, at start, for
the socket's public address.")
   (public-host :initform nil :accessor rtp-session-public-host
                :documentation "Public address of the socket, when STUN found one.")
   (public-port :initform nil :accessor rtp-session-public-port)
   (latch :initarg :latch :initform nil :accessor rtp-session-latch
          :documentation "When true, send to wherever the peer's packets actually
come from rather than the address it advertised (symmetric RTP), which is
what makes media work through a NAT.")
   (forward-source :initform nil
                   :documentation "SSRC of the stream being forwarded out of this
session, with the offsets that map its sequence numbers and timestamps onto
ours (see RTP-FORWARD).")
   (forward-sequence-offset :initform 0)
   (forward-timestamp-offset :initform 0)
   (socket :initform nil :accessor rtp-session-socket)
   (thread :initform nil :accessor rtp-session-thread)
   (running :initform nil :accessor rtp-session-running)
   (lock :initform (bt:make-lock "rtp-send") :reader rtp-session-lock)))

(defun make-rtp-session (&rest args &key local-host local-port remote-host remote-port
                                      payload-type ssrc on-packet latch port-range stun)
  (declare (ignore local-host local-port remote-host remote-port payload-type ssrc
                   on-packet latch port-range stun))
  (apply #'make-instance 'rtp-session args))

(defun rtp-session-bind (session)
  (flet ((bind (port)
           (usocket:socket-connect nil nil :protocol :datagram
                                           :local-host (rtp-session-local-host session)
                                           :local-port port)))
    (let ((range (slot-value session 'port-range)))
      (if range
          (or (loop for port from (car range) to (cdr range)
                    for socket = (ignore-errors (bind port))
                    when socket return socket)
              (error 'rtp-error
                     :message (format nil "no free RTP port in ~d-~d" (car range) (cdr range))))
          (bind (rtp-session-local-port session))))))

(defun rtp-session-start (session)
  "Bind the socket (an ephemeral port when the local port is 0) and start
receiving."
  (let ((socket (rtp-session-bind session)))
    (setf (rtp-session-socket session) socket
          (rtp-session-local-port session) (usocket:get-local-port socket))
    (let ((stun (slot-value session 'stun)))
      (when stun
        (multiple-value-bind (host port) (stun-query socket (car stun) (cdr stun))
          (setf (rtp-session-public-host session) host
                (rtp-session-public-port session) port))))
    (setf (rtp-session-running session) t
          (rtp-session-thread session)
          (bt:make-thread
           (lambda ()
             (let ((buffer (make-array 2048 :element-type '(unsigned-byte 8))))
               (loop while (rtp-session-running session)
                     do (handler-case
                            (when (usocket:wait-for-input socket :timeout 1/5 :ready-only t)
                              (multiple-value-bind (data size host port)
                                  (usocket:socket-receive socket buffer (length buffer))
                                (declare (ignore data))
                                (when (and size (>= size 12))
                                  (let ((packet (decode-rtp-packet buffer :end size)))
                                    (incf (rtp-session-packets-received session))
                                    (when (and (rtp-session-latch session) host port)
                                      (setf (rtp-session-remote-host session)
                                            (if (stringp host)
                                                host
                                                (format nil "~{~d~^.~}" (coerce host 'list)))
                                            (rtp-session-remote-port session) port))
                                    (setf (rtp-session-remote-ssrc session)
                                          (rtp-packet-ssrc packet))
                                    (when (rtp-session-on-packet session)
                                      (funcall (rtp-session-on-packet session)
                                               session packet))))))
                          (error ()
                            (unless (rtp-session-running session) (return)))))))
           :name (format nil "rtp-~d" (rtp-session-local-port session))))
    session))

(defun rtp-session-stop (session)
  "Stop receiving and close the socket.  Waits for the receive thread, so
do not call it from that thread (i.e. from an ON-PACKET callback)."
  (when (rtp-session-running session)
    (setf (rtp-session-running session) nil)
    (bt:join-thread (rtp-session-thread session))
    (usocket:socket-close (rtp-session-socket session))
    (setf (rtp-session-socket session) nil
          (rtp-session-thread session) nil))
  session)

(defun send-packet (session packet)
  (let ((octets (encode-rtp-packet packet)))
    (usocket:socket-send (rtp-session-socket session) octets (length octets)
                         :host (rtp-session-remote-host session)
                         :port (rtp-session-remote-port session))
    (incf (rtp-session-packets-sent session))
    packet))

(defun rtp-skip (session samples)
  "Advance the timestamp by SAMPLES without sending, to account for a gap
in the stream (silence suppression, RFC 3551 section 4.1)."
  (bt:with-lock-held ((rtp-session-lock session))
    (setf (rtp-session-timestamp session)
          (logand (+ (rtp-session-timestamp session) samples) #xFFFFFFFF))))

(defun rtp-send (session payload &key marker payload-type (samples (length payload)))
  "Send PAYLOAD as the next packet of the stream.  SAMPLES is how far the
timestamp advances afterwards: for G.711 that is the payload length."
  (bt:with-lock-held ((rtp-session-lock session))
    (prog1
        (send-packet session
                     (make-rtp-packet
                      :marker marker
                      :payload-type (or payload-type (rtp-session-payload-type session))
                      :sequence (rtp-session-sequence session)
                      :timestamp (rtp-session-timestamp session)
                      :ssrc (rtp-session-ssrc session)
                      :payload payload))
      (setf (rtp-session-sequence session)
            (logand (1+ (rtp-session-sequence session)) #xFFFF)
            (rtp-session-timestamp session)
            (logand (+ (rtp-session-timestamp session) samples) #xFFFFFFFF)))))

(defun rtp-forward (session packet &key payload-type payload)
  "Send PACKET, which arrived on another session, as part of SESSION's own
stream: a relay between two calls.  The packet keeps its marker and its
place in time (gaps, losses and reordering in the source stream are passed
on as they are, which is what the receiver's jitter buffer needs), but takes
this session's SSRC and continues this session's sequence numbers and
timestamps.  PAYLOAD-TYPE and PAYLOAD replace the packet's own when the two
calls negotiated different numbers or codecs.  Nothing is sent until the
session knows where its peer is."
  (when (and (rtp-session-socket session) (rtp-session-remote-host session))
    (bt:with-lock-held ((rtp-session-lock session))
      (with-slots (forward-source forward-sequence-offset forward-timestamp-offset) session
        (unless (eql forward-source (rtp-packet-ssrc packet))
          ;; A new source: its first packet continues where we left off.
          (setf forward-source (rtp-packet-ssrc packet)
                forward-sequence-offset (- (rtp-session-sequence session)
                                           (rtp-packet-sequence packet))
                forward-timestamp-offset (- (rtp-session-timestamp session)
                                            (rtp-packet-timestamp packet))))
        (let* ((payload (or payload (rtp-packet-payload packet)))
               (sequence (logand (+ (rtp-packet-sequence packet) forward-sequence-offset)
                                 #xFFFF))
               (timestamp (logand (+ (rtp-packet-timestamp packet) forward-timestamp-offset)
                                  #xFFFFFFFF)))
          (prog1
              (send-packet session
                           (make-rtp-packet
                            :marker (rtp-packet-marker packet)
                            :payload-type (or payload-type (rtp-packet-payload-type packet))
                            :sequence sequence :timestamp timestamp
                            :ssrc (rtp-session-ssrc session)
                            :payload payload))
            ;; Anything sent from here afterwards (an announcement, say)
            ;; carries on after the forwarded stream.
            (setf (rtp-session-sequence session) (logand (1+ sequence) #xFFFF)
                  (rtp-session-timestamp session)
                  (logand (+ timestamp (length payload)) #xFFFFFFFF))))))))

(defun rtp-send-dtmf (session digit &key (payload-type 101) (duration-ms 100)
                                      (clock-rate 8000) (volume 10))
  "Send the packets for one DTMF digit as a burst.  A real sender would pace
them 20 ms apart; receivers only need the final report."
  (bt:with-lock-held ((rtp-session-lock session))
    (let ((packets (dtmf-packets digit :payload-type payload-type
                                       :ssrc (rtp-session-ssrc session)
                                       :sequence (rtp-session-sequence session)
                                       :timestamp (rtp-session-timestamp session)
                                       :duration-ms duration-ms :clock-rate clock-rate
                                       :volume volume)))
      (dolist (packet packets)
        (send-packet session packet))
      (setf (rtp-session-sequence session)
            (logand (+ (rtp-session-sequence session) (length packets)) #xFFFF)
            (rtp-session-timestamp session)
            (logand (+ (rtp-session-timestamp session)
                       (floor (* clock-rate duration-ms) 1000))
                    #xFFFFFFFF))
      packets)))
