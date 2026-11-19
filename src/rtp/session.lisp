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
   (socket :initform nil :accessor rtp-session-socket)
   (thread :initform nil :accessor rtp-session-thread)
   (running :initform nil :accessor rtp-session-running)
   (lock :initform (bt:make-lock "rtp-send") :reader rtp-session-lock)))

(defun make-rtp-session (&rest args &key local-host local-port remote-host remote-port
                                      payload-type ssrc on-packet)
  (declare (ignore local-host local-port remote-host remote-port payload-type ssrc on-packet))
  (apply #'make-instance 'rtp-session args))

(defun rtp-session-start (session)
  "Bind the socket (an ephemeral port when the local port is 0) and start
receiving."
  (let ((socket (usocket:socket-connect nil nil :protocol :datagram
                                                :local-host (rtp-session-local-host session)
                                                :local-port (rtp-session-local-port session))))
    (setf (rtp-session-socket session) socket
          (rtp-session-local-port session) (usocket:get-local-port socket)
          (rtp-session-running session) t
          (rtp-session-thread session)
          (bt:make-thread
           (lambda ()
             (let ((buffer (make-array 2048 :element-type '(unsigned-byte 8))))
               (loop while (rtp-session-running session)
                     do (handler-case
                            (when (usocket:wait-for-input socket :timeout 1/5 :ready-only t)
                              (multiple-value-bind (data size)
                                  (usocket:socket-receive socket buffer (length buffer))
                                (declare (ignore data))
                                (when (and size (>= size 12))
                                  (let ((packet (decode-rtp-packet buffer :end size)))
                                    (incf (rtp-session-packets-received session))
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
