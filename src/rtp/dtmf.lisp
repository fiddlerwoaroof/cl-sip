(in-package :cl-sip.rtp)

;;;; Telephone events (RFC 4733 section 2.3)
;;;;
;;;;   0                   1                   2                   3
;;;;   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
;;;;  |     event     |E|R| volume    |          duration             |

(defstruct telephone-event
  (event 0 :type (unsigned-byte 8))
  (end nil)
  (volume 10 :type (integer 0 63))      ; power level in -dBm0
  (duration 0 :type (unsigned-byte 16))) ; in timestamp units

(defun encode-telephone-event (event)
  (let ((buffer (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref buffer 0) (telephone-event-event event)
          (aref buffer 1) (logior (if (telephone-event-end event) #x80 0)
                                  (telephone-event-volume event)))
    (put-u16 buffer 2 (telephone-event-duration event))
    buffer))

(defun decode-telephone-event (payload)
  (when (< (length payload) 4)
    (error 'rtp-error :message "telephone-event payload too short"))
  (make-telephone-event :event (aref payload 0)
                        :end (logbitp 7 (aref payload 1))
                        :volume (ldb (byte 6 0) (aref payload 1))
                        :duration (get-u16 payload 2)))

(defparameter +dtmf-characters+ "0123456789*#ABCD")

(defun dtmf-event-code (char)
  "Event code for a DTMF digit (RFC 4733 section 3.2)."
  (or (position (char-upcase char) +dtmf-characters+)
      (error 'rtp-error :message (format nil "not a DTMF digit: ~s" char))))

(defun dtmf-event-char (code)
  (and (< code (length +dtmf-characters+)) (char +dtmf-characters+ code)))

(defun dtmf-packets (digit &key (payload-type 101) (ssrc 0) (sequence 0) (timestamp 0)
                             (duration-ms 100) (packet-ms 20) (clock-rate 8000)
                             (volume 10) (end-repeats 3))
  "The RTP packets reporting one DTMF digit (RFC 4733 section 2.5.1): every
packet carries the timestamp of the start of the event and the duration so
far; the first has the marker bit; the final report, with the E bit, is sent
END-REPEATS times."
  (let* ((code (dtmf-event-code digit))
         (step (floor (* clock-rate packet-ms) 1000))
         (total (floor (* clock-rate duration-ms) 1000))
         (packets '()))
    (flet ((emit (duration &key marker end)
             (push (make-rtp-packet
                    :marker marker :payload-type payload-type
                    :sequence (logand sequence #xFFFF)
                    :timestamp timestamp :ssrc ssrc
                    :payload (encode-telephone-event
                              (make-telephone-event :event code :end end :volume volume
                                                    :duration (min duration #xFFFF))))
                   packets)
             (incf sequence)))
      (loop for duration from step below total by step
            for first = t then nil
            do (emit duration :marker first))
      (dotimes (i end-repeats)
        (emit total :end t :marker (and (null packets) (zerop i)))))
    (nreverse packets)))
