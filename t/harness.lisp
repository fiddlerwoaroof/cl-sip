(in-package :cl-sip.test)

;;;; A world is a virtual clock plus an in-memory network.  Tests create
;;;; agents in it and step time by hand.

(defstruct world
  (scheduler (sip:make-scheduler :virtual t))
  (network (sip:make-memory-network))
  (times '()))                          ; (time . message) newest first

(defun new-world ()
  (let ((world (make-world)))
    (setf (sip:network-filter (world-network world))
          (lambda (message from to)
            (declare (ignore from to))
            (push (cons (sip:now (world-scheduler world)) message) (world-times world))
            t))
    world))

(defun set-filter (world function)
  "FUNCTION decides the fate of each message (see MEMORY-NETWORK); sends
are still timestamped."
  (setf (sip:network-filter (world-network world))
        (lambda (message from to)
          (push (cons (sip:now (world-scheduler world)) message) (world-times world))
          (funcall function message from to))))

(defun agent (world aor host &rest args &key reliable &allow-other-keys)
  (let ((ua (apply #'sip:make-user-agent
                   :aor aor
                   :scheduler (world-scheduler world)
                   :transports (list (sip:make-memory-transport (world-network world) host
                                                                :reliable reliable))
                   (alexandria:remove-from-plist args :reliable))))
    (sip:start ua)
    ua))

(defun settle (world)
  (sip:pump (world-scheduler world)))

(defun tick (world seconds)
  (sip:advance-time (world-scheduler world) seconds))

(defun summary (message)
  (if (sip:request-p message)
      (sip:request-method message)
      (format nil "~d/~a" (sip:response-status message)
              (sip:cseq-method (sip:msg-cseq message)))))

(defun wire (world)
  "Summaries of every message sent so far, oldest first."
  (mapcar #'summary (sip:network-messages (world-network world))))

(defun wire-messages (world &optional method-or-status)
  (remove-if-not (lambda (m)
                   (or (null method-or-status)
                       (if (stringp method-or-status)
                           (and (sip:request-p m)
                                (string= (sip:request-method m) method-or-status))
                           (and (sip:response-p m)
                                (= (sip:response-status m) method-or-status)))))
                 (sip:network-messages (world-network world))))

(defun send-times (world method-or-status)
  "Virtual times at which matching messages were sent, oldest first."
  (reverse
   (loop for (time . m) in (world-times world)
         when (if (stringp method-or-status)
                  (and (sip:request-p m) (string= (sip:request-method m) method-or-status))
                  (and (sip:response-p m) (= (sip:response-status m) method-or-status)))
           collect time)))

(defun clear-wire (world)
  (setf (sip::network-log (world-network world)) '()
        (world-times world) '()))

(defun recorder ()
  "Returns an event callback and a function listing the events seen so far
as (event . plist), oldest first."
  (let ((events '()))
    (values (lambda (object event &rest args)
              (declare (ignore object))
              (push (cons event args) events))
            (lambda () (reverse events)))))

(defun event-names (events)
  (mapcar #'car events))

(defun crlf (&rest lines)
  "Join LINES with CRLF, ending with a blank line."
  (format nil "~{~a~c~c~}~c~c" (loop for l in lines append (list l #\Return #\Newline))
          #\Return #\Newline))
