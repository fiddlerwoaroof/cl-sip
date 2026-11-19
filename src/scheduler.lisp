(in-package :cl-sip)

;;;; Event loop: a clock, a set of timers and a queue of posted closures.
;;;;
;;;; All protocol state belongs to the thread running the loop.  The loop can
;;;; be driven by a real thread (RUN-LOOP) or by hand with a virtual clock
;;;; (PUMP, ADVANCE-TIME), which is what the tests do.

(defstruct (timer (:constructor %make-timer))
  (due 0 :type real)
  (function nil)
  (cancelled nil)
  (name nil))

(defclass scheduler ()
  ((virtual-p :initarg :virtual :initform nil :reader scheduler-virtual-p)
   (time :initform 0 :accessor scheduler-time
         :documentation "Current time in seconds when the clock is virtual.")
   (timers :initform '() :accessor scheduler-timers
           :documentation "Pending timers sorted by due time.")
   (queue :initform '() :accessor scheduler-queue)
   (lock :initform (bt:make-recursive-lock "sip-scheduler") :reader scheduler-lock)
   (condition :initform (bt:make-condition-variable) :reader scheduler-condition)
   (thread :initform nil :accessor scheduler-thread)
   (running :initform nil :accessor scheduler-running)
   (error-handler :initarg :error-handler :initform nil
                  :accessor scheduler-error-handler
                  :documentation "Called with a condition signalled by an event
when the loop runs in its own thread.  NIL logs to *ERROR-OUTPUT*.")))

(defun make-scheduler (&key virtual)
  (make-instance 'scheduler :virtual virtual))

(defun now (scheduler)
  "Current time in seconds."
  (if (scheduler-virtual-p scheduler)
      (scheduler-time scheduler)
      (/ (get-internal-real-time) internal-time-units-per-second)))

(defun schedule (scheduler delay function &optional name)
  "Arrange for FUNCTION to be called on the loop after DELAY seconds.
Returns a timer that can be passed to CANCEL-TIMER."
  (let ((timer (%make-timer :due (+ (now scheduler) delay) :function function :name name)))
    (bt:with-recursive-lock-held ((scheduler-lock scheduler))
      (setf (scheduler-timers scheduler)
            ;; MERGE is stable: on equal due times earlier timers stay first.
            (merge 'list (scheduler-timers scheduler) (list timer) #'< :key #'timer-due))
      (bt:condition-notify (scheduler-condition scheduler)))
    timer))

(defun cancel-timer (timer)
  (when timer (setf (timer-cancelled timer) t))
  nil)

(defun post (scheduler function)
  "Queue FUNCTION to run on the loop.  Safe to call from any thread."
  (bt:with-recursive-lock-held ((scheduler-lock scheduler))
    (setf (scheduler-queue scheduler)
          (nconc (scheduler-queue scheduler) (list function)))
    (bt:condition-notify (scheduler-condition scheduler)))
  nil)

(defun next-event (scheduler)
  "Pop the next runnable closure: a posted event, or a timer that is due."
  (bt:with-recursive-lock-held ((scheduler-lock scheduler))
    (cond ((scheduler-queue scheduler)
           (pop (scheduler-queue scheduler)))
          (t
           (loop while (and (scheduler-timers scheduler)
                            (timer-cancelled (first (scheduler-timers scheduler))))
                 do (pop (scheduler-timers scheduler)))
           (let ((timer (first (scheduler-timers scheduler))))
             (when (and timer (<= (timer-due timer) (now scheduler)))
               (pop (scheduler-timers scheduler))
               (timer-function timer)))))))

(defun next-timer-due (scheduler)
  (bt:with-recursive-lock-held ((scheduler-lock scheduler))
    (loop while (and (scheduler-timers scheduler)
                     (timer-cancelled (first (scheduler-timers scheduler))))
          do (pop (scheduler-timers scheduler)))
    (let ((timer (first (scheduler-timers scheduler))))
      (and timer (timer-due timer)))))

(defun pump (scheduler)
  "Run every queued event and every timer that is already due.  Returns the
number of events run."
  (loop for event = (next-event scheduler)
        while event
        count (progn (funcall event) t)))

(defun advance-time (scheduler seconds)
  "Virtual clocks only: move time forward by SECONDS, firing timers at the
moments they fall due."
  (assert (scheduler-virtual-p scheduler))
  (pump scheduler)
  (let ((target (+ (scheduler-time scheduler) seconds)))
    (loop for due = (next-timer-due scheduler)
          while (and due (<= due target))
          do (setf (scheduler-time scheduler) (max due (scheduler-time scheduler)))
             (pump scheduler))
    (setf (scheduler-time scheduler) target)
    (pump scheduler))
  (scheduler-time scheduler))

;;; Threaded driver

(defun on-loop-thread-p (scheduler)
  "True when the caller may touch protocol state directly: either the loop
is not running in a thread (manual driving) or we are that thread."
  (or (null (scheduler-thread scheduler))
      (eq (bt:current-thread) (scheduler-thread scheduler))))

(defun run-loop (scheduler)
  (loop while (scheduler-running scheduler)
        do (handler-case (pump scheduler)
             (error (e)
               (if (scheduler-error-handler scheduler)
                   (funcall (scheduler-error-handler scheduler) e)
                   (format *error-output* "~&cl-sip: error in event loop: ~a~%" e))))
           (bt:with-recursive-lock-held ((scheduler-lock scheduler))
             (when (and (scheduler-running scheduler)
                        (null (scheduler-queue scheduler)))
               (let* ((due (next-timer-due scheduler))
                      (wait (if due (- due (now scheduler)) 1)))
                 (when (plusp wait)
                   (bt:condition-wait (scheduler-condition scheduler)
                                      (scheduler-lock scheduler)
                                      :timeout (min wait 1))))))))

(defun start-scheduler (scheduler)
  (assert (not (scheduler-virtual-p scheduler)))
  (unless (scheduler-thread scheduler)
    (setf (scheduler-running scheduler) t
          (scheduler-thread scheduler)
          (bt:make-thread (lambda () (run-loop scheduler)) :name "sip-event-loop")))
  scheduler)

(defun stop-scheduler (scheduler)
  (let ((thread (scheduler-thread scheduler)))
    (when thread
      (setf (scheduler-running scheduler) nil)
      (post scheduler (lambda ()))
      (unless (eq thread (bt:current-thread))
        (bt:join-thread thread))
      (setf (scheduler-thread scheduler) nil)))
  scheduler)

(defun call-on-loop (scheduler function)
  "Run FUNCTION on the loop and return its values, waiting if called from
another thread.  Errors are re-signalled in the caller."
  (if (on-loop-thread-p scheduler)
      (funcall function)
      (let ((lock (bt:make-lock))
            (cv (bt:make-condition-variable))
            (done nil) (values nil) (failure nil))
        (post scheduler
              (lambda ()
                (handler-case (setf values (multiple-value-list (funcall function)))
                  (error (e) (setf failure e)))
                (bt:with-lock-held (lock)
                  (setf done t)
                  (bt:condition-notify cv))))
        (bt:with-lock-held (lock)
          (loop until done do (bt:condition-wait cv lock)))
        (when failure (error failure))
        (values-list values))))

(defmacro on-loop ((scheduler) &body body)
  `(call-on-loop ,scheduler (lambda () ,@body)))
