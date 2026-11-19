(in-package :cl-sip.voice)

;;;; The extension list: who may register, and where calls to each number go.
;;;; This is local policy; no RFC defines it.  See docs/pbx-plan.md.

(defstruct extension
  number                                ; what is dialled, a string
  password                              ; for digest authentication
  name                                  ; display name
  forward                               ; where every call goes instead
  busy                                  ; ... when the extension is busy
  no-answer                             ; ... when nobody answers
  ring-seconds                          ; how long it rings (server default if NIL)
  ring                                  ; a group: the numbers that ring together
  outbound)                             ; may dial out through the trunk

(defun extension-group-p (extension)
  (and (extension-ring extension) t))

(defun extension-label (extension)
  (format nil "~a~@[ (~a)~]" (extension-number extension) (extension-name extension)))

(defun parse-extension (form)
  "FORM is (number &key password name forward busy no-answer ring-seconds
ring outbound), or an EXTENSION."
  (if (extension-p form)
      form
      (flet ((fail (control &rest args)
               (error "Bad extension ~s: ~?" form control args)))
        (unless (and (consp form) (stringp (first form)) (evenp (length (rest form))))
          (fail "expected (\"number\" :key value ...)"))
        (destructuring-bind (number &key password name forward busy no-answer
                                      ring-seconds ring outbound)
            (handler-case (destructuring-bind (number &rest keys
                                               &key password name forward busy no-answer
                                                 ring-seconds ring outbound)
                              form
                            (declare (ignore password name forward busy no-answer
                                             ring-seconds ring outbound))
                            (cons number keys))
              (error () (fail "unknown option")))
          (when (or (zerop (length number)) (find-if (lambda (c) (find c " @:;<>\"")) number))
            (fail "the number may not be empty or contain spaces or punctuation"))
          (unless (or (null ring) (and (consp ring) (every #'stringp ring)))
            (fail ":ring takes a list of numbers"))
          (unless (or ring (and (stringp password) (plusp (length password))))
            (fail "a :password is required"))
          (unless (typep name '(or null string))
            (fail ":name must be a string"))
          (dolist (target (list forward busy no-answer))
            (unless (or (null target) (eq target :agent)
                        (and (stringp target) (plusp (length target))))
              (fail "a forwarding target is a number, :agent or nil")))
          (unless (typep ring-seconds '(or null (real (0))))
            (fail ":ring-seconds must be a positive number"))
          (make-extension :number number :password password :name name
                          :forward forward :busy busy :no-answer no-answer
                          :ring-seconds ring-seconds :ring ring
                          :outbound (and outbound t))))))

(defun parse-extensions (forms)
  "The extension list from FORMS, checked: numbers are unique and groups
ring numbers that exist."
  (let ((extensions (mapcar #'parse-extension forms)))
    (loop for (extension . rest) on extensions
          do (when (find (extension-number extension) rest
                         :key #'extension-number :test #'string=)
               (error "Extension ~a is listed twice." (extension-number extension)))
             (dolist (member (extension-ring extension))
               (let ((target (find member extensions :key #'extension-number
                                                     :test #'string=)))
                 (when (or (null target) (extension-group-p target))
                   (error "Group ~a rings ~a, which is not a phone's extension."
                          (extension-number extension) member)))))
    extensions))

(defun load-extensions (path)
  "Read an extension list from the file PATH: one form per extension, read
as data and never evaluated."
  (with-open-file (in path :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*package* (find-package :cl-sip.voice)))
        (parse-extensions
         (loop for form = (read in nil in)
               until (eq form in)
               collect form))))))

(defun find-extension (extensions number)
  (find number extensions :key #'extension-number :test #'string=))

(defun normalize-number (user)
  "What was dialled, from the user part of a URI.  For a telephone number
the visual separators and a leading plus mean nothing (RFC 3966 section
5.1.1) and parameters such as a phone-context are dropped; anything else is
left alone."
  (let* ((user (or user ""))
         (user (subseq user 0 (or (position #\; user) (length user)))))
    (if (and (some #'digit-char-p user)
             (every (lambda (c) (or (digit-char-p c) (find c "+-.() *#"))) user))
        (remove-if (lambda (c) (find c "+-.() ")) user)
        user)))
