(defpackage :robot-name
  (:use :cl)
  (:export :build-robot :robot-name :reset-name))

(in-package :robot-name)

(defvar *robot-names-history* nil)
(setf *random-state* (make-random-state t))

(defun random-digit ()
    (digit-char (random 10)))

(defun random-lowcase ()
    (code-char (+ (char-code #\a) (random 26))))

(defun random-upcase ()
    (char-upcase (random-lowcase)))

(defun generate-robot-name ()
    (coerce (list (random-upcase)
                  (random-upcase)
                  (random-digit)
                  (random-digit)
                  (random-digit))
            'string))

(defun generate-unique-robot-name ()
    (loop :for name = (generate-robot-name)
          :if (not (member name *robot-names-history*))
            :do (push name *robot-names-history*)
            :and :do (return name)))

(defclass robot ()
  ((name :accessor robot-name
         :initform (generate-unique-robot-name))))

(defun build-robot ()
    (make-instance 'robot))

(defmethod reset-name ((robot robot))
    (setf (robot-name robot) (generate-unique-robot-name)))
