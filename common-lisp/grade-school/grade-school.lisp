(defpackage :grade-school
  (:use :cl)
  (:export :make-school :add :roster :grade))

(in-package :grade-school)

;;;
;;; helpers
;;;

(defun sorted (seq pred)
    (sort (copy-seq seq) pred))

(defun hash-keys (hash)
    (loop :for key :being :the :hash-key :of hash :collect key))

(defmacro insertf (place value test)
    `(setf ,place (sorted (cons ,value ,place) ,test)))
;;;
;;; school class
;;;

(defclass school ()
  ((grade-students :initform (make-hash-table)
                   :accessor grade-students)))

(defun make-school ()
    (make-instance 'school))

(defmethod grade ((school school) grade)
    (gethash grade (grade-students school) nil))

(defmethod grades ((school school))
    (sorted (hash-keys (grade-students school)) #'<))

(defmethod roster ((school school))
    (loop :for grade :in (grades school)
          :append (grade school grade)))

(defmethod is-student ((school school) name)
    (member name (roster school) :test 'string-equal))

(defmethod add ((school school) name grade)
    (unless (is-student school name)
        (insertf (gethash grade (grade-students school)) name #'string<)))
