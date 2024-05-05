(defpackage :flatten-array
  (:use :cl)
  (:export :flatten))

(in-package :flatten-array)

(defun flatten (nested)
    (cond
      ((null nested) nil)
      ((atom nested) (list nested))
      (t (concatenate 'list (flatten (car nested)) (flatten (cdr nested))))))
