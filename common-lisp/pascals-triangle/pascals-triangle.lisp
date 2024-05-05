(defpackage :pascals-triangle
  (:use :cl)
  (:export :rows))
(in-package :pascals-triangle)


(defun rows-step (row)
    (loop with new-row = '(1)
          for a in row
          for b in (cdr row)
          do (push (+ a b) new-row)
          finally (return (cons 1 new-row))))

(defun rows (n)
    (loop with row = '(1)
          repeat n
          collect row
          do (setf row (rows-step row))))
