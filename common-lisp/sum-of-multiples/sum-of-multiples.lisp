(defpackage :sum-of-multiples
  (:use :cl)
  (:export :sum))

(in-package :sum-of-multiples)

(defun divides (n)
    (lambda (by) (unless (zerop by) (zerop (mod n by)))))

(defun sum (factors limit)
    (loop :for i :from 1 :below limit
          :if (some (divides i) factors)
            :sum i))
