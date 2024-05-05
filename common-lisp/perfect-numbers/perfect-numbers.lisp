(defpackage :perfect-numbers
  (:use :cl)
  (:export :classify))

(in-package :perfect-numbers)

(defun divides (d n)
    (zerop (mod n d)))

(defun aliquot-sum (n)
    (loop :for i :from 1 :to (/ n 2) :if (divides i n) :sum i))

(defun classify (n)
    (if (and n (< 0 n))
        (let ((aliquot-sum (aliquot-sum n)))
            (cond
              ((= n aliquot-sum) "perfect")
              ((> aliquot-sum n) "abundant")
              ((< aliquot-sum n) "deficient")))))
