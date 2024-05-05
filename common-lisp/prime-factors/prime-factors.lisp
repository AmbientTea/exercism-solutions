(defpackage :prime-factors
  (:use :cl)
  (:export :factors))

(in-package :prime-factors)

(defun factors (n)
    (loop :with divisor = 2
          :while (< 1 n)
          :if (zerop (mod n divisor))
            :collect divisor
            :and :do (setf n (/ n divisor))
          :else
            :do (setf divisor (1+ divisor))))
