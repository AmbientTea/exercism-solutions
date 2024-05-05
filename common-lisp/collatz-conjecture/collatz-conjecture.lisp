(defpackage :collatz-conjecture
  (:use :cl)
  (:export :collatz))

(in-package :collatz-conjecture)

(defun collatz (n)
    (unless (<= n 0)
        (loop :while (< 1 n)
              :do (setf n (if (evenp n) (/ n 2) (+ n n n 1)))
              :count t)))
