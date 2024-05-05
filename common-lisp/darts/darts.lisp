(defpackage :darts
  (:use :cl)
  (:export :score))

(in-package :darts)

(defun score (x y)
    (let ((dist2 (+ (* x x) (* y y))))
        (cond
          ((<= dist2 1) 10)
          ((<= dist2 25) 5)
          ((<= dist2 100) 1)
          (t 0))))
