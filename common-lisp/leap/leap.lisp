(defpackage :leap
  (:use :cl)
  (:export :leap-year-p))
(in-package :leap)

(defun divides (d n)
    (zerop (mod n d)))

(defun leap-year-p (year)
    (unless (unless (divides 400 year)
                (divides 100 year))
        (divides 4 year)))
