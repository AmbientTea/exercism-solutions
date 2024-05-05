(defpackage :all-your-base
  (:use :cl)
  (:export :rebase))

(in-package :all-your-base)

(defun from-base (in-base digits)
    (loop with multiplier = 1
          with result = 0
          for digit in (reverse digits)
          do (if (<= in-base digit) (return nil))
          sum (* digit multiplier)
          do (setf multiplier (* multiplier in-base))))

(defun to-base (out-base number)
    (cond ((not number) nil)
          ((zerop number) '(0))
          ((= out-base 1) (make-list number :initial-element 1))
          (t (reverse (loop while (> number 0)
                            collect (mod number out-base) into digits
                            do (setf number (truncate number out-base))
                            finally (return-from to-base (reverse digits)))))))

(defun negative (i) (< i 0))
(defun rebase (list-digits in-base out-base)
    (if (or  (< in-base 2)
             (< out-base 2)
             (some #'negative list-digits)) nil

             (to-base out-base (from-base in-base list-digits))))
