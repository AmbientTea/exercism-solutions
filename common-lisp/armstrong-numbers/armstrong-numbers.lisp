(defpackage :armstrong-numbers
  (:use :cl)
  (:export :armstrong-number-p))
(in-package :armstrong-numbers)

(defun digits-of (number)
    (loop :while (< 0 number)
          :collect (mod number 10)
          :do (setf number (truncate number 10))))

(defun raise-all-and-sum (numbers power)
    (loop :for number :in numbers
          :sum (expt number power)))

(defun armstrong-number-p (number)
    (let* ((digit-count (ceiling (log number 10)))
           (digits (digits-of number))
           (sum-of-digit-powers (raise-all-and-sum digits digit-count)))
        (= number sum-of-digit-powers)))
