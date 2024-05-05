(defpackage :logans-numeric-partition
  (:use :cl)
  (:export :categorize-number :partition-numbers))

(in-package :logans-numeric-partition)

;; Define categorize-number function
(defun categorize-number (acc n)
    (destructuring-bind (odd . even) acc
        (if (oddp n)
            (cons (cons n odd) even)
            (cons odd (cons n even)))))

;; Define partition-numbers function
(defun partition-numbers (nums)
    (reduce #'categorize-number nums :initial-value (cons nil nil)))
