(defpackage :raindrops
  (:use :cl)
  (:export :convert))

(in-package :raindrops)

(defun divides (d n)
    (zerop (mod n d)))

(defun convert (n)
    "Converts a number to a string of raindrop sounds."
    (apply #'concatenate 'string
           (or (concatenate 'list
                            (if (divides 3 n) '("Pling"))
                            (if (divides 5 n) '("Plang"))
                            (if (divides 7 n) '("Plong")))
               (list (write-to-string n)))))
