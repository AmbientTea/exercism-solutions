(defpackage :lillys-lasagna
  (:use :cl)
  (:export :expected-time-in-oven
   :remaining-minutes-in-oven
           :preparation-time-in-minutes
   :elapsed-time-in-minutes))

(in-package :lillys-lasagna)

(defun expected-time-in-oven ()
    "Define function expected-time-in-oven"
    337)

(defun remaining-minutes-in-oven (n)
    "Define function remaining-minutes-in-oven"
    (- (expected-time-in-oven) n))

(defun preparation-time-in-minutes (n)
    "Define function preparation-time-in-minutes"
    (* n 19))

(defun elapsed-time-in-minutes (l m)
    "Define function elapsed-time-in-minutes"
    (+ (preparation-time-in-minutes l) m))
