(defpackage :beer-song
  (:use :cl)
  (:export :verse :sing))

(in-package :beer-song)


(defun bottles (n)
    (if (zerop n)
        "no more bottles"
        (format nil "~A bottle~:p" n)))

(defun it-one (n)
    (case n (1 "it") (otherwise "one")))

(defun verse-1 (n)
    (format nil "~@(~A~) of beer on the wall, ~A of beer." (bottles n) (bottles n)))

(defun verse-2 (n)
    (if (zerop n)
        "Go to the store and buy some more, 99 bottles of beer on the wall."
        (format nil "Take ~A down and pass it around, ~A of beer on the wall."
                (it-one n)
                (bottles (1- n)))))

(defun verse (n)
    "Returns a string verse for a given number."
    (format nil "~A~%~A~&" (verse-1 n) (verse-2 n)))

(defun sing (start &optional (end 0))
    "Returns a string of verses for a given range of numbers."
    (loop :for i :from start :downto end
          :collect (verse i) :into result
          :finally (return (format nil "~{~A~%~}" result))))
