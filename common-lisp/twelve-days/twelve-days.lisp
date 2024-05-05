(defpackage :twelve-days
  (:use :cl)
  (:export :recite))

(in-package :twelve-days)

(defparameter *gifts*
  '("a Partridge in a Pear Tree"
    "two Turtle Doves"
    "three French Hens"
    "four Calling Birds"
    "five Gold Rings"
    "six Geese-a-Laying"
    "seven Swans-a-Swimming"
    "eight Maids-a-Milking"
    "nine Ladies Dancing"
    "ten Lords-a-Leaping"
    "eleven Pipers Piping"
    "twelve Drummers Drumming"))

(defun recite (&optional begin end)
    (unless begin
        (setf begin 1)
        (setf end 12))
    (unless end
        (setf end begin))
    "Returns a string of the requested verses for the 12 Days of Christmas."
    (loop :for day :from begin :to end
          :for day-gifts = (reverse (subseq *gifts* 0 day))
          :collect (format nil "On the ~:r day of Christmas my true love gave to me: ~{~a~#[~;, and ~:;, ~]~}." day day-gifts)
            :into result
          :finally (return (format nil "~{~a~^~&~}" result))))
