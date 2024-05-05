(defpackage :roman-numerals
  (:use :cl)
  (:export :romanize))

(in-package :roman-numerals)

(defparameter *roman-digits*
  '((#\M 1000 #\C 100)
    (#\D 500  #\C 100)
    (#\C 100  #\X 10)
    (#\L 50   #\X 10)
    (#\X 10   #\I 1)
    (#\V 5    #\I 1)
    (#\I 1    #\I 1)))

(defun romanize (number)
    "Returns the Roman numeral representation for a given number."
    (coerce
     (loop
       :with digits = *roman-digits*
       :while (< 0 number)
       :for (d v nd nv) = (car digits)
       :if (<= v number)
         :collect d
         :and :do (decf number v)
       :else :if (<= (- v nv) number)
               :append (list nd d)
               :and :do (decf number (- v nv))
       :else :do (pop digits))
     'string))
