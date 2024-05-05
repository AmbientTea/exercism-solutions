(defpackage :gigasecond-anniversary
  (:use :cl)
  (:export :from))
(in-package :gigasecond-anniversary)

(defmacro ---> (&rest exprs)
    `(uiop:nest ,@(reverse exprs)))
(defmacro flip (f &rest args)
    `(,f ,@(reverse args)))

(defparameter *gigasecond* 1000000000)

(defun from (year month day hour minute second)
    (--->
     (encode-universal-time second minute hour day month year 0)
     (+ *gigasecond*)
     (flip decode-universal-time 0)
     (multiple-value-list)
     (reverse)
     (cdddr)
     ))
