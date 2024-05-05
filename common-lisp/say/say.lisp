(defpackage :say
  (:use :cl)
  (:export :say))

(in-package :say)

(defparameter *ones-and-teens*
  (loop :for i :from 0 :to 19 :collect (format nil "~r" i)))

(defparameter *tens*
  (loop :for i :from 0 :to 90 :by 10 :collect (format nil "~r" i)))

(defparameter *scale*
  ;; digits . suffix
  '((2 . nil)
    (1 . "hundred")
    (3 . "thousand")
    (3 . "million")
    (3 . "billion")))

(defun chunk-number (number)
    "Split number into ones, tens, hundreds, thousands etc."
    (loop
      :while (< 0 number)
      :for (digits . suffix) :in *scale*
      :for (rest chunk-size) = (multiple-value-list
                                (truncate number (expt 10 digits)))
      :collect chunk-size
      :do (setf number rest)))

(defun say (number)
    (cond
      ((<= 0 number 19)
       (nth number *ones-and-teens*))

      ((<= 20 number 99)
       (multiple-value-bind (tens ones) (truncate number 10)
           (format nil "~a~@[-~a~]"
                   (nth tens *tens*)
                   (unless (zerop ones) (nth ones *ones-and-teens*)))))

      ((<= 100 number 999999999999)
       (loop :with chunks = (chunk-number number)
             :for chunk :in chunks
             :for (digits . suffix) :in *scale*
             :if (< 0 chunk)
               :collect (format nil "~a~@[ ~a~]" (say chunk) suffix)
                 :into result
             :finally (return (format nil "~{~a~^ ~}" (reverse result)))))))
