(defpackage :pangram
  (:use :cl)
  (:export :pangramp))

(in-package :pangram)

(defparameter *alphabet-size* 26)

(defun pangramp (sentence)
    (loop :for char :across sentence
          :when (alpha-char-p char)
            :unless (member char seen :test #'char-equal)
              :collect char :into seen
          :finally (return (= *alphabet-size* (length seen)))))
