(defpackage :pythagorean-triplet
  (:use :cl)
  (:export :triplets-with-sum))

(in-package :pythagorean-triplet)

(defun triplets-with-sum (n)
    (loop :for c :from (- n 2) :downto (/ n 3)
          :append (loop :for a :from 1 :to (/ (- n c 1) 2)
                        :for b = (- n a c)
                        :if (= (+ (* a a) (* b b)) (* c c))
                          :collect (list a b c))))
