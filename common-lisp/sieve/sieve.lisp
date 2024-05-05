(defpackage :sieve
  (:use :cl)
  (:export :primes-to)
  (:documentation "Generates a list of primes up to a given limit."))

(in-package :sieve)

(defun primes-to (n)
    "List primes up to `n' using sieve of Eratosthenes."
    (let ((numbers (make-array (+ n 2) :initial-element t)))
        (loop :for i :from 2 :to n
              :if (elt numbers i)
                :do (loop :for j :from (* i i) :to n :by i
                          :do (setf (elt numbers j) nil))
                :and :collect i)))
