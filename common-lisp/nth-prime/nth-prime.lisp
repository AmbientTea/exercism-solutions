(defpackage :nth-prime
  (:use :cl)
  (:export :find-prime))

(in-package :nth-prime)

(defvar *primes* '(2 3))
(defvar *primes-count* 2)
(defvar *primes-next-to-check* 4)

(defun is-prime (number)
    (loop :for prime :in *primes*
          :while (<= prime (sqrt number))
          :never (zerop (mod number prime))))

(defun compute-primes (number)
    (loop :for n :from *primes-next-to-check*
          :while (< *primes-count* number)
          :if (is-prime n)
            :do (progn
                    (push n (cdr (last *primes*)))
                    (incf *primes-count*))
          :finally (setf *primes-next-to-check* n)
          ))

(defun find-prime (number)
    (compute-primes number)
    (unless (< number 1)
        (nth (1- number) *primes*)))
