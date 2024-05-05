(defpackage :isbn-verifier
  (:use :cl)
  (:export :validp))

(in-package :isbn-verifier)

(defun isbn-digit (char)
    (if (char= char #\X) 10
        (digit-char-p char)))

(defun isbn-separator (char)
    (char= char #\-))

(defun isbn-char (char)
    (or (isbn-digit char) (isbn-separator char)))

(defun valid-isbn-check-digit (isbn-digits)
    (loop :for digit :in isbn-digits
          :for multiplier :from 10 :downto 1
          :sum (* digit multiplier) :into sum
          :finally (return (zerop (mod sum 11)))))

(defun isbn-digits (isbn)
    (remove-if #'null (map 'list #'isbn-digit isbn)))

(defun validp (isbn)
    (let ((digits (isbn-digits isbn)))
        (and
         (every #'isbn-char isbn)
         (= (length digits) 10)
         (every #'digit-char (butlast digits))
         (<= 1 (car (last digits)) 10)
         (valid-isbn-check-digit digits))))
