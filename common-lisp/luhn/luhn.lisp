(defpackage :luhn
  (:use :cl)
  (:export :validp))

(in-package :luhn)

(defun luhn-char-p (char)
    (or (digit-char-p char) (char= char #\space)))

(defun luhn-double (n)
    (let ((2n (* 2 n)))
        (if (< 2n 10) 2n (- 2n 9))))

(defun luhn-sum (digits)
    (loop :for digit :in (reverse digits)
          :for i :from 0
          :sum (if (oddp i) (luhn-double digit) digit)))

(defun luhn-digits-p (digits)
    (and (< 1 (length digits))
         (zerop (mod (luhn-sum digits) 10))))

(defun digits-of (str)
    (remove nil (map 'list #'digit-char-p str)))

(defun validp (input)
    (and (every #'luhn-char-p input)
         (luhn-digits-p (digits-of input))))
