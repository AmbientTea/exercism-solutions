(defpackage :phone-number
  (:use :cl)
  (:export :clean))

(in-package :phone-number)

(defun just-digits (phrase)
    (remove-if-not #'digit-char-p phrase))

(defun valid-country-code-p (char)
    (or (null char) (char= char #\1)))

(defun valid-nanp-digits-p (digits)
    (and digits
         (char< #\1 (char digits 0))
         (char< #\1 (char digits 3))))

(defun nanp-number-digits (digits)
    (case (length digits)
      (10 (values digits nil))
      (11 (values (subseq digits 1) (uiop:first-char digits)))))

(defun nanp-number (phrase)
    (multiple-value-bind (digits country-code) (nanp-number-digits (just-digits phrase))
        (when (and (valid-nanp-digits-p digits)
                   (valid-country-code-p country-code))
            digits)))

(defun clean (phrase)
    "Converts a PHRASE string into a string of digits.
Will evaluate to \"0000000000\" in case of an invalid input."
    (or (nanp-number phrase) "0000000000"))
