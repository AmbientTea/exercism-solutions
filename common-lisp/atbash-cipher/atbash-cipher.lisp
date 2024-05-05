(defpackage :atbash-cipher
  (:use :cl)
  (:export :encode))

(in-package :atbash-cipher)

(defun encode-char (char)
    (if (alpha-char-p char)
        (code-char (- (char-code #\z) (mod (char-code char) (char-code #\a))))
        char))

(defun encode (plaintext)
    (setf plaintext (string-downcase (remove-if-not #'alphanumericp plaintext)))
    (loop :for char :across plaintext
          :for i :from 0
          :if (and (not (zerop i)) (zerop (mod i 5)))
            :collect #\space :into result
          :collect (encode-char char) :into result
          :finally (return (coerce result 'string))))
