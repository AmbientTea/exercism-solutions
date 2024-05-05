(defpackage :rotational-cipher
  (:use :cl)
  (:export :rotate))

(in-package :rotational-cipher)

(defun clamp (value &key low high)
    (+ low (mod (- value low) (- high low -1))))

(defun rotate-char (char key)
    (cond
      ((lower-case-p char) (code-char (clamp (+ key (char-code char))
                                             :low (char-code #\a)
                                             :high (char-code #\z))))
      ((upper-case-p char) (code-char (clamp (+ key (char-code char))
                                             :low (char-code #\A)
                                             :high (char-code #\Z))))
      (t char)))

(defun rotate (text key)
    (flet ((rot (char) (rotate-char char key)))
        (map 'string #'rot text)))
