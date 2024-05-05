(defpackage :pig-latin
  (:use :cl :uiop)
  (:export :translate))

(in-package :pig-latin)

(defparameter *vowels* '(#\a #\e #\i #\o #\u #\y))

(defun vowel-p (char)
    (member char *vowels*))

(defun y-then-vowel-p (word)
    (and (< 1 (length word))
         (string-prefix-p "y" word)
         (vowel-p (char word 1))))

(defun consonant-prefix-length (word)
    (if (y-then-vowel-p word)
        1
        (let ((vowel-i (position-if #'vowel-p word)))
            (if (string-suffix-p (subseq word 0 (1+ vowel-i)) "qu")
                (1+ vowel-i)
                vowel-i))))

(defun consonant-prefix (word)
    (let ((prefix-length (consonant-prefix-length word)))
        (values (subseq word 0 prefix-length)
                (subseq word prefix-length))))

(defun translate-word (word)
    (multiple-value-bind (prefix rest) (consonant-prefix word)
        (if (string-prefix-p "xr" prefix)
            (concatenate 'string prefix rest "ay")
            (concatenate 'string rest prefix "ay"))))

(defun translate (phrase)
    (format nil "~{~a~^ ~}" (mapcar #'translate-word (split-string phrase))))
