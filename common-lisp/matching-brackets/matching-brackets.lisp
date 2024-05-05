(defpackage :matching-brackets
  (:use :cl)
  (:export :pairedp))

(in-package :matching-brackets)

(defun opposite (char)
    (when char
        (case char
          (#\( #\))
          (#\{ #\})
          (#\[ #\])
          )))

(defun match-brace (stack char)
    (case char
      ((#\( #\{ #\[) (cons char stack))
      ((#\) #\} #\]) (if (equal char (opposite (car stack)))
                         (cdr stack)
                         (cons #\e stack)))
      (otherwise stack)))

(defun pairedp (value)
    (null (reduce #'match-brace value :initial-value nil)))
