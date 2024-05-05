(defpackage :proverb
  (:use :cl)
  (:export :recite))

(in-package :proverb)

(defun recite (strings)
    (if (null strings)
        ""
        (loop
          :with last-line = (format nil "And all for the want of a ~a." (car strings))
          :for a :in strings
          :for b :in (cdr strings)
          :collect (format nil "For want of a ~a the ~a was lost." a b) :into result
          :finally (return (format nil "~{~a~&~}~a" result last-line)))))
