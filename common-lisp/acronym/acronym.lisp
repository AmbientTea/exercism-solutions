(defpackage :acronym
  (:use :cl)
  (:export :acronym))

(in-package :acronym)

(defmacro ---> (&rest exprs)
    `(uiop:nest ,@(reverse exprs)))


(defun acronym (str)
    "Returns the acronym for a noun of tech jargon."
    (--->
     str
     (substitute-if-not #\space #'alpha-char-p)
     (uiop:split-string)
     (remove-if #'uiop:emptyp)
     (map 'string #'uiop:first-char)
     (string-upcase)))
