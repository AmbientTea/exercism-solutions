(defpackage :lucys-magnificent-mapper
  (:use :cl)
  (:export :make-magnificent-maybe :only-the-best))

(in-package :lucys-magnificent-mapper)

;; Define make-magnificent-maybe function
(defun make-magnificent-maybe (f l)
    (mapcar f l))

;; Define only-the-best function
(defun only-the-best (f l)
    (loop :for n :in l
          :unless (or (= 1 n) (funcall f n))
            :collect n))
