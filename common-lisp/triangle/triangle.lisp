(defpackage :triangle
  (:use :cl)
  (:export :triangle-type-p))

(in-package :triangle)

(defun triangle-p (a b c)
    (and
     (< 0 a (+ b c))
     (< 0 b (+ a c))
     (< 0 c (+ a b))))

(defun triangle-type-p (type a b c)
    "Deterimines if a triangle (given by side lengths A, B, C) is of the given TYPE"
    (and (triangle-p a b c)
         (case type
           (:equilateral (= a b c))
           (:isosceles (or (= a b) (= b c) (= a c)))
           (:scalene (/= a b c)))))
