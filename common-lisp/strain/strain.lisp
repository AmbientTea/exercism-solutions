(defpackage :strain
  (:use :cl)
  (:export :keep :discard))

(in-package :strain)

(defun keep (keep-p elements)
    "Returns a sublist of elements according to a given predicate."
    (labels ((keep-impl (acc elems)
                 (cond
                   ((not elems) acc)
                   ((funcall keep-p (car elems)) (keep-impl (cons (car elems) acc) (cdr elems)))
                   (t (keep-impl acc (cdr elems))))))
        (reverse (keep-impl nil elements)))
    )

(defun discard (discard-p elements)
    "Returns a sublist of elements not matching a given predicate."
    (flet ((keep-p (e) (not (funcall discard-p e))))
        (keep #'keep-p elements)))
