(defpackage :sublist
  (:use :cl)
  (:export :sublist))

(in-package :sublist)

(defun prefix-p (list1 list2)
    (cond
      ((not list1) t)
      ((equal (car list1) (car list2)) (prefix-p (cdr list1) (cdr list2)))
      (t nil)))

(defun subseq-p (list1 list2)
    (if (not list2) nil
        (or (prefix-p list1 list2)
            (subseq-p list1 (cdr list2)))))


(defun sublist (list1 list2)
    "what is list1 of list2 (sublist, superlist, equal or unequal)"
    (cond
      ((equal list1 list2) :equal)
      ((subseq-p list1 list2) :sublist)
      ((subseq-p list2 list1) :superlist)
      (t :unequal)))
