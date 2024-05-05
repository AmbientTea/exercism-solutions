(defpackage :larrys-winning-checker
  (:use :cl)
  (:export
   :make-empty-board
   :make-board-from-list
   :all-the-same-p
   :row
   :column))

(in-package :larrys-winning-checker)

(defun make-empty-board ()
    (make-array '(3 3) :initial-element nil))

(defun make-board-from-list (list)
    (make-array '(3 3) :initial-contents list))

(defun all-the-same-p (row-or-col)
    (and (eq (aref row-or-col 0) (aref row-or-col 1))
         (eq (aref row-or-col 1) (aref row-or-col 2))))

(defun row (board row-num)
    (loop :for i :from 0 :to 2
          :collect (aref board row-num i) :into elems
          :finally (return (coerce elems 'vector))))

(defun column (board col-num)
    (loop :for i :from 0 :to 2
          :collect (aref board i col-num) :into elems
          :finally (return (coerce elems 'vector))))
