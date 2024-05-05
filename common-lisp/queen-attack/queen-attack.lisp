(defpackage :queen-attack
  (:use :cl)
  (:export :valid-position-p
   :attackp))

(in-package :queen-attack)

(defun valid-position-p (coordinates)
    (destructuring-bind (x . y) coordinates
        (and (<= 0 x 7)
             (<= 0 y 7))))

(defun attackp (white-queen black-queen)
    (destructuring-bind (wx . wy)  white-queen
        (destructuring-bind (bx . by) black-queen
            (let ((dx (abs (- wx bx)))
                  (dy (abs (- wy by))))
                (or (zerop dx)
                    (zerop dy)
                    (= dx dy))))))
