(defpackage :pizza-pi
  (:use :cl)
  (:export :dough-calculator :pizzas-per-cube
           :size-from-sauce :fair-share-p))

(in-package :pizza-pi)

(defun dough-calculator (pizzas diameter)
    (round (* pizzas
              (+ 200 (* 45 pi diameter 1/20)))))

(defun size-from-sauce (sauce)
    (/ (round (* 100 (sqrt (/ (* 40 sauce) (* 3 pi))))) 100))

(defun pizzas-per-cube (l d)
    (floor (* 2 l l l) (* 3 pi d d)))

(defun fair-share-p (pizzas friends)
    (zerop (mod (* pizzas 8) friends)))
