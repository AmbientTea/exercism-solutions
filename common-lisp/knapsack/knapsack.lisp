(defpackage :knapsack
  (:use :cl)
  (:export :maximum-value))

(in-package :knapsack)

(defun maximum-value (maximum-weight items)
    (loop
      :for (((_wm . weight) (_vm . value)) . rest) :on items
      :if (<= weight maximum-weight)
        :maximize (+ value (maximum-value (- maximum-weight weight) rest))
      :maximize (maximum-value maximum-weight rest)))
