(defpackage :saddle-points
  (:use :cl)
  (:export :saddle-points))

(in-package :saddle-points)

(defun row-maximums (matrix)
    (loop :with (max-y max-x) = (array-dimensions matrix)
          :for y :from 0 :below max-y
          :collect (loop :for x :from 0 :below max-x
                         :maximize (aref matrix y x))))

(defun col-minimums (matrix)
    (loop :with (max-y max-x) = (array-dimensions matrix)
          :for x :from 0 :below max-x
          :collect (loop :for y :from 0 :below max-y
                         :minimize (aref matrix y x))))

(defun saddle-points (matrix)
    (loop :with row-maximums = (coerce (row-maximums matrix) 'vector)
          :with col-minimums = (coerce (col-minimums matrix) 'vector)
          :for col-minimum :across col-minimums
          :for x :from 0
          :append (loop :for row-maximum :across row-maximums
                        :for y :from 0
                        :for yx-value = (aref matrix y x)
                        :if (= row-maximum col-minimum yx-value)
                          :collect (list (1+ y) (1+ x)))))
