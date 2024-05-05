(defpackage :book-store
  (:use :cl)
  (:export :calculate-price))

(in-package :book-store)

(defparameter *base-book-price* 8)

(defun discount (n)
    (case n
      (2 95)
      (3 90)
      (4 80)
      (5 75)
      (otherwise 100)))

(defun count-occurences (list)
    "Count elements of list into a hash table."
    (loop
      :with counts = (make-hash-table)
      :for item :in list
      :do (incf (gethash item counts 0))
      :finally (return counts)))

(defun values-sorted (hash-map &key (test #'<))
    "Hash table elements sorted by `test`"
    (sort (loop :for item :being :the :hash-value :of hash-map :collect item) test))

(defun differential (list)
    (loop :for i :in list
          :for j :in (cons 0 list)
          :collect (- i j)))

(defun calculate-batch-price (amount batch-size)
    (* *base-book-price* amount batch-size (discount batch-size)))

(defun change-35s-to-4s (batches)
    (if (< (length batches) 5)
        batches
        (let ((to-change (min (third batches) (fifth batches))))
            (decf (third batches) to-change)
            (decf (fifth batches) to-change)
            (incf (fourth batches) (* 2 to-change))
            batches)))

(defun calculate-batch-sizes (basket)
    (reverse (differential (values-sorted (count-occurences basket)))))

(defun calculate-price (basket)
    (let* ((batches-switched (change-35s-to-4s (calculate-batch-sizes basket))))
        (loop :for batch-count :in batches-switched
              :for batch-size :from 1
              :sum (calculate-batch-price batch-count batch-size))))
