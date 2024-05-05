(defpackage :largest-series-product
  (:use :cl)
  (:export :largest-product))

(in-package :largest-series-product)

(defmacro mulf (obj value)
    `(setf ,obj (* ,obj ,value)))

(defun prefix-product (list &key count)
    (loop
      :with product = 1
      :for i :from 0 :below count
      :for elem :in list
      :do (mulf product elem)
      :finally (return (when (= i count) product))))

(defun all-or-nothing (seq)
    (unless (some #'null seq) seq))

(defun digits-of (string)
    (all-or-nothing (map 'list #'digit-char-p string)))

(defun largest-product (digits span)
    (uiop:nest
     (let* ((digits (digits-of digits))
            (len (length digits))))
     (when (<= 1 span len))
     (loop
       :for prefix :on digits
       :for product = (prefix-product prefix :count span)
       :while product
       :maximize product)))
