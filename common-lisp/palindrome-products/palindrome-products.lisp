(defpackage :palindrome-products
  (:use :cl)
  (:export :smallest
   :largest))

(in-package :palindrome-products)

(defun reverse-digits (number)
    (when (<= 0 number)
        (loop :with result = 0
              :until (zerop number)
              :for next-digit = (mod number 10)
              :do (setf result (+ (* 10 result) next-digit))
              :do (setf number (floor (/ number 10)))
              :finally (return result))))

(defun palindrome-p (number)
    (= number (reverse-digits number)))

(defun divides (factor number)
    (zerop (mod number factor)))

(defun factors-in-range (number min max)
    (when number
        (loop :for n :from min :to max
              :for m = (/ number n)
              :if (and (integerp m) (<= min n m max))
                :collect (list n m))))

(defmacro palindrome-product-gen (name start direction finish)
    (let ((ord (case direction (:to '<) (:downto '>)))
          (args (case direction (:to (list start finish)) (:downto (list finish start)))))
        `(defun ,name ,args
             (loop
               :with best = nil
               :for n :from ,start ,direction ,finish
               :do (loop :for m :from n ,direction ,finish
                         :for product = (* n m)
                         :while (or (null best) (,ord product best))
                         :if (palindrome-p product)
                           :do (setf best product))
               :finally
                  (return
                      (let ((factors (factors-in-range best min-factor max-factor)))
                          (when factors
                              (values best factors))))))))

(palindrome-product-gen smallest min-factor :to max-factor)
(palindrome-product-gen largest max-factor :downto min-factor)
