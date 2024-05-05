(defpackage :allergies
  (:use :cl)
  (:shadow :list)
  (:export :allergic-to-p :list))

(in-package :allergies)

(defmacro define-allergens (name &rest allergens)
    `(defparameter ,name
       '(,@(loop :for allergen :in allergens
                 :for index :from 0
                 :collect (cons allergen index)))))

(define-allergens *allergens*
    "eggs"
    "peanuts"
    "shellfish"
    "strawberries"
    "tomatoes"
    "chocolate"
    "pollen"
    "cats")


(defun allergic-to-p (score allergen)
    "Returns true if given allergy score includes given allergen."
    (let ((bit (cdr (assoc allergen *allergens* :test #'string=))))
        (logbitp bit score)))

(defun list (score)
    "Returns a list of allergens for a given allergy score."
    (loop
      :for (allergen . bit) :in *allergens*
      :if (logbitp bit score)
        :collect allergen))
