(defpackage :lillys-lasagna-leftovers
  (:use :cl)
  (:export
   :preparation-time
   :remaining-minutes-in-oven
   :split-leftovers))

(in-package :lillys-lasagna-leftovers)

(defparameter *layer-preparation-time* 19)
(defparameter *oven-times*
  '((:very-short . 137)
    (:shorter . 237)
    (:normal . 337)
    (:longer . 437)
    (:very-long . 537)))

;; Define function preparation-time
(defun preparation-time (&rest layers)
    (* (length layers) *layer-preparation-time*))

;; Define function remaining-minutes-in-oven
(defun remaining-minutes-in-oven (&optional (mode :normal))
    (or (cdr (assoc mode *oven-times*)) 0))

;; Define function split-leftovers
(defun split-leftovers (&key (weight nil weight-provided) (human 10) (alien 10))
    (cond (weight (- weight human alien))
          (weight-provided :looks-like-someone-was-hungry)
          (t :just-split-it)))
