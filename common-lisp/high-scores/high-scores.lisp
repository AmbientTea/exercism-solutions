(defpackage :high-scores
  (:use :cl)
  (:export :make-high-scores-table :add-player
           :set-score :get-score :remove-player))

(in-package :high-scores)

;; Define make-high-scores-table function
(defun make-high-scores-table ()
    (make-hash-table :test 'eql))
;; Define add-player function
(defun add-player (table name)
    (setf (gethash name table) 0))
;; Define set-score function
(defun set-score (table name score)
    (setf (gethash name table) score))
;; Define get-score function
(defun get-score (table name)
    (or (gethash name table) 0))
;; Define remove-player function
(defun remove-player (table name)
    (remhash name table))
