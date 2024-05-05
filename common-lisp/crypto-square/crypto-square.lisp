(defpackage :crypto-square
  (:use :cl)
  (:export :encipher))
(in-package :crypto-square)

(defun non-empty (text)
    (unless (uiop:emptyp text) text))

(defun normalize (text)
    (non-empty (remove-if-not #'alphanumericp (string-downcase text))))

(defun pad (text len char)
    (let* ((padding-size (max 0 (- len (length text))))
           (padding (make-string padding-size :initial-element char)))
        (concatenate 'string text padding)))

(defun square (text)
    (when text
        (loop
          :with len = (length text)
          :with height = (ceiling (sqrt len))
          :with width = (ceiling (/ len height))
          :for start :from 0 :below height
          :for cipher-line = (loop :for i :from start :below len :by height
                                   :collect (aref text i))
          :collect (pad cipher-line width #\space))))

(defun encipher (plaintext)
    (format nil "~{~A~^ ~}" (square (normalize plaintext))))
