(defpackage :isogram
  (:use :cl)
  (:export :isogram-p))

(in-package :isogram)

;; lazy solution
;; (defun isogram-p (string)
;;     "Is string an Isogram?"
;;     (setf string (string-upcase (remove-if-not #'alpha-char-p string)))
;;     (equal (remove-duplicates string) string))

(defun isogram-p (string)
    (loop :for char :across (string-upcase string)
          :with seen = nil
          :if (member char seen)
            :do (return nil)
          :if (alpha-char-p char)
            :do (push char seen)
          :finally (return t)))
