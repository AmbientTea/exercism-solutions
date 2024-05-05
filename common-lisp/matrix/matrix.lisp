(defpackage :matrix
  (:use :cl)
  (:export :row
   :column))

(in-package :matrix)

(defmacro push-non-empty (obj target)
    (let ((obj-sym (gensym "obj")))
        `(let ((,obj-sym ,obj))
             (unless (zerop (length ,obj-sym)) (push ,obj-sym ,target)))))

(defun words (string &key (delim #\space))
    "Splits string into non-empty strings by delim"
    (flet ((rev-to-string (chars) (coerce (reverse chars) 'string)))
        (loop
          :with result = nil
          :with acc = nil
          :for char :across string
          :if (char-not-equal char delim)
            :do (push char acc)
          :else
            :do (push-non-empty (rev-to-string acc) result)
            :and :do (setf acc nil)
          :finally (progn (push-non-empty (rev-to-string acc) result)
                          (return (reverse result))))))

(defun word-lines (input-string &key (parse-elem #'identity))
    "Splits string into lines, then lines into words"
    (loop :for row :in (words input-string :delim #\newline)
          :collect (loop :for elem :in (words row :delim #\space)
                         :collect (funcall parse-elem elem))))

(defun coerce-to-array (nested-list)
    (let ((height (length nested-list))
          (width (length (first nested-list))))
        (make-array (list height width) :initial-contents nested-list)))

(defun parse-matrix (input-string)
    (coerce-to-array (word-lines input-string :parse-elem #'parse-integer)))

(defun matrix-row (matrix row)
    (loop :for y :from 0 :below (array-dimension matrix 1)
          :collect (aref matrix row y)))

(defun matrix-col (matrix col)
    (loop :for x :from 0 :below (array-dimension matrix 0)
          :collect (aref matrix x col)))

(defun row (input-matrix index)
    (matrix-row (parse-matrix input-matrix) (1- index)))

(defun column (input-matrix index)
    (matrix-col (parse-matrix input-matrix) (1- index)))
