(defpackage :diamond
  (:use :cl)
  (:export :rows))

(in-package :diamond)

(defun diagonal (to-letter)
    (loop
      :with start = (char-code #\A)
      :with end = (char-code to-letter)
      :with row-size = (- end start -1)
      :for i :from start :to end
      :for letter = (code-char i)
      :for row = (make-string row-size :initial-element #\space)
      :do (setf (char row (- end i)) letter)
      :collect row))

(defun mirror-seq (type row)
    (concatenate type row (reverse (subseq row 0 (1- (length row))))))

(defun rows (letter)
    (loop :for row :in (mirror-seq 'list (diagonal letter))
          :collect (mirror-seq 'string row) ))
