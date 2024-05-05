(defpackage :run-length-encoding
  (:use :cl :uiop)
  (:export :encode
   :decode))

(in-package :run-length-encoding)

(defun consume-while (seq pred)
    (let ((cutoff (or (position-if-not pred seq) (length seq))))
        (values
         (subseq seq 0 cutoff)
         (subseq seq cutoff)
         cutoff)))

(defun encode-one (char count)
    (cond ((= count 1) (string char))
          ((< 1 count) (format nil "~a~a" count char))
          (t "")))

(defun encode (plain)
    (loop :until (emptyp plain)
          :for char = (first-char plain)
          :for (char-repeated rest) = (multiple-value-list (consume-while plain (lambda (ch) (char= ch char))))
          :collect (encode-one char (length char-repeated)) :into result
          :do (setf plain rest)
          :finally (return (format nil "~{~a~}" result))))

(defun decode (compressed)
    (loop
      :until (emptyp compressed)
      :for (numeric-prefix rest) = (multiple-value-list (consume-while compressed #'digit-char-p))
      :for repetitions = (if (emptyp numeric-prefix) 1 (parse-integer numeric-prefix))
      :collect (make-string repetitions :initial-element (first-char rest))
        :into result
      :do (setf compressed (subseq rest 1))
      :finally (return (format nil "~{~a~}" result))))
