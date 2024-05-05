(defpackage :affine-cipher
  (:use :cl)
  (:export :encode
   :decode))

(in-package :affine-cipher)

(defparameter *alphabet-size* 26)

(defun divides (a b)
    (zerop (mod b a)))

(defun greatest-common-factor (a b)
    (labels ((rec (a b)
                 (if (divides b a)
                     b
                     (rec b (mod a b)))))
        (rec (max a b) (min a b))))

(defun coprime-p (a b)
    (= 1 (greatest-common-factor a b)))

(defun affine-e (a b m i)
    (mod (+ (* a i) b) m))

(defun map-char-code (char f)
    (code-char (funcall f (char-code char))))

(defun map-char-code-0 (char f)
    (flet ((f0 (ch) (+ (funcall f (- ch (char-code #\a)))
                       (char-code #\a))))
        (map-char-code char #'f0)))

(defun curried (f &rest args)
    (lambda (x) (apply f (append args (list x)))))

(defun affine-encoding-table (a b)
    (loop :for i :from 0 :below *alphabet-size*
          :for encoded-char = (map-char-code #\a (curried #'+ i))
          :for encoding = (map-char-code-0 encoded-char (curried #'affine-e a b *alphabet-size*))
          :collect (cons encoded-char encoding)))

(defun chunk-string (chunk-size str)
    (loop :for i :from 0
          :for char :across str
          :if (and (divides chunk-size i) (not (zerop i)))
            :collect #\space :into result
          :collect char :into result
          :finally (return (coerce result 'string))))

(defun encode (plaintext a b)
    (when (coprime-p a *alphabet-size*)
        (loop
          :with table = (affine-encoding-table a b)
          :for char :across (string-downcase plaintext)
          :if (alpha-char-p char)
            :collect (cdr (assoc char table)) :into result
          :else :if (alphanumericp char)
                  :collect char :into result
          :finally (return (chunk-string 5 (coerce result 'string))))))

(defun decode (ciphertext a b)
    (when (coprime-p a *alphabet-size*)
        (loop
          :with table = (affine-encoding-table a b)
          :for char :across (string-downcase ciphertext)
          :if (alpha-char-p char)
            :collect (car (rassoc char table)) :into result
          :else :if (alphanumericp char)
                  :collect char :into result
          :finally (return (coerce result 'string)))))
