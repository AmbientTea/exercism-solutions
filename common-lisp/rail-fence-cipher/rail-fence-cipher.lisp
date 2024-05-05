(defpackage :rail-fence-cipher
  (:use :cl)
  (:export :encode
   :decode))

(in-package :rail-fence-cipher)

(defun range (from to)
    (loop :for i :from from :below to :collect i))

(defun rails-indices (rails)
    "Produces cyclic list of form (0 1 ... rails-1 .. 1 0 ...)"
    (let ((indices (append (range 0 rails)
                           (reverse (range 1 (1- rails))))))
        (setf (cdr (last indices)) indices)))

(defun railify (seq rails)
    "Permute seq according to rail fence cipher rules"
    (loop
      :with chunks = (make-array rails :initial-element nil)
      :with indices = (rails-indices rails)
      :for elem :across (coerce seq 'vector)
      :for i :in indices
      :do (push elem (aref chunks i))
      :finally (return (apply #'append (map 'list #'reverse chunks)))))

(defun encode (msg rails)
    (coerce (railify msg rails) 'string))

(defun decode (msg rails)
    (loop
      :with len = (length msg)
      :with result = (make-array len)
      :for i :in (railify (range 0 len) rails)
      :for char :across msg
      :do (setf (aref result i) char)
      :finally (return (coerce result 'string))))
