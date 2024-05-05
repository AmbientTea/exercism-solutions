(defpackage :etl
  (:use :cl)
  (:export :transform))

(in-package :etl)

(defun transform (data)
    "Transforms hash values into keys with their keys as their values."
    (loop
      :with new-score-table = (make-hash-table :test 'eql)
      :for letters :being :the :hash-value :of data :using (hash-key score)
      :do (loop :for letter :in letters
                :do (setf (gethash (char-downcase letter) new-score-table) score))
      :finally (return new-score-table)))
