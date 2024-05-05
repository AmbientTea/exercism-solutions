(defpackage :nucleotide-count
  (:use :cl)
  (:export :dna-count :nucleotide-counts :invalid-nucleotide))

(in-package :nucleotide-count)

(defparameter *nucleotide-list*
  '(#\A #\C #\G #\T))

(define-condition invalid-nucleotide () ())

(defun validate-nucleotide (nucleotide)
    (unless (member nucleotide *nucleotide-list*)
        (error 'invalid-nucleotide)))

(defun empty-counts-from (keys)
    (loop :with counts = (make-hash-table)
          :for key :in keys
          :do (setf (gethash key counts) 0)
          :finally (return counts)))

(defun dna-count (nucleotide strand)
    "Returns a count of the given nucleotide appearing in a DNA strand."
    (validate-nucleotide nucleotide)
    (loop :for nucl :across strand
          :do (validate-nucleotide nucl)
          :count (char-equal nucl nucleotide)))

(defun nucleotide-counts (strand)
    "Returns a hash of nucleotides and their counts in a given DNA strand."
    (loop
      :with counts = (empty-counts-from *nucleotide-list*)
      :for nucl :across strand
      :do (validate-nucleotide nucl)
      :do (incf (gethash nucl counts 0))
      :finally (return counts)))
