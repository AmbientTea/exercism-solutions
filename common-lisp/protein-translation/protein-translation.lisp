(defpackage :protein-translation
  (:use :cl)
  (:export :proteins
   :invalid-protein))

(in-package :protein-translation)

(define-condition invalid-protein (error) ())

(progn
    (defparameter *codons* (make-hash-table :test 'equal))
    (setf (gethash "AUG" *codons*) "Methionine")
    (setf (gethash "UUU" *codons*) "Phenylalanine")
    (setf (gethash "UUC" *codons*) "Phenylalanine")
    (setf (gethash "UUA" *codons*) "Leucine")
    (setf (gethash "UUG" *codons*) "Leucine")
    (setf (gethash "UCU" *codons*) "Serine")
    (setf (gethash "UCC" *codons*) "Serine")
    (setf (gethash "UCA" *codons*) "Serine")
    (setf (gethash "UCG" *codons*) "Serine")
    (setf (gethash "UAU" *codons*) "Tyrosine")
    (setf (gethash "UAC" *codons*) "Tyrosine")
    (setf (gethash "UGU" *codons*) "Cysteine")
    (setf (gethash "UGC" *codons*) "Cysteine")
    (setf (gethash "UGG" *codons*) "Tryptophan"))

(defparameter *stop-codons* (list "UAA" "UAG" "UGA"))

(defun proteins (strand)
    (loop
      :for strand-length = (length strand)
      :while (< 0 strand-length)

      :for head-size = (min 3 strand-length)
      :for head = (subseq strand 0 head-size)

      :do (setf strand (subseq strand head-size))

      :while (not (member head *stop-codons* :test 'equal))


      :collecting (or (gethash head *codons*)
                      (error 'invalid-protein)))
    )
