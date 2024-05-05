(defpackage :rna-transcription
  (:use :cl)
  (:export :to-rna))
(in-package :rna-transcription)

(define-condition invalid-nucl (error) ())

(defparameter *translation-table*
  '((#\G . #\C)
    (#\C . #\G)
    (#\T . #\A)
    (#\A . #\U)))

(defun to-rna-nucl (n)
    (or (cdr (assoc n *translation-table*))
        (error 'invalid-nucl)))

(defun to-rna (str)
    "Transcribe a string representing DNA nucleotides to RNA."
    (map 'string #'to-rna-nucl str))
