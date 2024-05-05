(defpackage :binary-search
  (:use :cl)
  (:export :binary-find :value-error))

(in-package :binary-search)

(defun binary-find-in-range (arr el start end)
    (uiop:nest
     (when (< start end))
     (let* ((middle-index (truncate (+ start end) 2))
            (middle-value (aref arr middle-index))))
     (cond
       ((< el middle-value) (binary-find-in-range arr el start middle-index))
       ((< middle-value el) (binary-find-in-range arr el (1+ middle-index )end))
       (t middle-index))))

(defun binary-find (arr el)
    (binary-find-in-range arr el 0 (length arr)))
