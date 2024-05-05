(defpackage :word-count
  (:use :cl)
  (:export :count-words))
(in-package :word-count)

(defmacro ---> (&rest forms)
    `(uiop:nest ,@(reverse forms)))

(defparameter *punctuation* '(#\space #\tab #\return #\, #\. #\! #\? #\;))

(defun word-char (char)
    (or (alphanumericp char) (char= char #\')))

(defun non-empty (word)
    (unless (uiop:emptyp word) word))

(defun sanitize-word (word)
    (--->
     word
     (remove-if-not #'word-char)
     (string-trim "'")
     (string-downcase)
     (non-empty)))

(defun sanitized-words (sentence)
    (--->
     (uiop:split-string sentence :separator *punctuation*)
     (mapcar #'sanitize-word)
     (remove-if #'null)))

(defun hash-table-to-alist (table)
    (loop :for key :being :the :hash-keys :of table
          :collect (cons key (gethash key table))))

(defun count-words (sentence)
    (loop
      :with words = (sanitized-words sentence)
      :with counts = (make-hash-table :test 'equal)
      :for word :in words
      :do (incf (gethash word counts 0))
      :finally (return (hash-table-to-alist counts))))
