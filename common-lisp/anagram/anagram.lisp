(defpackage :anagram
  (:use :cl)
  (:export :anagrams-for))

(in-package :anagram)

(defun anagram-p (subject candidate)
    (and
     (not (equalp subject candidate))
     (equalp
      (sort (copy-seq subject) #'char-lessp)
      (sort (copy-seq candidate) #'char-lessp))))

(defun anagrams-for (subject candidates)
    "Returns a sublist of candidates which are anagrams of the subject."
    (loop :for c :in candidates :if (anagram-p subject c) :collect c))
