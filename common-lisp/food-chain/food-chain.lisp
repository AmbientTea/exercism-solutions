(defpackage :food-chain
  (:use :cl)
  (:export :recite))

(in-package :food-chain)

(defparameter *food-chain*
  '(("fly"    nil                                               nil)
    ("spider" "It wriggled and jiggled and tickled inside her." "wriggled and jiggled and tickled inside her")
    ("bird"   "How absurd to swallow a bird!"                   nil)
    ("cat"    "Imagine that, to swallow a cat!"                 nil)
    ("dog"    "What a hog, to swallow a dog!"                   nil)
    ("goat"   "Just opened her throat and swallowed a goat!"    nil)
    ("cow"    "I don't know how she swallowed a cow!"           nil)
    ("horse"  "She's dead, of course!"                          nil))
  )

(defun recite-causes (food-chain)
    (loop
      :for chain :on food-chain

      :for (swallowed) = (first chain)
      :for (to-catch _unused to-catch-description) = (second chain)

      :collect (if to-catch
                   (format nil "She swallowed the ~a to catch the ~a~@[ that ~a~]." swallowed to-catch to-catch-description)
                   (format nil "I don't know why she swallowed the ~a. Perhaps she'll die." swallowed))
        :into result

      :finally (return (format nil "~{~a~^~&~}" result))))

(defun recite-verse (number)
    (uiop:nest
     (let ((food-chain-max (length *food-chain*))))
     (when (<= 1 number food-chain-max))
     (let ((food-chain (reverse (subseq *food-chain* 0 number)))))
     (destructuring-bind (eaten-now exclamation _1) (car food-chain)
         (declare (ignore _1)))
     (format nil "I know an old lady who swallowed a ~a.~@[~%~a~]~@[~&~a~]"
             eaten-now exclamation
             (unless (= number food-chain-max) (recite-causes food-chain)))))

(defun recite (start-verse end-verse)
    (loop :for verse :from start-verse :to end-verse
          :collect (recite-verse verse) :into result
          :finally (return (format nil "~{~a~^~&~^~%~}" result))))
