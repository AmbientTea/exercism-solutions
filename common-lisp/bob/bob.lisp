(defpackage :bob
  (:use :cl)
  (:export :response))
(in-package :bob)

(defparameter *whitespace* '(#\space #\tab #\return #\newline))
(defparameter *whitespace-str* (coerce *whitespace* 'string))

(defun white-space-p (char)
    (member char *whitespace*))

(defun silence-p (str)
    (every #'white-space-p str))

(defun question-p (str)
    (char= (uiop:last-char str) #\?))

(defun yell-p (str)
    (let ((just-letters (remove-if-not #'alpha-char-p str)))
        (and (not (uiop:emptyp just-letters)) (every #'upper-case-p just-letters))))

(defun response (hey-bob)
    (setf hey-bob (string-trim *whitespace-str* hey-bob))
    (cond
      ((silence-p hey-bob) "Fine. Be that way!")
      ((and (yell-p hey-bob) (question-p hey-bob)) "Calm down, I know what I'm doing!")
      ((yell-p hey-bob) "Whoa, chill out!")
      ((question-p hey-bob) "Sure.")
      (t "Whatever.")))
