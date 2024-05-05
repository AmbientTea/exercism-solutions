(defpackage :two-bucket
  (:use :cl)
  (:export :measure))

(in-package :two-bucket)

(defparameter *capacity-one* nil)
(defparameter *capacity-two* nil)
(defparameter *start-bucket* nil)
(defparameter *goal* nil)
(defparameter *seen-states* nil)

(defun measure-result (moves bucket other)
    `((:moves . ,moves) (:goal-bucket . ,bucket) (:other-bucket . ,other)))

(defun goal-reached (state moves)
    (destructuring-bind (v1 v2) state
        (cond ((= v1 *goal*) (measure-result moves :one v2))
              ((= v2 *goal*) (measure-result moves :two v1)))))

(defun legal-state-p (state)
    (not (equal state (case *start-bucket*
                        (:one `(0 ,*capacity-two*))
                        (:two `(,*capacity-one* 0))))))

(defun seen-state-p (state)
    (member state *seen-states* :test 'equal))

(defun next-states (state)
    (loop
      :with (v1 v2) = state
      :with max-> = (min v1 (- *capacity-two* v2))
      :with max<- = (min v2 (- *capacity-one* v1))
      :for next-state :in (list
                           ;; pour ->
                           (list (- v1 max->) (+ v2 max->))
                           ;; pour <-
                           (list (+ v1 max<-) (- v2 max<-))
                           ;; empty 1
                           (list 0 v2)
                           ;; empty 2
                           (list v1 0)
                           ;; fill 1
                           (list *capacity-one* v2)
                           ;; fill 2
                           (list v1 *capacity-two*))
      :when (legal-state-p next-state)
        :unless (seen-state-p next-state)
          :collect next-state))

(defun measure-search (initial-state)
    (block outer
        (loop
          :with states-to-search = (list initial-state)
          :while states-to-search
          :for moves :from 1
          :for *seen-states* = (append states-to-search *seen-states*)
          :for next-states = (loop
                               :for state :in states-to-search
                               :for result = (goal-reached state moves)
                               :if result
                                 :do (return-from outer result)
                               :else
                                 :append (next-states state))
          :do (setf states-to-search next-states))))

(defun measure (capacity-one capacity-two goal start-bucket)
    "Function to solve the two-bucket puzzle, if possible, when given the capacities
of both buckets, a goal, and which bucket to start with.  Returns an alist of moves
required to reach the goal, the name of the bucket that reach the goal, and the
amount of water left over in the other bucket."
    (let ((*goal* goal)
          (*capacity-one* capacity-one)
          (*capacity-two* capacity-two)
          (*start-bucket* start-bucket)
          (initial-state (case start-bucket
                           (:one `(,capacity-one 0))
                           (:two `(0 ,capacity-two)))))
        (measure-search initial-state)))
