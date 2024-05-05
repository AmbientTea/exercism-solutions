(defpackage :spiral-matrix
  (:use :cl)
  (:export :spiral-matrix))

(in-package :spiral-matrix)

(defclass spiral-iter ()
  ((x :initarg :x :accessor x :initform 0)
   (y :initarg :y :accessor y :initform 0)
   (direction :initarg :direction
              :accessor direction
              :initform :right)
   (steps :initarg :steps
          :accessor steps
          :initform 0
          :documentation "Steps taken incurrent direction")
   (step-target :initarg :step-target
                :accessor step-target
                :documentation "Steps to take before turning right once")
   (shorten-target-in :initarg :shorten-target-in
                      :accessor shorten-target-in
                      :initform 3
                      :documentation "Turns to take before shortening step-target by 1")))

(defun make-spiral-iter (steps)
    (make-instance 'spiral-iter :step-target steps))

(defmethod print-object ((iter spiral-iter) stream)
    (with-slots (x y direction steps step-target shorten-target-in) iter
        (print-unreadable-object (iter stream)
            (format stream
                    "(~w, ~w), steps: ~w, target: ~w, short in: ~w"
                    x y steps step-target shorten-target-in))))

(defmethod turn-right ((iter spiral-iter))
    (setf (direction iter)
          (case (direction iter)
            (:right :down)
            (:down :left)
            (:left :up)
            (:up :right))))

(defmethod shift-in-direction ((iter spiral-iter) &key (by 1))
    (case (direction iter)
      (:right (incf (x iter) by))
      (:down (incf (y iter) by))
      (:left (decf (x iter) by))
      (:up (decf (y iter) by))))

(defmethod spiral-iter-step ((iter spiral-iter))
    (shift-in-direction iter)
    (incf (steps iter))
    (when (= (steps iter) (step-target iter))
        (setf (steps iter) 0)
        (turn-right iter)
        (decf (slot-value iter 'shorten-target-in))
        (when (zerop (shorten-target-in iter))
            (decf (step-target iter))
            (setf (shorten-target-in iter) 2))))

(defun spiral-matrix (size)
    (when (< 0 size)
        (loop :with arr = (make-array (list size size))
              :with iter = (make-spiral-iter (1- size))
              :for i :from 1 :to (* size size)
              :do (setf (aref arr (y iter) (x iter)) i)
              :do (spiral-iter-step iter)
              :finally (return arr))))
