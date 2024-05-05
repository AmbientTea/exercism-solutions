(defpackage :robot-simulator
  (:use :cl)
  (:export :+north+ :+east+ :+south+ :+west+ :execute-sequence
   :robot :robot-position :robot-bearing :make-robot))

(in-package :robot-simulator)

(define-condition unknown-move (error) ())

(defclass direction ()
  ((dx :initarg :dx :accessor direction-dx)
   (dy :initarg :dy :accessor direction-dy)
   (left :initform nil :accessor left)
   (right :initform nil :accessor right)))

(defparameter +north+ (make-instance 'direction :dx 0 :dy 1))
(defparameter +east+ (make-instance 'direction :dx 1 :dy 0))
(defparameter +south+ (make-instance 'direction :dx 0 :dy -1))
(defparameter +west+ (make-instance 'direction :dx -1 :dy 0))

(defun set-neighbours (left right)
    (setf (right left) right
          (left right) left))
(set-neighbours +north+ +east+)
(set-neighbours +east+ +south+)
(set-neighbours +south+ +west+)
(set-neighbours +west+ +north+)

(defclass robot ()
  ((x :initarg :x
      :initform 0
      :accessor robot-x)
   (y :initarg :y
      :initform 0
      :accessor robot-y)
   (bearing :initarg :bearing
            :initform +north+
            :accessor robot-bearing)))

(defun make-robot (&key (bearing +north+) (position '(0 . 0)))
    (destructuring-bind (x . y) position
        (make-instance 'robot :bearing bearing :x x :y y)))

(defmethod robot-position ((robot robot))
    (cons (robot-x robot) (robot-y robot)))

(defmethod advance ((robot robot))
    (with-slots (x y bearing) robot
        (with-slots (dx dy) bearing
            (setf (robot-x robot) (+ x dx)
                  (robot-y robot) (+ y dy)))))

(defmethod right ((robot robot))
    (setf (robot-bearing robot) (right (robot-bearing robot))))

(defmethod left ((robot robot))
    (setf (robot-bearing robot) (left (robot-bearing robot))))

(defun execute-sequence (robot sequence)
    (loop :for move :in (coerce sequence 'list)
          :do (case move
                (#\R (right robot))
                (#\L (left robot))
                (#\A (advance robot))
                (otherwise (error 'unknown-move)))
          :finally (return robot)))
