(defpackage :space-age
  (:use :cl)
  (:export
   :on-mercury
   :on-venus
   :on-earth
   :on-mars
   :on-jupiter
   :on-saturn
   :on-uranus
   :on-neptune))

(in-package :space-age)

(defconstant earth-seconds-per-year 31557600)

(defun prefix-symbol (prefix symbol)
    (intern (concatenate 'string prefix (string symbol))))

(defmacro def-planet (name ratio)
    (let ((fname (prefix-symbol "ON-" name)))
        `(defun ,fname (seconds)
             (/ seconds earth-seconds-per-year ,ratio))))

(def-planet mercury 0.2408467)
(def-planet venus 0.61519726)
(def-planet earth 1)
(def-planet mars 1.8808158)
(def-planet jupiter 11.862615)
(def-planet saturn 29.447498)
(def-planet uranus 84.016846)
(def-planet neptune 164.79132)
