(defpackage :log-levels
  (:use :cl)
  (:export :log-message :log-severity :log-format))

(in-package :log-levels)

(defun log-message (log-string)
    (subseq log-string 8))

(defun log-severity (log-string)
    (let ((level-str (subseq log-string 1 5)))
        (cond
          ((string-equal "info" level-str) :everything-ok)
          ((string-equal "warn" level-str) :getting-worried)
          ((string-equal "ohno" level-str) :run-for-cover))))

(defun log-format (log-string)
    (let ((msg (log-message log-string)))
        (case (log-severity log-string)
          (:everything-ok (string-downcase msg))
          (:getting-worried (string-capitalize msg))
          (:run-for-cover (string-upcase msg)))))
