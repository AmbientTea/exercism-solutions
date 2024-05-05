(defpackage :meetup
  (:use :cl)
  (:export :meetup))

(in-package :meetup)

(defconstant +seconds-in-day+ (* 24 60 60))

(defun day-of-week-num (dow)
    (case dow
      (:monday 0)
      (:tuesday 1)
      (:wednesday 2)
      (:thursday 3)
      (:friday 4)
      (:saturday 5)
      (:sunday 6)))

(defun teen-p (day)
    (<= 13 day 19))

(defun match-to-schedule (days schedule)
    (case schedule
      (:first (first days))
      (:second (second days))
      (:third (third days))
      (:fourth (fourth days))
      (:fifth (fifth days))
      (:last (car (last days)))
      (:teenth (find-if #'teen-p days))))

(defun start-of-day (day month year)
    (encode-universal-time 0 0 0 day month year 0))

(defun day-month-year-dow (time)
    (cdddr (multiple-value-list (decode-universal-time time 0))))

(defun dows-in-month (for-month for-year target-day-of-week)
    (loop
      :for time :from (start-of-day 1 for-month for-year) :by +seconds-in-day+
      :for (day month year day-of-week) = (day-month-year-dow time)
      :while (= for-month month)
      :when (= day-of-week target-day-of-week)
        :collect day))

(defun meetup (month year dow schedule)
    "Returns a date in the format (y m d) for a given meetup date."
    (let* ((dows (dows-in-month month year (day-of-week-num dow)))
           (matching-day (match-to-schedule dows schedule)))
        (when matching-day
            (list year month matching-day))))
