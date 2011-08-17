(in-package #:eshop)


(defun time.get-short-date ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (format nil
            "~d~2,'0d~2,'0d"
            year
            month
            date)))

(defun time.get-date-time ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second))
    (format nil
            "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
            year
            month
            date
            hour
            minute)))
