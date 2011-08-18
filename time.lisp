(in-package #:eshop)


(defun time.get-short-date ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (format nil
            "~d~2,'0d~2,'0d"
            year
            month
            date)))

;;get-date-time
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

;;sitemap-get-lastmod-time
(defun time.get-lastmod-time ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (format nil
            "~d-~2,'0d-~2,'0d"
            year
            month
            date)))

;;article-encode-data
(defun time.article-encode-date(article)
  (multiple-value-bind (second minute hour date month year) (decode-universal-time (date article))
    (declare (ignore second minute hour))
    (format nil
            "~2,'0d.~2,'0d.~d"
            date
            month
            year)))

(defun time.article-decode-date(input-string)
  (let ((r 0)
        (counts)
        (date)
        (month)
        (year))
    (when (and (not (null input-string))
               (not (string= ""
                     (stripper input-string))))
      (setf counts (split-sequence:split-sequence #\. input-string))
      (setf date (parse-integer (first counts)))
      (setf month (parse-integer (second counts)))
      (setf year (parse-integer (third counts)))
      (setf r (encode-universal-time 0 0 0 date month year)))
    r))
