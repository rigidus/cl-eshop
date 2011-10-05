(in-package #:eshop)

;; year 2011 month 9 date 30 -> 20110930
(defun time.get-short-date ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (format nil
            "~d~2,'0d~2,'0d"
            year
            month
            date)))


;; кодирование и декодирование даты вида 2011-09-30 12:01
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

;; декодирование даты вида 2011-09-30 12:01
(defun time.decode-gateway-time (input-string)
    (let ((r 0)
          (counts)
          (dates)
          (times)
          (date)
          (month)
          (year)
          (minute)
          (hour))
      (when (and (not (null input-string))
                 (not (string= ""
                               (stripper input-string))))
        ;; разделяем на даты и на часы минуты
        (setf counts (split-sequence:split-sequence #\  input-string))
        ;; получаем год месяц и день
        (setf dates (split-sequence:split-sequence #\- (first counts)))
        ;; получаем часы и минуты
        (setf times (split-sequence:split-sequence #\: (second counts)))
        ;; установки переменных
        (setf year (parse-integer (first dates)))
        (setf month (parse-integer (second dates)))
        (setf date (parse-integer (third dates)))
        (setf hour (parse-integer (first times)))
        (setf minute (parse-integer (second times)))
      (setf r (encode-universal-time 0 minute hour date month year)))
    r))

(defun time.get-date ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second hour minute))
    (format nil
            "~2,'0d.~2,'0d.~2,'0d"
            date
            month
            (mod year 100))))

(defun time.get-time ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore year month date))
    (format nil
            "~2,'0d:~2,'0d:~2,'0d"
            hour minute second)))


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


(defun time.decode-date-time (uni-time)
   (multiple-value-bind (second minute hour date month year)
       (decode-universal-time uni-time)

     (format nil
             "~a:~a:~a ~a.~a.~a"
             hour minute second date month year)))


(defun time.get-full-human-time ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil
            "~a.~2,'0d.~2,'0d ~a:~a:~a"
            year month date hour minute second)))

;; декодирование даты вида 26.08.2011
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

