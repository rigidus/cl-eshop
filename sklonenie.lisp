(in-package #:eshop)


(defparameter *group-skls* (make-hash-table :test #'equal))

(defun restore-skls-from-files ()
  (let ((t-storage))
      (print "start load skls....{")
      ;;(sb-ext:gc :full t)
      (let ((*group-skls* (make-hash-table :test #'equal)))
        (load-sklonenie)
        (print *group-skls*)
        (setf t-storage  *group-skls*))
      (setf *group-skls* t-storage)
      ;;(sb-ext:gc :full t)
      (print "...} finish load skls")))

(defun load-sklonenie ()
  (let ((proc (sb-ext:run-program
               "/usr/bin/xls2csv"
               (list "-q3" (format nil "~a/seo/~a" *path-to-dropbox* "sklonenija.xls")) :wait nil :output :stream)))
    (with-open-stream (stream (sb-ext:process-output proc))
      (loop
         :for line = (read-line stream nil)
         :until (or (null line)
                    (string= "" (string-trim "#\," line)))
         :do (let* ((words (split-sequence:split-sequence #\, line))
                    (skls (mapcar #'(lambda (w) (string-trim "#\""  w))
                             words))
                    (key (string-downcase (car skls))))
               (setf (gethash key *group-skls*) skls)
               ;; (format t "~&~a: ~{~a~^,~}" key skls)
               )
              ))))

(defun sklonenie (name skl)
  (let* ((key (string-downcase name))
         (words (gethash key *group-skls*)))
    (if (null words)
        key
        (nth (- skl 1) words))))

(restore-skls-from-files)
