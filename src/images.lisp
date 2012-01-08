(in-package #:eshop)

(defun images-get-dimensions (path-to-image)
  (let* ((string
          (let ((proc (sb-ext:run-program "/usr/bin/identify"
                                          (list "-format" "%w %h" path-to-image) :wait nil :output :stream)))
            (with-open-stream (in (sb-ext:process-output proc))
              (read-line in))))
         (space-pos (search " " string)))
    (values-list (list (parse-integer (subseq string 0 space-pos))
                       (parse-integer (subseq string (+ 1 space-pos)))))))

(defun images-style-for-resize (width height req-size)
  (if (>= width height)
    (format nil "width:~apx" (min width req-size))
    (when (> height width)
      (format nil "height:~apx" (min height req-size)))))

