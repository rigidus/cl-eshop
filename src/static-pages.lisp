;; Контент для статических страниц
(in-package #:eshop)

;; хранилище статических страниц
(defvar static-pages.*storage* (make-hash-table :test #'equal))



;; загрузка статей из папки
(defun static-pages.process-dir (path &optional (ctype "article"))
  (let ((files))
    (mapcar #'(lambda (x)
                (if (not (cl-fad:directory-pathname-p x))
                    (push x files)))
            (directory (format nil "~a/*.art" path)))
    (mapcar #'(lambda (file)
                (wlog file)
                (unserialize (format nil "~a" file) (make-instance 'article :ctype ctype)))
            files)))


;;заргузка статических страниц из файлов
(defun static-pages.restore ()
  (let ((t-storage))
      (wlog "START:RESTOR:static-pages")
      (sb-ext:gc :full t) ;; запуск сборщика мусора
      (let ((*storage-articles* (make-hash-table :test #'equal)))
        (static-pages.process-dir *path-to-static-pages* "static")
        (setf t-storage *storage-articles*))
      (setf static-pages.*storage* t-storage)
      (maphash #'(lambda (k v)
                   (setf (gethash k *storage*) v))
               static-pages.*storage*)
      (sb-ext:gc :full t) ;; запуск сборщика мусора
      (wlog "FIN:RESTOR:static-pages")))

;;обновление шаблонов для отображения
(defun static-pages.update ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("index.html")))

