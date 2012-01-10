;; Контент для статических страниц
(in-package #:eshop)

;; хранилище статических страниц
(defvar static-pages.*storage* (make-hash-table :test #'equal))



;;заргузка статических страниц из файлов
(defun static-pages.restore ()
  (let ((t-storage))
      (wlog "START:RESTOR:static-pages")
      (sb-ext:gc :full t) ;; запуск сборщика мусора
      (let ((*storage-articles* (make-hash-table :test #'equal)))
        (process-articles-dir *path-to-static-pages* "static")
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

