;; Контент для статических страниц
(in-package #:eshop)

;; хранилище статических страниц
(defvar static-pages.*storage* (make-hash-table :test #'equal))



;;заргузка статических страниц из файлов
(defun static-pages.restore ()
  (let ((t-storage))
      (wlog "START:RESTOR:static-pages")
      (sb-ext:gc :full t) ;; запуск сборщика мусора
      (let ((static-pages.*storage* (make-hash-table :test #'equal)))
        (process-articles-dir *path-to-static-pages*)
        (setf t-storage static-pages.*storage*))
      (setf static-pages.*storage* t-storage)
      (sb-ext:gc :full t) ;; запуск сборщика мусора
      (wlog "FIN:RESTOR:static-pages")))

;;обновление шаблонов для отображения
(defun static-pages.update ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("index.html")))




;; отображение страницы статьи
(defmethod restas:render-object ((designer eshop-render) (object article))
   (root:main (list :keywords keywords
                    :description description
                    :title title
                    :header (root:header (append (list :logged (root:notlogged)
                                                       :cart (root:cart))
                                                 (main-page-show-banner "line" (banner *main-page.storage*))))
                    :footer (root:footer)
                    :content (static:main
                              (list :menu (menu)
                                    :breadcrumbs (funcall (find-symbol (string-upcase "breadcrumbs") ∆))
                                    :subcontent  (funcall (find-symbol (string-upcase "subcontent") ∆))
                                    :rightblock  (funcall (find-symbol (string-upcase "rightblock") ∆)))))))
