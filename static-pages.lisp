;; Контент
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
(defun articles-update ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("index.html")))





;;;;;;;;;;;;;;;;;;;;; RENDER ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-articles-list (&optional request-get-plist)
  (let ((articles)
        (showall (getf request-get-plist :showall))
        (date (getf request-get-plist :showall)))
    (maphash #'(lambda (k v)
                 (if (or (not (null showall))
                          (not (= (date v) 0)))
                     (push v articles)))
     *storage-articles*)
    articles))

(defun get-articles-by-tags (articles-list &optional tags)
  (let ((articles))
    (if (or (null tags)
            (string= ""
                     (stripper tags)))
        (mapcar #'(lambda (v) (push v articles))
                 articles-list)
        (progn
          (let ((tags (split-sequence:split-sequence #\, tags)))
            (mapcar #'(lambda (v)
                         (let ((has-tags t))
                           (mapcar #'(lambda (tag)
                                       (setf has-tags (and has-tags
                                                           (gethash tag (tags v)))))
                                   tags)
                           (if has-tags
                               (push v articles))))
                     articles-list))
            ))
    articles))

(defun articles-view-articles (articles)
  (mapcar #'(lambda (v)
              (list  :name (name v)
                     :date (article-encode-data v)
                     :key (key v)))
          articles))

;; отображение списка статей
(defun articles-page (request-get-plist)
  (let* ((tags (getf request-get-plist :tags))
         (breadcrumbs)
         (menu  (soy.articles:articles-menu (list :tag tags))))
    (if (not (null tags))
        (setf breadcrumbs
              (format nil "       <a href=\"/\">Главная</a> /
                                  <a href=\"/articles\">Материалы</a> /
                                  ~a" tags))
        (setf breadcrumbs "       <a href=\"/\">Главная</a> /
                                  Материалы"))
    (multiple-value-bind (paginated pager)
        (paginator request-get-plist
                   (articles-sort
                    (get-articles-by-tags
                     (get-articles-list request-get-plist) tags))
                   10)
      (default-page
          (static:main
           (list :menu (menu)
                 :breadcrumbs breadcrumbs
                 :subcontent  (soy.articles:articles-main
                               (list :menu menu
                                     :articles (soy.articles:articles-list
                                                (list :pager pager
                                                      :articles
                                                      (mapcar #'(lambda (v)
                                                                  (list :name (name v)
                                                                        :date (article-encode-data v)
                                                                        :descr (descr v)
                                                                        :key (key v)
                                                                        :tags
                                                                        (if (< 0 (hash-table-count (tags v)))
                                                                            (soy.articles:articles-tags
                                                                             (list :tags
                                                                                   (loop
                                                                                      :for key being the hash-keys
                                                                                      :of (tags v)
                                                                                      :collect key)))
                                                                            "")))
                                                            paginated)))))
               :rightblock  ""))))))

(defun get-article-breadcrumbs(article)
    (format nil "
                  <a href=\"/\">Главная</a> /
                  <a href=\"/articles\">Материалы</a> /
                  ~a " (name article)))

;; отображение страницы статьи
(defmethod restas:render-object ((designer eshop-render) (object article))
  (root:main (list :keywords "" ;;keywords
                   :description "" ;;description
                   :title  (name object)
                   :headext (soy.articles:head-share-buttons (list :key (key object)))
                   :header (root:header (list :logged (root:notlogged)
                                              :cart (root:cart)))
                   :footer (root:footer)
                   :content (static:main
                             (list :menu (menu)
                                   :breadcrumbs (get-article-breadcrumbs object)
                                   :subcontent  (soy.articles:article-big (list :sharebuttons (soy.articles:share-buttons
                                                                                               (list :key (key object)))
                                                                                :name (name object)
                                                                                :date (article-encode-data object)
                                                                                :body (prerender-string-replace (body object))
                                                                                :tags
                                                                                (if (< 0 (hash-table-count (tags object)))
                                                                                    (soy.articles:articles-tags
                                                                                     (list :tags
                                                                                           (loop
                                                                                              :for key being the hash-keys
                                                                                              :of (tags object)
                                                                                              :collect key)))
                                                                                    "")
                                                                                ))
                                   :rightblock  "")))))

