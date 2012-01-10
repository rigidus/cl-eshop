;;;; articles.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(in-package #:eshop)

;; хранилище статей
(defvar *storage-articles* (make-hash-table :test #'equal))

;; описание полей статьи
(defclass article ()
  ((key         :initarg :key       :initform nil       :accessor key)
   (name        :initarg :name      :initform nil       :accessor name)
   (descr       :initarg :descr     :initform nil       :accessor descr)
   (bredcrumbs  :initarg :bredcrumbs :initform nil      :accessor bredcrumbs)
   (rightblock  :initarg :rightblock :initform nil      :accessor rightblock)
   (title       :initarg :title      :initform nil      :accessor title)
   (body        :initarg :body      :initform nil       :accessor body)
   (date        :initarg :date      :initform nil       :accessor date)
   (ctype        :initarg :ctype    :initform "article"  :accessor ctype) ;; article / static
   (tags        :initarg :tags    :initform (make-hash-table :test #'equal) :accessor tags)
   ))

;;тэги через запятую
(defun make-tags-table(tags input-string)
  (let ((words (split-sequence:split-sequence #\, input-string)))
    (mapcar #'(lambda (w)
                (if (not (null w))
                    (setf (gethash (stripper w) tags) w)))
            words)
    ;; (format t "~&~a: ~{~a~^,~}" key skls)
    ))


;;
(defmethod unserialize (filepath (dummy article))
    (let* ((file-content (alexandria:read-file-into-string filepath))
           (raw (decode-json-from-string file-content))
           (key (pathname-name filepath))
           (body (cdr (assoc :body raw)))
           (breadcrumbs (cdr (assoc :breadcrumbs raw)))
           (rightblock (cdr (assoc :rightblock raw)))
           (name (cdr (assoc :name raw)))
           (date (time.article-decode-date (cdr (assoc :date raw))))
           (descr (cdr (assoc :descr raw)))
           (tags-line (cdr (assoc :tags raw)))
           (title (cdr (assoc :title raw)))
           (new (make-instance 'article
                               :key key
                               :name name
                               :descr descr
                               :bredcrumbs breadcrumbs
                               :body body
                               :rightblock rightblock
                               :title title
                               :ctype (ctype dummy)
                               :date date)))
      (make-tags-table (tags new) tags-line)
      (setf (gethash key *storage-articles*) new)
      ;; Возвращаем key статьи
      key))


;; загрузка статей из папки
(defun process-articles-dir (path &optional (ctype "article"))
  (let ((files))
    (mapcar #'(lambda (x)
                (if (not (cl-fad:directory-pathname-p x))
                    (push x files)))
            (directory (format nil "~a/*" path)))
    (mapcar #'(lambda (file)
                (wlog file)
                (unserialize (format nil "~a" file) (make-instance 'article :ctype ctype)))
            files)))


;;
(defun articles.restore ()
  (let ((t-storage))
      (wlog "start loadc articles....{")
      (sb-ext:gc :full t)
      (let ((*storage-articles* (make-hash-table :test #'equal)))
        (process-articles-dir *path-to-articles* "article")
        (setf t-storage *storage-articles*))
      (setf *storage-articles* t-storage)
      (sb-ext:gc :full t)
      (wlog "...} finish load articles")))

;;шаблоны
(defun articles.update ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("index.html"
            "articles.soy"
            "footer.html")))


(defun articles.sort (unsort-articles)
  (sort unsort-articles #'> :key #'date))


;;;;;;;;;;;;;;;;;;;;; RENDER ;;;;;;;;;;;;;;;;;;;;;;;;;



(defun get-articles-list (&optional request-get-plist)
  (let ((articles)
        (showall (getf request-get-plist :showall))
        (date (getf request-get-plist :showall)))
    (declare (ignore date))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
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
                     :date (time.article-encode-date v)
                     :key (key v)))
          articles))

;; отображение списка статей
;; (defun articles-page (request-get-plist)
;;   (let* ((tags (getf request-get-plist :tags))
;;          (breadcrumbs)
;;          (menu  (soy.articles:articles-menu (list :tag tags))))
;;     (if (not (null tags))
;;         (setf breadcrumbs
;;               (format nil "       <a href=\"/\">Главная</a> /
;;                                   <a href=\"/articles\">Материалы</a> /
;;                                   ~a" tags))
;;         (setf breadcrumbs "       <a href=\"/\">Главная</a> /
;;                                   Материалы"))
;;     (multiple-value-bind (paginated pager)
;;         (paginator request-get-plist
;;                    (articles.sort
;;                     (get-articles-by-tags
;;                      (get-articles-list request-get-plist) tags))
;;                    10)
;;       (default-page
;;           (static:main
;;            (list :menu (new-classes.menu)
;;                  :breadcrumbs breadcrumbs
;;                  :subcontent  (soy.articles:articles-main
;;                                (list :menu menu
;;                                      :articles (soy.articles:articles-list
;;                                                 (list :pager pager
;;                                                       :articles
;;                                                       (mapcar #'(lambda (v)
;;                                                                   (list :name (name v)
;;                                                                         :date (time.article-encode-date v)
;;                                                                         :descr (descr v)
;;                                                                         :key (key v)
;;                                                                         :tags
;;                                                                         (if (< 0 (hash-table-count (tags v)))
;;                                                                             (soy.articles:articles-tags
;;                                                                              (list :tags
;;                                                                                    (loop
;;                                                                                       :for key being the hash-keys
;;                                                                                       :of (tags v)
;;                                                                                       :collect key)))
;;                                                                             "")))
;;                                                             paginated)))))
;;                :rightblock  ""))))))

(defun get-article-breadcrumbs(article)
    (format nil "
                  <a href=\"/\">Главная</a> /
                  <a href=\"/articles\">Материалы</a> /
                  ~a " (name article)))

;; отображение страницы статьи
;; (defmethod restas:render-object ((designer eshop-render) (object article))
;;   (if (equal (ctype object) "static")
;;       (root:main (list :keywords "" ;;keywords
;;                        :description "" ;;description
;;                        :title (name object)
;;                        :header (root:header (append (list :logged (root:notlogged)
;;                                                           :cart (root:cart))
;;                                                     (main-page-show-banner "line" (banner *main-page.storage*))))
;;                        :footer (root:footer)
;;                        :content  (static:main
;;                                  (list :menu (new-classes.menu)
;;                                        :breadcrumbs (bredcrumbs object)
;;                                        :subcontent  (body object)
;;                                        :rightblock  (rightblock object)))
;;                  ))
;;       (root:main (list :keywords "" ;;keywords
;;                        :description "" ;;description
;;                        :title  (if (title object)
;;                                    (title object)
;;                                    (name object))
;;                        :headext (soy.articles:head-share-buttons (list :key (key object)))
;;                        :header (root:header (append (list :logged (root:notlogged)
;;                                                           :cart (root:cart))
;;                                                     (main-page-show-banner "line" (banner *main-page.storage*))))
;;                        :footer (root:footer)
;;                        :content (static:main
;;                                  (list :menu (new-classes.menu)
;;                                        :breadcrumbs (get-article-breadcrumbs object)
;;                                        :subcontent  (soy.articles:article-big (list :sharebuttons (soy.articles:share-buttons
;;                                                                                                    (list :key (key object)))
;;                                                                                     :name (name object)
;;                                                                                     :date (time.article-encode-date object)
;;                                                                                     :body (prerender-string-replace (body object))
;;                                                                                     :articles (let ((articles (articles.sort (remove-if #'(lambda(v)(equal v object)) (get-articles-list)))))
;;                                                                                                 (if articles
;;                                                                                                     (articles-view-articles (subseq articles 0 7))
;;                                                                                                     nil))
;;                                                                                     :tags
;;                                                                                     (if (< 0 (hash-table-count (tags object)))
;;                                                                                         (soy.articles:articles-tags
;;                                                                                          (list :tags
;;                                                                                            (loop
;;                                                                                               :for key being the hash-keys
;;                                                                                               :of (tags object)
;;                                                                                               :collect key)))
;;                                                                                         "")
;;                                                                                     ))
;;                                        :rightblock (soy.articles:r_b_articles (list :articles (let ((articles (articles.sort (remove-if #'(lambda(v)(equal v object)) (get-articles-list)))))
;;                                                                                                 (if articles
;;                                                                                                     (articles-view-articles (subseq articles 0 10))
;;                                                                                                     nil))))))))))


;; (let ((object (gethash "kakdobratsja" (storage *global-storage*))))
;;       (root:main (list :keywords "" ;;keywords
;;                        :description "" ;;description
;;                        :title (name object)
;;                        :header (root:header (append (list :logged (root:notlogged)
;;                                                           :cart (root:cart))
;;                                                     (main-page-show-banner "line" (banner *main-page.storage*))))
;;                        :footer (root:footer)
;;                        :content  (static:main
;;                                  (list :menu (new-classes.menu)
;;                                        :breadcrumbs (bredcrumbs object)
;;                                        :subcontent  (body object)
;;                                        :rightblock  (rightblock object)))
;;                        ))
