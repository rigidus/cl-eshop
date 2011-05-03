
(in-package #:eshop)

;; хранилище статей
(defparameter *storage-articles* (make-hash-table :test #'equal))

;; описание полей статьи
(defclass article ()
  ((key               :initarg :key       :initform nil       :accessor key)
   (name              :initarg :name      :initform nil       :accessor name)
   (descr             :initarg :descr      :initform nil       :accessor descr)
   (body              :initarg :body      :initform nil       :accessor body)
   (date              :initarg :date      :initform nil       :accessor date)
   (tag               :initarg :tag       :initform (make-hash-table :test #'equal) :accessor tag)
   ))

;;
(defmethod unserialize (filepath (dummy article))
    (let* ((file-content (alexandria:read-file-into-string filepath))
           (raw (decode-json-from-string file-content))
           (key (pathname-name filepath))
           (body (cdr (assoc :body raw)))
           (name (cdr (assoc :name raw)))
           (date (cdr (assoc :date raw)))
           (descr (cdr (assoc :descr raw)))
           (new (make-instance 'article
                               :key key
                               :name name
                               :descr descr
                               :body body
                               :date date
                               :tag nil)))
      (setf (gethash key *storage-articles*) new)
      ;; Возвращаем key статьи
      key))


;; загрузка статей из папки
(defun process-articles-dir (path)
  (let ((files))
    (mapcar #'(lambda (x)
                (if (not (cl-fad:directory-pathname-p x))
                    (push x files)))
            (directory (format nil "~a/*" path)))
    (mapcar #'(lambda (file)
                (unserialize (format nil "~a" file) (make-instance 'article)))
            files)))


;;
(defun restore-articles-from-files ()
  (let ((t-storage))
    (handler-bind ((WRONG-ARTILE-FILE
                    #'(lambda (e)
                        (format t "~%warn: WRONG-ARTICLE-FILE: ~a"
                                (filepath e))
                        (invoke-restart 'ignore)
                        )))
      (print "start load articles....{")
      (sb-ext:gc :full t)
      (let ((*storage-articles* (make-hash-table :test #'equal)))
        (process-articles-dir *path-to-articles*)
        (setf t-storage *storage-articles*))
      (setf *storage-articles* t-storage)
      (sb-ext:gc :full t)
      (print "...} finish load articles"))))


;; загрузить статьи
(print ">> Articles <<")
(restore-articles-from-files)

(defun get-articles (&optional tags)
  (let ((articles))
    (maphash #'(lambda (k v) (push v articles))
     *storage-articles*)
    articles))

;; отображение списка статей
(defun articles-page ()
  (default-page
      (static:main
       (list :menu (menu)
             :breadcrumbs "<p class=\"breadcrumbs\"><a href=\"/\">Главная</a> / Статьи"
             :subcontent  (articles:articles-list (list :articles
                                                        (mapcar #'(lambda (v)
                                                                    (list :name (name v)
                                                                          :date (date v)
                                                                          :descr (descr v)
                                                                          :key (key v)))
                                                                (get-articles))))
             :rightblock  ""))))

(defun get-article-breadcrumbs(article)
  (format nil "<p class=\"breadcrumbs\">
                  <a href=\"/\">Главная</a> /
                  <a href=\"/articles\">Статьи</a> /
                  ~a </p>" (name article)))

;; отображение страницы статьи
(defmethod restas:render-object ((designer eshop-render) (object article))
  (default-page
      (static:main
       (list :menu (menu)
             :breadcrumbs (get-article-breadcrumbs object)
             :subcontent  (articles:article-big (list :name (name object)
                                                      :date (date object)
                                                      :body (body object)))
             :rightblock  ""))))
