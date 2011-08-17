
(in-package #:eshop)

;; хранилище статей
(defvar *storage-articles* (make-hash-table :test #'equal))

;; описание полей статьи
(defclass article ()
  ((key               :initarg :key       :initform nil       :accessor key)
   (name              :initarg :name      :initform nil       :accessor name)
   (descr             :initarg :descr     :initform nil       :accessor descr)
   (body              :initarg :body      :initform nil       :accessor body)
   (date              :initarg :date      :initform nil       :accessor date)
   (tags               :initarg :tags     :initform (make-hash-table :test #'equal) :accessor tags)
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

(defun article-decode-date(input-string)
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

(defun article-encode-data(article)
  (multiple-value-bind (second minute hour date month year) (decode-universal-time (date article))
    (declare (ignore second))
    (format nil
            "~2,'0d.~2,'0d.~d"
            date
            month
            year)))

;;
(defmethod unserialize (filepath (dummy article))
    (let* ((file-content (alexandria:read-file-into-string filepath))
           (raw (decode-json-from-string file-content))
           (key (pathname-name filepath))
           (body (cdr (assoc :body raw)))
           (name (cdr (assoc :name raw)))
           (date (article-decode-date (cdr (assoc :date raw))))
           (descr (cdr (assoc :descr raw)))
           (tags-line (cdr (assoc :tags raw)))
           (new (make-instance 'article
                               :key key
                               :name name
                               :descr descr
                               :body body
                               :date date)))
      (make-tags-table (tags new) tags-line)
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
      (print "start load articles....{")
      (sb-ext:gc :full t)
      (let ((*storage-articles* (make-hash-table :test #'equal)))
        (process-articles-dir *path-to-articles*)
        (setf t-storage *storage-articles*))
      (setf *storage-articles* t-storage)
      (sb-ext:gc :full t)
      (print "...} finish load articles")))

;;обновление страницы
(defun articles-update ()
  (articles-compile-templates))

;;шаблоны
(defun articles-compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("index.html"
            "articles.soy"
            "footer.html")))

;; загрузить статьи
;; (articles-update)
(print ">> Articles <<")


;; (defun get-date-time ()
;;   (multiple-value-bind (second minute hour date month year) (get-decoded-time)
;;     (declare (ignore second))
;;     (format nil
;;             "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
;;             year
;;             month
;;             date
;;             hour
;;             minute)))


(defun articles-sort (unsort-articles)
  (sort unsort-articles #'> :key #'date))


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
