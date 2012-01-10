(in-package #:eshop)

;; <loc>http://www.example.com/</loc>
;; <lastmod>2005-01-01</lastmod>
;; <changefreq>monthly</changefreq>
;; <priority>0.8</priority>



(defparameter *sitemap-lastmod-time* nil)
(defparameter *sitemap-num-routs* nil)
(defparameter *sitemap-routs-storage* nil)
(defparameter *sitemap-stream* nil)

(defun sitemap-push-format-r (r)
  ;; (push r *sitemap-routs-storage*)
  (incf *sitemap-num-routs*)
  (format *sitemap-stream* "~&~a" (sitemap:route r)))

;;статические страницы
(defun sitemap-static-routes ()
  (maphash #'(lambda (k v)
               (sitemap-push-format-r (list :loc (format nil "http://www.320-8080.ru/~a" k)
                                            :lastmod *sitemap-lastmod-time*
                                            :changefreq "monthly"
                                            :priority "0.5")))
           static-pages.*storage*))

;;группы
(defun sitemap-one-group (g)
  ;; (print g)
  (sitemap-push-format-r (list :loc (format nil "http://www.320-8080.ru/~a" (hunchentoot:url-encode (key g)))
                 :lastmod *sitemap-lastmod-time*
                 :changefreq "daily"
                 :priority "0.5")))

;;производители
(defun sitemap-vendors (g)
  ;; (get-list-of-producers g  #'(lambda (product) t))
  (mapcar #'(lambda (v)
              (sitemap-push-format-r (list :loc (format nil "http://www.320-8080.ru/~a?vendor=~a"
                                                        (hunchentoot:url-encode (key g))
                                                        (hunchentoot:url-encode (car v)))
                                               :lastmod *sitemap-lastmod-time*
                                               :changefreq "daily"
                                               :priority "0.5")))
          (get-list-of-producers g  #'(lambda (product) (declare (ignore product)) t))))

;;фильтры
(defun sitemap-filters (flist)
  (loop
     :for filter in flist
     :do  (sitemap-push-format-r (list :loc (format nil "http://www.320-8080.ru/~a" (hunchentoot:url-encode
                                                                       (name filter)))
                         :lastmod *sitemap-lastmod-time*
                         :changefreq "daily"
                         :priority "0.5"))))

;;продукты
(defun sitemap-products (plist)
  (mapcar #'(lambda (product)
              (sitemap-push-format-r (list :loc (format nil "http://www.320-8080.ru/~a" (articul product))
                             :lastmod *sitemap-lastmod-time*
                             :changefreq "monthly"
                             :priority "0.5")))
          plist))


;;статьи
(defun sitemap-article-routes ()
  (maphash #'(lambda (k v)
               (sitemap-push-format-r (list :loc (format nil "http://www.320-8080.ru/~a" (hunchentoot:url-encode k))
                                            :lastmod *sitemap-lastmod-time*
                                            :changefreq "monthly"
                                            :priority "0.5")))
           *storage-articles*))

;;особые маршруты
(defun sitemap-special-routs ()
  (sitemap-push-format-r (list :loc (format nil "http://www.320-8080.ru/")
                               :lastmod *sitemap-lastmod-time*
                               :changefreq "hourly"
                               :priority "0.5"))
  (sitemap-push-format-r (list :loc (format nil "http://www.320-8080.ru/articles")
                               :lastmod *sitemap-lastmod-time*
                               :changefreq "daily"
                               :priority "0.5"))
  (sitemap-push-format-r (list :loc (format nil "http://www.320-8080.ru/articles/news")
                               :lastmod *sitemap-lastmod-time*
                               :changefreq "daily"
                               :priority "0.5"))
  (sitemap-push-format-r (list :loc (format nil "http://www.320-8080.ru/articles/papers")
                               :lastmod *sitemap-lastmod-time*
                               :changefreq "daily"
                               :priority "0.5"))
  (sitemap-push-format-r (list :loc (format nil "http://www.320-8080.ru/articles/reviews")
                               :lastmod *sitemap-lastmod-time*
                               :changefreq "daily"
                               :priority "0.5")))

(defun sitemap-group-routes-proc (g)
  ;; (print g)
  (sitemap-one-group g)
  (sitemap-vendors g)
  (sitemap-filters (filters g))
  (sitemap-products (products g)))

(defun sitemap-group-routes ()
  (maphash #'(lambda (k g)
               (when (and (equal (type-of g) 'group)
                          (active g))
                 (sitemap-group-routes-proc g)))
           *storage*))



;;список всех страниц сайта
(defun sitemap-all-routes ()
  (let ((*sitemap-lastmod-time* (time.get-lastmod-time)))
    (sitemap-special-routs)
    (sitemap-static-routes)
    (sitemap-group-routes)
    (sitemap-article-routes)
    ))


(defun make-sitemap-data (stream)
  (let ((*sitemap-routs-storage* nil)
        (*sitemap-num-routs* 0)
        (*sitemap-stream* stream))
    (format *sitemap-stream* "~&~a" (sitemap:head))
    (sitemap-all-routes)
    (format *sitemap-stream* "~&~a" (sitemap:tail))
    *sitemap-num-routs*))


(defun create-sitemap-file ()
  (format t "~&create Sitemap.XML: ")
  (let ((filename (format nil "~a/sitemap.xml" *path-to-conf*)))
    (with-open-file
        (stream filename :direction :output :if-exists :supersede)
      (make-sitemap-data stream))))


