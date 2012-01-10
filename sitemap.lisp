;;;; sitemap.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(in-package #:eshop)

;; <loc>http://www.example.com/</loc>
;; <lastmod>2005-01-01</lastmod>
;; <changefreq>monthly</changefreq>
;; <priority>0.8</priority>



(defparameter *sitemap-lastmod-time* nil)
(defparameter *sitemap-num-routs* nil)
(defparameter *sitemap-routs-storage* nil)
(defparameter *sitemap-stream* nil)

(defun sitemap.get-groups-routes ()
  (remove-if #'null
             (mapcar #'(lambda (group)
                         (when (and (active group)
                                    (not (empty group)))
                           (list :loc (format nil "http://www.320-8080.ru/~a" (hunchentoot:url-encode (key group)))
                                 :lastmod *sitemap-lastmod-time*
                                 :changefreq "daily"
                                 :priority "0.5")))
                     (groups *global-storage*))))

(defun sitemap.get-products-routes ()
  (mapcar #'(lambda (product)
              (list :loc (format nil "http://www.320-8080.ru/~a" (hunchentoot:url-encode (key product)))
                    :lastmod *sitemap-lastmod-time*
                    :changefreq "daily"
                    :priority "0.5"))
          (active-products *global-storage*)))

(defun sitemap.get-filters-routes ()
  (mapcar #'(lambda (filter)
              (list :loc (format nil "http://www.320-8080.ru/~a" (hunchentoot:url-encode (key filter)))
                    :lastmod *sitemap-lastmod-time*
                    :changefreq "daily"
                    :priority "0.5"))
          (filters *global-storage*)))

(defun sitemap.get-articles-routes ()
  (maphash #'(lambda (k v)
               (declare (ignore v))
               (list :loc (format nil "http://www.320-8080.ru/~a" (hunchentoot:url-encode k))
                     :lastmod *sitemap-lastmod-time*
                     :changefreq "monthly"
                     :priority "0.5"))
           *storage-articles*))

(defun sitemap.get-vendors-routes ()
  (let ((res))
    (mapcar #'(lambda (group)
                (maphash #'(lambda (key v)
                             (declare (ignore v))
                             (push
                              (list :loc (format nil "http://www.320-8080.ru/~a?vendor=~a"
                                                 (hunchentoot:url-encode (key group))
                                                 (hunchentoot:url-encode key))
                                    :lastmod *sitemap-lastmod-time*
                                    :changefreq "daily"
                                    :priority "0.5")
                              res))
                         (storage.get-vendors group)))
          (groups *global-storage*))
    res))

(defun sitemap.get-static-routes ()
  (mapcar #'(lambda (k)
               (list :loc (format nil "http://www.320-8080.ru/~a" k)
                     :lastmod *sitemap-lastmod-time*
                     :changefreq "monthly"
                     :priority "0.5"))
           (let ((statics))
             (maphash #'(lambda (k v)
                          (if (equal (type-of v) 'article)
                              (push k statics)))
                      (storage *global-storage*))
             statics)))

(defun sitemap.get-special-routes ()
  (list
   (list :loc (format nil "http://www.320-8080.ru/")
         :lastmod *sitemap-lastmod-time*
         :changefreq "hourly"
         :priority "0.5")
   (list :loc (format nil "http://www.320-8080.ru/articles")
         :lastmod *sitemap-lastmod-time*
         :changefreq "daily"
         :priority "0.5")
   (list :loc (format nil "http://www.320-8080.ru/articles/news")
         :lastmod *sitemap-lastmod-time*
         :changefreq "daily"
         :priority "0.5")
   (list :loc (format nil "http://www.320-8080.ru/articles/papers")
         :lastmod *sitemap-lastmod-time*
         :changefreq "daily"
         :priority "0.5")
   (list :loc (format nil "http://www.320-8080.ru/articles/reviews")
         :lastmod *sitemap-lastmod-time*
         :changefreq "daily"
         :priority "0.5")))

(defun sitemap.get-all-routes-list ()
  (append
   (sitemap.get-groups-routes)
   (sitemap.get-products-routes)
   (sitemap.get-filters-routes)
   (sitemap.get-articles-routes)
   (sitemap.get-vendors-routes)
   (sitemap.get-static-routes)
   (sitemap.get-special-routes)))


;; (defun sitemap.create-sitemap-file ()
;;   (wlog "create Sitemap.XML: ")
;;   (setq *sitemap-lastmod-time* (time.get-lastmod-time))
;;   (let ((filepath *path-sitemap*)
;;         (routes (sitemap.get-all-routes-list))
;;         (number 0))
;;     (wlog (format nil "routes num: ~a" (length routes)))
;;     (loop
;;        while routes
;;        do
;;          (incf number)
;;          (with-open-file
;;              (stream (pathname (format nil "~a/sitemap~a.xml" filepath number))
;;                      :direction :output :if-exists :supersede)
;;            (format stream "~a" (soy.sitemap:head))
;;            (loop
;;               for i
;;               from 0 to (min (- (length routes) 1) 45000)
;;               do
;;                 (format stream "~a~%"
;;                         (soy.sitemap:route (nth i routes))))
;;            (format stream "~a" (soy.sitemap:tail)))
;;          (setf routes (nthcdr 45000 routes)))
;;     (with-open-file
;;         (stream (pathname (format nil "~a/sitemap-index.xml" filepath))
;;                 :direction :output :if-exists :supersede)
;;       (format stream "~a"
;;               (soy.sitemap:sitemap-index-file
;;                (list :names (loop
;;                                for i from 1 to number
;;                                collect (format nil "sitemap~a.xml" i))
;;                      :lastmod *sitemap-lastmod-time*))))))



