;;;; routes.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(in-package #:eshop)

(defun clear ()
  (loop :for var :being :the :symbols :in :eshop.impl.routes :do (unintern var))
  (restas:reconnect-all-routes))

(clear)

;; (restas:define-route storage-object-route  ("/:key")
;;   "Позвони мне")

;; (setf swank::*connections* nil)


;; FILTER

(defun test-route-filter ()
  (let* ((request-list (request-list))
         (key (cadr request-list))
         (filter (caddr request-list))
         (grp (gethash key *storage*))
         (fltr (gethash filter *storage*)))
    (and (not (null grp))
         (not (null fltr))
         (equal (type-of grp) 'group)
         (equal (type-of fltr) 'filter)
         (equal (key (parent fltr)) key))))

(defun route-filter (filter)
  (gethash filter *storage*))

(restas:define-route filter/-route ("/:key/:filter/" :requirement #'test-route-filter)
  (route-filter filter))

(restas:define-route filter-route ("/:key/:filter" :requirement #'test-route-filter)
  (route-filter filter))


;; STORAGE OBJECT

(defun test-route-storage-object ()
  (let ((obj (gethash (cadr (request-list)) *storage*)))
    (if (not (null obj))
        (if (and (equal (type-of obj)
                        'group)
                 (not (null (getf (request-get-plist) :vendor))))
            (let ((vendor (getf (request-get-plist) :vendor)))
              (not (= (length (remove-if-not #'(lambda (p)
                                                 (vendor-filter-controller p (request-get-plist)))
                                             (get-recursive-products obj)))
                      0)))
            t)
        nil)))

;; (defun test-route-storage-object ()
;;   (gethash (cadr (request-list)) *storage*))



(defun route-storage-object (key)
  (gethash key *storage*))

(restas:define-route storage-object-route  ("/:key" :requirement #'test-route-storage-object)
  (route-storage-object key))

(restas:define-route storage-object/-route  ("/:key/" :requirement #'test-route-storage-object)
  (route-storage-object key))


;; MAIN
(defun test-get-parameters ()
  (null (request-get-plist)))

(restas:define-route main-route ("/" :requirement #'test-get-parameters)
  (main-page-show (request-str)))




;; CATALOG

(restas:define-route catalog-route ("/catalog")
  (default-page (catalog:main (list :menu (menu "")))))


;; STATIC
(defparameter *static-pages* (list "delivery"         "about"             "faq"             "kakdobratsja"
                                   "kaksvjazatsja"    "levashovsky"       "partners"        "payment"
                                   "servicecenter"    "otzyvy"            "pricesc"         "warrantyservice"
                                   "warranty"         "moneyback"         "article"         "news1"
                                   "news2"            "news3"             "news4"           "news5"
                                   "news6"            "dilers"            "corporate"       "vacancy"
                                   "bonus"            "burunduk"          "listservice"     "suslik"))


(defmacro static ()
  `(progn ,@(mapcar #'(lambda (x)
                        `(restas:define-route ,(intern (string-upcase x) *package*) (,x)
                           (static-page)))
                    *static-pages*)))

(static)


;; CART & CHECKOUTS & THANKS

(restas:define-route cart-route ("/cart")
  (cart-page))

(restas:define-route checkout-route ("/checkout")
  (newcart-show))

(restas:define-route checkout0-route ("/checkout0")
  (newcart-show))
 ;; (checkout-page-0))

(restas:define-route checkout1-route ("/checkout1")
  (newcart-show))
  ;; (checkout-page-1))

(restas:define-route checkout2-route ("/checkout2")
  (newcart-show))
  ;; (checkout-page-2))

(restas:define-route checkout3-route ("/checkout3")
  (newcart-show))
  ;; (checkout-page-3))

(restas:define-route thanks-route ("/thanks")
  (thanks-page))


;; GATEWAY

;; (restas:define-route gateway-route ("/gateway")
;;   (gateway-page))

(restas:define-route gateway/post-route ("/gateway" :method :post)
  (gateway-page))


;; (restas:define-route gateway/-route ("/gateway/")
;;   (gateway-page))

;; (restas:define-route gateway/post/-route ("/gateway/" :method :post)
;;   (gateway-page))


;; SEARCH

(restas:define-route search-route ("/search")
  (search-page))


;; YML

(restas:define-route yml-route ("/yml")
  (yml-page))

(restas:define-route yml/-route ("/yml/")
  (yml-page))

(restas:define-route parseryml-route ("/parseryml")
  (yml-page-for-parser))

;; ARTICLES
;;TODO возможно проверять входные тэги
(defun test-article-get-parameters ()
  t)

;;проверяем есть ли такая статья
(defun test-route-article-object ()
  (not (null (gethash (caddr (request-list)) *storage-articles*))))

;;архив матерьялов
(restas:define-route article-route ("/articles" :requirement #'test-article-get-parameters)
  (articles-page (request-get-plist)))

;;список статей
(restas:define-route article-papers-route ("/articles/papers" :requirement #'test-article-get-parameters)
  (let ((request-get-plist (request-get-plist)))
    (if (null (getf request-get-plist :tags))
        (setf (getf request-get-plist :tags) "Статьи"))
    (articles-page request-get-plist)))

;;список новостей
(restas:define-route article-news-route ("/articles/news" :requirement #'test-article-get-parameters)
  (let ((request-get-plist (request-get-plist)))
    (if (null (getf request-get-plist :tags))
        (setf (getf request-get-plist :tags) "Новости"))
    (articles-page request-get-plist)))

;;список обзоры
(restas:define-route article-review-route ("/articles/reviews" :requirement #'test-article-get-parameters)
  (let ((request-get-plist (request-get-plist)))
    (if (null (getf request-get-plist :tags))
        (setf (getf request-get-plist :tags) "Обзоры"))
    (articles-page request-get-plist)))

;;конкретная статья
(restas:define-route article/-key-route ("/articles/:key" :requirement #'test-route-article-object)
  (gethash (caddr (request-list)) *storage-articles*))

;; 404

(restas:define-route not-found-route-404 ("/404.html")
  (restas:abort-route-handler
   (babel:string-to-octets
    (default-page
        (static:main (list :menu (menu "") :subcontent (error-404:content))))
    :encoding :utf-8)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))

;;необходимо отдавать 404 ошибку для несуществеющих страниц
(restas:define-route not-found-route ("*any")
  (restas:abort-route-handler
   (babel:string-to-octets
    (default-page
        (static:main (list :menu (menu "") :subcontent (error-404:content))))
    :encoding :utf-8)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))

(restas:define-route request-route ("/request")
 "<div class=\"xsnazzy\" style=\"width:200px;\"  >
 <b class=\"ytop\"><b class=\"yb1\"></b><b class=\"yb2 color_a\">
 </b><b class=\"yb3 color_a\"></b><b class=\"yb4 color_a\"></b></b>
 <div class=\"yboxcontent\"><div  style=\"float: right;\">
 <img alt=\"закрыть\" src=\"http://img.sotmarket.ru/des/close.gif\"></div><p>Ваш телефон:<input type=\"text\" id=\"nrtelefon\"><br>
  <input type=\"button\" value=\"Сохранить\"><br>Пример: 7-915-123-45-67<br>Наш менеджер с Вами свяжется и примет заказ. Желательно указывать мобильный телефон.</div>
↵<b class=\"ybottom\"><b class=\"yb4\"></b><b class=\"yb3\"></b>
↵<b class=\"yb2\"></b><b class=\"yb1\"></b></b>
↵</div><br><p></p><p></p><div  class=\"\"></div>")
