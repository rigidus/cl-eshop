;;;; routes.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(in-package #:eshop)

;; (defun clear ()
;;   (loop :for var :being :the :symbols :in :eshop.impl.routes :do (unintern var))
;;   (restas:reconnect-all-routes))

;; (clear)

;; (setf swank::*connections* nil)


(restas:define-route request-static-route-img ("/img/*")
  (let ((full-uri (format nil "~a" (restas:request-full-uri))))
    (pathname (concatenate 'string *path-to-dropbox* "/htimgs/" (subseq full-uri (search "/img/" full-uri))))))

(restas:define-route request-static-route-pic ("/pic/*")
  (let ((full-uri (format nil "~a" (restas:request-full-uri))))
    (pathname (concatenate 'string  *path-to-pics* "/" (subseq full-uri (+ 5 (search "/pic/" full-uri)))))))

(restas:define-route request-static-route-css ("/css/*")
  (let ((full-uri (format nil "~a" (restas:request-full-uri))))
    (pathname (concatenate 'string *path-to-dropbox* "/htimgs/" (subseq full-uri (search "/css/" full-uri))))))

(restas:define-route request-static-route-js ("/js/*")
  (let ((full-uri (format nil "~a" (restas:request-full-uri))))
    (pathname (concatenate 'string *path-to-dropbox* "/htimgs/" (subseq full-uri (search "/js/" full-uri))))))

(restas:define-route request-route-static-favicon ("/favicon.ico")
  (pathname (concatenate 'string  *path-to-dropbox* "/htimgs/img/favicon.ico")))

(restas:define-route request-route-static-robots ("/robots.txt")
  (pathname (concatenate 'string *path-to-conf* "/robots.txt")))

(restas:define-route request-route-static-yml ("/yml.xml")
  (pathname (concatenate 'string *path-to-conf* "/yml.xml")))

(restas:define-route request-route-static-sitemap ("/sitemap.xml")
  (pathname (concatenate 'string *path-to-conf* "/sitemap.xml")))

(restas:define-route request-route-static-sitemap1 ("/sitemap1.xml")
  (pathname (concatenate 'string *path-to-conf* "/sitemap1.xml")))



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
  t) ;; (null (request-get-plist)))

(restas:define-route main-route ("/" :requirement #'test-get-parameters)
  (main-page-show (request-str)))



;; CATALOG

(restas:define-route catalog-page-route ("/catalog")
  (default-page (catalog-entity)
      :keywords "Купить компьютер и другую технику вы можете в Цифрах. Цифровая техника в Интернет-магазине 320-8080.ru"
      :description "каталог, компьютеры, купить компьютер, компьютерная техника, Петербург, Спб, Питер, Санкт-Петербург, продажа компьютеров, магазин компьютерной техники, магазин компьютеров, интернет магазин компьютеров, интернет магазин компьютерной техники, продажа компьютерной техники, магазин цифровой техники, цифровая техника, Цифры, 320-8080"
      :title "Каталог интернет-магазина: купить компьютер, цифровую технику, комплектующие в Санкт-Петербурге"))


(restas:define-route sitemap-page-route ("/sitemap")
  (default-page (sitemap-page)
      :keywords "Купить компьютер и другую технику вы можете в Цифрах. Цифровая техника в Интернет-магазине 320-8080.ru"
      :description "каталог, компьютеры, купить компьютер, компьютерная техника, Петербург, Спб, Питер, Санкт-Петербург, продажа компьютеров, магазин компьютерной техники, магазин компьютеров, интернет магазин компьютеров, интернет магазин компьютерной техники, продажа компьютерной техники, магазин цифровой техники, цифровая техника, Цифры, 320-8080"
      :title "Каталог интернет-магазина: купить компьютер, цифровую технику, комплектующие в Санкт-Петербурге"))


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

;;список статей
(restas:define-route article-route ("/articles" :requirement #'test-article-get-parameters)
  (articles-page))

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
         (default-page (sitemap-page t)
             :keywords "Купить компьютер и другую технику вы можете в Цифрах. Цифровая техника в Интернет-магазине 320-8080.ru"
             :description "каталог, компьютеры, купить компьютер, компьютерная техника, Петербург, Спб, Питер, Санкт-Петербург, продажа компьютеров, магазин компьютерной техники, магазин компьютеров, интернет магазин компьютеров, интернет магазин компьютерной техники, продажа компьютерной техники, магазин цифровой техники, цифровая техника, Цифры, 320-8080"
             :title "Каталог интернет-магазина: купить компьютер, цифровую технику, комплектующие в Санкт-Петербурге")
         :encoding :utf-8)
           :return-code hunchentoot:+http-not-found+
              :content-type "text/html"))

(restas:define-route request-route ("/request")
  (oneclickcart-page (request-get-plist)))


;; submodules

(restas:mount-submodule -css- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("css"))
  (restas.directory-publisher:*directory* (path "css/")))

(restas:mount-submodule -img- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("img"))
  (restas.directory-publisher:*directory* (path "img/")))

(restas:mount-submodule -js- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("js"))
  (restas.directory-publisher:*directory* (path "js/")))
