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
         (filter (caddr request-list)))
    (and (not (null (gethash key *storage*)))
         (not (null (gethash filter *storage*))))))

(defun route-filter (filter)
  (gethash filter *storage*))

(restas:define-route filter/-route ("/:key/:filter/" :requirement #'test-route-filter)
  (route-filter filter))

(restas:define-route filter-route ("/:key/:filter" :requirement #'test-route-filter)
  (route-filter filter))


;; STORAGE OBJECT

(defun test-route-storage-object ()
  (not (null (gethash (cadr (request-list)) *storage*))))

(defun route-storage-object (key)
  (gethash key *storage*))

(restas:define-route storage-object-route  ("/:key" :requirement #'test-route-storage-object)
  (route-storage-object key))

(restas:define-route storage-object/-route  ("/:key/" :requirement #'test-route-storage-object)
  (route-storage-object key))


;; MAIN

(restas:define-route main-route ("/")
  (default-page (root:content (list :menu (menu (request-str))
                                            :dayly (root:dayly)
                                            :banner (root:banner)
                                            :olist (root:olist)
                                            :lastreview (root:lastreview)
                                            :best (root:best)
                                            :hit (root:hit)
                                            :new (root:new)
                                            :post (root:post)
                                            :plus (root:plus)))
      :KEYWORDS "компьютеры, купить компьютер, компьютерная техника, Петербург, Спб, Питер, Санкт-Петербург, продажа компьютеров, магазин компьютерной техники, магазин компьютеров, интернет магазин компьютеров, интернет магазин компьютерной техники, продажа компьютерной техники, магазин цифровой техники, цифровая техника, Цифры, 320-8080"
      :DESCRIPTION "Купить компьютер и другую технику вы можете в Цифрах. Цифровая техника в Интернет-магазине 320-8080.ru"
      :TITLE "Интернет-магазин: купить компьютер, цифровую технику, комплектующие в Санкт-Петербурге"))




;; CATALOG

(restas:define-route catalog-route ("/catalog")
  (default-page (catalog:main (list :menu (menu "")))))


;; STATIC

(defmacro static ()
  `(progn ,@(mapcar #'(lambda (x)
                        `(restas:define-route ,(intern (string-upcase x) *package*) (,x)
                           (static-page)))
                    (list "delivery"         "about"             "faq"             "kakdobratsja"
                          "kaksvjazatsja"    "levashovsky"       "partners"        "payment"
                          "servicecenter"    "otzyvy"            "pricesc"         "warrantyservice"
                          "warranty"         "moneyback"         "article"         "news1"
                          "news2"            "news3"             "news4"           "news5"
                          "news6"            "dillers"           "corporate"       "vacancy"
                          "bonus"            "burunduk"))))

(static)


;; CART & CHECKOUTS & THANKS

(restas:define-route cart-route ("/cart")
  (cart-page))

(restas:define-route cart/-route ("/cart")
  (cart-page))

(restas:define-route checkout0-route ("/checkout0")
  (checkout-page-0))

(restas:define-route checkout1-route ("/checkout1")
  (checkout-page-1))

(restas:define-route checkout2-route ("/checkout2")
  (checkout-page-2))

(restas:define-route checkout3-route ("/checkout3")
  (checkout-page-3))

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

;; 404

(restas:define-route not-found-route (":any")
  (restas:abort-route-handler
   (babel:string-to-octets
    (default-page
        (static:main (list :menu (menu "") :subcontent (error-404:content))))
    :encoding :utf-8)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))


