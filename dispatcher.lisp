;;;; dispatcher.lisp
;;;;
;;;; This file is part of the eshop project,
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(asdf:operate 'asdf:load-op '#:alexandria)
(asdf:operate 'asdf:load-op '#:anaphora)
(asdf:operate 'asdf:load-op '#:hunchentoot)
(setf ppcre:*use-bmh-matchers* nil)
(asdf:operate 'asdf:load-op '#:closure-template)
(asdf:operate 'asdf:load-op '#:split-sequence)
(asdf:operate 'asdf:load-op '#:babel)
(asdf:operate 'asdf:load-op '#:cl-json)
(asdf:operate 'asdf:load-op '#:postmodern)
(asdf:operate 'asdf:load-op '#:cl-store)
(asdf:operate 'asdf:load-op '#:ironclad)
(asdf:operate 'asdf:load-op '#:uffi)
(asdf:operate 'asdf:load-op '#:mel-base)
(asdf:operate 'asdf:load-op '#:cl-base64)
(asdf:operate 'asdf:load-op '#:arnesi)
(asdf:operate 'asdf:load-op '#:cl-fad)
(asdf:operate 'asdf:load-op '#:drakma)
(asdf:operate 'asdf:load-op '#:restas)


(defpackage #:wolfor-stuff
  (:use #:cl)
  (:export :range-filter
           :checkbox-filter
           :price-filter
           :radio-filter
           ))


(restas:define-module #:eshop
    (:use :cl
          :closure-template
          :anaphora
          :split-sequence
          :cl-ppcre
          :json
          :cl-fad)
  (:import-from :arnesi :parse-float)
  (:export :compile-templates
		   :*storage*
           :name
           :unserialize
           :plist-representation))


(in-package #:eshop)

;; PATH
(defparameter *path-to-tpls* (format nil "~aDropbox/httpls" (user-homedir-pathname)))
(export '*path-to-tpls*)
(defparameter *path-to-bkps* (format nil "~aDropbox/htbkps" (user-homedir-pathname)))
(export '*path-to-bkps*)
(defparameter *path-to-conf* (format nil "~aDropbox/htconf" (user-homedir-pathname)))
(export '*path-to-conf*)

(defparameter *path-to-pics* (format nil "~ahtpics" (user-homedir-pathname)))
(export '*path-to-pics*)


(defun compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("index.html"            "product.html"            "product-accessories.html"
            "product-reviews.html"  "product-simulars.html"   "product-others.html"
            "catalog.html"          "catalog-in.html"         "catalog-staff.html"
            "login.html"            "notebook_b.html"         "notebook_d.html"
            "register.html"         "dayly.html"              "best.html"
            "hit.html"              "new.html"                "post.html"
            "plus.html"             "footer.html"             "subscribe.html"
            "menu.html"             "banner.html"             "olist.html"
            "lastreview.html"       "notlogged.html"          "logged.html"
            "cart-widget.html"      "cart.html"               "checkout.html"
            "admin.html"            "article.html"            "search.html"
            "agent.html"            "update.html"             "outload.html"
            "header.html"           "fullfilter.html"         "static.html"
            "delivery.html"         "about.html"
            "faq.html"              "kakdobratsja.html"       "kaksvjazatsja.html"
            "levashovsky.html"      "partners.html"           "payment.html"
            "servicecenter.html"    "otzyvy.html"
            "pricesc.html"          "warrantyservice.html"    "warranty.html"
            "moneyback.html"        "yml.html"
            "news1.html"            "news2.html"              "vacancy.html"
            "news3.html"            "news4.html"              "bonus.html"
            "news5.html"            "news6.html"              "corporate.html"
            "dillers.html"          "sendmail.html"           "404.html"
            )))

(compile-templates)

;; (mapcar #'(lambda (fname)
;;             (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
;;               (closure-template:compile-template :common-lisp-backend pathname)))
;;         '("sendmail.html" "checkout.html"))



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
                                    :plus (root:plus)))))


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
                          "bonus"))))

(static)


;; CART & CHECKOUTS & THANKS

(restas:define-route cart-route ("/cart")
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

(restas:define-route gateway-route ("/gateway")
  (gateway-page))

(restas:define-route gateway/post-route ("/gateway" :method :post)
  (gateway-page))

(restas:define-route gateway/-route ("/gateway/")
  (gateway-page))

(restas:define-route gateway/post/-route ("/gateway/" :method :post)
  (gateway-page))


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




(setf hunchentoot:*hunchentoot-default-external-format* (flexi-streams:make-external-format :utf-8 :eol-style :lf))
(setf hunchentoot:*handle-http-errors-p* nil)


(restas:start '#:eshop :port 4243)
(restas:debug-mode-on)


(setq swank:*log-events* t)
(setq swank:*log-output* (open (format nil "~adropbox.lisp" (user-homedir-pathname))
                               :direction :output
                               :if-exists :append
                               :if-does-not-exist :create))
