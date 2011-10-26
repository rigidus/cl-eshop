(in-package #:eshop)
(print "ESHOP config")

;; PATH
(defparameter *path-to-dropbox* (format nil "~aDropbox" (user-homedir-pathname)))
(export '*path-to-dropbox*)
(defparameter *path-to-articles* (format nil "~aDropbox/content/articles" (user-homedir-pathname)))
(export '*path-to-articles*)
(defparameter *path-to-static-pages* (format nil "~aDropbox/content/static-pages" (user-homedir-pathname)))
(export '*path-to-static-pages*)
(defparameter *path-to-tpls* (format nil "~aDropbox/httpls/release" (user-homedir-pathname)))
(export '*path-to-tpls*)
(defparameter *path-to-bkps* (format nil "~aDropbox/htbkps" (user-homedir-pathname)))
(export '*path-to-bkps*)
(defparameter *path-to-conf* (format nil "~aDropbox/htconf" (user-homedir-pathname)))
(export '*path-to-conf*)
(defparameter *path-to-pics* (format nil "~ahtpics" (user-homedir-pathname)))
(export '*path-to-pics*)
(defparameter *path-to-product-pics* (format nil "~ahtpics1" (user-homedir-pathname)))
(export '*path-to-product-pics*)
(defparameter *path-to-logs* (format nil "~aeshop-logs" (user-homedir-pathname)))
(export '*path-to-logs*)

;; ORDER
(defparameter *path-order-id-file* "order-id.txt")
(export '*path-order-id-file*)
;; sitemap
(defparameter *path-sitemap* "sitemap.xml")
(export '*path-sitemap*)

;; Список email для рассылки писем от ошибках выгрузки 1с
(defvar *conf.emails.gateway.warn* (list "Supplers@alpha-pc.com"
                                         "web_design@alpha-pc.com"
                                         "wolforus@gmail.com"
                                         "slamly@gmail.com"))

(defvar *conf.emails.xls.warn* (list "wolforus@gmail.com"
                                     "web_design@alpha-pc.com"))

;; Список email для заказов
(defvar *conf.emails.cart* (list "internetorder@alpha-pc.com"
                                 "shop@320-8080.ru"
                                 "zakaz320@yandex.ru"
                                 "slamly@gmail.com"
                                 "wolforus@gmail.com"))





(defun wlog (s)
  (format t "~&~a> ~a" (time.get-date-time) s))

(defun compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (wlog (format nil "~&compile-template: ~a" pathname))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '(
            "index.html"            "product.html"            "product-accessories.html"
            "product-reviews.html"  "product-simulars.html"   "product-others.html"
            "catalog.html"          "catalog-in.html"         "catalog-staff.html"
            "footer.html"
            "menu.html"             "banner.html"
            "notlogged.html"
            "cart-widget.html"      "cart.html"               "checkout.html"
            "admin.html"            "admin-table.soy"
            "search.html"
            "agent.html"            "update.html"
            "header.html"           "static.html"
            ;; "levashovsky.html"
            "yml.html"                "fullfilter.html"
            "sendmail.html"
            "404.html"
            "articles.soy"         "sitemap.html"            "newcart.soy"
            "oneclickcart.soy"
             "buttons.soy"
            "main-page.soy"
            "new-catalog.soy"
            "class_forms.soy"
            "admin.soy"
            )))

(print "Compiling all templates")
(compile-templates)
(print "Compiling all templates finish")
