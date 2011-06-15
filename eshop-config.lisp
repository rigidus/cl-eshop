(in-package #:eshop)
(print "ESHOP config")

;; PATH
(defparameter *path-to-dropbox* (format nil "~aDropbox" (user-homedir-pathname)))
(export '*path-to-dropbox*)
(defparameter *path-to-articles* (format nil "~aDropbox/httpls/release/articles" (user-homedir-pathname)))
(export '*path-to-articles*)
(defparameter *path-to-tpls* (format nil "~aDropbox/httpls/release" (user-homedir-pathname)))
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
            "header.html"           "static.html"             "burunduk.html"
            "delivery.html"         "about.html"              "suslik.html"
            "faq.html"              "kakdobratsja.html"       "kaksvjazatsja.html"
            "levashovsky.html"      "partners.html"           "payment.html"
            "servicecenter.html"    "otzyvy.html"             "listservice.html"
            "pricesc.html"          "warrantyservice.html"    "warranty.html"
            "moneyback.html"        "yml.html"                "fullfilter.html"
            "news1.html"            "news2.html"              "vacancy.html"
            "news3.html"            "news4.html"              "bonus.html"
            "news5.html"            "news6.html"              "corporate.html"
            "dilers.html"           "sendmail.html"           "404.html"
            "articles.soy"         "sitemap.html"            "newcart.soy"
            )))

(print "Compiling all templates")
(compile-templates)
(print "Compiling all templates finish")
