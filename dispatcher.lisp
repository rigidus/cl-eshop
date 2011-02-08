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
  (:import-from :alexandria :read-file-into-string)
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
            "header.html"           "static.html"
            "delivery.html"         "about.html"
            "faq.html"              "kakdobratsja.html"       "kaksvjazatsja.html"
            "levashovsky.html"      "partners.html"           "payment.html"
            "servicecenter.html"    "otzyvy.html"
            "pricesc.html"          "warrantyservice.html"    "warranty.html"
            "moneyback.html"        "yml.html"                "fullfilter.html"
            "news1.html"            "news2.html"              "vacancy.html"
            "news3.html"            "news4.html"              "bonus.html"
            "news5.html"            "news6.html"              "corporate.html"
            "dillers.html"          "sendmail.html"           "404.html"
            )))

(compile-templates)

;; (mapcar #'(lambda (fname)
;;             (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
;;               (closure-template:compile-template :common-lisp-backend pathname)))
;;         '("fullfilter.html" "product-others.html"))


(load "errors.lisp")
(load "classes.lisp")
(load "serializers.lisp")
(load "servo.lisp")
(load "spike.lisp")
(load "generics.lisp")

(load "trans.lisp")

(load "cart.lisp")
(load "gateway.lisp")
(load "xls.lisp")
(load "search.lisp")
(load "yml.lisp")

(load "wolfor-stuff.lisp")

(load "routes.lisp")
(load "render.lisp")


(setf hunchentoot:*hunchentoot-default-external-format* (flexi-streams:make-external-format :utf-8 :eol-style :lf))
(setf hunchentoot:*handle-http-errors-p* nil)



(restas:start '#:eshop :port 4243)
(restas:debug-mode-on)
(setf hunchentoot:*catch-errors-p* nil)

;; (setq swank:*log-events* t)
;; (setq swank:*log-output* (open (format nil "~adropbox.lisp" (user-homedir-pathname))
;;                                :direction :output
;;                                :if-exists :append
;;                                :if-does-not-exist :create))
