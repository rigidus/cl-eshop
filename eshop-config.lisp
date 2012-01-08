(in-package #:eshop)

(print "ESHOP config")

;; (let ((path '(:RELATIVE "repo/cl-eshop")))
;;   (setf asdf:*central-registry*
;;         (remove-duplicates (append asdf:*central-registry*
;;                                    (list (merge-pathnames
;;                                           (make-pathname :directory path)
;;                                           (user-homedir-pathname))))
;;                            :test #'equal)))

(defparameter *basedir*
  ;; (asdf:component-pathname (asdf:find-system '#:rigidus)))
  #p"/home/rigidus/repo/cl-eshop/")

(defun path (relative)
  (merge-pathnames relative *basedir*))


;; PATH
(defparameter *path-to-tpls* (format nil "~arepo/cl-eshop/tpl" (user-homedir-pathname)))
(defparameter *path-to-bkps* (format nil "~arepo/cl-eshop/htbkps" (user-homedir-pathname)))
(defparameter *path-to-conf* (format nil "~aDropbox/htconf" (user-homedir-pathname)))
(defparameter *path-to-pics* (format nil "~ahtpics" (user-homedir-pathname)))


(defun compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("index.html"            "product.html"            "product-accessories.html"
            "product-reviews.html"  "product-simulars.html"   "product-others.html"
            "catalog.html"          "catalog-in.html"         "catalog-staff.html"
            #|"login.html"            "notebook_b.html"         "notebook_d.html"
            "register.html"       |#"dayly.html"              "best.html"
            "hit.html"              "new.html"                "post.html"
            "plus.html"             "footer.html"#|           "subscribe.html"
            |#"menu.html"             "banner.html"             "olist.html"
            "lastreview.html"       "notlogged.html"        #|"logged.html"|#
            "cart-widget.html"      "cart.html"               "checkout.html"
            #|"admin.html"            "article.html"            "search.html"
            "agent.html"            "update.html"             "outload.html"
            |#"header.html"           "static.html"#|
            "delivery.html"         "about.html"
            "faq.html"              "kakdobratsja.html"       "kaksvjazatsja.html"
            "levashovsky.html"      "partners.html"           "payment.html"
            "servicecenter.html"    "otzyvy.html"
            "pricesc.html"          "warrantyservice.html"    "warranty.html"
            "moneyback.html"      |#"yml.html"                "fullfilter.html"#|
            "news1.html"            "news2.html"              "vacancy.html"
            "news3.html"            "news4.html"              "bonus.html"
            "news5.html"            "news6.html"              "corporate.html"
            "dillers.html"        |#"sendmail.html"           "404.html"
            )))


(print "Compiling templates")
(compile-templates)

;; (mapcar #'(lambda (fname)
;;             (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
;;               (closure-template:compile-template :common-lisp-backend pathname)))
;;         '("fullfilter.html" "product-others.html"))

