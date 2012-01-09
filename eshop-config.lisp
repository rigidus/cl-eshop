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
;; commit to sync https://github.com/wolfor/cl-restas-eshop/commit/e6c611291d4fce5f6f6ebd41c7426b8bf1d87566
;; удаление шаблонов из репозитория
;; +(defparameter *path-to-tpls* (format nil "~aDropbox/httpls/tosha" (user-homedir-pathname)))

;; (defparameter *path-to-dropbox* (format nil "~aDropbox" (user-homedir-pathname)))
;; (export '*path-to-dropbox*)
;; (defparameter *path-to-articles* (format nil "~aDropbox/httpls/release/articles" (user-homedir-pathname)))
;; (export '*path-to-articles*)


(defparameter *path-to-bkps* (format nil "~arepo/cl-eshop/htbkps" (user-homedir-pathname)))
(defparameter *path-to-conf* (format nil "~aDropbox/htconf" (user-homedir-pathname)))
(defparameter *path-to-pics* (format nil "~ahtpics" (user-homedir-pathname)))



(defparameter *path-order-id-file* "order-id.txt")
(export '*path-order-id-file*)


(defun compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (format t "~&closure-template:compile-template: ~a" fname)
                (closure-template:compile-template :common-lisp-backend pathname)))
          '(
            ;; "post.html"
            "index.html" "product.html" "product-accessories.html"
            "product-reviews.html" "product-simulars.html" "product-others.html"
            "catalog.html" "catalog-in.html" "catalog-staff.html"
            ;; "login.html"
            ;; "notebook_b.html"
            ;; "notebook_d.html"
            ;; "register.html"
            ;; "dayly.html"
            ;; "best.html"
            ;; "hit.html"
            ;; "new.html"
            ;; "plus.html"
            "footer.html"
            ;; "subscribe.html"
            "menu.html" "banner.html"
            ;; "olist.html"
            ;; "lastreview.html"
            "notlogged.html"
            ;; "logged.html"
            "cart-widget.html" "cart.html" "checkout.html"
            "admin.html"
            ;; "article.html"
            "search.html"
            "agent.html" "update.html"
            ;; "outload.html"
            "header.html" "static.html" "burunduk.html"
            "delivery.html" "about.html" "suslik.html"
            "faq.html" "kakdobratsja.html" "kaksvjazatsja.html"
            "levashovsky.html" "partners.html" "payment.html"
            "servicecenter.html" "otzyvy.html" "listservice.html"
            "pricesc.html" "warrantyservice.html" "warranty.html"
            "moneyback.html" "yml.html" "fullfilter.html"
            ;; "news1.html"
            ;; "news2.html"
            "vacancy.html"
            ;; "news3.html"
            ;; "news4.html"
            "bonus.html"
            ;; "news5.html"
            ;; "news6.html"
            "corporate.html"
            "dilers.html" "sendmail.html"
            "404.html"
            "articles.soy" "sitemap.html" "newcart.soy"
            "god_kills_a_kitten.soy"
            "oneclickcart.soy"
            "buttons.soy"
            "main-page.soy"
            "new-catalog.soy"
            )))




(print "Compiling templates")
(compile-templates)

;; (mapcar #'(lambda (fname)
;;             (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
;;               (closure-template:compile-template :common-lisp-backend pathname)))
;;         '("fullfilter.html" "product-others.html"))

