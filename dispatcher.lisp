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

(restas:define-module #:cl-eshop
    (:use :cl
          :closure-template
          :anaphora
          :split-sequence
          :cl-ppcre
          :alexandria
          ))

(in-package #:cl-eshop)

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


(load "packages.lisp")
(load "service.lisp")


(defun dispatcher ()
  (let ((₤ (make-hash-table :test #'equal)))
    #'(lambda (×)
        (if (equal 'cons (type-of ×))
            (progn (setf (gethash (car ×) ₤) (cadr ×)) ₤)
            (let ((¤ (loop :for ¿ :being the hash-key :in ₤ :using (hash-value Ł) :do
                        (when (eval ¿) (return (funcall Ł))))))
              (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
              (when (null ¤)
                (restas:abort-route-handler
                 (babel:string-to-octets
                  (service:default-page
                      (static:main (list :menu (service:menu "") :subcontent (error-404:content))))
                  :encoding :utf-8)
                 :return-code hunchentoot:+http-not-found+
                 :content-type "text/html"))
              (babel:string-to-octets ¤ :encoding :utf-8))))))

(defparameter *dispatcher* (dispatcher))
(export '*dispatcher*)


(load "my.lisp")
(load "option-class.lisp")
(load "optgroup-class.lisp")
(load "optlist-class.lisp")
(load "product-class.lisp")
(load "filter-class.lisp")
(load "group-class.lisp")

(load "trans.lisp")

(load "cart.lisp")
(load "gateway.lisp")
(load "search.lisp")
(load "wolfor-stuff.lisp")


(funcall *dispatcher*
         `((string= "" (service:request-str))
           ,#'(lambda ()
                (service:default-page (root:content (list :menu (service:menu (service:request-str))
                                                          :dayly (root:dayly)
                                                          :banner (root:banner)
                                                          :olist (root:olist)
                                                          :lastreview (root:lastreview)
                                                          :best (root:best)
                                                          :hit (root:hit)
                                                          :new (root:new)
                                                          :post (root:post)
                                                          :plus (root:plus)))))))


;; catalog
(funcall *dispatcher*
         `((string= "/catalog" (service:request-str))
           ,#'(lambda ()
                (service:default-page (catalog:main (list :menu (service:menu "")))))))

;; static
(mapcar #'(lambda (∆)
            (funcall *dispatcher*
                     `((string= ,(concatenate 'string "/" ∆) (service:request-str))
                       ,#'service:static-page)))
        (list "delivery"         "about"             "faq"             "kakdobratsja"
              "kaksvjazatsja"    "levashovsky"       "partners"        "payment"
              "servicecenter"    "otzyvy"            "pricesc"         "warrantyservice"
              "warranty"         "moneyback"         "article"         "news1"
              "news2"            "news3"             "news4"           "news5"
              "news6"            "dillers"           "corporate"       "vacancy"
              "bonus"))


(setf hunchentoot:*hunchentoot-default-external-format* (flexi-streams:make-external-format :utf-8 :eol-style :lf))

(restas:define-route main ("/:get" :requirement (lambda () t))
  (funcall *dispatcher* hunchentoot:*request*))

(restas:define-route main/post ("/:get" :requirement (lambda () t)
                                        :method :post)
  (funcall *dispatcher* hunchentoot:*request*))


(restas:start '#:cl-eshop :port 4242)
(restas:debug-mode-on)


(setq swank:*log-events* t)
(setq swank:*log-output* (open (format nil "~adropbox.lisp" (user-homedir-pathname))
                               :direction :output
                               :if-exists :append
                               :if-does-not-exist :create))
