(in-package #:cl-user)

(asdf:operate 'asdf:load-op '#:alexandria)
(asdf:operate 'asdf:load-op '#:hunchentoot)
(setf ppcre:*use-bmh-matchers* nil)
(asdf:operate 'asdf:load-op '#:clsql)
(asdf:operate 'asdf:load-op '#:closure-template)
(asdf:operate 'asdf:load-op '#:split-sequence)
(asdf:operate 'asdf:load-op '#:babel)
(asdf:operate 'asdf:load-op '#:cl-json)
(asdf:operate 'asdf:load-op '#:postmodern)
(asdf:operate 'asdf:load-op '#:cl-store)
(asdf:operate 'asdf:load-op '#:cl-ppcre)
(asdf:operate 'asdf:load-op '#:ironclad)
(asdf:operate 'asdf:load-op '#:html-entities)
(asdf:operate 'asdf:load-op '#:uffi)
(asdf:operate 'asdf:load-op '#:mel-base)
(asdf:operate 'asdf:load-op '#:cl-base64)
(asdf:operate 'asdf:load-op '#:clon)
(asdf:operate 'asdf:load-op '#:arnesi)
(asdf:operate 'asdf:load-op '#:cl-fad)

;; SERVER
(defparameter *user* "webadmin")

;; LOCAL
;; (defparameter *user* "rigidus")

;; PATH
(defparameter *path-to-tpls* (format nil "/home/~a/Dropbox/httpls" *user*))
(defparameter *path-to-bkps* (format nil "/home/~a/Dropbox/htbkps" *user*))
;; (defparameter *path-to-bkps* (format nil "/home/~a/migra" *user*))
(defparameter *path-to-conf* (format nil "/home/~a/Dropbox/htconf" *user*))
(defparameter *path-to-pics* (format nil "/home/~a/Dropbox/htpics-big" *user*))


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
            "dillers.html"          "sendmail.html"
            )))

(compile-templates)

;; (mapcar #'(lambda (fname)
;;             (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
;;               (closure-template:compile-template :common-lisp-backend pathname)))
;;         '("sendmail.html" "checkout.html"))


(load "packages.lisp")


(load "my.lisp")
(load "option-class.lisp")
(load "optgroup-class.lisp")
(load "optlist-class.lisp")
(load "product-class.lisp")
(load "filter-class.lisp")
(load "group-class.lisp")
(load "xls.lisp")
(load "trans.lisp")
(load "service.lisp")
(load "cart.lisp")
(load "checkout.lisp")
(load "gateway.lisp")
(load "search.lisp")
(load "html.lisp")
(load "conditions.lisp")
(load "actions.lisp")
(load "rule-class.lisp")
(load "agent-class.lisp")
(load "parser.lisp")
(load "data.lisp")
(load "update.lisp")
(load "outload.lisp")
(load "admin.lisp")

(defvar *catch-errors-p* nil)

(defun err-on ()
  (setf *catch-errors-p* nil))
(defun err-off ()
  (setf *catch-errors-p* t))


(defclass debuggable-acceptor (hunchentoot:acceptor) ())

(defmethod hunchentoot:acceptor-request-dispatcher ((acceptor debuggable-acceptor))
  (if *catch-errors-p*
	  (call-next-method)
	  (let ((dispatcher (handler-bind ((error #'invoke-debugger))
						  (call-next-method))))
		(lambda (request)
		  (handler-bind ((error #'invoke-debugger))
			(funcall dispatcher request))))))



(defun dispatcher ()
  (let ((₤ (make-hash-table :test #'equal)))
    #'(lambda (×)
        (if (equal 'cons (type-of ×))
            (progn
              (setf (gethash (car ×) ₤) (cadr ×))
              ₤)
            (loop :for ¿ :being the hash-key :in ₤ :using (hash-value Ł) :do
               (when (eval ¿)
                 (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
                 (let ((¤ (return (funcall Ł))))
                   (babel:string-to-octets
                    (if (null ¤) (service:default-page × request-list) ¤) :encoding :utf-8))))))))


(defparameter *dispatcher* (dispatcher))
(export '*dispatcher*)

;; (funcall *dispatcher* `(t ,#'(lambda () (service:default-page  "Диспетчер временно в разобранном состоянии - ждите!"))))


(defun request-dispatcher (request)
  (funcall *dispatcher* request))

(defparameter *debuggable-acceptor* (make-instance 'debuggable-acceptor
                                                   :request-dispatcher 'request-dispatcher
                                                   :port 4242))

(hunchentoot:start *debuggable-acceptor*)
(setf hunchentoot:*handle-http-errors-p* nil)
(setf hunchentoot:*hunchentoot-default-external-format* (flexi-streams:make-external-format :utf-8 :eol-style :lf))

(setq swank:*log-events* t)
(setq swank:*log-output* (open (format nil "/home/~a/dropbox.lisp" *user*)
                               :direction :output
                               :if-exists :append
                               :if-does-not-exist :create))
