;;;; lib.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop)

(defun get-system-path ()
  (asdf:component-pathname (asdf:find-system +system-name+)))

(defun get-dir (path)
  (make-pathname :defaults path
                 :name nil
                 :type nil))

(defun get-core-path () sb-ext:*core-pathname*)

(defun get-resources-path (&optional (core-pathname (get-core-path)))
  (make-pathname :defaults (get-dir core-pathname)
                 :directory (append (pathname-directory core-pathname)
                                    (list +res-dirname+
                                          (string-downcase (princ-to-string +system-name+))))))

(defun map-components (comp fn)
  (unless (typep comp 'asdf:component)
    (setf comp (asdf:find-system comp)))
  (dolist (cmp (asdf:module-components comp))
    (funcall fn cmp)
    (when (typep cmp 'asdf:module)
      (map-components cmp fn))))

(defun copy-directory (path-from path-to)
  (asdf:run-shell-command
   (format nil "rm -rf ~A" (sb-ext:native-namestring path-to)))
  (asdf:run-shell-command
   (format nil
           "cp -R ~A ~A"
           (sb-ext:native-namestring path-from)
           (sb-ext:native-namestring path-to))))

(defun get-username (&aux (pid (sb-posix:getpid)))
  (sb-posix:passwd-name
   (sb-posix:getpwuid
    (sb-posix:stat-uid
     (sb-posix:stat (format nil "/proc/~A" pid))))))

(cond ((string= +dbg-username+ (get-username))
       (progn
         (defparameter *dbg* t)
         (defparameter *db-password* "root")))
      (t (error "unk-username/config")))

(defparameter *basedir* (get-system-path))

(defun path (relative)
  (merge-pathnames relative *basedir*))



;; PATH
;; (defparameter *path-to-dropbox* (format nil "~aDropbox" (user-homedir-pathname)))
;; (defparameter *path-to-articles* "/home/rigidus/repo/cl-eshop/httpls/articles")
;; (defparameter *path-to-static-pages* (format nil "~aDropbox/content/static-pages" (user-homedir-pathname)))
(defparameter *path-to-tpls* (path "httpls"))
;; (defparameter *path-to-bkps* (format nil "~aDropbox/htbkps" (user-homedir-pathname)))
;; (defparameter *path-to-conf* (format nil "~aDropbox/htconf" (user-homedir-pathname)))
;; (defparameter *path-to-pics* (format nil "~ahtpics" (user-homedir-pathname)))
;; (defparameter *path-to-product-pics* (format nil "~ahtpics1" (user-homedir-pathname)))
(defparameter *path-to-logs* (path "htlogs"))

;; ;; ;; ORDER
;; ;; (defparameter *path-order-id-file* "order-id.txt")
;; ;; (export '*path-order-id-file*)
;; ;; ;; sitemap
;; ;; (defparameter *path-sitemap* (format nil "~aDropbox/htconf" (user-homedir-pathname)))
;; ;; (export '*path-sitemap*)

;; ;; ;; Список email для рассылки писем от ошибках выгрузки 1с
;; ;; (defvar *conf.emails.gateway.warn* (list "Supplers@alpha-pc.com"
;; ;;                                          "web_design@alpha-pc.com"
;; ;;                                          "wolforus@gmail.com"
;; ;;                                          "slamly@gmail.com"))

;; ;; (defvar *conf.emails.xls.warn* (list "wolforus@gmail.com"
;; ;;                                      "web_design@alpha-pc.com"))

;; ;; ;; Список email для заказов
;; ;; (defvar *conf.emails.cart* (list "internetorder@alpha-pc.com"
;; ;;                                  "shop@320-8080.ru"
;; ;;                                  "zakaz320@yandex.ru"
;; ;;                                  "slamly@gmail.com"
;; ;;                                  "wolforus@gmail.com"))

(defun wlog (s)
  (format t "~&~a> ~a" (time.get-date-time) s))

(defun compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (wlog (format nil "~&compile-template: ~a" pathname))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '(
            "index.html"
            ;; "product.soy"
            ;; "product-accessories.html"
            ;; "product-reviews.html"
            ;; "product-simulars.html"
            ;; "product-others.html"
            ;; "catalog.html"
            "catalog-in.html"
            "catalog-staff.html"
            "footer.html"
            "menu.html"
            ;; "banner.html"
            "notlogged.html"
            "cart-widget.html"
            ;; "cart.html"
            ;; "checkout.html"
            "admin.html"
            ;; "admin-table.soy"
            ;; "search.html"
            ;; "agent.html"
            ;; "update.html"
            "header.html"
            "static.html"
            ;; ;; "levashovsky.html"
            "yml.html"
            "fullfilter.html"
            "sendmail.html"
            ;; "404.html"
            ;; "articles.soy"
            ;; "sitemap.soy"
            ;; "newcart.soy"
            ;; "oneclickcart.soy"
            ;; "buttons.soy"
            ;; "main-page.soy"
            ;; "new-catalog.soy"
            ;; "class_forms.soy"
            ;; "admin.soy"
            ;; "elka2012.soy"
            )))

(compile-templates)
