(require 'asdf)
(print "LOAD LIBS:")
(defun cl-eshop-path (&rest filenames)
  (pathname (format nil "~{~a~^/~}" (append (list (user-homedir-pathname) "cl-restas-eshop") filenames))))

(defun cl-eshop-push-asdf (name)
  (push (truename (cl-eshop-path "libs" name)) asdf:*central-registry*))

(defun cl-eshop-load-asdf (name)
  (asdf:oos 'asdf:load-op (intern (string-upcase (format nil "~a" name)))))

(defparameter cl-eshop-libs
  (list
   "slime-archimag" ;; SWANK
   "alexandria"     ;; hunch
   "bordeaux-threads" ;; hunch
   "usocket-0.5.2" ;; hunch
   "md5-1.8.5"     ;; hunch
   "rfc2388" ;; hunch
   "flexi-streams-1.0.7" ;; hunch
   "cl-base64-3.3.3" ;; hunch
   "cl-fad-0.6.4" ;; hunch
   "trivial-garbage_0.19" ;; hunch
   "cl-who-0.11.1" ;; hunch
   "cl-ppcre-2.0.3" ;; hunch
   "chunga-1.1.1" ;; hunch
   "trivial-gray-streams-2008-11-02" ;;hunch
   "trivial-backtrace" ;; hunch
   "cffi" ;; hunch
   "trivial-features_0.6" ;; hunch
   "babel_0.3.0"   ;; hunch
   "cl-plus-ssl"   ;; hunch
   "hunchentoot"   ;; HUNCH
   "closer-mop" ;;restas
   "data-sift" ;;restas
   "cl-puri-1.5.5" ;;restas
   "parse-number-1.2" ;;restas
   "cl-routes-0.2.5" ;;restas
   "split-sequence-1.0" ;;restas
   "iterate-1.4.3" ;;restas
   "restas" ;; RESTAS
   "cl-closure-template" ;; шаблонизатор
   "cl-json_0.4.1" ;; JSON сериализатор
   "arnesi_dev-20080427" ;; parse-float
   "parenscript" ;;closure-template
   "named-readtables-0.9" ;;closure-template
   "anaphora-0.9.4" ;; closure-template | macro collection from Hell http://www.cliki.net/Anaphora
   "esrap" ;; closure-template | packrat parser http://nikodemus.github.com/esrap/
   "log5" ;; логирование | logging framework http://common-lisp.net/project/log5/
   ;; "cl-store" ;; сохранение данных | Serialization Package http://common-lisp.net/project/cl-store/
   ))

(defparameter cl-eshop-modules
  (list
   "restas"
   "cl-json"
   "arnesi"
   "closure-template"
   "log5"
   ;; "cl-store"
   ))

;; регистрация дерикторий библиотек
(mapcar #'cl-eshop-push-asdf cl-eshop-libs)
;; загрузка SWANK
(asdf:oos 'asdf:load-op :swank)
;; загрузка модулей
(mapcar #'cl-eshop-load-asdf cl-eshop-modules)


(print "restas:define-module CL-ESHOP")
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
           :plist-representation
           :*path-to-bkps*
           :*path-to-pics*
           :*path-to-conf*
           :*path-to-tpls*))

(defparameter cl-eshop-lisp-files
  (list
   ;; "start.lisp"
   "eshop-config.lisp"
   "errors.lisp"
   "spike.lisp"
   "classes.lisp"
   "serializers.lisp"
   "servo.lisp"
   "trans.lisp"
   "routes.lisp"
   "render.lisp"
   "cart.lisp"
   "generics.lisp"
   "gateway.lisp"
   "search.lisp"
   "xls.lisp" ;;необходима xls2csv |  sudo apt-get install catdoc
   "yml.lisp"
   "articles.lisp"
   "report.lisp"
   "sklonenie.lisp"
   "newcart.lisp"
   "main-page.lisp"
   "sitemap.lisp"
   "wolfor-stuff.lisp"
   "report.lisp"
   "rename.lisp"
   "catalog.lisp"
   "prerender.lisp"
   "filters.lisp"
   "oneclickcart.lisp"
   "images.lisp"))


;;закрузка файлов
(mapcar #'(lambda (filename) (load (cl-eshop-path filename)))
        cl-eshop-lisp-files)

(print "Restoring data from files")
(eshop::restore-skls-from-files)
(eshop::restore-articles-from-files)
(eshop::main-page-restore)
(eshop::restore-from-files)

