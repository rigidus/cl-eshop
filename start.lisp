
(print "Start to load CL-ESHOP")

(require 'asdf)

(asdf:oos 'asdf:load-op '#:quicklisp)

(ql:quickload '("alexandria"
                "cl-fad"
                "anaphora"
                "split-sequence"
                "babel"
                "cl-json"
                "cl-utilities"
                "postmodern"
                "cl-store"
                "ironclad"
                "uffi"
                "mel-base"
                "cl-base64"
                "arnesi"
                "drakma"
                "closure-template"
                "hunchentoot"
                "restas"
                "restas-directory-publisher"
               ))

;; (setf ppcre:*use-bmh-matchers* nil)

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



;; (setf hunchentoot:*hunchentoot-default-external-format* (flexi-streams:make-external-format :utf-8 :eol-style :lf))
;; (setf hunchentoot:*handle-http-errors-p* nil)

(print "Restas start")

(restas:start '#:eshop :port 4243)
(restas:debug-mode-on)


(restas:start '#:eshop :port 4244)
;; (restas:debug-mode-on)
;; (restas:debug-mode-off)
;; (setf hunchentoot:*catch-errors-p* t)
;; (setf hunchentoot:*log-lisp-errors-p* nil)


;; (mapcar #'(lambda (fname)
;;             (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
;;               (closure-template:compile-template :common-lisp-backend pathname)))
;;         '("fullfilter.html" "product-others.html"))



;; (setq swank:*log-events* t)
;; (setq swank:*log-output* (open (format nil "~adropbox.lisp" (user-homedir-pathname))
;;                                :direction :output
;;                                :if-exists :append
;;                                :if-does-not-exist :create))
