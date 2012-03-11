(asdf:defsystem #:eshop
  :version      "11.03.2011"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "GPLv3"
  :description  "eshop"
  :depends-on   (#:cl-ppcre
                 #:restas-directory-publisher
                 #:closure-template)
  :serial       t
  :components   ((:module "tpl"
                          :components ((:static-file "templates.htm")))
                 (:file "defmodule")
                 (:file "render")
                 ;; (:file "routes")
                 ;; (:file "init")
                 ;; (:module "daemon"
                 ;;          :components ((:static-file "daemon.conf")
                 ;;                       (:static-file "daemon.lisp")
                 ;;                       (:static-file "daemon.sh")))
                 ))
