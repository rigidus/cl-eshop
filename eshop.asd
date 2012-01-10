;;;; eshop.asd
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(defsystem eshop
  :version      "0.0.2"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "GPLv3"
  :description  "site http://320-8080.ru"
  :depends-on (#:restas #:cl-json #:arnesi #:closure-template #:log5)
  :serial       t
  :components
    ((:file "defmodule")
     (:file "time")
     (:file "lib")
     (:file "err")
     (:file "log")
     (:file "classes")
     (:file "new-classes")
     ;; ((:module "src"
     ;;          :components
     ;;          (
     ;;           ;; (:file "serializers" :depends-on ("classes"))
     ;;           (:file "servo" :depends-on ("classes"))
     ;;           (:file "trans" :depends-on ("servo"))
     ;;           (:file "routes" :depends-on ("trans"))
     ;;           (:file "render" :depends-on ("routes"))
     ;;           (:file "cart" :depends-on ("render"))
     ;;           (:file "generics" :depends-on ("cart"))
     ;;           (:file "search" :depends-on ("generics"))
     ;;           (:file "xls" :depends-on ("search"))  ;;необходима xls2csv | sudo apt-get install catdoc
     ;;           (:file "yml" :depends-on ("xls"))
     ;;           (:file "articles" :depends-on ("yml"))
     ;;           (:file "wolfor-stuff" :depends-on ("articles"))
     ;;           (:file "report" :depends-on ("wolfor-stuff"))
     ;;           (:file "sklonenie" :depends-on ("report"))
     ;;           (:file "newcart" :depends-on ("sklonenie"))
     ;;           (:file "main-page" :depends-on ("newcart"))
     ;;           (:file "sitemap" :depends-on ("main-page"))
     ;;           (:file "rename" :depends-on ("sitemap"))
     ;;           (:file "catalog" :depends-on ("rename"))
     ;;           (:file "prerender" :depends-on ("catalog"))
     ;;           (:file "storage" :depends-on ("prerender"))
     ;;           (:file "filters" :depends-on ("storage"))
     ;;           (:file "oneclickcart" :depends-on ("filters"))
     ;;           (:file "images" :depends-on ("oneclickcart")) ;; imagemagic
     ;;           (:file "spike" :depends-on ("images"))
     ;;           (:file "static-pages" :depends-on ("spike"))
     ;;           (:file "list-filters" :depends-on ("static-pages"))
     ;;           (:file "object-fields" :depends-on ("list-filters"))
     ;;           (:file "new-classes" :depends-on ("storage"))
     ;;           (:file "admin-gateway" :depends-on ("new-classes"))
     ;;           (:file "gateway" :depends-on ("admin-gateway"))
     ;;           (:file "email" :depends-on ("gateway"))
     ;;           )))
     ))
