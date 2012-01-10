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
     (:file "serializers")
     (:file "servo")
     (:file "trans")
     (:file "routes")
     (:file "render")
     (:file "cart")
     (:file "generics")
     (:file "search")
     (:file "xls")
     (:file "yml")
     (:file "articles")
     (:file "wolfor-stuff")
     (:file "report")
     (:file "sklonenie")
     (:file "newcart")
     (:file "main-page")
     (:file "sitemap")
     (:file "rename")
     (:file "catalog")
     (:file "prerender")
     (:file "storage")
     (:file "filters")
     ;; ((:module "src"
     ;;          :components
     ;;          (
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
