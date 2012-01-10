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
     (:file "oneclickcart")
     (:file "images")
     (:file "static-pages")
     (:file "list-filters")
     (:file "object-fields")
     (:file "admin-gateway")
     (:file "gateway")
     (:file "email")))
