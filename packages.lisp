;;;; packages.lisp
;;;;
;;;; This file is part of the eshop project,
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(defpackage #:eshop
  (:use #:cl
        #:cl-user
        #:json
        #:split-sequence
        #:cl-fad)
  (:export :compile-templates
		   :*storage*
           :name
           :unserialize
           :plist-representation))


(defpackage #:wolfor-stuff
  (:use #:cl)
  (:export :range-filter
           :checkbox-filter
           :price-filter
           :radio-filter
           ))

(defpackage #:eshop-test
  (:use #:cl
        #:eshop
        #:wolfor-stuff))
