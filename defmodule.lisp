;;;; defmodule.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(restas:define-module #:eshop
    (:use #:cl
          #:closure-template
          #:anaphora
          #:split-sequence
          #:cl-ppcre
          #:json
          #:cl-fad)
  (:import-from #:arnesi      #:parse-float)
  (:import-from #:alexandria  #:read-file-into-string)
  (:export #:compile-templates
		   #:*storage*
           #:name
           #:unserialize
           #:plist-representation
           #:*path-to-bkps*
           #:*path-to-pics*
           #:*path-to-conf*
           #:*path-to-tpls*))

(defpackage #:wolfor-stuff
  (:use #:cl
        #:eshop))

(defpackage #:eshop-test
  (:use #:cl
        #:eshop
        #:wolfor-stuff))


(in-package #:eshop)


(defconstant +system-name+ :eshop)
(defconstant +dbg-username+ "rigidus")
