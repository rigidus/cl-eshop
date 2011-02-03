;;;; spike.lisp
;;;;
;;;; This file is part of the eshop project,
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(defpackage #:product
  (:use #:cl
        #:cl-user
        #:json
        #:split-sequence)
  (:export
   ;; #:product
   ;; #:articul
   #:parent
   ;; #:name
   ;; #:realname
   #:price
   ;; #:siteprice
   ;; #:ekkprice
   #:active
   ;; #:newbie
   ;; #:sale
   ;; #:descr
   ;; #:shortdescr
   ;; #:count-transit
   ;; #:count-total
   #:options
   ;; #:plist-representation
   ;; ;; Отображение
   ;; #:get-pics
   ;; #:show
   ;; #:view
   ;; #:serialize
   ;; #:unserialize
   ))

(in-package #:product)

(defmethod product:price (object)
  (eshop::price object))

(defmethod product:parent (object)
  (eshop::parent object))

(defmethod product:active (object)
  (eshop::active object))

(defmethod product:options (object)
  (make-instance 'optlist :optlist (eshop::optgroups object)))




(defpackage #:optgroup
  (:use #:cl
        #:cl-user
        #:json)
  (:export
   ;; #:optgroup
   ;; #:show
   ;; #:serialize
   #:name
   #:options
   ;; #:unserialize
   ))

(in-package #:optgroup)

(defun optgroup:options (object)
  (eshop::options object))

(defun optgroup:name (object)
  (eshop::name object))



(defpackage #:option
  (:use #:cl
        #:cl-user
        #:json)
  (:export
   ;; #:option
   ;; #:show
   #:name
   #:value
   ;; #:optype
   ;; #:boolflag
   ;; #:serialize
   ;; #:unserialize
   ))


(in-package #:option)

(defun option:name (object)
  (eshop::name object))

(defun option:value (object)
  (eshop::value object))



(defpackage #:optlist
  (:use #:cl
        #:cl-user)
  (:export #:optlist))

(in-package #:optlist)


(defclass OPTLIST:OPTLIST ()
  ((optlist :initarg :optlist :initform nil :accessor get-optlist)))

(defun optlist:optlist (object)
  (eshop::optgroups object))
