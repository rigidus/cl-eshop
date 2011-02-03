;;;; filter.lisp
;;;;
;;;; This file is part of the eshop project,
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(in-package #:filter)


(defclass filter ()
  ((key               :initarg :key            :initform nil       :accessor key)
   (parent            :initarg :parent         :initform nil       :accessor parent)
   (name              :initarg :name           :initform nil       :accessor name)
   (func              :initarg :func           :initform nil       :accessor func)
  ))


(defmethod show ((object filter) request-get-plist)
  (list :name (name object)
        :breadcrumbs (catalog:breadcrumbs (service:breadcrumbs object))
        :menu (service:menu object)
        :rightblocks (list (catalog:rightblock1)
                           (catalog:rightblock2)
                           (catalog:rightblock3))
        :tradehits (catalog:tradehits (list :reviews (list group::*trade-hits-1*
                                                           group::*trade-hits-2*
                                                           group::*trade-hits-1*
                                                           group::*trade-hits-2*)))
        :subcontent (catalog:centerproduct (list
                                            :producers (group:make-vendor-filter (parent object))
                                            :accessories (catalog:accessories)
                                            :products (loop
                                                         :for product
                                                         :in (service:paginator request-get-plist
                                                              (remove-if-not (filter:func object)
                                                                             (remove-if-not #'(lambda (product)
                                                                                                (product:active product))
                                                                                            (group:get-recursive-products
                                                                                             (parent object)))))
                                                         :collect
                                                         (product:view product))))))


(defun unserialize (pathname)
  (let* ((file-content (read-file-into-string pathname))
         (raw (decode-json-from-string file-content))
         (key (pathname-name pathname))
         (parent (gethash (nth 1 (reverse (split-sequence #\/ pathname))) trans:*group*))
         (new (make-instance 'filter
                             :key key
                             :parent parent
                             :name (cdr (assoc :name raw))
                             :func (eval (read-from-string (cdr (assoc :func raw))))
                             )))
    (when (equal 'group:group (type-of parent))
      (push new (group:filters parent)))
    (setf (gethash key trans:*filter*) new)
    key
    ))


(defmethod plist-representation ((object filter) &rest fields)
  (let ((result))
    (loop :for item :in fields do
       (let ((method (intern (symbol-name item) 'filter)))
         (push item result)
         (push (funcall method object) result)))
    (reverse result)))
