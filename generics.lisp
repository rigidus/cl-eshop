;;;; generics.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop)


(defgeneric plist-representation (object &rest fields)
  (:documentation ""))

(defmethod plist-representation (object &rest fields)
  (let ((result))
    (loop :for item :in fields do
       (let ((method (intern (symbol-name item) 'eshop)))
         (push item result)
         (push (funcall method object) result)))
    (reverse result)))


