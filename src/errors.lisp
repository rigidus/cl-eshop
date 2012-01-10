;;;; errors.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop)


(define-condition wrong-product-slot-value (error)
  ((text      :initarg :text     :accessor text)
   (value     :initarg :value    :accessor value)
   (product   :initarg :product  :accessor product))
  (:report (lambda (condition stream)
             (format stream "Incorrect value of product slot ~a ~%[value: '~a']~%[type '~a']"
                     (text condition)
                     (value condition)
                     (type-of (value condition))))))


(define-condition wrong-type-of-slot (error) ())


(define-condition wrong-product-file (error)
  ((filepath       :initarg  :filepath       :accessor filepath)
   (in-condition   :initarg  :in-condition   :accessor in-condition))
  (:report (lambda (condition stream)
             (format stream "Unable unserialize product:  ~a ~% Reason: ~a"
                     (filepath condition)
                     (format nil "Incorrect value of product slot ~a ~%   [value: '~a']~%   [type '~a']"
                             (text (in-condition condition))
                             (value (in-condition condition))
                             (type-of (value (in-condition condition))))))))


(defun read-new-integer-value ()
  (format t "Enter a new integer value: ")
  (multiple-value-list (eval (read))))


