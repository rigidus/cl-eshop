(restas:define-module #:eshop
    (:use #:cl #:iter #:alexandria))

(in-package #:eshop)

(let ((path '(:RELATIVE "repo/eshop")))
  (setf asdf:*central-registry*
        (remove-duplicates (append asdf:*central-registry*
                                   (list (merge-pathnames
                                          (make-pathname :directory path)
                                          (user-homedir-pathname))))
                           :test #'equal)))

(defparameter *basedir*
  (asdf:component-pathname (asdf:find-system '#:eshop)))

;; (defparameter *host* "")

(defun path (relative)
  (merge-pathnames relative *basedir*))

;; (closure-template:compile-template :common-lisp-backend (path "templates.htm"))
