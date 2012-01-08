
;; (in-package :product)

;; (defun price (object)
;;   (eshop:price object))

;; (defun parent (object)
;;   (eshop:parent object))

;; (defun active (object)
;;   (eshop:active object))

;; (defun options (object)
;;   (make-instance 'eshop:optlist :optlist (eshop:optgroups object)))


;; (in-package :cl-user)

;; (defpackage #:optgroup
;;   (:use #:cl
;;         #:cl-user
;;         #:json)
;;   (:export
;;    ;; #:optgroup
;;    ;; #:show
;;    ;; #:serialize
;;    #:name
;;    #:options
;;    ;; #:unserialize
;;    ))


;; (in-package #:optgroup)

;; (defun optgroup:options (object)
;;   (eshop::options object))

;; (defun optgroup:name (object)
;;   (eshop::name object))


;; (in-package :cl-user)

;; (defpackage #:option
;;   (:use #:cl
;;         #:cl-user
;;         #:json)
;;   (:export
;;    ;; #:option
;;    ;; #:show
;;    #:name
;;    #:value
;;    ;; #:optype
;;    ;; #:boolflag
;;    ;; #:serialize
;;    ;; #:unserialize
;;    ))


;; (in-package #:option)

;; (defun option:name (object)
;;   (eshop::name object))

;; (defun option:value (object)
;;   (eshop::value object))


;; (in-package :cl-user)

;; (defpackage #:optlist
;;   (:use #:cl
;;         #:cl-user)
;;   (:export #:optlist))


;; (in-package #:optlist)

;; (defclass OPTLIST:OPTLIST ()
;;   ((optlist :initarg :optlist :initform nil :accessor get-optlist)))

;; (defun optlist:optlist (object)
;;   (eshop::optgroups object))

