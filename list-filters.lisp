;;;; list-filters.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(in-package #:eshop)

(defclass field-filter ()
  ((fieldacc           :initarg :fieldacc         :initform nil       :accessor fieldacc)
   ;; func for type of filtering field. Unused for now (always #'list-filter.field-filter)
   (filtertype         :initarg :filtertype       :initform nil       :accessor filtertype)
   ;; func for apply to field value. For example #'> or #'string/= (lambda if necessary)
   (filterfunc         :initarg :filterfunc       :initform nil       :accessor filterfunc)
   (inclusive          :initarg :inclusive        :initform t         :accessor inclusive)
   (value              :initarg :value            :initform nil       :accessor value)))


(defmacro list-filters.inclusive-check (inclusive expression value)
  "Returns value if inclusive and expression are both t or both nil"
  `(let ((incl ,inclusive)
         (expr ,expression)
         (val ,value))
     (when (or (and incl expr)
               (not (or incl expr)))
       val)))

(defun list-filters.filter-list (items &rest filters)
  "Filters given list with given filters"
  (remove-if
   #'null
   (mapcar
    #'(lambda (item)
        (let ((res item))
          (loop for filter in filters
             do
               (setf res
                     (and res
                          (apply (filtertype filter) (list item filter)))))
          res))
    items)))


(defun list-filters.field-filter (item filter)
  (list-filters.inclusive-check
   (inclusive filter)
   (apply (filterfunc filter) (list (funcall (fieldacc filter) item)
                                    (value filter)))
   item))

(defun list-filters.limit-start (items start)
  (nthcdr start items))

(defun list-filters.limit-end (items end)
  (if (> end (length items))
      items
      (subseq items 0 end)))

(defun list-filters.limit-region (items start length)
  (let ((end (+ start length)))
    (if (> end (length items))
        (list-filters.limit-start items start)
        (subseq items start end))))

(defun list-filters.limit-page (items page-size page-number)
  ;; numbering from 1
  (subseq items (* page-size (- page-number 1)) page-size))


;; (defun list-filters.get-sort-func (elt sorter)
;;   (cond
;;     ((stringp elt)
;;      (if (equal (getf sorter :direction) "desc")


(defun list-filters.sort-list (list json-sorters)
  (let ((sorters (mapcar #'servo.alist-to-plist (decode-json-from-string json-sorters))))
    list))

;; (defun list-filters.get-json ()
;;   (print (hunchentoot:request-uri hunchentoot:*request*))
;;   (print (hunchentoot:post-parameters hunchentoot:*request*))
;;   (let* ((params (servo.alist-to-plist (hunchentoot:get-parameters hunchentoot:*request*)))
;;          (type (getf params :type))
;;          (parent (getf params :parent))
;;          (start (getf params :start))
;;          (limit (getf params :limit))
;;          (sorters (getf params :sort)))
;;     (log5::log-for test "~a" params)
;;     (setf start
;;           (if start
;;               (parse-integer start)
;;               0))
;;     (setf limit
;;           (if limit
;;               (parse-integer limit)
;;               50))
;;     (cond
;;       ((equalp type "groups")
;;        (let ((group-list (groups *global-storage*)))
;;          ;; debug
;;          (setf group-list (list-filters.sort-list group-list sorters))
;;          ;; /debug
;;          (soy.admin-table:json-data
;;           (list
;;            :number (length group-list)
;;            :type "groups"
;;            :elts (mapcar #'(lambda (g)
;;                              (soy.admin-table:json-group-elt
;;                               (list
;;                                :name (name g)
;;                                :key (key g)
;;                                :numprod (length (products g))
;;                                :order (order g)
;;                                :active (if (active g)
;;                                            "true"
;;                                            "false"))))
;;                          (list-filters.limit-region group-list start limit))))))
;;       ((equalp type "products")
;;        (if (or (null parent)
;;                (null (gethash parent (storage *global-storage*))))
;;            "Incorrect parent"
;;            ;; else
;;            (let ((product-list (products (gethash parent (storage *global-storage*)))))
;;              (soy.admin-table:json-data
;;               (list
;;                :number (length product-list)
;;                :type "products"
;;                :elts (mapcar #'(lambda (g)
;;                                  (soy.admin-table:json-product-elt
;;                                   (list
;;                                    :name (name-provider g)
;;                                    :key (key g)
;;                                    :active (if (active g)
;;                                                "true"
;;                                                "false"))))
;;                              (list-filters.limit-region product-list start limit)))))))
;;       (t "Ololo! Dont know"))))
