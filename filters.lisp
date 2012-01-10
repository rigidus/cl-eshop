;;;; filters.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop)

;;Возвращает длину списка активных продуктов-потомков подходящих под фильтр
(defun get-filtered-product-list-len (object filter)
  (length (remove-if-not (func filter)
                         (remove-if-not #'active
                                        (get-recursive-products object)))))

(defun is-empty-filtered-list (object filter)
  (= 0 (get-filtered-product-list-len object filter)))


;;Составление строки для представления фильтров в 1 клик на странице с fullfilter
(defun filters.make-string-filter (filter num &optional itsme)
  (if itsme
      (format nil "<b>~a</b> (~a)<br/>"
              (name filter)
              num)
      (format nil "<a class=\"rightfilter\" href=\"/~a/~a\">~a</a> (~a)<br/>"
              (key (new-classes.parent filter))
              (key filter)
              (name filter)
              num)))

;;количество непустых фильтров у группы
(defun num-nonempty-filters (object)
  (length (remove-if #'(lambda (fil) (is-empty-filtered-list object fil))
                     (filters object))))

(defmethod filters.get-filters ((group group) (products list))
  "Возвращает список ненулевых фильтров на списке объектов и количество объесктов в выборке"
  (filters.get-filters (filters group) products))

(defmethod filters.get-filters ((filters list) (products list))
  "Возвращает список ненулевых фильтров на списке объектов и количество объесктов в выборке"
  (remove-if #'null (mapcar #'(lambda (filter)
              (let ((num (length
                          (remove-if-not #'(lambda (p) (funcall (func filter) p)) products))))
                (if (not (= num 0))
                    (cons filter num))))
              filters)))


;; (let* ((child (gethash "komputery" (storage *global-storage*))))
;;   (remove-if #'(lambda (filter)
;;                  (wlog (func-string filter))
;;                  (is-empty-filtered-list child filter))
;;              (filters child)))

;; (let* ((gr (gethash "noutbuki" (storage *global-storage*)))
;;        (filt (car (filters gr)))
;;        (prod  (car (get-recursive-products gr))))
;;   (wlog (name gr))
;;   (wlog (func-string filt))
;;   (wlog (name-seo prod))
;;   (funcall (func filt) prod))
