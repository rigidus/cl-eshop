;;;; render.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop)


(defclass eshop-render () ())

(setf *default-render-method* (make-instance 'eshop-render))


(defmethod restas:render-object ((designer eshop-render) (object group))
  (default-page
      (catalog:content
       (list :name (name object)
             :breadcrumbs (catalog:breadcrumbs (breadcrumbs object))
             :menu (menu object)
             :rightblocks (let ((ret (rightblocks)))
                            (if (not (null (fullfilter object)))
                                (push (restas:render-object designer (fullfilter object)) ret))
                            ret)
             :tradehits (tradehits)
             :subcontent (if (and (null (products object))
                                  (null (getf (request-get-plist) :fullfilter))
                                  (null (getf (request-get-plist) :vendor)))
                             ;; Отображаем группы
                             (catalog:centergroup
                              (list
                               :producers "(restas:render-object designer (make-producers object))"
                               :accessories (catalog:accessories)
                               :groups (remove-if ;; удаляем пустые группы
                                        #'(lambda (x)
                                            (equal 0 (getf x :cnt)))
                                        (loop :for child :in (sort (copy-list (childs object)) #'menu-sort) :collect
                                           (list :name (name child)
                                                 :key (key child)
                                                 :cnt (let ((products (products child)))
                                                        (if (null products)
                                                            "-"
                                                            (length (remove-if-not #'(lambda (product)
                                                                                       (active product))
                                                                                   (products child)))))
                                                 :pic (pic child)
                                                 :filters (loop :for filter :in (filters child) :collect
                                                             (list :name (name filter)
                                                                   :groupkey (key child)
                                                                   :key (key filter))))))))
                             ;; else
                             (with-sorted-paginator
                                 (remove-if-not #'(lambda (product)
                                                    (active product))
                                                (cond
                                                  ((getf (request-get-plist) :fullfilter)
                                                   (filter-controller object (request-get-plist)))
                                                  ((getf (request-get-plist) :vendor)
                                                   (vendor-controller object (request-get-plist)))
                                                  (t (copy-list (products object)))))
                               (catalog:centerproduct
                                (list
                                 :sorts (sorts)
                                 :producers (restas:render-object designer (make-producers object))
                                 :accessories (catalog:accessories)
                                 :pager pager
                                 :products
                                 (loop
                                    :for product :in  paginated :collect (view product))))))))
      :keywords (format nil "~a" (name object))
      :description (format nil "~a" (name object))
      :title (format nil "~a - купить ~a  по низкой цене, продажа ~a с доставкой и гарантией в ЦиFры 320-8080"
                     (name object)
                     (name object)
                     (name object))))


(defmethod restas:render-object ((designer eshop-render) (object group-filter))
  (fullfilter:container
   (list :name (name object)
         :base (format nil "~{~a~}"
                       (mapcar #'(lambda (elt)
                                   (filter-element elt (request-get-plist)))
                               (base object)))
         :advanced (format nil "~{~a~}"
                           (mapcar #'(lambda (elt)
                                       (fullfilter:group
                                        (list :name (car elt)
                                              :elts (mapcar #'(lambda (inelt)
                                                                (filter-element inelt (request-get-plist)))
                                                            (cadr elt))
                                              )))
                                   (advanced object))))))



(defmethod restas:render-object ((designer eshop-render) (object product))
  (multiple-value-bind (diffprice procent)
      (get-procent (price object) (siteprice object))
    (let ((pics (get-pics (articul object))))
      (default-page
          (product:content (list :menu (menu object)
                         :breadcrumbs (product:breadcrumbs (breadcrumbs object))
                         :articul (articul object)
                         :name (realname object)
                         :siteprice (siteprice object)
                         :storeprice (price object)
                         :diffprice diffprice
                         :procent procent
                         :subst (format nil "/~a" (articul object))
                         :pics pics
                         :firstpic (if (null pics) nil (car pics))
                         :optlist (if (null (optgroups object))
                                      ""
                                      (product:optlist
                                       (list :optgroups (mapcar #'(lambda (optgroup)
                                                                    (restas:render-object designer optgroup))
                                                                        (optgroups object)))))
                         :accessories (product:accessories)
                         :reviews (product:reviews)
                         :simular (product:simulars)
                         :others (product:others
                                  (list :others (mapcar #'(lambda (x)
                                                            (if (equal 'product (type-of x))
                                                                (view x)
                                                                (list :aricul "0"
                                                                      :name ""
                                                                      :pic "/img/temp/i6.jpg"
                                                                      :price "0"
                                                                      :siteprice "0" :subst ""
                                                                      :firstpic "/img/temp/i6.jpg")))
                                                        (relink object))))
                         :keyoptions (get-keyoptions object)
                         :active (active object)
                         :descr (descr object)
                         :shortdescr (shortdescr object)
                         ))
          :keywords (format nil "~a"
                            (realname object))
          :description (format nil "купить ~a в ЦиFры 320-8080 по лучшей цене с доставкой по Санкт-Петербургу"
                               (realname object))
          :title (format nil "~a купить в ЦиFры - цена, фотография и описание, продажа ~a с гарантией и доставкой в ЦиFры 320-8080"
                         (realname object)
                         (realname object))))))


(defun make-producters-lists(list &optional (column-number 4))
  (loop
     for current-list on list
     for i from 1 to column-number
     collect (loop for item in current-list
                by #'(lambda (list)
                       (let ((result-list list))
                         (loop for j from 1 to column-number
                            do (setf result-list (cdr result-list)))
                         result-list))
                collect item)))


(defmethod restas:render-object ((designer eshop-render) (object producers))
  (multiple-value-bind (base hidden)
      (cut 12 (mapcar #'(lambda (x)
                          (list :vendor (car x)
                                :cnt (cadr x)
                                :link (format nil "?vendor=~a" (car x))))
                      (producers object)))
    (catalog:producers (list :vendorblocks (make-producters-lists base)
                             :vendorhiddenblocks (make-producters-lists hidden)))))


(defmethod restas:render-object ((designer eshop-render) (object filter))
  (with-sorted-paginator
      (remove-if-not (func object)
                     (remove-if-not #'(lambda (product)
                                        (active product))
                                    (get-recursive-products
                                     (parent object))))
      (default-page
          (catalog:content
           (list :name (name object)
                 :breadcrumbs (catalog:breadcrumbs (breadcrumbs object))
                 :menu (menu object)
                 :rightblocks (rightblocks)
                 :tradehits (tradehits)
                 :subcontent (catalog:centerproduct
                              (list
                               :sorts (sorts)
                               :producers (restas:render-object designer (make-producers (parent object)))
                               :accessories (catalog:accessories)
                               :pager pager
                               :products (loop
                                            :for product
                                            :in  paginated
                                            :collect (view product))))))
          :keywords (format nil "~a" (name object))
          :description (format nil "~a" (name object))
          :title (format nil "~a - купить ~a по низкой цене, продажа ~a с доставкой и гарантией в ЦиFры 320-8080"
                         (name object)
                         (name object)
                         (name object)))))


(defmethod restas:render-object ((designer eshop-render) (object optgroup))
  (product:optgroup (list :name (name object)
                          :options (mapcar #'(lambda (option)
                                               (restas:render-object designer option))
                                           (options object)))))


(defmethod restas:render-object ((designer eshop-render) (object option))
  (product:option (list :name (name object)
                        :value (if (and (equal (optype object) :bool)
                                        (boolflag object))
                                   (format nil "~a ~a" "<img src=\"img/ok.png\" alt=\"*\"/>" (value object))
                                   (value object)))))


