;;;; render.lisp


(in-package #:eshop)


(defclass eshop-render () ())

(setf *default-render-method* (make-instance 'eshop-render))

(defmethod restas:render-object ((designer eshop-render) (object group))
  (default-page
      (catalog:content
       (list :name (name object)
             :breadcrumbs (catalog:breadcrumbs (breadcrumbs-add-vendor (breadcrumbs object)))
             :menu (menu object)
             :rightblocks (append
                            (if (= 0 (num-nonempty-filters object))
                                nil
                                (list (fullfilter:rightfilter
                                       (list :filters (loop :for filter
                                                         :in (remove-if
                                                              #'(lambda (fil)
                                                                  (is-empty-filtered-list object fil))
                                                              (filters object))
                                                         :collect (make-string-filter object filter))))))
                            ;;fullfilter
                            (let ((ret (rightblocks)))
                              (if (not (null (fullfilter object)))
                                  (push (restas:render-object designer (fullfilter object)) ret))
                              ret))
             :subcontent (if (and (null (products object))
                                  (null (getf (request-get-plist) :fullfilter))
                                  (null (getf (request-get-plist) :vendor)))
                             ;; Отображаем группы
                             (catalog:centergroup
                              (list
                               :producers nil;;(restas:render-object designer (make-producers object))
                               :accessories (catalog:accessories)
                               :groups (remove-if ;; удаляем пустые и неактивные группы
                                        #'(lambda (x)
                                            (or (equal 0 (getf x :cnt))
                                                (null (getf x :is-active))))
                                        (loop :for child :in (sort (copy-list (childs object)) #'menu-sort) :collect
                                           (list
                                            :is-active (active child)
                                            :name (name child)
                                            :key (key child)
                                            :cnt (let ((products (get-recursive-products child)))
                                                   (if (null products)
                                                       "-"
                                                      (length (remove-if-not #'(lambda (product)
                                                                                  (active product))
                                                                              products))))
                                            :pic (pic child)
                                            :filters (loop :for filter
                                                        :in (remove-if #'(lambda (filter)
                                                                           (is-empty-filtered-list child filter))
                                                                       (filters child))
                                                        :collect (list :name (name filter)
                                                                       :groupkey (key child)
                                                                       :key (key filter)
                                                                       :num (format nil "(~a)"
                                                                                    (get-filtered-product-list-len child filter)))))))))
                             ;; else
                             (let ((products-list
                                    (if (getf (request-get-plist) :showall)
                                        (copy-list (products object))
                                        (remove-if-not #'(lambda (product)
                                                           (active product))
                                                       (get-recursive-products object))))
                                   (request-get-plist (request-get-plist)))
                               (if (null (getf request-get-plist :sort))
                                   (setf (getf request-get-plist :sort) "pt"))
                               (if (getf (request-get-plist) :vendor)
                                   (setf products-list
                                         (remove-if-not #'(lambda (p)
                                                            (vendor-filter-controller p (request-get-plist)))
                                                            products-list)))
                               (if (getf (request-get-plist) :fullfilter)
                                   (setf products-list (fullfilter-controller products-list  object (request-get-plist))))
                               (with-sorted-paginator
                                   products-list
                                 request-get-plist
                                 (catalog:centerproduct
                                  (list
                                   :sorts (sorts request-get-plist)
                                   :producers (restas:render-object designer (make-producers object))
                                   :accessories (catalog:accessories)
                                   :pager pager
                                   :products
                                   (loop
                                      :for product :in  paginated :collect (view product)))))
                               ))))
      :keywords (format nil "~a" (name object))
      :description (format nil "~a" (name object))
      :title (let ((vendor (getf (request-get-plist) :vendor)))
               (string-convertion-for-title
                (if vendor
                    (format nil "~a ~a - купить ~a ~a по низкой цене, продажа ~a ~a с доставкой и гарантией в ЦиFры 320-8080"
                            (sklonenie (name object) 1)
                            vendor
                            (sklonenie (name object) 2)
                            vendor
                            (sklonenie (name object) 3)
                            vendor)
                    (format nil "~a - купить ~a  по низкой цене, продажа ~a с доставкой и гарантией в ЦиFры 320-8080"
                            (sklonenie (name object) 1)
                            (sklonenie (name object) 2)
                            (sklonenie (name object) 3)))))))


(defmethod restas:render-object ((designer eshop-render) (object group-filter))
  (fullfilter:container
   (list :name (name object)
         :vendor (getf (request-get-plist) :vendor)
         :sort (getf (request-get-plist) :sort)
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
                                                            (cadr elt)))))
                                   (advanced object)))
         :isshowadvanced (is-need-to-show-advanced object (request-get-plist)))))



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
                         :bestprice (> (price object) (siteprice object))
                         :formatsiteprice (get-format-price (siteprice object))
                         :formatstoreprice (get-format-price (price object))
                         :equalprice (= (siteprice object) (price object))
                         :diffprice diffprice
                         :procent procent
                         :subst (format nil "/~a" (articul object))
                         :pics (cdr pics)
                         :firstpic (if (null pics) nil (car pics))
                         :optlist (let ((optlist  (remove-if #'null (mapcar #'(lambda (optgroup)
                                                                     ;;не отображать группу опций с именем "Secret"
                                                                     (if (not (string= (name optgroup)
                                                                                      "Secret"))
                                                                        (restas:render-object designer optgroup)))
                                                                           (optgroups object)))))
                                    (if (null optlist)
                                        nil
                                        (product:optlist (list :optgroups optlist))))
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
                         :dontshdev (gethash (articul object) *special-products*)
                         :seotextflag (or (and (descr object)
                                               (not (string= "" (stripper (descr object)))))
                                          (and (shortdescr object)
                                               (not (string= "" (stripper (shortdescr object))))))
                         :predzakaz (predzakaz object)
                         :addproductcart (if (predzakaz object)
                                             (soy.buttons:add-predzakaz (list :articul (articul object)))
                                             (soy.buttons:add-product-cart (list :articul (articul object)
                                                                                 :name (realname object)
                                                                                 :pic (if (null pics) nil (car pics))
                                                                                 :siteprice (siteprice object)
                                                                                 :price (price object)
                                                                                 :deliveryprice (delivery-price object))))
                         :addoneclick (if (not (predzakaz object))
                                          (soy.buttons:add-one-click (list :articul (articul object))))))
          :keywords (format nil "~a"
                            (realname object))
          :description (format nil "купить ~a в ЦиFры 320-8080 по лучшей цене с доставкой по Санкт-Петербургу"
                               (realname object))
          :title (string-convertion-for-title
                  (format nil "~a купить в ЦиFры - цена, фотография и описание, продажа ~a с гарантией и доставкой в ЦиFры 320-8080"
                          (realname object)
                          (realname object)))))))


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
  (let ((url-parameters (request-get-plist)))
    (remf url-parameters :page)
    (multiple-value-bind (base hidden)
        (cut 12 (mapcar #'(lambda (x)
                            (setf (getf url-parameters :vendor) (hunchentoot:url-encode (car x)))
                            (list :vendor (car x)
                                  :cnt (cadr x)
                                  :link (format nil "?~a" (make-get-str url-parameters))))
                        (cond
                          ((getf (request-get-plist) :showall)
                           (producersall object))
                          (t (producers object)))))
      (catalog:producers (list :vendorblocks (make-producters-lists base)
                               :vendorhiddenblocks (make-producters-lists hidden))))))


(defmethod restas:render-object ((designer eshop-render) (object filter))
  (let ((products-list (remove-if-not (func object)
                                     (remove-if-not #'active
                                                    (get-recursive-products (parent object)))))
        (request-get-plist (request-get-plist))
        (fltr-name  (name object))
        (grname (name (parent object))))
    (if (null (getf request-get-plist :sort))
        (setf (getf request-get-plist :sort) "pt"))
    (if (getf (request-get-plist) :vendor)
        (setf products-list
              (remove-if-not #'(lambda (p)
                                 (vendor-filter-controller p (request-get-plist)))
                             products-list)))
    (with-sorted-paginator
        products-list
      request-get-plist
      (default-page
          (catalog:content
           (list :name (name object)
                 :breadcrumbs (catalog:breadcrumbs (breadcrumbs object))
                 :menu (menu object)
                 :rightblocks (append
                               (if (= 0 (num-nonempty-filters (parent object)))
                                   nil
                                   (list (fullfilter:rightfilter
                                          (list :filters (loop
                                                            :for filter
                                                            :in (remove-if #'(lambda (fil)
                                                                               (is-empty-filtered-list (parent object) fil))
                                                                           (filters (parent object)))
                                                            :collect (make-string-filter (parent object)
                                                                                         filter
                                                                                         (equal object filter)))))))
                                   (rightblocks))
                 :subcontent (catalog:centerproduct
                              (list
                               :sorts (sorts request-get-plist)
                               :producers (restas:render-object designer (make-producers (parent object)))
                               :accessories (catalog:accessories)
                               :pager pager
                               :products (loop
                                            :for product
                                            :in  paginated
                                            :collect (view product))))))
          :keywords (format nil "~a ~a" grname fltr-name)
          :description (format nil "~a ~a" grname fltr-name)
          :title (let ((vendor (getf (request-get-plist) :vendor))
                       (name-1 (sklonenie fltr-name 1))
                       (name-2 (sklonenie fltr-name 2))
                       (name-3 (sklonenie fltr-name 3))
                       (grname-1 (sklonenie grname 1))
                       (grname-2 (sklonenie grname 2))
                       (grname-3 (sklonenie grname 3)))
                   (string-convertion-for-title
                    (if vendor
                        (format nil "~a ~a ~a - купить ~a ~a ~a по низкой цене, продажа ~a ~a ~a с доставкой и гарантией в ЦиFры 320-8080"
                                grname-1
                                name-1
                                vendor
                                grname-2
                                name-2
                                vendor
                                grname-3
                                name-3
                                vendor)
                        (format nil "~a ~a - купить ~a ~a  по низкой цене, продажа ~a ~a с доставкой и гарантией в ЦиFры 320-8080"
                                grname-1
                                name-1
                                grname-2
                                name-2
                                grname-3
                                name-3))))))))



(defmethod restas:render-object ((designer eshop-render) (object optgroup))
  (let ((options (mapcar #'(lambda (option)
                             (if ( not (or (null (value option))
                                           ;; Не отображать опции в пустыми значениями
                                           (string=  (string-trim
                                                      '(#\Space #\Tab #\Newline)
                                                      (value option))
                                                     "")))
                                 (restas:render-object designer option)))
                         (options object))))
    (if (not (null (remove-if  #'(lambda (v) (null v)) options)))
        (product:optgroup (list :name (name object)
                                :options options))
        "")))


(defmethod restas:render-object ((designer eshop-render) (object option))
  (product:option (list :name (name object)
                        :value (if (and (equal (optype object) :bool)
                                        (boolflag object))
                                   (format nil "~a ~a" "<img src=\"img/ok.png\" alt=\"*\"/>" (value object))
                                   (value object)))))


