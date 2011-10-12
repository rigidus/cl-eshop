;;;; render.lisp


(in-package #:eshop)


(defclass eshop-render () ())

(setf *default-render-method* (make-instance 'eshop-render))

(defmethod restas:render-object ((designer eshop-render) (object group))
  (default-page
      (catalog:content
       (list :name (name object)
             :breadcrumbs (catalog:breadcrumbs (breadcrumbs-add-vendor (new-classes.breadcrumbs object)))
             :menu (new-classes.menu object)
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
                              ret)
                            )
             :subcontent  (if (and (null (products object))
                                  (null (getf (request-get-plist) :fullfilter))
                                  (null (getf (request-get-plist) :vendor)))
                             ;; Отображаем группы
                             (catalog:centergroup
                              (list
                               :producers nil;;(restas:render-object designer (make-producers object))
                               :accessories (catalog:accessories)
                               :groups (let* ((object (gethash "noutbuki-i-komputery" (storage *global-storage*)))
                                              (sort-groups (sort (remove-if-not #'active (groups object)) #'menu-sort)))
                                         (mapcar #'(lambda (child)
                                                     (list
                                                      :is-active (active child)
                                                      :name (name child)
                                                      :key (key child)
                                                      :cnt (let ((products (get-recursive-products child)))
                                                             (if (null products)
                                                                 "-"
                                                                 (length (remove-if-not #'active products))))
                                                      :pic (pic child)
                                                      :filters (let ((filters (remove-if #'(lambda (filter)
                                                                                     (is-empty-filtered-list child filter))
                                                                                 (filters child))))
                                                                 (mapcar #'(lambda (filter)
                                                                             (list :name (name filter)
                                                                                   :groupkey (key child)
                                                                                   :key (key filter)
                                                                                   :num (format nil "(~a)"
                                                                                                (get-filtered-product-list-len child filter))
                                                                                   ))
                                                                         filters))))
                                                 sort-groups))))
                             ;;else
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
                                      :for product :in  paginated :collect (render.view product)))))))
             ))
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



(defmethod render.view ((object product))
  (let ((pics (get-pics (articul object))))
    (let ((group (new-classes.parent object)))
      ;; (when (not (null group))
      (list :articul (articul object)
            :name (name-seo object)
            :groupname (if (null group)
                           "group not found"
                           (name group))
            :groupkey  (if (null group)
                           ""
                           (key group))
            :price (siteprice object)
            :formatprice (get-format-price (siteprice object))
            :bestprice (> (delta-price object) 0)
            :firstpic (car pics)
            ;; :keyopts (get-keyoptions object)
            :oneclickbutton  (if (not (preorder object))
                                 (soy.buttons:add-one-click (list :articul (articul object))))
            :addbutton (if (preorder object)
                           (soy.buttons:add-predzakaz (list :articul (articul object)))
                           (soy.buttons:add-product-cart (list :articul (articul object)
                                                               :name (name-seo object)
                                                               :pic (if (null pics) nil (car pics))
                                                               :deliveryprice (delivery-price object)
                                                               :siteprice (price object)
                                                               :price (siteprice object))))
            ))))


(defun render.render-optgroups (optgroups)
  (let ((optlist
         (remove-if
          #'null
          (mapcar
           #'(lambda (optgroup)
               ;;не отображать группу опций с именем "Secret"
               (if (string/= (getf optgroup :name)
                             "Secret")
                   (let ((options
                          (mapcar #'(lambda (option)
                                      (soy.product:option
                                       (list :name (getf option :name)
                                             :value (getf option :value))))
                                  (getf optgroup :options))))
                     (if (not (null (remove-if
                                     #'null
                                     options)))
                         (soy.product:optgroup (list :name (getf optgroup :name)
                                                     :options options))
                         ""))))
           optgroups))))
    (if (null optlist)
        nil
        (soy.product:optlist (list :optgroups optlist)))))

(defmethod render.get-keyoptions ((object product))
  (let ((parent (new-classes.parent object)))
    (when parent
      (mapcar #'(lambda (pair)
                  (let ((key-optgroup (getf pair :optgroup))
                        (key-optname  (getf pair :optname))
                        (optvalue))
                    (mapcar #'(lambda (optgroup)
                                (when (string= (getf optgroup :name) key-optgroup)
                                    (let ((options (getf optgroup :options)))
                                      (mapcar #'(lambda (option)
                                                  (if (string= (getf option :name) key-optname)
                                                      (setf optvalue (getf option :value))))
                                              options))))
                            (optgroups object))
                    (list :optgroup key-optgroup
                          :optname key-optname
                          :optvalue optvalue)))
              (keyoptions parent)))))

(defmethod render.relink ((object product))
  (let ((temp-rs1)
        (temp-rs2))
    ;;2 случайных товара из списка
    (setf temp-rs1 (get-randoms-from-list
                   ;; список активных товаров той же группы и того же производителя
                   ;; кроме его самого
                   (let* ((base-vendor))
                     (with-option1 object "Общие характеристики" "Производитель"
                                   (setf base-vendor (getf option :value)))
                     (remove-if-not
                      #'(lambda (x)
                          (and (not (equal x object))
                               (active x)
                               (let ((vendor))
                                 (with-option1 x "Общие характеристики" "Производитель"
                                              (setf vendor (getf option :value)))
                                 (equal vendor base-vendor))))
                      (products (new-classes.parent object))))
                   2))
    ;;4 случайных товара из списка
    (setf temp-rs2 (get-randoms-from-list
                    ;;список всех активных товаров кроме object
                    (let ((all))
                      (mapcar #'(lambda (v)
                                  (when (not (equal v object))
                                     ;; (print v)
                                    (push v all)))
                              (active-products *global-storage*))
                      all)
                    4))
    (loop
       :for item in (append temp-rs1 temp-rs2)
       :for n
       :from 1 to 4
       :collect item)))

(defmethod restas:render-object ((designer eshop-render) (object product))
  (let* ((pics (get-pics (articul object)))
         (diff-percent (servo.diff-percentage (price object) (siteprice object))))
    (default-page
        (soy.product:content (list :menu (new-classes.menu object)
                                   :breadcrumbs (soy.product:breadcrumbs (new-classes.breadcrumbs object))
                                   :articul (articul object)
                                   :name (name-seo object)
                                   :siteprice (siteprice object)
                                   :storeprice (price object)
                                   :bestprice (> (delta-price object) 0)
                                   :formatsiteprice (get-format-price (siteprice object))
                                   :formatstoreprice (get-format-price (price object))
                                   :equalprice (= (delta-price object) 0)
                                   :diffprice (delta-price object)
                                   :procent diff-percent
                                   :subst (format nil "/~a" (articul object))
                                   :pics (cdr pics)
                                   :firstpic (if (null pics) nil (car pics))
                                   :optlist (render.render-optgroups (optgroups object))
                                   :accessories (soy.product:accessories)
                                   :reviews (soy.product:reviews)
                                   :simular (soy.product:simulars)
                                   :others (soy.product:others
                                            (list :others (mapcar #'(lambda (x)
                                                                      (if (equal 'product (type-of x))
                                                                          (render.view x)
                                                                          (list :aricul "0"
                                                                                :name ""
                                                                                :pic "/img/temp/i6.jpg"
                                                                                :price "0"
                                                                                :siteprice "0" :subst ""
                                                                                :firstpic "/img/temp/i6.jpg")))
                                                                  (render.relink object))))
                                   :keyoptions (render.get-keyoptions object)
                                   :active (active object)
                                   ;; :descr (descr object)
                                   :shortdescr (seo-text object)
                                   ;; :dontshdev (gethash (articul object) *special-products*)
                                   :seotextflag (and (not (null (seo-text object)))
                                                     (string/= (seo-text object) ""))
                                                ;; (or (and (descr object)
                                                ;;          (not (string= "" (stripper (descr object)))))
                                                ;;     (and (shortdescr object)
                                                ;;          (not (string= "" (stripper (shortdescr object))))))
                                   :predzakaz (preorder object)
                                   :addproductcart (if (preorder object)
                                                       (soy.buttons:add-predzakaz (list :articul (articul object)))
                                                       (soy.buttons:add-product-cart (list :articul (articul object)
                                                                                           :name (name-seo object)
                                                                                           :pic (if (null pics) nil (car pics))
                                                                                           :siteprice (siteprice object)
                                                                                           :price (price object)
                                                                                           ;; :deliveryprice (delivery-price object)
                                                                                           )))
                                   :addoneclick (if (not (preorder object))
                                                    (soy.buttons:add-one-click (list :articul (articul object))))))
        :keywords (format nil "~a"
                          (name-seo object))
        :description (format nil "купить ~a в ЦиFры 320-8080 по лучшей цене с доставкой по Санкт-Петербургу"
                             (name-seo object))
        :title (string-convertion-for-title
                (format nil "~a купить в ЦиFры - цена, фотография и описание, продажа ~a с гарантией и доставкой в ЦиFры 320-8080"
                        (name-seo object)
                        (name-seo object))))))


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

