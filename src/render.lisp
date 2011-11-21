;;;; render.lisp


(in-package #:eshop)


(defclass eshop-render () ())

(setf *default-render-method* (make-instance 'eshop-render))

(defmethod render.get-oneclick-filters ((group group) &optional (full nil))
  "Отобрадение фильтров в один клик"
  (let ((products)
        (filters))
    (setf products (if full
                      (storage.get-filtered-products group #'atom)
                      (storage.get-filtered-products group #'active)))
    (setf filters (filters.get-filters group products))
    (if filters
        (list (fullfilter:rightfilter
               (list :filters (mapcar #'(lambda (v)
                                          (filters.make-string-filter (car v)
                                                                      (cdr v)))
                                      filters))))
        nil)))



(defmethod render.get-keywords ((object group) &optional (parameters (request-get-plist)))
  (let* ((name (name object))
         (vendor (getf parameters :vendor))
         (page-name))
    (if vendor
        (setf page-name (format nil "~a ~a" (sklonenie name 1) vendor))
        (setf page-name (format nil "~a" (sklonenie name 1))))
    (format nil "~a, ~a купить, ~a цена, ~a в Санкт-Петербурге, ~a продажа" page-name page-name page-name page-name  page-name)))

(defmethod render.get-keywords ((object product) &optional (parameters (request-get-plist)))
  (declare (ignore parameters))
  (let ((page-name (name-seo object)))
    (format nil "~a, ~a купить, ~a цена, ~a в Санкт-Петербурге, ~a продажа" page-name page-name page-name page-name  page-name)))


(defmethod render.get-title ((object group) &optional (parameters (request-get-plist)))
  (let ((name (name object))
        (vendor (getf parameters :vendor)))
               (string-convertion-for-title
                (if vendor
                    (format nil "~a ~a - купить ~a ~a по низкой цене, продажа ~a ~a с доставкой и гарантией в ЦиFры 320-8080"
                            (sklonenie name 1)
                            vendor
                            (sklonenie name 2)
                            vendor
                            (sklonenie name 3)
                            vendor)
                    (format nil "~a - купить ~a  по низкой цене, продажа ~a с доставкой и гарантией в ЦиFры 320-8080"
                            (sklonenie name 1)
                            (sklonenie name 2)
                            (sklonenie name 3))))))

(defmethod render.get-description ((object group) &optional (parameters (request-get-plist)))
  (let ((name (name object))
        (vendor (getf parameters :vendor)))
               (string-convertion-for-title
                (if vendor
                    (format nil "Купить ~a ~a по низкой цене, продажа ~a ~a с доставкой и гарантией в ЦиFры 320-8080"
                            (sklonenie name 2)
                            vendor
                            (sklonenie name 3)
                            vendor)
                    (format nil "Купить ~a  по низкой цене, продажа ~a с доставкой и гарантией в ЦиFры 320-8080"
                            (sklonenie name 2)
                            (sklonenie name 3))))))


(defmethod render.render ((object group) &optional (parameters (request-get-plist)))
  (let ((name (name object)))
    (default-page
      (catalog:content
       (list :name name
             :breadcrumbs (catalog:breadcrumbs (breadcrumbs-add-vendor1 (new-classes.breadcrumbs object) parameters))
             :menu (new-classes.menu object)
             :rightblocks (append
                           (render.get-oneclick-filters object
                                                        (getf parameters :showall))
                           ;;fullfilter
                           (let ((ret (rightblocks object parameters)))
                             (if (and (not (null (fullfilter object)))
                                      (not (equal "" (fullfilter object))))
                                 (push (render.render (fullfilter object) parameters) ret))
                             ret))
             :subcontent  (if (and (null (products object))
                                   (null (getf parameters :fullfilter))
                                   (null (getf parameters :vendor)))
                              ;; Отображаем группы
                              (catalog:centergroup
                               (list
                                :producers (if (getf parameters :showall)
                                               (render.show-producers (storage.get-filtered-products object #'atom))
                                               nil)
                                :accessories (catalog:accessories)
                                :groups (let ((sort-groups (sort (remove-if-not #'active (groups object)) #'menu-sort)))
                                          (mapcar #'(lambda (child)
                                                      (let* ((show-func (if (getf parameters :showall)
                                                                            #'atom
                                                                            #'active))
                                                             (products (storage.get-filtered-products child show-func))
                                                             (filters (filters.get-filters child products)))
                                                        (list
                                                         :is-active (active child)
                                                         :name (name child)
                                                         :key (key child)
                                                         :cnt (if products
                                                                  (length products)
                                                                  "-")
                                                         :pic (pic child)
                                                         :filters (mapcar #'(lambda (v)
                                                                              (let ((filter (car v))
                                                                                    (num (cdr v)))
                                                                                (list :name (name filter)
                                                                                      :groupkey (key child)
                                                                                      :key (key filter)
                                                                                      :num (format nil "(~a)" num))))
                                                                            filters))))
                                                      sort-groups))))
                              ;;else
                              (let ((products-list (if (getf parameters :showall)
                                                       (storage.get-filtered-products object #'atom)
                                                       (storage.get-filtered-products object #'active))))
                                (if (null (getf parameters :sort))
                                    (setf (getf parameters :sort) "pt"))
                                (if (getf parameters :vendor)
                                    (setf products-list
                                          (remove-if-not #'(lambda (p)
                                                             (vendor-filter-controller p parameters))
                                                         products-list)))
                                (if (getf parameters :fullfilter)
                                    (setf products-list (fullfilter-controller products-list object parameters)))
                                (with-sorted-paginator
                                    products-list
                                  parameters
                                  (catalog:centerproduct
                                   (list
                                    :sorts (sorts parameters)
                                    :producers (render.show-producers (if (getf parameters :showall)
                                                                          (storage.get-filtered-products object #'atom)
                                                                          (storage.get-filtered-products object #'active)))
                                    :accessories (catalog:accessories)
                                    :pager pager
                                    :products
                                    (loop
                                       :for product :in  paginated :collect (render.view product)))))))))
        :keywords (render.get-keywords object parameters)
        :description (render.get-description object parameters)
        :title (render.get-title object parameters))))

(defmethod render.render ((object group-filter) &optional (parameters (request-get-plist)))
  (when (not (equal "" object))
             (fullfilter:container
              (list :name (name object)
                    :vendor (getf parameters :vendor)
                    :sort (getf parameters :sort)
                    :base (format nil "~{~a~}"
                                  (mapcar #'(lambda (elt)
                                              (filter-element elt parameters))
                                          (base object)))
                    :advanced (format nil "~{~a~}"
                                      (mapcar #'(lambda (elt)
                                                  (fullfilter:group
                                                   (list :name (car elt)
                                                         :elts (mapcar #'(lambda (inelt)
                                                                           (filter-element inelt parameters))
                                                                       (cadr elt)))))
                                              (advanced object)))
                    :isshowadvanced (is-need-to-show-advanced object parameters)))))


(defmethod restas:render-object ((designer eshop-render) (object group-filter))
  (render.render object))



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
            :formatstoreprice (get-format-price
                               (+ (siteprice object)
                                  (delta-price object)))
            :bestprice (> (delta-price object) 0)
            :firstpic (car pics)
            :promotiontext (let ((value))
                             (with-option1 object "Secret" "Продающий текст"
                                           (setf value (getf option :value)))
                             value)
            :keyopts (render.get-catalog-keyoptions object)
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
          (mapcar #'(lambda (optgroup)
                      ;;не отображать группу опций с именем "Secret"
                      (if (string/= (getf optgroup :name)
                                    "Secret")
                          (let ((options
                                 (mapcar #'(lambda (option)
                                             (when (not (equal "" (getf option :value)))
                                                 (soy.product:option
                                                  (list :name (getf option :name)
                                                        :value (getf option :value)))))
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

(defmethod render.get-catalog-keyoptions ((object product))
  (let ((parent (new-classes.parent object)))
    (when parent
      (remove-if #'null
                  (mapcar #'(lambda (pair)
                              (let ((key-optgroup (getf pair :optgroup))
                                    (key-optname  (getf pair :optname))
                                    (key-alias  (getf pair :showname))
                                    (optvalue))
                                (mapcar #'(lambda (optgroup)
                                            (when (string= (getf optgroup :name) key-optgroup)
                                              (let ((options (getf optgroup :options)))
                                                (mapcar #'(lambda (option)
                                                            (if (string= (getf option :name) key-optname)
                                                      (setf optvalue (getf option :value))))
                                                        options))))
                                        (optgroups object))
                                (if (and optvalue
                                         (not (equal "" optvalue)))
                                    (list :optgroup key-alias
                                          :optvalue (if (equal "Есть" optvalue)
                                                        ""
                                                        optvalue)))))
                          (catalog-keyoptions parent))))))



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
                      (storage.get-filtered-products (new-classes.parent object))))
                   2))
    ;;4 случайных товара из списка
    (setf temp-rs2 (get-randoms-from-list
                    ;;список всех активных товаров кроме object
                    (let ((all))
                      (mapcar #'(lambda (v)
                                  (when (not (equal v object))
                                     ;; (print v)
                                    (push v all)))
                            (storage.get-filtered-products (products *global-storage*)))
                      all)
                    4))
    (loop
       :for item in (append temp-rs1 temp-rs2)
       :for n
       :from 1 to 4
       :collect item)))

(defmethod restas:render-object ((designer eshop-render) (object product))
  (let* ((pics (get-pics (articul object)))
         (diff-percent (servo.diff-percentage (price object) (siteprice object)))
         (is-vintage (null (active object)))
         (product-view))
    (setf product-view (list :menu (new-classes.menu object)
                             :breadcrumbs (soy.product:breadcrumbs (new-classes.breadcrumbs object))
                             :articul (articul object)
                             :name (name-seo object)
                             :siteprice (siteprice object)
                             :storeprice (price object)
                             :bestprice (> (delta-price object) 0)
                             :formatsiteprice (get-format-price (siteprice object))
                             :formatstoreprice (get-format-price
                                                (+ (siteprice object)
                                                   (delta-price object)))
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
                             :slogan (let ((value))
                                       (with-option1 object "Secret" "Продающий текст"
                                                     (setf value (getf option :value)))
                                       value)
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
                             :bestproducts (soy.product:similar-products
                                            (list :products (mapcar #'(lambda (prod)
                                                                        (catalog:product
                                                                         (render.view prod)))
                                                                    (list-filters.limit-end (servo.find-relative-product-list object) 4))))
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
    (default-page
        (if  is-vintage
             (soy.product:vintage-card product-view)
             (soy.product:content product-view)
             )
        :keywords (render.get-keywords object nil)
        :description (format nil "купить ~a в ЦиFры 320-8080 по лучшей цене с доставкой по Санкт-Петербургу"
                             (name-seo object))
        :title (string-convertion-for-title
                (format nil "~a купить в ЦиFры - цена, фотография и описание, продажа ~a с гарантией и доставкой в ЦиFры 320-8080"
                        (name-seo object)
                        (name-seo object))))))


 (defun render.make-producters-reverse-lists-by-column (list  &optional (columns 4))
   (let ((len (length list))
         (rs)
         (start-pos 0)
         (segment))
     (multiple-value-bind (division remainder)
         (truncate len columns)
       (loop
          :for i from 1 to columns
          :do (progn
                (setf segment division)
                (when (< 0 remainder)
                    (setf segment (+ 1 segment))
                    (setf remainder (- remainder 1)))
                ;; (format t "~&~a:~a:~a" start-pos segment remainder)
                (push (subseq list start-pos (+ start-pos segment)) rs)
                (setf start-pos (+ start-pos segment)))))
     rs))

 (defun render.make-producters-base (list  &key (columns 4) cut)
   (let ((reverse-lists (render.make-producters-reverse-lists-by-column list columns))
         (rs))
     (mapcar #'(lambda (v)
                 (let ((len cut))
                   (if (or (null cut)
                           (< (length v) cut))
                       (setf len (length v)))
                   (push (subseq v 0 len) rs)))
             reverse-lists)
     (remove-if #'null rs)))

 (defun render.make-producters-hidden (list  &key (columns 4) cut)
   (let ((reverse-lists (render.make-producters-reverse-lists-by-column list columns))
         (rs))
     (mapcar #'(lambda (v)
                 (let ((len cut))
                   (if (or (null cut)
                           (< (length v) cut))
                       (setf len (length v)))
                   (push (subseq v len) rs)))
             reverse-lists)
     (remove-if #'null rs)))


 (defmethod restas:render-object ((designer eshop-render) (object filter))
   (let ((request-get-plist (request-get-plist))
         (fltr-name  (name object))
         (grname (name (new-classes.parent object)))
         (products-list)
         (all-products-list))
     (setf all-products-list (if (getf request-get-plist :showall)
                                 (storage.get-filtered-products (new-classes.parent object) #'atom)
                                 (storage.get-filtered-products (new-classes.parent object) #'active)))
     (setf products-list (remove-if-not (func object) all-products-list))
     (if (null (getf request-get-plist :sort))
         (setf (getf request-get-plist :sort) "pt"))
     (if (getf (request-get-plist) :vendor)
         (setf products-list
               (remove-if-not #'(lambda (p)
                                  (vendor-filter-controller p (request-get-plist)))
                              products-list)))
     ;; (log5:log-for test "~&filter ~{~a|~}" request-get-plist)
     (with-sorted-paginator
         products-list
       request-get-plist
       (default-page
           (catalog:content
            (list :name (name object)
                  :breadcrumbs (catalog:breadcrumbs (new-classes.breadcrumbs object))
                  :menu (new-classes.menu object)
                  :rightblocks (append
                                (render.get-oneclick-filters (new-classes.parent object)
                                                             (getf request-get-plist :showall))
                                (rightblocks (new-classes.parent object) request-get-plist))
                  :subcontent (catalog:centerproduct
                               (list
                                :sorts (sorts request-get-plist)
                                :producers (render.show-producers all-products-list)
                                :accessories (catalog:accessories)
                                :pager pager
                                :products (loop
                                             :for product
                                             :in  paginated
                                             :collect (render.view product))))))
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



 (defmethod render.show-producers ((products list))
   (let* ((vendors (storage.get-vendors products))
          (url-parameters (request-get-plist))
          (veiws nil))
     (remf url-parameters :page)
     (maphash #'(lambda (k x)
                  (setf (getf url-parameters :vendor) (hunchentoot:url-encode k))
                  (push (list :vendor k
                              :cnt x
                              :link (format nil "?~a" (make-get-str url-parameters)))
                        veiws))
              vendors)
     (print (length veiws))
     (setf veiws (sort veiws #'string<= :key #'(lambda (v) (string-upcase (getf v :vendor)))))
     (catalog:producers
      (list
       :vendorblocks (render.make-producters-base
                     veiws :cut 3)
      :vendorhiddenblocks (render.make-producters-hidden
                           veiws :cut 3)))))


(defmethod restas:render-object ((designer eshop-render) (object group))
   (render.render object))
