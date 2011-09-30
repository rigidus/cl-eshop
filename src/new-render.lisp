

(defmethod restas:render-object ((designer eshop-render) (object product))
  (let ((pics (get-pics (articul object)))
        (price (+ (siteprice object) (delta-price object)))
        (name (or (name-seo product) (name-provider product))))
    (multiple-value-bind (diffprice procent)
        (get-procent (+ (siteprice object) (delta-price object)) (siteprice object))
      (default-page
          (product:content (list :menu (menu object)
                                 :breadcrumbs (product:breadcrumbs (breadcrumbs object))
                                 :articul (articul object)
                                 :name name
                                 :siteprice (siteprice object)
                                 :storeprice (price)
                                 :bestprice (> (delta-price object) 0)
                                 :formatsiteprice (get-format-price (siteprice object))
                                 :formatstoreprice (get-format-price price)
                                 :equalprice (= (delta-price object) 0)
                                 :diffprice diffprice
                                 :procent procent
                                 :subst (format nil "/~a" (articul object))
                                 :pics (cdr pics)
                                 :firstpic (if (null pics) nil (car pics))

                                 ;;!!!!!!
                                 :optlist (let ((optlist
                                                 (remove-if
                                                  #'null
                                                  (mapcar #'(lambda (optgroup)
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
                                 :shortdescr (seo-text object)
                                 :dontshdev (gethash (articul object) *special-products*)
                                 :seotextflag (not (null (seo-text object)))
                                 :predzakaz (preorder object)
                                 :addproductcart (if (preorder object)
                                                     (soy.buttons:add-predzakaz (list :articul (articul object)))
                                                     (soy.buttons:add-product-cart (list :articul (articul object)
                                                                                         :name name
                                                                                         :pic (if (null pics) nil (car pics))
                                                                                         :siteprice (siteprice object)
                                                                                         :price price
                                                                                         :deliveryprice (delivery-price object))))
                                 :addoneclick (if (not (preorder object))
                                                  (soy.buttons:add-one-click (list :articul (articul object)))))
                           :keywords (format nil "~a" name )
                           :description (format nil "купить ~a в ЦиFры 320-8080 по лучшей цене с доставкой по Санкт-Петербургу"
                                                name)
                           :title (string-convertion-for-title
                                   (format nil "~a купить в ЦиFры - цена, фотография и описание, продажа ~a с гарантией и доставкой в ЦиFры 320-8080"
                                           name name)))))))
