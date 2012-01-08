(in-package #:eshop)

;;методы для перевода старых значений в новые
(defun transform.serialize-old-product (object)
  (format nil "{\"key\":~a,\"articul\":~a,\"nameProvider\":~a,\"nameSeo\":~a,\"siteprice\":~a,\"deltaPrice\":~a,\"dateModified\":~a,\"dateCreated\":~a,\"bonuscount\":~a,\"preorder\":~a,\"newbie\":~a,\"sale\":~a,\"seoText\":~a,\"countTransit\":~a,\"countTotal\":~a,\"optgroups\":~a,\"deliveryPrice\":~a,\"parents\":~a}~%"
          (format nil "\"~a\"" (articul object))
          (encode-json-to-string (articul object))
          (format nil "\"~a\"" (object-fields.string-escaping (name object)));;name-provider
          (format nil "\"~a\"" (object-fields.string-escaping (realname object)));;name-seo
          (encode-json-to-string (siteprice object));;siteprice
          (encode-json-to-string (- (price object) (siteprice object)));;delta-price
          (encode-json-to-string nil) ;;DBG (date-modified object));;date-modified
          (encode-json-to-string nil) ;;DBG (date-created object));;date-created
          (encode-json-to-string (bonuscount object));;bonuscount
          (encode-json-to-string (predzakaz object));;preorder
          (encode-json-to-string (newbie object));;newbie
          (encode-json-to-string (sale object));;sale
          (format nil "\"~a\"" (object-fields.string-escaping (object-fields.string-replace-newlines (shortdescr object))));;seo-text
          (encode-json-to-string (if (active object)
                                     (count-transit object)
                                     0));;count-transit
          (encode-json-to-string (if (active object)
                                     (count-total object)
                                     0));;count-total
          ;; (if (null (optgroups object));;optgroups
              (format nil "null")
              ;; (format nil " [~{~a~^,~}]"
              ;;         (mapcar #'(lambda (optgroup)
              ;;                     (serialize optgroup))
              ;;                 (optgroups object)))
              ;; )
          (encode-json-to-string nil) ;;DBG (delivery-price object));;delivery-price
          (aif (parent object)
               (format nil "[ \"~a\" ]" (key it))
               "null"))) ;;parent


(defun transform.serialize-old-group (object)
  (format nil "{\"key\":~a,\"parents\":~a,\"name\":~a,\"active\":~a,\"order\":~a,\"ymlshow\":~a,\"pic\":~a,\"icon\":~a,\"deliveryPrice\":~a,\"seoText\":~a,\"fullfilter\":~a,\"keyoptions\":~a, \"vendorsSeo\":~a}~%"
          (format nil "\"~a\"" (key object)) ;;key
          (if (not (null (parent object)))
            (format nil "[ \"~a\" ]" (key (parent object)))
            (format nil "null")) ;;parents
          (format nil "\"~a\"" (object-fields.string-escaping (object-fields.string-delete-newlines (name object))));;name
          (encode-json-to-string (active object));;active
          (encode-json-to-string (order object));;order
          (encode-json-to-string (ymlshow object));;ymlshow
          (format nil "\"~a\"" (object-fields.string-escaping (pic object)));;pic
          (format nil "\"~a\"" (object-fields.string-escaping (icon object)));;icon
          "null" ;;(encode-json-to-string (delivery-price object));;deliveryPrice
          (format nil "\"~a\"" (object-fields.string-escaping (object-fields.string-replace-newlines (descr object))));;seo-text
          "\"\"" ;; (format nil "\"~a\"" (object-fields.string-escaping (object-fields.string-replace-newlines (raw-fullfilter object))))
          ;;raw-fullfilter
          (if (not (null (keyoptions object)))
              (format nil "[~{~a~^,~}]"
                      (loop :for item :in (keyoptions object) :collect
                         (format nil "{\"optgroup\":\"~a\",\"optname\":\"~a\"}"
                                 (getf item :optgroup)
                                 (getf item :optname))))
              (format nil "null"))
          (when (vendors object)
            (object-fields.textedit-hashtable-field-serialize (vendors object)))))

(defun transform.serialize-old-filter (object)
  (format nil "{\"key\":~a,\"parents\":~a,\"name\":~a,\"func-string\":~a}~%"
          (format nil "\"~a\"" (key object)) ;;key
          (if (not (null (parent object)))
            (format nil "[ \"~a\" ]" (key (parent object)))
            (format nil "null")) ;;parents
          (format nil "\"~a\"" (object-fields.string-escaping (name object)));;name
          (format nil "\"~a\""
                  (object-fields.string-escaping
                   (object-fields.string-replace-newlines (func-string object))));;func-string
          ))





(defun transform.print-to-file (text pathname)
  (with-open-file (file pathname
                        :direction :output
                        :if-exists :supersede
                        :external-format :utf-8)
    (format file text)))

(defun transform.serialize-all-products-to-file (pathname)
  (with-open-file (file pathname
                        :direction :output
                        :if-exists :supersede
                        :external-format :utf-8)
    (let ((cnt 0))
      (maphash #'(lambda (key value)
                   (declare (ignore key))
                   (when (and (not (null value)) (equal (type-of value) 'product))
                     (format t "~a ~%" cnt)
                     (setf cnt (+ 1 cnt))
                     (format file "~a" (transform.serialize-old-product value))))
               *storage*))))

(defun transform.serialize-all-groups-to-file (pathname)
  (with-open-file (file pathname
                        :direction :output
                        :if-exists :supersede
                        :external-format :utf-8)
    (let ((cnt 0))
      (maphash #'(lambda (key value)
                   (declare (ignore key))
                   (when (and (not (null value)) (equal (type-of value) 'group))
                     (format t "~a ~%" cnt)
                     (setf cnt (+ 1 cnt))
                     (format file "~a" (transform.serialize-old-group value))))
               *storage*))))

(defun transform.serialize-all-filters-to-file (pathname)
  (with-open-file (file pathname
                        :direction :output
                        :if-exists :supersede
                        :external-format :utf-8)
    (let ((cnt 0))
      (maphash #'(lambda (key value)
                   (declare (ignore key))
                   (when (and (not (null value)) (equal (type-of value) 'filter))
                     (format t "~a ~%" cnt)
                     (setf cnt (+ 1 cnt))
                     (format file "~a" (transform.serialize-old-filter value))))
               *storage*))))

