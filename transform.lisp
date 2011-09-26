(in-package #:eshop)

;;методы для перевода старых значений в новые
(defun transform.serialize-old-product (product)
  (format nil "{~% \"key\": ~a,~%  \"articul\": ~a,~%   \"nameProvider\": ~a,~%   \"nameSeo\": ~a,~%   \"siteprice\": ~a,~%   \"deltaPrice\": ~a,~%   \"dateModified\": ~a,~%  \"dateCreated\": ~a,~%  \"bonuscount\": ~a,~% \"preorder\": ~a,~%  \"newbie\": ~a,~%   \"sale\": ~a,~%   \"seoText\": ~a,~%   \"countTransit\": ~a,~%   \"countTotal\": ~a,~%   \"optgroups\": ~a, \"deliveryPrice\": ~a,~% \"parents\": ~a~%}"
          (format nil "\"~a\"" (articul object))
          (encode-json-to-string (articul object))
          (format nil "\"~a\"" (object-fields.string-escaping (name object)));;name-provider
          (format nil "\"~a\"" (object-fields.string-escaping (realname object)));;name-seo
          (encode-json-to-string (price object));;siteprice
          (encode-json-to-string (siteprice object));;delta-price
          (encode-json-to-string (date-modified object));;date-modified
          (encode-json-to-string (date-created object));;date-created
          (encode-json-to-string (bonuscount object));;bonuscount
          (encode-json-to-string (predzakaz object));;preorder
          (encode-json-to-string (newbie object));;newbie
          (encode-json-to-string (sale object));;sale
          (format nil "\"~a\"" (object-fields.string-escaping (descr object)));;seo-text
          (encode-json-to-string (count-transit object));;count-transit
          (encode-json-to-string (count-total object));;count-total
          (if (null (optgroups object));;optgroups
              (format nil " null")
              (format nil " [    ~{~a~^,~}~%   ]~%"
                      (mapcar #'(lambda (optgroup)
                                  (serialize optgroup))
                              (optgroups object)))
              )
          (encode-json-to-string (delivery-price object));;delivery-price
          ;;parents
          ))

