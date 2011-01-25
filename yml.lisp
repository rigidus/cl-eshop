(in-package #:yml)

;; Группы представляют собой лес для YML нам нужны не только сами
;; группы маркированные для выгрузки но и их родители
;; На самом деле нам нужно минимальные оставные деревья,
;; но будем выгружать полные деревья исключая только листья, если это нужно
(defun yml-groups ()
(let ((groups))
  (maphash #'(lambda(k g) (if (or
                               (not (null (group:childs g)))
                               (group:ymlshow g))
                            (setf groups (cons g groups)))) trans:*group*)
  groups))


(funcall cl-eshop:*dispatcher*
         `((string= "/yml" (service:request-str))
           ,#'(lambda ()
                (setf (hunchentoot:content-type*) "application/xml; charset=utf-8")
                (yml:xml (list :datetime (get-date-time)
                               :marketname "ЦиFры 320-8080"
                               :marketcompany "ЦиFры 320-8080"
                               :marketurl "http://www.320-8080.ru/"
                               :categoryes
                               (loop
                                  :for g
                                  :in (yml-groups)
                                  :collect (list :id (group:id g)
                                                 :name (group:name g)
                                                 :parent (if (null (group:parent g))
                                                             -1 ;; если это вершина дерева
                                                             (group:id (group:parent g)))))


                               :offers (format nil "~{~a~}"
                               (loop
                                  :for product
                                  :being :the hash-values
                                  :in trans:*product*
                                  ;;продукт должен находиться в группе маркированной как ymlshow
                                  ;;быть активным и иметь не нулевую цену
                                  :when (and (not (null (product:parent product)))
                                             (group:ymlshow (product:parent product))
                                             (product:active product)
                                             (not (null (product:price product)))
                                             (> (product:price product) 0))
                                  :collect (yml:offer (list :articul (product:articul product)
                                                            :price (product:price product)
                                                            :category (group:id (product:parent product))
                                                            :picture  (let ((pics (product:get-pics product)))
                                                                        (if (null pics) nil (car pics)))
                                                            :name (product:name product)
                                                            :description (product:descr product)
                                                            )))))))))
