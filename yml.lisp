(in-package #:yml)

;; Группы представляют собой лес для YML нам нужны не только сами
;; группы маркированные для выгрузки но и их родители
;; На самом деле нам нужно минимальные оставные деревья,
;; но будем выгружать полные деревья исключая только листья, если это нужно


(defparameter *yml-group-ids* (make-hash-table))

(defun yml-groups ()
  "Строим *yml-group-ids*"
  (let ((current-id 1))
    (clrhash *yml-group-ids*)
    (maphash #'(lambda(k g)
                 ;; Если группа имеет дочерние группы ИЛИ yml-show == true
                 (when (or
                        (not (null (group:childs g)))
                        (group:ymlshow g))
                   ;; Кладем ее в *yml-group-ids* и увеличиваем current-id
                   (setf (gethash current-id *yml-group-ids*) current-id )
                   (incf current-id)))
             trans:*group*)
    *yml-group-ids*))


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
                                  :for key
                                  :being :the hash-key
                                  :using (hash-value id)
                                  :in (yml-groups)
                                  :collect (list :id id
                                                 :name (group:name (gethash key trans:*group*))
                                                 :parent (if (null (group:parent (gethash key trans:*group*)))
                                                             0 ;; если это вершина дерева
                                                             (gethash
                                                              (group:key
                                                               (group:parent (gethash key trans:*group*)))
                                                              *yml-group-ids*))))
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
                                                            :category (gethash
                                                                       (group:key (product:parent product))
                                                                       *yml-group-ids*)
                                                            :picture  (let ((pics (product:get-pics product)))
                                                                        (if (null pics) nil (car pics)))
                                                            :name (product:name product)
                                                            :description (product:descr product)
                                                            )))))))))

