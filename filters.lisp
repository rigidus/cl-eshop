(in-package #:eshop)

;;Возвращает длину списка активных продуктов-потомков подходящих под фильтр
(defun get-filtered-product-list-len (object filter)
  (length (remove-if-not (func filter)
                         (remove-if-not #'active
                                        (get-recursive-products object)))))

(defun is-empty-filtered-list (object filter)
  (= 0 (get-filtered-product-list-len object filter)))


;;Составление строки для представления фильтров в 1 клик на странице с fullfilter
(defun make-string-filter (object filter &optional itsme)
  (if itsme
      (format nil "<b>~a</b> (~a)<br/>"
              (name filter)
              (get-filtered-product-list-len object filter))
      (format nil "<a class=\"rightfilter\" href=\"/~a/~a\">~a</a> (~a)<br/>"
              (key (parent filter))
              (key filter)
              (name filter)
              (get-filtered-product-list-len object filter))))

;;количество непустых фильтров у группы
(defun num-nonempty-filters (object)
  (length (remove-if #'(lambda (fil) (is-empty-filtered-list object fil)) (filters object))))
