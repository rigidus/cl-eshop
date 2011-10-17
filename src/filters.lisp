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
              (key (new-classes.parent filter))
              (key filter)
              (name filter)
              (get-filtered-product-list-len object filter))))

;;количество непустых фильтров у группы
(defun num-nonempty-filters (object)
  (length (remove-if #'(lambda (fil) (is-empty-filtered-list object fil))
                     (filters object))))


;; (let* ((child (gethash "komputery" (storage *global-storage*))))
;;   (remove-if #'(lambda (filter)
;;                  (wlog (func-string filter))
;;                  (is-empty-filtered-list child filter))
;;              (filters child)))

;; (let* ((gr (gethash "noutbuki" (storage *global-storage*)))
;;        (filt (car (filters gr)))
;;        (prod  (car (get-recursive-products gr))))
;;   (wlog (name gr))
;;   (wlog (func-string filt))
;;   (wlog (name-seo prod))
;;   (funcall (func filt) prod))
