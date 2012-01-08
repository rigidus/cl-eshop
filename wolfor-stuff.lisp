(defpackage #:wolfor-stuff
  (:use #:cl))

(in-package #:wolfor-stuff)


(defun num-active-product (products)
  (let ((num 0))
    (mapcar #'(lambda (p)
                (if (eshop::active p)
                    (incf num)))
            products)
    num))

(defun sum-siteprices-product (products)
  (let ((sum 0))
    (mapcar #'(lambda (p)
                (if (eshop::active p)
                    (setf sum (+
                               (eshop::siteprice p)
                               sum))))
            products)
    sum))

(defun sum-prices-product (products)
  (let ((sum 0))
    (mapcar #'(lambda (p)
                (if (eshop::active p)
                    (setf sum (+
                               (eshop::price p)
                               sum))))
            products)
    sum))

;; (defun get-products-list ()
;;   (let ((product-list))
;;     (maphash #'(lambda (k v)
;;                  (setf product-list (cons v product-list )))
;;              trans:*product* )
;;     product-list))

;; (defun get-group-routs ()
;;   (let ((routs))
;;     (maphash #'(lambda (k g)
;;                  (if (null (group:parent g))
;;                      (setf routs (cons g routs))))
;;              trans:*group*)
;;     routs))

;; (defun get-group-active ()
;;   (let ((routs))
;;     (maphash #'(lambda (k g)
;;                  (if (group:active g)
;;                      (setf routs (cons g routs))))
;;              trans:*group*)
;;     routs))

;; (defun get-group-ymlshow ()
;;   (let ((routs))
;;     (maphash #'(lambda (k g)
;;                  (if (group:ymlshow g)
;;                      (setf routs (cons g routs))))
;;              trans:*group*)
;;     routs))

;; (defun show-group (g n)
;;   (format t "~&~Va~a: ~a  | yml:~a active:~a  продуктов:~a  (key:~a)"
;;           n
;;           ""
;;           (group:id g)
;;           (group:name g)
;;           (group:ymlshow g)
;;           (group:active g)
;;           (length (group:get-recursive-products g))
;;           (group:key g))
;;   (mapcar #'(lambda (g)
;;               (show-group g (+ n 5)))
;;           (group:childs g)))

;; (defun num-product-in-groups (groups)
;;   (let ((num 0))
;;     (mapcar #'(lambda (g)
;;                 (setf num (+ num
;;                              (length (remove-if
;;                                       #'(lambda(g) (not (group:active g)))
;;                                       (group:products g))))))
;;             groups)
;;     num))



;; (defun num-active-product-with-bad-price ()
;;   (length (remove-if #'(lambda(p) (or (not (product:active p))
;;                                       (and (not (null (product:price p)))
;;                                            (> (product:price p) 0)))) (get-products-list))))

;; (defun statistics ()
;;   (mapcar #'(lambda (g) (show-group g 0)) (get-group-routs))
;;   (format t "~&Количество товаров на сайте: ~a" (list-length (get-products-list)))
;;   (format t "~&Количество активных товаров: ~a" (num-active-product (get-products-list)))
;;   (format t "~&Количество товаров для YML: ~a" (num-product-in-groups (get-group-ymlshow)))
;;   (format t "~&Средняя цена товара: ~5$" (let ((n (num-active-product (get-products-list))))
;;                                            (if (not (= n 0))
;;                                                (/ (sum-prices-product (get-products-list))
;;                                                   n)
;;                                                0)))
;;   (format t "~&Количество групп: ~a" (hash-table-count trans:*group*))
;;   (format t "~&Количество активных групп: ~a" (length (get-group-active)))
;;   (format t "~&Количество YML групп: ~a" (length (get-group-ymlshow)))
;;   (format t "~&Количество активных товаров с нулевой ценой: ~a" (num-active-product-with-bad-price))
;;   (format t "~&~a" "")
;;   )

;; (let ((num 0))
;;   (maphash
;;    #'(lambda (k product)
;;        (if (and (not (null (product:parent product)))
;;                 (group:ymlshow (product:parent product))
;;                 (product:active product)
;;                 (not (null (product:price product)))
;;                 (> (product:price product) 0))
;;            (incf num)))
;;    trans:*product*) num)


;; (defun num-products-in-group (g)
;;   (let ((cnt 0))
;;     (maphash #'(lambda (k v)
;;                  (if (equal g (product:parent v))
;;                      (incf cnt)))
;;              trans:*product*)
;;     cnt))

;; (maphash #'(lambda (k v)
;;              (if (not (= (num-products-in-group v)
;;                          (length (group:products v))))
;;                  (format t "~&~a  ~a:~a"
;;                          (group:key v)
;;                          (num-products-in-group v)
;;                          (length (group:products v)))))
;;          trans:*group*)

;; (length
;;  (mapcar #'(lambda (v)
;;              (format nil "> ~a" (product:parent v)))
;;          (group:products (gethash "pamyat-dlya-noutbukov" trans:*group*))))


;; (defun store-products ()
;;   (let ((cnt 0))
;;     (maphash #'(lambda (k v)
;;                  (declare (ignore k))
;;                  (if (equal (type-of v) 'eshop::product)
;;                      (progn
;;                        (incf cnt)
;;                        (eshop::serialize v))))
;;              eshop:*storage*)
;;     (format t "~& Num of products was serializes: ~a" cnt)))

;; :keywords (format nil "" )
;; :description (format nil "")
;; :title (format nil "")

;; (defvar *test-request*)


