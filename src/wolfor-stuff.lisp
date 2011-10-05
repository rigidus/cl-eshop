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

(defun get-products-list ()
  (let ((product-list))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (if (equal (type-of v)
                            'eshop::product)
                     (setf product-list (cons v product-list ))))
             eshop:*storage* )
    product-list))

(defun get-group-routs ()
  (let ((routs))
    (maphash #'(lambda (k g)
                 (declare (ignore k))
                 (if (and (equal (type-of g)
                                 'eshop::group)
                          (null (eshop::parent g)))
                     (setf routs (cons g routs))))
             eshop:*storage*)
    routs))

(defun get-group-active ()
  (let ((routs))
    (maphash #'(lambda (k g)
                 (declare (ignore k))
                 (if (and (equal (type-of g)
                                 'eshop::group)
                          (eshop::active g))
                     (setf routs (cons g routs))))
             eshop:*storage*)
    routs))

;; (defun get-group-ymlshow ()
;;   (let ((routs))
;;     (maphash #'(lambda (k g)
;;                  (declare (ignore k))
;;                  (if (and (equal (type-of g)
;;                                  'eshop::group)
;;                           (eshop::ymlshow g))
;;                      (setf routs (cons g routs))))
;;              eshop:*storage*)
;;     routs))

(defun show-group (g n)
  (format t "~&~Va: ~a  | yml:~a active:~a  продуктов:~a  (key:~a)"
          n
          ""
          (eshop::name g)
          (eshop::ymlshow g)
          (eshop::active g)
          (length (eshop::get-recursive-products g))
          (eshop::key g))
  (mapcar #'(lambda (g)
              (show-group g (+ n 5)))
          (eshop::childs g)))

(defun num-product-in-groups (groups)
  (let ((num 0))
    (mapcar #'(lambda (g)
                (setf num (+ num
                             (length (remove-if
                                      #'(lambda(g)
                                          (not (eshop::active g)))
                                      (eshop::products g))))))
            groups)
    num))



(defun num-active-product-with-bad-price ()
  (length (remove-if #'(lambda(p) (or (not (eshop::active p))
                                      (and (not (null (eshop::price p)))
                                           (> (eshop::siteprice p) 0))))
                     (get-products-list))))

(defun get-group-ymlshow()
  (let ((result))
    (maphash #'(lambda(k g)
                 (declare (ignore k))
                 (if (and (equal (type-of g)
                                 'eshop::group)
                          (eshop::ymlshow g))
                     (push g result)))
             eshop:*storage*)
    result))

(defun get-groups-products(groups)
  (let ((result))
    (mapcar #'(lambda(g)
                (setf result
                      (append result
                              (eshop::products g))))
            groups)
    result))

(defun statistics ()
  (mapcar #'(lambda (g) (show-group g 0)) (get-group-routs))
  (format t "~&Количество товаров на сайте: ~a" (list-length (get-products-list)))
  (format t "~&Количество активных товаров: ~a" (num-active-product (get-products-list)))
  (format t "~&Количество товаров для YML: ~a" (num-product-in-groups (get-group-ymlshow)))
  (format t "~&Средняя цена товара в магазине: ~5$" (let ((n (num-active-product (get-products-list))))
                                           (if (not (= n 0))
                                               (/ (sum-prices-product (get-products-list))
                                                  n)
                                               0)))
  (format t "~&Средняя цена товара на сайте: ~5$" (let ((n (num-active-product (get-products-list))))
                                           (if (not (= n 0))
                                               (/ (sum-siteprices-product (get-products-list))
                                                  n)
                                               0)))
  (format t "~&Количество групп: ~a" (let ((num 0))
                                         (maphash #'(lambda(k v)
                                                      (declare (ignore k))
                                                      (if (equal (type-of v)
                                                                 'eshop::group)
                                                          (incf num)))
                                                  eshop:*storage*)
                                         num))
  (format t "~&Количество активных групп: ~a" (length (get-group-active)))
  (format t "~&Количество YML групп: ~a" (length (get-group-ymlshow)))
  (format t "~&Количество активных товаров с нулевой ценой: ~a" (num-active-product-with-bad-price))
  (format t "~&~a" ""))

;; (let ((num 0))
;;   (maphash
;;    #'(lambda (k product)
;;        (if (and (not (null (eshop::parent product)))
;;                 (group:ymlshow (eshop::parent product))
;;                 (eshop::active product)
;;                 (not (null (eshop::price product)))
;;                 (> (eshop::price product) 0))
;;            (incf num)))
;;    trans:*product*) num)


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
;;              (format nil "> ~a" (eshop::parent v)))
;;          (group:products (gethash "pamyat-dlya-noutbukov" trans:*group*))))




;; :keywords (format nil "" )
;; :description (format nil "")
;; :title (format nil "")

;; (defvar *test-request*)


(defun set-recursive-yml (group flag)
  (mapcar #'(lambda (g)
              (format t "~&~a" (eshop::name g))
              (setf (eshop::ymlshow g) flag)
              (set-recursive-yml g flag))
          (eshop::childs group)))


(car (remove-if-not #'(lambda (v) (search "8008" (sb-thread:thread-name v))) (sb-thread:list-all-threads)))
