(in-package #:wolfor-stuff)

(defun range-filter (product request-get-plist opt-label opt-group-name opt-name)
  (let ((value-f (getf request-get-plist (read-from-string (format nil ":~a-f" opt-label))))
        (value-t (getf request-get-plist (read-from-string (format nil ":~a-t" opt-label))))
        (product-value 0))
    (mapcar #'(lambda (optgroup)
                (if (string= (eshop::name optgroup) opt-group-name)
                    (let ((options (eshop::options optgroup)))
                      (mapcar #'(lambda (opt)
                                  (if (string= (eshop::name opt) opt-name)
                                      (setf product-value
                                            (format nil "~a" (eshop::value opt)))))
                              options))))
            (eshop::optgroups product))
    (if (null product-value)
        (setf product-value "0"))
    (if (null value-f)
        (setf value-f "0"))
    (if (null value-t)
        (setf value-t "99999999"))
    (setf value-f (arnesi:parse-float (format nil "~as" value-f)))
    (setf value-t (arnesi:parse-float (format nil "~as" value-t)))
    (setf product-value (arnesi:parse-float (format nil "~as" product-value)))
    (if (and (<= value-f product-value)
             (>= value-t product-value))
        (print (list value-f value-t product-value opt-label opt-group-name opt-name)))
    ;; (arnesi:parse-float "")
    (and (<= value-f product-value)
         (>= value-t product-value))))

(defun checkbox-filter (p g) t)

(defun price-filter (product request-get-plist)
  (let ((price-f
         (parse-integer
          (getf request-get-plist :price-f)
          :junk-allowed t))
        (price-t
         (parse-integer
          (getf request-get-plist :price-t)
          :junk-allowed t))
        (product-price
         (eshop::price product)))
    (unless (integerp price-f)
      (setf price-f 0))
    (unless (integerp price-t)
      (setf price-t 999999999999))
    (print (list price-f price-t))
    (and
     (<= price-f product-price)
     (>= price-t
         product-price))))

(defun radio-filter (p g opts og o) t)


(defun num-active-product (products)
  (let ((num 0))
<<<<<<< HEAD
    (mapcar #'(lambda (p) (if (active p) (incf num))) products)
=======
    (mapcar #'(lambda (p)
                (if (product:active p)
                    (incf num)))
            products)
>>>>>>> wolfor-master
    num))

(defun sum-prices-product (products)
  (let ((sum 0))
<<<<<<< HEAD
    (mapcar #'(lambda (p) (if (active p) (setf sum (+ (price p) sum)))) products)
=======
    (mapcar #'(lambda (p)
                (if (product:active p)
                    (setf sum (+
                               (product:price p)
                               sum))))
            products)
>>>>>>> wolfor-master
    sum))

(defun get-products-list ()
  (let ((product-list))
<<<<<<< HEAD
    (maphash #'(lambda (k v) (setf product-list (cons v product-list ))) eshop::*storage* )
=======
    (maphash #'(lambda (k v)
                 (setf product-list (cons v product-list )))
             trans:*product* )
>>>>>>> wolfor-master
    product-list))

(defun get-group-routs ()
  (let ((routs))
    (maphash #'(lambda (k g)
<<<<<<< HEAD
                 (if (null (parent g))
                     (setf routs (cons g routs))))
             eshop::*storage*)
=======
                 (if (null (group:parent g))
                     (setf routs (cons g routs))))
             trans:*group*)
>>>>>>> wolfor-master
    routs))

(defun get-group-active ()
  (let ((routs))
<<<<<<< HEAD
    (maphash #'(lambda (k g) (if (active g) (setf routs (cons g routs)))) eshop::*storage*)
=======
    (maphash #'(lambda (k g)
                 (if (group:active g)
                     (setf routs (cons g routs))))
             trans:*group*)
>>>>>>> wolfor-master
    routs))

(defun get-group-ymlshow ()
  (let ((routs))
<<<<<<< HEAD
    (maphash #'(lambda (k g) (if (ymlshow g) (setf routs (cons g routs)))) eshop::*storage*)
    routs))

(defun show-group (g n)
  (format t "~&~Va~a: ~a  | yml:~a active:~a  продуктов:~a  (key:~a)" n "" (id g) (name g) (ymlshow g) (active g) (length (get-recursive-products g)) (key g))
  (mapcar #'(lambda (g) (show-group g (+ n 5))) (childs g))
  )

(defun num-product-in-groups (groups)
  (let ((num 0))
    (mapcar #'(lambda (g) (setf num (+ num (length (remove-if #'(lambda(g) (not (active g))) (products g)))))) groups)
=======
    (maphash #'(lambda (k g)
                 (if (group:ymlshow g)
                     (setf routs (cons g routs))))
             trans:*group*)
    routs))

(defun show-group (g n)
  (format t "~&~Va~a: ~a  | yml:~a active:~a  продуктов:~a  (key:~a)"
          n
          ""
          (group:id g)
          (group:name g)
          (group:ymlshow g)
          (group:active g)
          (length (group:get-recursive-products g))
          (group:key g))
  (mapcar #'(lambda (g)
              (show-group g (+ n 5)))
          (group:childs g)))

(defun num-product-in-groups (groups)
  (let ((num 0))
    (mapcar #'(lambda (g)
                (setf num (+ num
                             (length (remove-if
                                      #'(lambda(g) (not (group:active g)))
                                      (group:products g))))))
            groups)
>>>>>>> wolfor-master
    num))



(defun num-active-product-with-bad-price ()
  (length (remove-if #'(lambda(p) (or (not (active p))
                                      (and (not (null (price p)))
                                           (> (price p) 0)))) (get-products-list))))

(defun statistics ()
  (mapcar #'(lambda (g) (show-group g 0)) (get-group-routs))
  (format t "~&Количество товаров на сайте: ~a" (list-length (get-products-list)))
  (format t "~&Количество активных товаров: ~a" (num-active-product (get-products-list)))
  (format t "~&Количество товаров для YML: ~a" (num-product-in-groups (get-group-ymlshow)))
  (format t "~&Средняя цена товара: ~5$" (let ((n (num-active-product (get-products-list))))
                                           (if (not (= n 0))
                                               (/ (sum-prices-product (get-products-list))
                                                  n)
                                               0)))
<<<<<<< HEAD
  (format t "~&Количество групп: ~a" (hash-table-count eshop::*storage*))
=======
  (format t "~&Количество групп: ~a" (hash-table-count trans:*group*))
>>>>>>> wolfor-master
  (format t "~&Количество активных групп: ~a" (length (get-group-active)))
  (format t "~&Количество YML групп: ~a" (length (get-group-ymlshow)))
  (format t "~&Количество активных товаров с нулевой ценой: ~a" (num-active-product-with-bad-price))
  (format t "~&~a" "")
  )


<<<<<<< HEAD
;; (let ((num 0))
;;   (maphash #'(lambda (k product)
;;                (if (and (not (null (product:parent product)))
;;                         (group:ymlshow (product:parent product))
;;                         (product:active product)
;;                         (not (null (product:price product)))
;;                         (> (product:price product) 0))
;;                    (incf num)))
;;            trans:*product*)
;;   num)


(defun num-products-in-group (g)
  (let ((cnt 0))
    (maphash #'(lambda (k v)
                 (if (equal g (parent v))
                     (incf cnt)))
             eshop::*storage*)
    cnt))

;; (maphash #'(lambda (k v)
;;              (if (not (= (num-products-in-group v)
;;                          (length (group:products v))))
;;                  (format t "~&~a ~a:~a"
;;                          (group:key v)
;;                          (num-products-in-group v)
;;                          (length (group:products v)))))
;;          trans:*group*)

;; (length
;;  (mapcar #'(lambda (v)
;;              (format nil "> ~a" (product:parent v)))
;;          (group:products (gethash "pamyat-dlya-noutbukov" trans:*group*))))

=======
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


  (defun num-products-in-group (g)
    (let ((cnt 0))
      (maphash #'(lambda (k v)
                   (if (equal g (product:parent v))
                       (incf cnt)))
               trans:*product*)
      cnt))

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
>>>>>>> wolfor-master
