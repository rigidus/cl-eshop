(in-package #:eshop)


(defun write-products-report (stream)
  (format stream "~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~%"
          "артикул" "цена магазина" "цена сайта" "имя" "имя real" "имя yml" "seo текст"
          "фотографии" "характеристики" "активный" "группа" "родительская группа"
          "secret" "DTD" "vendor")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               ;; (wlog stream)
               (let ((id "нет") (name "нет") (name-real "нет") (name-yml "нет")
                     (desc "нет") (img "нет") (options "нет") (active "нет")
                     (group-name "нет") (parent-group-name "нет") (secret "нет"))
                 (when (equal (type-of v)
                              'product)
                   (setf id (articul v))
                   (setf name (stripper (name-provider v)))
                   (setf name-real (stripper (name-seo v)))
                   (with-option1 v "Secret" "Yandex"
                                 (setf name-yml (stripper (getf option :value))))
                   (setf desc (if (and (not (null (seo-text v)))
                                       (not (string= "" (stripper (seo-text v)))))
                                  "есть"
                                  "нет"))
                   (setf img (length (get-pics (articul v))))
                   (setf options (if (is-valide-option v)
                                     "есть"
                                     "нет"))
                   (setf active (if (active v)
                                    "да"
                                    "нет"))
                   (setf group-name (if (not (null (new-classes.parent v)))
                                        (stripper (name (new-classes.parent v)))))
                   (setf parent-group-name (if (and (not (null (new-classes.parent v)))
                                                    (not (null (new-classes.parent (new-classes.parent v)))))
                                               (stripper (name (new-classes.parent (new-classes.parent v))))))
                   (setf secret "Нет")
                   (with-option1 v "Secret" "Checked"
                                 (setf secret (getf option :value)))
                   (if (string= (format nil "~a" id) "172466")
                       (wlog v))
                   (format stream "~a;~a;~a;\"~a\";\"~a\";\"~a\";~a;~a;~a;~a;\"~a\";\"~a\";~a;~a;~a~%"
                           id (price v) (siteprice v) name name-real
                           name-yml desc img options active group-name
                           parent-group-name secret
                           (gethash (articul v) *xls.product-table*)
                           (vendor v))
                   )))
           (storage *global-storage*)))

(defun is-valide-option (product)
  (let ((flag nil))
    (mapcar #'(lambda (v) (mapcar #'(lambda (l)
                                      (when (and (not (equal (getf v :name) "Secret"))
                                                 l
                                               (getf l :value)
                                               (not (equal (getf l :value) ""))
                                               (not (equal (getf l :value) "Производитель"))
                                               (not (equal (getf l :value) "Модель")))
                                        ;; (print (getf l :value))
                                        (setf flag t)))
                       (getf v :options)))
        (optgroups product))
  flag
  ))

(defun write-groups (stream)
  (format stream "~a;~a;~a;~a;%"
          "Название категории"
          "url страницы"
          "Active"
          "seo-text")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (equal (type-of v)
                              'group)
                 (format stream "\"~a\";http://www.320-8080.ru/~a;~a;~a;~%"
                           (stripper (name v))
                           (key v)
                           (if (active v)
                               "yes"
                               "no")
                           (if (and (not (null (descr v)))
                                       (not (string= "" (stripper (descr v)))))
                                  "yes"
                                  "no"))
                   ))
           (storage *global-storage*)))

(defun write-groups-active-product-num (stream)
  (format stream "~a;~a;~a;~a;~%"
          "Название категории"
          "url страницы"
          "Active"
          "кол-во товаров")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (equal (type-of v)
                              'group)
                 (format stream "\"~a\";http://www.320-8080.ru/~a;~a;~a;~%"
                           (stripper (name v))
                           (key v)
                           (if (active v)
                               "yes"
                               "no")
                           (length (remove-if-not #'active (get-recursive-products v))))))
           (storage *global-storage*)))



(defun write-products (stream)
  (let ((vendor-name)
        (desc))
    (format stream "~a;~a;~a;~a;~a;~a;~%"
            "Название категории"
            "Брэнд"
            "Название товара"
            "url страницы"
            "Active"
            "seo-text")
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (equal (type-of v)
                              'product)
                   (setf vendor-name "Нет")
                   (with-option v "Общие характеристики" "Производитель"
                                (setf vendor-name (value option)))
                   (setf desc (if (and (not (null (shortdescr v)))
                                       (not (string= "" (stripper (shortdescr v)))))
                                  "yes"
                                  "no"))
                   (format stream "\"~a\";\"~a\";\"~a\";http://www.320-8080.ru/~a;~a;~a;~%"
                           (if (not (null (parent v)))
                               (stripper (name (parent v)))
                               "Нет категории")
                           (stripper vendor-name)
                           (stripper (realname v))
                           (articul v)
                           (if (active v)
                               "yes"
                               "no")
                           desc)))
             (storage *global-storage*))))


(defun write-vendors (stream)
  (format stream "~a;~a;~a;~a;~a;~%"
          "Название категории"
          "Брэнд"
          "url страницы"
          "Active"
          "seo-text")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (and (equal (type-of v)
                            'group)
                          (null (childs v)))
                 (mapcar #'(lambda (vendor)
                             (format stream "\"~a\";\"~a\";http://www.320-8080.ru/~a?vendor=~a;~a;~a;~%"
                                     (stripper (name v))
                                     (stripper (car vendor))
                                     (key v)
                                     (stripper (car vendor))
                                     "yes"
                                     (let ((desc (gethash (car vendor) (vendors v))))
                                       (if (and (not (null desc))
                                                (not (string= "" desc)))
                                           "yes"
                                           "no"))))
                         (producersall (make-producers v)))))
           (storage *global-storage*)))

(defun create-report (file-name report-func)
  (let ((filename (format nil "~a/~a" *path-to-dropbox* file-name)))
    (with-open-file
        (stream filename :direction :output :if-exists :supersede :external-format :cp1251)
      (print stream)
      (funcall report-func stream)
      )))


(defun check-valid-siteprice ()
  (format t "~&~a;\"~a\";~a;~a;~a;"
          "Артикул"
          "Имя"
          "Активный"
          "Цена магазина"
          "Цена 3208080")
  (maphash #'(lambda(k v)
               (declare (ignore k))
               (when (equal (type-of v)
                            'product)
                 (if (< (price v)
                        (siteprice v))
                     (format t "~&~a;\"~a\";~a;~a;~a;"
                             (articul v)
                             (stripper (name v))
                             (if (active v)
                                 "yes"
                                 "no")
                             (price v)
                             (siteprice v)))))
           (storage *global-storage*)))

(defun show-last-history (stream)
  (when (not (null *history*))
    ;; Делаем все продукты неактивными
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (equal (type-of v) 'product)
                   (setf (active v) nil)))
             (storage *global-storage*))
    (loop :for packet :in (reverse (caddr (car *history*))) :do
       (format stream "~a" (sb-ext:octets-to-string packet :external-format :cp1251)))))





(defun error-price-report ()
  (let ((products (remove-if-not #'(lambda (v) (< (price v) (siteprice v)))
                                 (remove-if-not #'eshop::active (wolfor-stuff::get-products-list)))))
    (when products
      (gateway-send-error-mail (list "web_design@alpha-pc.com"
                                     "wolforus@gmail.com"
                                     "slamly@gmail.com")
                               (format nil "~&Цена на сайте выше цены в магазине: ~a<br/>~{~&~a<br/>~}"
                                       (length products)
                                       (mapcar #'(lambda (v)
                                                   (format nil "~&<a href=\"http://www.320-8080.ru/~a\">~a</a>:~a | siteprice:~a price:~a"
                                                           (articul v)
                                                     (articul v)
                                                     (name v)
                                                     (siteprice v)
                                                     (price v)))
                                               products))
                               "Siteprice > Price"))))


                          ;; (setf (active (gethash "160420" (storage *global-storage*))) nil)
                          ;; (serialize (gethash "160420" (storage *global-storage*)))
                          ;; (setf (active (gethash "165359" (storage *global-storage*))) nil)
                          ;; (serialize (gethash "165359" (storage *global-storage*)))
                          ;; (setf (active (gethash "165360" (storage *global-storage*))) nil)
                          ;; (serialize (gethash "165360" (storage *global-storage*)))
;; (setf (active (gethash "157499" (storage *global-storage*))) nil)
;; (serialize (gethash "157499" (storage *global-storage*)))
;; (setf (active (gethash "153599" (storage *global-storage*))) nil)
;; (serialize (gethash "153599" (storage *global-storage*)))



(defparameter *special-products* (make-hash-table :test #'equal))



(defun post-proccess-gateway ()
    (mapcar #'(lambda (v)
                (let ((p (gethash v (storage *global-storage*)))
                  ;; (p (make-instance 'product
                  ;;                   :key v
                  ;;                   :articul v))
                  )
              ;; (if p1
                  ;; (setf p p1))
              (when (not (null p))
                  (setf (preorder p) t)
                  (setf (active p) t)
                  ;; (serialize p)
                  (setf (gethash v (storage *global-storage*)) p)
                  (setf (gethash v *special-products*) p))))
        (list
         "711265"
         "834786"
         "938111"
         "777888"
         "888777"
         "999111"
         "999777"
         ))
  (mapcar #'(lambda (v)
              (let ((p (gethash v (storage *global-storage*))))
                (when (not (null p))
                  (setf (preorder p) t)
                  ;; (serialize p)
                  )))
          (list "166545"
                ;; "166578"
              "166579"
              "166580"
              "166581"
              "167530"
              "167531"
              "167532"
              "167533"
              "167534"
              "167535"))


  ;; (maphash #'(lambda (k v)
  ;;              (declare (ignore k))
  ;;              (when (equal (type-of v)
  ;;                             'group)
  ;;                (when (not (delivery-price v))
  ;;                    (wlog (name v))
  ;;                    (setf (delivery-price v) 300))))
  ;;          (storage *global-storage*))
  )



(defun product-delivery (p)
  (let ((g (parent p))
        (daily (gethash (articul p) (daily *main-page.storage*))))
    (if daily
        0
        (aif (delivery-price p)
             it
             (if (and (not (null g))
                      (delivery-price g))
                 (delivery-price g)
                 300))))
  )


;; (with-open-file (stream "/home/webadmin/Dropbox/htconf/test.csv")
;;     (do ((line (read-line stream nil)
;;                (read-line stream nil)))
;;         ((null line))
;;       ;; (print line)
;;       (let* ((words (split-sequence:split-sequence #\, line))
;;              (article (car words))
;;              (price (parse-integer (cadr words)))
;;              (siteprice (parse-integer (caddr words)))
;;              (prod (gethash article (storage *global-storage*))))
;;         (format t "~&~a: ~a ~a" article price siteprice)
;;         (if prod
;;             (setf (price prod) price)
;;             (setf (siteprice prod) siteprice))
;;       )))


;; (create-report "seo/last-gateway-string.txt" #'show-last-history)
;; (time (create-report "xls/products.csv" #'write-products-report))
;; (create-report "seo/report-groups.csv" #'write-groups)
;; (create-report "seo/report-products.csv" #'write-products)
;; (create-report "seo/report-vendors.csv" #'write-vendors)
;; (create-report "seo/write-groups-active-product-num.csv" #'write-groups-active-product-num)

;; (let ((res)
;;       (res1))
;;   (maphash #'(lambda (k v)
;;                (if (equal (type-of k)
;;                           (type-of 12345))
;;                    (push v res)))
;;            (storage *global-storage*))
;;   (print (length res))
;;   (mapcar #'(lambda (v)
;;               (let ((key (key v)))
;;                 (setf (key v) (format nil "~a" key))
;;                 (storage.edit-object v)
;;                 (remhash key (storage *global-storage*))))
;;               res)
;;   (print (length res1)))
