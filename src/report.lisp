(in-package #:eshop)


(defun write-products-report (stream)
  (format stream "~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~%"
          "артикул"
          "цена магазина"
          "цена сайта"
          "имя"
          "имя real"
          "имя yml"
          "seo текст"
          "фотографии"
          "характеристики"
          "активный"
          "группа"
          "родительская группа"
          "secret")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (let ((id "нет")
                     (name "нет")
                     (name-real "нет")
                     (name-yml "нет")
                     (desc "нет")
                     (img "нет")
                     (options "нет")
                     (active "нет")
                     (group-name "нет")
                     (parent-group-name "нет")
                     (secret "нет"))
                 (when (equal (type-of v)
                              'product)
                   (setf id (articul v))
                   (setf name (stripper (name v)))
                   (setf name-real (stripper (realname v)))
                   (with-option v "Secret" "Yandex"
                                (setf name-yml (stripper (value option))))
                   (setf desc (if (and (not (null (shortdescr v)))
                                       (not (string= "" (stripper (shortdescr v)))))
                                  "есть"
                                  "нет"))
                   (setf img (length (get-pics (articul v))))
                   (setf options ;;(if (not (null (optgroups v)))
                                   (if (is-valide-option v)
                                     "есть"
                                     "нет"))
                   (setf active (if (active v)
                                    "да"
                                    "нет"))
                   (setf group-name (if (not (null (parent v)))
                                        (stripper (name (parent v)))))
                   (setf parent-group-name (if (and (not (null (parent v)))
                                                    (not (null (parent (parent v)))))
                                               (stripper (name (parent (parent v))))))
                   (setf secret "Нет")
                   (with-option v "Secret" "Checked"
                                (setf secret (value option)))
                   (format stream "~a;~a;~a;\"~a\";\"~a\";\"~a\";~a;~a;~a;~a;\"~a\";\"~a\";~a;~%"
                           id
                           (price v)
                           (siteprice v)
                           name
                           name-real
                           name-yml
                           desc
                           img
                           options
                           active
                           group-name
                           parent-group-name
                           secret)
                   )))
           *storage*))

(defun is-valide-option (product)
  (let ((flag nil))
    (mapcar #'(lambda (v) (mapcar #'(lambda (l)
                                      (when (and (not (equal (name v) "Secret"))
                                               l
                                               (value l)
                                               (not (equal (value l) ""))
                                               (not (equal (value l) "Производитель"))
                                               (not (equal (value l) "Модель")))

                                          (setf flag t)))
                       (options v)))
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
           *storage*))

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
           *storage*))



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
             *storage*)))


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
           *storage*))

(defun create-report (file-name report-func)
  (let ((filename (format nil "~a/~a" *path-to-dropbox* file-name)))
    (with-open-file
        (stream filename :direction :output :if-exists :supersede :external-format :cp1251)
      (funcall report-func stream))))


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
           *storage*))

(defun show-last-history (stream)
  (when (not (null *history*))
    ;; Делаем все продукты неактивными
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (equal (type-of v) 'product)
                   (setf (active v) nil)))
             *storage*)
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


                          ;; (setf (active (gethash "160420" *storage*)) nil)
                          ;; (serialize (gethash "160420" *storage*))
                          ;; (setf (active (gethash "165359" *storage*)) nil)
                          ;; (serialize (gethash "165359" *storage*))
                          ;; (setf (active (gethash "165360" *storage*)) nil)
                          ;; (serialize (gethash "165360" *storage*))
;; (setf (active (gethash "157499" *storage*)) nil)
;; (serialize (gethash "157499" *storage*))
;; (setf (active (gethash "153599" *storage*)) nil)
;; (serialize (gethash "153599" *storage*))



(defparameter *special-products* (make-hash-table :test #'equal))



(defun post-proccess-gateway ()
    (mapcar #'(lambda (v)
                (let ((p (gethash v *storage*))
                  ;; (p (make-instance 'product
                  ;;                   :key v
                  ;;                   :articul v))
                  )
              ;; (if p1
                  ;; (setf p p1))
              (when (not (null p))
                  (setf (predzakaz p) t)
                  (setf (active p) t)
                  (serialize p)
                  (setf (gethash v *storage*) p)
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
              (let ((p (gethash v *storage*)))
                (when (not (null p))
                  (setf (predzakaz p) t)
                  (serialize p))))
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
  ;;          *storage*)
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
;;              (prod (gethash article *storage*)))
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

