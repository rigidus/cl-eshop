(in-package #:eshop)


(defun write-products-report (stream)
  (format stream "~a;~a;~a;~a;~a;~a;~a;~a;~a~%"
          "артикул"
          "имя"
          "seo текст"
          "фотографии"
          "характеристики"
          "активный"
          "группа"
          "родительская группа"
          "secret")
  (maphash #'(lambda (k v)
               (let ((id "нет")
                     (name "нет")
                     (desc "нет")
                     (img "нет")
                     (option "нет")
                     (active "нет")
                     (group-name "нет")
                     (parent-group-name "нет")
                     (secret "нет"))
                 (when (equal (type-of v)
                              'product)
                   (setf id (articul v))
                   (setf name (stripper (realname v)))
                   (setf desc (if (not (null(descr v)))
                                  "есть"
                                  "нет"))
                   (setf img (length (get-pics (articul v))))
                   (setf options (if (not (null (optgroups v)))
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
                   (format stream "~a;\"~a\";~a;~a;~a;~a;\"~a\";\"~a\";~a~%"
                           id
                           name
                           desc
                           img
                           options
                           active
                           group-name
                           parent-group-name
                           secret)
                   )))
           *storage*))


(defun write-groups (stream)
  (format stream "~a;~a;~a;~%"
          "Название категории"
          "url страницы"
          "Active")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (equal (type-of v)
                              'group)
                 (format stream "\"~a\";http://www.320-8080.ru/~a;~a;~%"
                           (stripper (name v))
                           (key v)
                           (if (active v)
                               "yes"
                               "no"))
                   ))
           *storage*))



(defun write-products (stream)
  (let ((vendor-name))
    (format stream "~a;~a;~a;~a;~a;~%"
            "Название категории"
            "Брэнд"
            "Название товара"
            "url страницы"
            "Active")
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (equal (type-of v)
                              'product)
                   (setf vendor-name "Нет")
                   (with-option v "Общие характеристики" "Производитель"
                                (setf vendor-name (value option)))
                   (format stream "\"~a\";\"~a\";\"~a\";http://www.320-8080.ru/~a;~a;~%"
                           (if (not (null (parent v)))
                               (stripper (name (parent v)))
                               "Нет категории")
                           (stripper vendor-name)
                           (stripper (name v))
                           (articul v)
                           (if (active v)
                               "yes"
                               "no"))))
             *storage*)))


(defun write-vendors (stream)
  (format stream "~a;~a;~a;~a~%"
          "Название категории"
          "Брэнд"
          "url страницы"
          "Active")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (equal (type-of v)
                            'group)
                 (mapcar #'(lambda (vendor)
                             (format stream "\"~a\";\"~a\";http://www.320-8080.ru/~a?vendor=~a;~a;~%"
                                     (stripper (name v))
                                     (stripper (car vendor))
                                     (key v)
                                     (stripper (car vendor))
                                     "yes"))
                         (producersall (make-producers v)))))
           *storage*))

(defun create-report (file-name report-func)
  (let ((filename (format nil "~a/~a" *path-to-dropbox* file-name)))
    (with-open-file
        (stream filename :direction :output :if-exists :supersede)
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

;; (create-report "seo/last-gateway-string.txt" #'show-last-history)
;; (create-report "xls/products.csv" #'write-products-report)
;; (create-report "seo/report-groups.csv" #'write-groups)
;; (create-report "seo/report-products.csv" #'write-products)
;; (create-report "seo/report-vendors.csv" #'write-vendors)
