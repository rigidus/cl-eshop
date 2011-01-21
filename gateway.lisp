(in-package #:gateway)

(defclass gateway ()
  ((timepoint        :initarg :time              :initform nil       :accessor   timepoint)
   (raw              :initarg :raw               :initform nil       :accessor   raw)
   (data             :initarg :data              :initform nil       :accessor   data)))


(funcall *dispatcher*
         `((string= "/gateway" (service:request-str))
           ,#'(lambda ()
                (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
                (let ((raw (hunchentoot:raw-post-data)))
                  (if (null raw)
                      "NIL"
                      (progn
                        ;; Сохраняем в файле
                        (with-open-file (file (format nil "~a/~a" *path-to-conf* "last-gateway-string")
                                              :direction :output
                                              :if-exists :supersede
                                              :external-format :utf-8)
                          (format file "~a" raw))
                        ;; Создаем объект
                        (let ((gateway (make-instance 'gateway :raw raw)))
                          ;; Обрабатываем объект
                          (process gateway))
                        "OK"
                        ))))))


;; Представление объекта в консоли
(defmethod plist-representation ((object gateway) &rest fields)
  (let ((result))
    (loop :for item :in fields do
       (let ((method (intern (symbol-name item) 'gateway)))
         (push item result)
         (push (funcall method object) result)))
    (reverse result)))


(defmethod process ((object gateway) &key)
  ;; Устанавливаем метку времени
  (setf (timepoint object) (get-date-time))
  ;; Декодируем JSON из RAW в DATA
  (setf (data object)
        (json:decode-json-from-string
         (sb-ext:octets-to-string (raw object) :external-format :cp1251)))
  ;; Делаем все продукты неактивными
  (maphash #'(lambda (k v)
               (setf (product:active v ) nil))
           trans:*product*)
  ;; Перебираем продукты
  (loop
     :for elt
     :in  (data object) do
     (block iteration
       (let ((articul   (ceiling (parse-float (cdr (assoc :id elt)))))
             (price     (ceiling (parse-float (cdr (assoc :price elt)))))
             (isnew     (cdr (assoc :isnew  elt)))
             (isspec    (cdr (assoc :isspec elt)))
             (count-total    (ceiling (parse-float (cdr (assoc :count--total elt)))))
             (count-transit  (ceiling (parse-float (cdr (assoc :count--transit elt))))))
         ;; Нам не нужны продукты с нулевой ценой (вероятно это группы продуктов)
         (when (equal 0 price)
           ;; (print elt)
           (return-from iteration))
         ;; Проверяем, есть ли у нас уже этот продукт
         (if (null (gethash articul trans:*product*))
             ;; Это новый продукт
             (setf (gethash articul trans:*product*)
                   (make-instance 'product:product
                                  :articul        articul
                                  :name           (cdr (assoc :name elt))
                                  :price          price
                                  :siteprice      (ceiling (parse-float (cdr (assoc :price--site elt))))
                                  :ekkprice       (ceiling (parse-float (cdr (assoc :price--ekk elt))))
                                  :active         (if (= count-total 0 ) nil t)
                                  :newbie	      (if (string= "0" isnew) nil t)
                                  :sale           (if (string= "0" isspec) nil t)
                                  :count-total    count-total
                                  :count-transit  count-transit
                                  ))
             ;; else Это уже существующий продукт
             (let ((product (gethash articul trans:*product*)))
               (setf (product:name  product)          (cdr (assoc :name elt)))
               (setf (product:price product)          price)
               (setf (product:siteprice product)      (ceiling (parse-float (cdr (assoc :price--site elt)))))
               (setf (product:ekkprice product)       (ceiling (parse-float (cdr (assoc :price--ekk elt)))))
               (setf (product:active product)         (if (= count-total 0 ) nil t))
               (setf (product:newbie product)         (if (string= "0" isnew) nil t))
               (setf (product:sale product)           (if (string= "0" isspec) nil t))
               :count-total    count-total
               :count-transit  count-transit
               )
             )))))
