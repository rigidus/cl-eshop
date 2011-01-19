(in-package #:gateway)

(defclass seans ()
    ((timepoint        :initarg :time              :initform nil       :accessor   timepoint)
     (raw              :initarg :raw               :initform nil       :accessor   raw)
     (data             :initarg :data              :initform nil       :accessor   data)))

(defparameter *loadlist* nil)

;; Для сохранения выгрузок
;; (require 'cl-store)
;; (cl-store:store *loadlist* "loadlist")


(defun store-seans (raw)
  (push (make-instance 'seans :raw raw) *loadlist*)
  "DONE")

(defun get-date-time ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second))
    (format nil
            "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
            year
            month
            date
            hour
            minute)))


(funcall *dispatcher*
         `((string= "/gateway" (service:request-str))
           ,#'(lambda ()
                (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
                (let ((raw (hunchentoot:raw-post-data)))
                  (if (null raw)
                      "NIL"
                      (store-seans (sb-ext:octets-to-string raw :external-format :cp1251)))))))


(defun load-from-conf ()
  (store-seans (alexandria:read-file-into-string
                (format nil "~a/last-gateway-string.txt" cl-user::*path-to-conf*))))


;; Представление объекта в консоли
(defmethod plist-representation ((object seans) &rest fields)
  (let ((result))
    (loop :for item :in fields do
       (let ((method (intern (symbol-name item) 'gateway)))
         (push item result)
         (push (funcall method object) result)))
    (reverse result)))


(defmethod initialize-instance :after ((object seans) &key)
  ;; Устанавливаем метку времени
  (setf (timepoint object) (get-date-time))
  ;; Декодируем JSON
  (setf (data object) (json:decode-json-from-string (raw object)))
  ;; Делаем все продукты неактивными
  (maphash #'(lambda (k v)
               (setf (product:active v ) nil))
           trans:*product*)
  ;; Перебираем продукты
  (loop
     :for elt
     :in  (data object) do
     (block iteration
       (let ((articul   (parse-integer (cdr (assoc :id elt))))
             (price     (parse-integer (cdr (assoc :price elt))))
             (isnew     (cdr (assoc :isnew  elt)))
             (isspec    (cdr (assoc :isspec elt)))
             (count-total    (parse-integer (cdr (assoc :count--total elt))))
             (count-transit  (parse-integer (cdr (assoc :count--transit elt)))))
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
                                  :siteprice      (parse-integer (cdr (assoc :price--site elt)))
                                  :ekkprice       (parse-integer (cdr (assoc :price--ekk elt)))
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
               (setf (product:siteprice product)      (parse-integer (cdr (assoc :price--site elt))))
               (setf (product:ekkprice product)       (parse-integer (cdr (assoc :price--ekk elt))))
               (setf (product:active product)         (if (= count-total 0 ) nil t))
               (setf (product:newbie product)         (if (string= "0" isnew) nil t))
               (setf (product:sale product)           (if (string= "0" isspec) nil t))
               :count-total    count-total
               :count-transit  count-transit
               )
             )))))


;; (load-from-conf)
