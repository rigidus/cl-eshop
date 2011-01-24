(in-package #:gateway)

(defparameter *history* nil)
(defparameter *load-list* nil)
(defparameter *order* nil)

;; (length *history*)

;; (length (json:decode-json-from-string
;;          (sb-ext:octets-to-string (cadr *load-list*) :external-format :cp1251)))


(funcall *dispatcher*
         `((string= "gateway" (cadr (service:request-list)))
           ,#'(lambda ()
                (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
                (let ((raw (hunchentoot:raw-post-data)))
                  (if (null raw)
                      "NIL"
                      (progn
                        (cond ((string= "0" (hunchentoot:get-parameter "num"))

                               ;; Обработка последнего пакета
                               (progn
                                 ;; Делаем все продукты неактивными
                                 (maphash #'(lambda (k v)
                                              (setf (product:active v ) nil))
                                          trans:*product*)
                                 ;; Засылаем последний пакет в *load-list* и *order*
                                 (push raw *load-list*)
                                 (push (hunchentoot:get-parameter "num") *order*)
                                 ;; Обрабатываем все сохраненные пакеты
                                 (loop :for packet :in (reverse *load-list*) :do
                                    (process packet))
                                 ;; Сохраняем *load-list* и *order* для истории
                                 (push (list (get-date-time) *order* *load-list*) *history*)
                                 "last"))

                              ((string= "1" (hunchentoot:get-parameter "num"))
                               ;; Обработка первого пакета
                               (progn
                                 ;; Обнуляем *load-list* и *order*
                                 (setf *load-list* nil)
                                 (setf *order* nil)
                                 ;; Засылаем первый пакет в *load-list*
                                 (push raw *load-list*)
                                 (push (hunchentoot:get-parameter "num") *order*)
                                 "first"))

                              (t
                               ;; Обработка промежуточных пакетов
                               (progn
                                   ;; Засылаем первый пакет в *load-list*
                                   (push raw *load-list*)
                                   (push (hunchentoot:get-parameter "num") *order*)
                                   "ordinal"))
                              )))))))


(defun process (raw)
  ;; Декодируем JSON из RAW в DATA
  (let ((data (json:decode-json-from-string
               (sb-ext:octets-to-string raw :external-format :cp1251))))
    ;; dbg
    ;; (format nil "~a" data)


    ;; Перебираем продукты
    (loop :for elt  :in data :collect
       (block iteration
         (let ((articul   (ceiling (parse-float (cdr (assoc :id elt)))))
               (price     (ceiling (parse-float (cdr (assoc :price elt)))))
               (siteprice (ceiling (parse-float (cdr (assoc :price--site elt)))))
               (ekkprice  (ceiling (parse-float (cdr (assoc :price--ekk elt)))))
               (isnew     (cdr (assoc :isnew  elt)))
               (isspec    (cdr (assoc :isspec elt)))
               (name      (cdr (assoc :name elt)))
               (realname  (cdr (assoc :realname elt)))
               (count-total    (ceiling (parse-float (cdr (assoc :count--total elt)))))
               (count-transit  (ceiling (parse-float (cdr (assoc :count--transit elt))))))
           ;; Нам не нужны продукты с нулевой ценой (вероятно это группы продуктов)
           (when (equal 0 price)
             (return-from iteration))
           (process-product articul price siteprice ekkprice isnew isspec name realname count-total count-transit))))))


(defun process-product (articul price siteprice ekkprice isnew isspec name realname count-total count-transit)
  (if (equal articul 147421)
      (setf *tmp* (list
                   :articul articul
                   :price price
                   :siteprice siteprice
                   :ekkprice ekkprice
                   :isnew isnew
                   :isspec isspec
                   :name name
                   :realname realname
                   :count-total count-total
                   :count-transit count-transit
                   )))
  (let ((product (aif (gethash articul trans:*product*)
                      it
                      (make-instance 'product:product :articul articul))))
    (setf (product:articul product)         articul
          (product:name product)            name
          (product:realname product)        (if (or (null realname)
                                                    (string= "" realname))
                                                name
                                                realname)
          (product:price product)           price
          (product:siteprice product)       siteprice
          (product:ekkprice product)        ekkprice
          (product:active product)          (if (= count-total 0) nil t)
          (product:newbie product)	        (if (string= "0" isnew) nil t)
          (product:sale product)            (if (string= "0" isspec) nil t)
          (product:count-total product)     count-total
          (product:count-transit  product)  count-transit)
    (setf (gethash articul trans:*product*) product)
    (if (= articul 147421)
        (product:plist-representation product :articul :name :realname :count-total :active)
        "")))
