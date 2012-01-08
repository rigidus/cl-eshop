;;;; gateway.lisp
;;;;
;;;; This file is part of the eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop)

(defparameter *history* nil)
(defparameter *load-list* nil)
(defparameter *order* nil)

;; (length *history*)

;; (length (json:decode-json-from-string
;;          (sb-ext:octets-to-string (cadr *load-list*) :external-format :cp1251)))


(defun gateway-page ()
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
                                (declare (ignore k))
                                (when (equal (type-of v) 'product)
                                  (setf (active v) nil)))
                            *storage*)
                   ;; Засылаем последний пакет в *load-list* и *order*
                   (push raw *load-list*)
                   (push (hunchentoot:get-parameter "num") *order*)
                   ;; Обрабатываем все сохраненные пакеты
                   (loop :for packet :in (reverse *load-list*) :do
                      (process packet))
                   ;; Заполняем siteprice если он равен 0
                   (copy-price-to-siteprice)
                   ;; Сохраняем *load-list* и *order* для истории
                   (push (list (get-date-time) *order* *load-list*) *history*)
                   ;; Обнуляем *load-list* и *order* (если приходит 1 пакет, то он num=0)
                   (setf *load-list* nil)
                   (setf *order* nil)
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
                )))))


(defun process (raw)
  ;; Декодируем JSON из RAW в DATA
  (let ((data (json:decode-json-from-string
               (sb-ext:octets-to-string raw :external-format :cp1251))))
    ;; dbg
    ;; (format nil "~a" data)

     ;; Перебираем продукты
    (loop :for elt  :in data :collect
       (block iteration
         (let ((articul   (ceiling (arnesi:parse-float (cdr (assoc :id elt)))))
               (price     (ceiling (arnesi:parse-float (cdr (assoc :price elt)))))
               (siteprice (ceiling (arnesi:parse-float (cdr (assoc :price--site elt)))))
               (ekkprice  (ceiling (arnesi:parse-float (cdr (assoc :price--ekk elt)))))
               (bonuscount  (ceiling (arnesi:parse-float (cdr (assoc :bonuscount elt)))))
               (isnew     (cdr (assoc :isnew  elt)))
               (isspec    (cdr (assoc :isspec elt)))
               (name      (cdr (assoc :name elt)))
               (realname  (cdr (assoc :realname elt)))
               (count-total    (ceiling (arnesi:parse-float (cdr (assoc :count--total elt)))))
               (count-transit  (ceiling (arnesi:parse-float (cdr (assoc :count--transit elt))))))
           ;; Нам не нужны продукты с нулевой ценой (вероятно это группы продуктов)
           (when (equal 0 price)
             (return-from iteration))
           (process-product articul price siteprice ekkprice isnew isspec name realname count-total count-transit bonuscount))))))


(defun process-product (articul price siteprice ekkprice isnew isspec name realname count-total count-transit bonuscount)
  (let ((product (aif (gethash (format nil "~a" articul) *storage*)
                      it
                      (make-instance 'product :articul articul))))
    (when (equal (type-of product) 'product)
      (setf (articul product)         articul
            (name product)            name
            (realname product)        (if (or (null (realname product))
                                              (string= ""  (realname product)))
                                          (if (or (null realname)
                                                  (string= "" realname))
                                              name
                                              realname)
                                          (realname product))

            (price product)           price
            (siteprice product)       siteprice
            (bonuscount product)      bonuscount
            (ekkprice product)        ekkprice
            (active product)          (if (= count-total 0) nil t)
            (newbie product)	        (if (string= "0" isnew) nil t)
            (sale product)            (if (string= "0" isspec) nil t)
            (count-total product)     count-total
            (count-transit  product)  count-transit)
      (setf (gethash (format nil "~a" articul) *storage*) product))))


(defun use-revert-history ()
  (when (not (null *history*))
    ;; Делаем все продукты неактивными
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (equal (type-of v) 'product)
                   (setf (active v) nil)))
             *storage*)
    (loop :for packet :in (reverse (caddr (car *history*))) :do
       (process packet))))

;; (let ((a 0))
;;   (maphash #'(lambda (k v)
;;                (when (active v)
;;                  (incf a)))
;;            *storage*)
;;   a)
