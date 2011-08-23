(in-package #:eshop)

(defparameter *history* nil)
(defparameter *load-list* nil)
(defparameter *order* nil)
(defparameter *serialize-check-flag* t)

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

                   ;;создаем новый yml файл
                   (create-yml-file)
                   ;; Заполняем siteprice если он равен 0
                   (copy-price-to-siteprice)
                   ;; Сохраняем *load-list* и *order* для истории
                   (push (list (time.get-date-time) *order* *load-list*) *history*)
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
           (process-product articul price siteprice isnew isspec name realname count-total count-transit bonuscount))))))


(defun gateway-send-error-mail (to mailbody error-name)
  (let* ((sendmail-process (sb-ext:run-program *sendmail*
                                               to
                                               :input :stream
                                               :output nil
                                               :error nil
                                               :wait nil))
         (sendmail (sb-ext:process-input sendmail-process)))
    (unwind-protect
         (progn
		   (format sendmail "From: shop@320-8080.ru~%")
		   (format sendmail "To: ~a~%" (car to))
		   (format sendmail "Subject: ~a~a~%" "Gateway WARN:" error-name)
		   (format sendmail "MIME-Version: ~a~%" "1.0")
		   (format sendmail "Content-Type: ~a~%" "multipart/mixed; boundary = becd713b5f8316a655d07bd225b48c406")
		   (format sendmail "%")
		   (format sendmail
				   "This is a MIME encoded message.

--becd713b5f8316a655d07bd225b48c406
Content-Type: text/html; charset=windows-1251
Content-Transfer-Encoding: base64

~a

--becd713b5f8316a655d07bd225b48c406--
"
				   (encode64 mailbody)
				   ))
      (close sendmail)
      (sb-ext:process-wait sendmail-process)
      (sb-ext:process-close sendmail-process))))


(defun gateway-check-price (product price siteprice)
  (let ((price-old (price product))
        (siteprice-old (siteprice product))
        (mailbody))
    (if (or (and (< 3000 siteprice-old)
                 (<= 0.2 (float (/ (abs (- siteprice-old siteprice 1)) siteprice-old))))
            (and (< 3000 price-old)
                 (<= 0.2 (float (/ (abs (- price-old price 1)) price-old)))))
        (progn
          (setf mailbody (format nil "~&<a href=\"http://www.320-8080.ru/~a\">~a: ~a</a>
                                        <br/>Изменение цены боллее чем на 20%
                                        <br/>Старая цене на сайте:~a |  новая:~a
                                        <br/>Разница в цене в магазине:~a | новая:~a"
                                 (articul product)
                                 (articul product)
                                 (name product)
                                 siteprice-old siteprice
                                 price-old price))
          (if *serialize-check-flag*
            (progn (wlog mailbody)
                   (setf (siteprice product) siteprice)
                   (setf (price product) price)
                   (serialize product)
                   (gateway-send-error-mail (list "CallCenter@alpha-pc.com"
                                                  "Supplers@alpha-pc.com"
                                                  "web_design@alpha-pc.com"
                                                  "wolforus@gmail.com"
                                                  "slamly@gmail.com") mailbody (format nil "price ~a" (articul product)))
                   t)
            (progn
              (gateway-send-error-mail (list "CallCenter@alpha-pc.com"
                                             "Supplers@alpha-pc.com"
                                             "web_design@alpha-pc.com"
                                             "wolforus@gmail.com"
                                             "slamly@gmail.com") mailbody (format nil "price ~a" (articul product)))
              nil)))
        t)))

;(defun gateway-check-1c-name (product name-new)
;    (print "TEST"))

(defun process-product (articul price siteprice isnew isspec name realname count-total count-transit bonuscount)
  (let ((product (aif (gethash (format nil "~a" articul) *storage*)
                      it
                      (make-instance 'product :articul articul))))
    (when (and (equal (type-of product) 'product)
               (gateway-check-price product price siteprice))
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
            (date-modified product)   (get-universal-time)
            (active product)          (if (= count-total 0) nil t)
            (newbie product)	        (if (string= "0" isnew) nil t)
            (sale product)            (if (string= "0" isspec) nil t)
            (count-total product)     count-total
            (count-transit  product)  count-transit)
      (setf (gethash (format nil "~a" articul) *storage*) product))))

;; (setf (siteprice (gethash "154278" *storage*)) 7290)
;; (setf (price (gethash "154278" *storage*)) 7590)
;; (serialize (gethash "154278" *storage*))

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

(defun use-unchecked-revert-history ()
    (let ((*serialize-check-flag* t))
      (use-revert-history)))

