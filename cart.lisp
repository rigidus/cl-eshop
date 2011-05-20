;;;; cart.lisp
;;;;
;;;; This file is part of the eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop)

(defvar *order-id* nil)

;;генерирует псевдоуникальный номер заказа
(defun get-order-id ()
  (let ((current-order-id *order-id*)
        (order-id-pathname (format nil "~a/~a" *path-to-conf* "order-id.txt")))
    (if (not (null *order-id*))
        (progn
          (incf *order-id*)
          (with-open-file (file order-id-pathname
                                :direction :output
                                :if-exists :supersede
                                :external-format :utf-8)
            (format file "~a" *order-id*))
          current-order-id)
        (progn
          ;;если в файле шлак, то сбрасываем счетчик заказов до 1
          (setf *order-id*
                (handler-case
                    (parse-integer
                     (alexandria:read-file-into-string
                      order-id-pathname))
                  (SB-INT:SIMPLE-PARSE-ERROR () 1)
                  (SB-INT:SIMPLE-FILE-ERROR () 1)))
          (get-order-id)))))



(defun cart-processor (alist)
  (loop :for item :in alist :collect (item-processor item)))


(defun item-processor (item)
  (let* ((articul   (parse-integer (cdr (assoc :ID item)) :junk-allowed t))
         (group-id  (cdr (assoc :GROUP--ID item)))
         (name      (cdr (assoc :NAME item)))
         (price     (cdr (assoc :PRICE item)))
         (count     (cdr (assoc :COUNT item)))
         (item-link (cdr (assoc :ITEM--LINK item)))
         (img-link  (cdr (assoc :IMG--LINK item)))
         (object    (gethash (format nil "~a" articul) *storage*)))
    (if (null object)
        nil
        (let ((pics (get-pics (articul object))))
          (list :count count
                :itemlink item-link
                :firstpic (if (null pics) "" (car pics))
                :articul (articul object)
                :name (name object)
                :siteprice (siteprice object)
                :price (price object)
                )))))



(defun cart-page ()
  (if (null (hunchentoot:cookie-in "cart"))
      "null cart"
      (let* ((cart (json:decode-json-from-string (hunchentoot:cookie-in "cart")))
             (out  (cart-processor cart)))
        (default-page
            (cart:content (list :accessories (product:accessories)
                                :products (format nil "~{~a~}" (mapcar #'(lambda (x)
                                                                           (cart:product x))
                                                                       out))))))))


(defun checkout-page-0 ()
  (if (null (hunchentoot:cookie-in "cart"))
      "null cart"
      (checkout-page (checkout:content0 (list :accessories (product:accessories)
                                              :order (checkout:order))))))

(defun checkout-page-1 ()
  (if (null (hunchentoot:cookie-in "cart"))
      "null cart"
      (checkout-page (checkout:content1 (list :accessories (product:accessories)
                                              :order (checkout:order)
                                              )))))

(defun checkout-page-2 ()
  (if (null (hunchentoot:cookie-in "cart"))
      "null cart"
      (checkout-page (checkout:content2 (list :accessories (product:accessories)
                                                      :order (checkout:order)
                                                      )))))

(defun checkout-page-3 ()
  (if (null (hunchentoot:cookie-in "cart"))
      "null cart"
      (checkout-page (checkout:content3 (list :accessories (product:accessories)
                                                      :order (checkout:order)
                                                      )))))


;;проверка заказа на валидность
;;TODO сделать полную проверку
(defun if-order-valid (products)
  (not (null products)))


(defun thanks-page ()
  (let ((cart) (user) (products) (auth) (delivery) (pay) (client-mail) (mail-file)
        (deliverysum 0)
        (itogo 0)
        (order-id))
    (mapcar #'(lambda (cookie)
                (cond ((string= (car cookie) "cart") (setf cart (json:decode-json-from-string (cdr cookie))))
                      ((string= (car cookie) "user") (setf user (json:decode-json-from-string (cdr cookie))))
                      (t nil)))
            (hunchentoot:cookies-in hunchentoot:*request*))
    (setf products (mapcar #'(lambda (product)
                               (let* ((articul (parse-integer (cdr (assoc :id  product)) :junk-allowed t))
                                      (cnt     (cdr (assoc :count product)))
                                      (plist   (plist-representation (gethash (format nil "~a" articul) *storage*)
                                                                             :articul
                                                                             :name
                                                                             :price
                                                                             :siteprice))
                                      (product-real-price (if
                                                           (= (getf plist :siteprice)
                                                              0)
                                                           (getf plist :price)
                                                           (getf plist :siteprice)))
                                      (sum (* product-real-price cnt)))
                                 (setf itogo (+ itogo sum))
                                 (list* :cnt cnt
                                        :sum sum
                                        plist)))
                           cart))
    (if (if-order-valid products)
        (progn
             (setf auth     (cdr (assoc :auth user)))
             (setf delivery (cdr (assoc :delivery user)))
             (setf pay      (cdr (assoc :pay user)))
             (setf order-id (get-order-id)) ;;генерация идентификатора заказа происходит только если заказ валиден
             ;;Временно доставка 300 на все
             (if (string= (cdr (assoc :deliverytype delivery))
                          "courier")
                 (setf deliverysum 300))
             (setf client-mail
                (sendmail:clientmail
                 (list :datetime (get-date-time)
                       :order_id order-id
                       :name (cdr (assoc :name auth))
                       :family (cdr (assoc :family auth))
                       :paytype (let ((tmp (cdr (assoc :paytype pay))))
                                  (cond ((string= tmp "cash") "Наличными")
                                        ((string= tmp "card") "Кредитной картой")
                                        ((string= tmp "credit") "Безналичный расчет")
                                        ((string= tmp "bank") "банковским переводом")
                                        (t tmp)))
                       :deliverytype (let ((tmp (cdr (assoc :deliverytype delivery))))
                                       (cond ((string= tmp "auto") "Самовывоз")
                                             ((string= tmp "courier") "Курьер")
                                             (t tmp)))
                       :addr (cdr (assoc :addr delivery))
                       :bankaccount (let ((bankaccount (cdr (assoc :bankaccount pay))))
                                      (if (null bankaccount)
                                          ""
                                          bankaccount))
                       :phone (cdr (assoc :phone auth))
                       :email (cdr (assoc :email auth))
                       :comment (cdr (assoc :comment delivery))
                       :products products
                       :deliverysum deliverysum
                       :itogo (+ itogo deliverysum)
                       )))
          (setf mail-file (list :order_id order-id
                                :ekk ""
                                :name (cdr (assoc :name auth))
                                :family (cdr (assoc :family auth))
                                :addr (cdr (assoc :addr delivery))
                                :phone (cdr (assoc :phone auth))
                                :email (cdr (assoc :email auth))
                                :date (get-date-time)
                                :comment (cdr (assoc :comment delivery))
                                :products products))
          (setf filename (multiple-value-bind (second minute hour date month year) (get-decoded-time)
                           (declare (ignore second))
                           (format nil
                                   "~d~2,'0d~2,'0d_~a.txt"
                                   year
                                   month
                                   date
                                   order-id
                                   )))
          ;;
          (save-order-text order-id client-mail)
          (send-mail (list "internetorder@alpha-pc.com") client-mail filename (sendmail:mailfile mail-file) order-id)
          (send-mail (list "stetoscop@gmail.com") client-mail filename (sendmail:mailfile mail-file) order-id)
          (send-mail (list "shop@320-8080.ru") client-mail filename (sendmail:mailfile mail-file) order-id)
          (send-mail (list "zakaz320@yandex.ru") client-mail filename (sendmail:mailfile mail-file) order-id)
          (send-mail (list "wolforus@gmail.com") client-mail filename (sendmail:mailfile mail-file) order-id)
          (send-client-mail (list (cdr (assoc :email auth)))
                            client-mail order-id)
          (checkout-thankes-page (checkout:thanks (list :order (checkout:order)
                                                :orderid order-id))))
        (progn
          (checkout-thankes-page (checkout:thankserror))))))


(defun save-order-text (file-name body)
  (let ((filename (format nil "~a/orders/~a.html" *path-to-dropbox* file-name)))
    (with-open-file
        (stream filename :direction :output :if-exists :supersede)
      (format stream "~a" body))))

(defvar *sendmail*
  (find-if #'fad:file-exists-p
           (list "/usr/bin/sendmail"
                 "/usr/sbin/sendmail")))

(defun send-mail (to clientmail filename mailfile order-id)
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
		   (format sendmail "Subject: ~a~a~%" "www.320-8080.ru - 3AKA3 " order-id)
		   (format sendmail "MIME-Version: ~a~%" "1.0")
		   (format sendmail "Content-Type: ~a~%" "multipart/mixed; boundary = becd713b5f8316a655d07bd225b48c406")
		   (format sendmail "%")
		   (format sendmail
				   "This is a MIME encoded message.

--becd713b5f8316a655d07bd225b48c406
Content-Type: text/html; charset=windows-1251
Content-Transfer-Encoding: base64

~a

--becd713b5f8316a655d07bd225b48c406
Content-Type: Content-type: text/plain; charset=\"windows-1251\"; name = \"~a\"
Content-Transfer-Encoding: base64

~a

--becd713b5f8316a655d07bd225b48c406--
"
				   (encode64 clientmail)
				   filename
				   (encode64 (encode1251 mailfile))
				   ))
      (close sendmail)
      (sb-ext:process-wait sendmail-process)
      (sb-ext:process-close sendmail-process))))


(defun send-client-mail (to clientmail order-id)
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
		   (format sendmail "Subject: ~a~a~%" "www.320-8080.ru - 3AKA3 " order-id)
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
				   (encode64 clientmail)
				   ))
      (close sendmail)
      (sb-ext:process-wait sendmail-process)
      (sb-ext:process-close sendmail-process))))



(defun encode64 (param)
  (base64:usb8-array-to-base64-string
   ;; (babel:string-to-octets
   ;;  param
   ;;  :encoding :cp1251)))
   (sb-ext:string-to-octets param  :external-format :cp1251)))

;; (sb-ext:octets-to-string
;;  (sb-ext:string-to-octets "русские буквы" :external-format :cp1251) :external-format :cp1251)
;; (sb-ext:octets-to-string
;;  (sb-ext:string-to-octets "русские буквы"))


(defun encode1251 (param)
  (let (($ret nil))
	(loop
	   for x across param collect x
	   do (if (equal x (code-char 10))
			  (progn
				(push (code-char 13) $ret)
				(push x $ret))
			  (push x $ret)))
	(coerce (reverse $ret) 'string)))

