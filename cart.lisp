(in-package #:cart)

(funcall *dispatcher*
         `((string= "/cart" (service:request-str))
           ,#'(lambda ()
                (if (null (cookie-in "cart"))
                    "null cart"
                    (let* ((cart (json:decode-json-from-string (cookie-in "cart")))
                           (out
                            (remove-if #'null
                                       (loop :for item :in  cart
                                          :collect (let* ((articul   (parse-integer (cdr (assoc :ID item)) :junk-allowed t))
                                                          (group-id  (cdr (assoc :GROUP--ID item)))
                                                          (name      (cdr (assoc :NAME item)))
                                                          (price     (cdr (assoc :PRICE item)))
                                                          (count     (cdr (assoc :COUNT item)))
                                                          (item-link (cdr (assoc :ITEM--LINK item)))
                                                          (img-link  (cdr (assoc :IMG--LINK item)))
                                                          (object    (gethash articul trans:*product*))
                                                          )
                                                     (if (null object)
                                                         nil
                                                         ;; else
                                                         (let ((pics (product:get-pics (product:articul object))))
                                                           (list :count count
                                                                 :itemlink item-link
                                                                 :firstpic (if (null pics) "" (car pics))
                                                                 :articul (product:articul object)
                                                                 :name (product:name object)
                                                                 :siteprice (product:siteprice object)
                                                                 :price (product:price object)
                                                                 ))))))))
                      (service::default-page
                          (cart:content (list :accessories (product:accessories)
                                              :products (format nil "~{~a~}" (mapcar #'(lambda (x)
                                                                    (cart:product x))
                                                                out)))))
                      )))))


(funcall *dispatcher*
         `((string= "/checkout0" (service:request-str))
           ,#'(lambda ()
                (if (null (cookie-in "cart"))
                    "null cart"
                    (service:checkout-page (checkout:content0 (list :accessories (product:accessories)
                                                                    :order (checkout:order)
                                                                    )))))))

(funcall *dispatcher*
         `((string= "/checkout1" (service:request-str))
           ,#'(lambda ()
                (if (null (cookie-in "cart"))
                    "null cart"
                    (service:checkout-page (checkout:content1 (list :accessories (product:accessories)
                                                                    :order (checkout:order)
                                                                    )))))))

(funcall *dispatcher*
         `((string= "/checkout2" (service:request-str))
           ,#'(lambda ()
                (if (null (cookie-in "cart"))
                    "null cart"
                    (service:checkout-page (checkout:content2 (list :accessories (product:accessories)
                                                                    :order (checkout:order)
                                                                    )))))))

(funcall *dispatcher*
         `((string= "/checkout3" (service:request-str))
           ,#'(lambda ()
                (if (null (cookie-in "cart"))
                    "null cart"
                  (service:checkout-page (checkout:content3 (list :accessories (product:accessories)
                                                                  :order (checkout:order)
                                                                  )))))))

(funcall *dispatcher*
         `((string= "/thanks" (service:request-str))
           ,#'(lambda ()
                (let ((cart) (user) (products) (auth) (delivery) (pay) (client-mail) (mail-file)
                      (itogo 0)
                      (order-id 311337))
                  (mapcar #'(lambda (cookie)
                              (cond ((string= (car cookie) "cart") (setf cart (json:decode-json-from-string (cdr cookie))))
                                    ((string= (car cookie) "user") (setf user (json:decode-json-from-string (cdr cookie))))
                                    (t nil)))
                          (hunchentoot:cookies-in hunchentoot:*request*))
                  (setf products (mapcar #'(lambda (product)
                                             (let* ((articul (parse-integer (cdr (assoc :id  product)) :junk-allowed t))
                                                    (cnt     (cdr (assoc :count product)))
                                                    (plist   (product:plist-representation (gethash articul trans:*product*)
                                                                                           :articul
                                                                                           :name
                                                                                           :price))
                                                    (sum     (* (getf plist :price) cnt)))
                                               (setf itogo (+ itogo sum))
                                               (list* :cnt cnt
                                                      :sum sum
                                                      plist)))
                                         cart))
                  (setf auth     (cdr (assoc :auth user)))
                  (setf delivery (cdr (assoc :delivery user)))
                  (setf pay      (cdr (assoc :pay user)))
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
                                                   :itogo itogo
                                                   )))
                  (setf mail-file (list :order_id order-id
                                        :ekk ""
                                        :name (cdr (assoc :name auth))
                                        :family (cdr (assoc :family auth))
                                        :addr (cdr (assoc :addr delivery))
                                        :phone (cdr (assoc :phone auth))
                                        :email (cdr (assoc :email auth))
                                        :date (get-date-time)
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
                  (send-mail (list "avenger-f@yandex.ru") client-mail filename (sendmail:mailfile mail-file) order-id)
                  (send-mail (list "stetoscop@gmail.com") client-mail filename (sendmail:mailfile mail-file) order-id)
                  (send-mail (list "shop@320-8080.ru") client-mail filename (sendmail:mailfile mail-file) order-id)
                  (send-mail (list "zakaz320@yandex.ru") client-mail filename (sendmail:mailfile mail-file) order-id)
                  (send-client-mail (list (cdr (assoc :email auth))) client-mail order-id)
                  (service:checkout-page (checkout:thanks (list :order (checkout:order))))))))



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

