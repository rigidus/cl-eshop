;;;; newcart.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(in-package #:eshop)

;;категория для логирования
(log5:defcategory :newcart-log)

;;старт логирования ошибок в стандартный поток ошибок
(log5:start-sender 'newcart-sender
                   (log5:stream-sender :location *error-output*)
                   :category-spec 'log5:warn+
                   :output-spec '("WARN:: " log5:message))

;;обновление страницы
(defun newcart-update ()
  (newcart-compile-templates))

;;шаблоны
(defun newcart-compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("index.html"
            "newcart.soy"
            "footer.html")))

;; (newcart-update)

;; возвращает список отображений продуктов, количество, суммарную цену заказа
(defun newcart-cart-products (alist)
  (let ((counter 0)
        (sum-counter 0)
        (sum 0)
        (res-list))
    ;; (error (print alist))
    (setf res-list (mapcar #'(lambda (item)
                               (let ((articul (cdr (assoc :id  item)))
                                     (cnt     (cdr (assoc :count item)))
                                     (product)
                                     (price 0))
                                 (when (and (not (null articul))
                                            (not (null cnt)))
                                   (setf cnt (parse-integer (format nil "~a" cnt) :junk-allowed t))
                                   (setf product (gethash articul (storage *global-storage*)))
                                   (when (and (not (null product))
                                              (not (= 0 cnt)))
                                     (incf counter)
                                     (setf price (if (= 0 (siteprice product))
                                                     (price product)
                                                     (siteprice product)))
                                     (setf sum (+ sum (* cnt price)))
                                     (setf sum-counter (+ sum-counter cnt))
                                     (list :numpos counter
                                           :count cnt
                                           :name (name-seo product)
                                           :price price
                                           ;;данные для корзины
                                           :siteprice (siteprice product)
                                           :articul (if (= 5 (length articul))
                                                        (format nil "0~a" articul)
                                                        articul)
                                           :url (format nil "/~a" articul)
                                           :firstpic (car (get-pics articul))
                                           ;;для sendmail
                                           :cnt cnt)))))
                           alist))
    (values-list (list res-list sum-counter sum))))


(defun newcart-tovar (n)
  (let ((k n))
    (if (< 20 n)
        (setf k (mod n 10)))
    (if (= 0 k)
        "товаров"
        (if (= 1 k)
            "товар"
            (if (> 5 k)
                "товара"
                "товаров")))))




;;отображение страницы
;; (defun newcart-show (&optional (request-str ""))
;;   (declare (ignore request-str))
;;   (let ((cart-cookie (hunchentoot:cookie-in "cart"))
;;         (cart)
;;         (products)
;;         (count 0)
;;         (pricesum 0))
;;     (when (not (null cart-cookie))
;;       (setf cart (json:decode-json-from-string cart-cookie))
;;       (multiple-value-bind (lst cnt sm) (newcart-cart-products cart)
;;         (setf products (remove-if #'null lst))
;;         (setf count cnt)
;;         (setf pricesum sm)))
;;     (if (and (not (null products))
;;              (< 0 pricesum)
;;              (< 0 count))
;;         (progn
;;           (soy.newcart:fullpage (list :head (root:newcart-head)
;;                                       :header (soy.newcart:header)
;;                                       :footer (root:newcart-footer)
;;                                       :leftcells (soy.newcart:leftcells)
;;                                       :rightcells (soy.newcart:rightcells
;;                                                    (list :notfinished "true"
;;                                                          :deliverysum pricesum
;;                                                          :productscount count
;;                                                          :tovar (newcart-tovar count)
;;                                                          :products (mapcar #'soy.newcart:product-item products))))))
;;         (progn
;;           ;; страница для пустой корзины с автоматическим редиректом на главную
;;           ;; (log5:log-for (or eshop::newcart-log
;;           ;;                   log5:warn+) "trying to checkout null cart")
;;           (soy.newcart:fullpage (list :head (soy.newcart:head-redirect (list :timeout 5
;;                                                                              :url "/"))
;;                                       :header (soy.newcart:header)
;;                                       :footer (root:newcart-footer)
;;                                       :leftcells (soy.newcart:leftcells-empty)))))))


;; корзина товаров
;; (defun cart-page ()
;;   (let ((cart-cookie (hunchentoot:cookie-in "cart"))
;;         (cart);; dbg  (list (list (cons :id "145982") (cons :count 2))))
;;         (products)
;;         (count)
;;         (pricesum))
;;     (when (not (null cart-cookie))
;;       (setf cart (json:decode-json-from-string cart-cookie))
;;       (multiple-value-bind (lst cnt sm) (newcart-cart-products cart)
;;         (setf products (remove-if #'null lst))
;;         ;; (print products)
;;         (setf count cnt)
;;         (setf pricesum sm)))
;;     (if (not (null products))
;;         (default-page
;;             (soy.newcart:cart-content (list :accessories (soy.product:accessories)
;;                                             :products (format nil "~{~a~}"
;;                                                               (mapcar #'(lambda (x)
;;                                                                           (print (cart:product x))
;;                                                                           (soy.newcart:cart-product x))
;;                                                                       products))))
;;             :no-need-cart t)
;;         ;; страница для пустой корзины с автоматическим редиректом на главную
;;         (soy.newcart:fullpage (list :head (soy.newcart:head-redirect (list :timeout 5
;;                                                                            :url "/"))
;;                                     :header (soy.newcart:header)
;;                                     :footer (root:newcart-footer)
;;                                     :leftcells (soy.newcart:leftcells-empty))))))

;; извлекает данные из ассоциативного списка и нормализовывает
(defun newcart-get-data-from-alist(symbol-name alist)
  (string-trim (list #\Space #\Tab #\Newline) (format nil "~@[~a~]" (cdr (assoc symbol-name alist)))))

;; данные о пользователе
(defun newcart-user (user)
  (let ((phone         (newcart-get-data-from-alist :phone user))  ;; обязательное поле телефон
        ;;два вида доставки курьером и самовывоз (express | pickup)
        (delivery-type (newcart-get-data-from-alist :delivery-type user))
        (name          (newcart-get-data-from-alist :name user)) ;; имя
        (family        (newcart-get-data-from-alist :family user)) ;; фамилия
        (email         (newcart-get-data-from-alist :email user)) ;; email заказчика
        (city          (newcart-get-data-from-alist :city user))
        (addr          (newcart-get-data-from-alist :addr user)) ;; адрес без города
        (courier_comment (newcart-get-data-from-alist :courier--comment user)) ;; комментарий курьеру
        (pickup          (newcart-get-data-from-alist :pickup user)) ;; pickup-1 Левашовский
        (pickup_comment  (newcart-get-data-from-alist :pickup--comment user)) ;; комментарий к заказу
        (payment         (newcart-get-data-from-alist :payment user)) ;; payment_method-1
        (bankaccount     (newcart-get-data-from-alist :bankaccount user)) ;; реквизиты банковского перевода
        (discount-cart   (newcart-get-data-from-alist :discount-card user)) ;; карта ЕКК (true | false)
        (discount-cart-number   (newcart-get-data-from-alist :DISCOUNT-CARD-NUMBER user)) ;; номер карты
        (ekk nil))
    ;; проставление значений по умолчанию
    (if (string= delivery-type "") (setf delivery-type "pickup"))
    (if (string= payment "") (setf payment "payment_method-1"))
    ;;Выставляем адрес доставки для филиалов
    (when (string= delivery-type "pickup")
      (setf addr (cond ((string= pickup "pickup-1") "Левашовский пр., д.12")
                       ((string= pickup "pickup-2") "Петергоф, ул. Ботаническая, д.18, к.3")
                       (t "Левашовский пр., д.12")))) ;; по умолчанию главный магазин
    (if discount-cart
        (setf ekk discount-cart-number))
    (values-list (list phone delivery-type name email city addr courier_comment pickup pickup_comment payment bankaccount ekk family))))

;; страница информации об отправленном заказе
;; (defun thanks-page ()
;;   (let ((cart) ;; ;; dbg  (list (list (cons :id "145982") (cons :count 2)))) ;; товары
;;         (user) ;; данные о пользователе
;;         (products)
;;         (count)  ;; количество товаров в корзине
;;         (pricesum)) ;; сумма заказа
;;     ;; кукисы пользователя
;;     (mapcar #'(lambda (cookie)
;;                 (cond ((string= (car cookie) "cart") (setf cart (json:decode-json-from-string (cdr cookie))))
;;                       ((string= (car cookie) "user-nc") (setf user (json:decode-json-from-string (cdr cookie))))
;;                       (t nil)))
;;             (hunchentoot:cookies-in hunchentoot:*request*))
;;     ;;;;; DBG
;;     ;; (setf cart (list (list (cons :id "145982") (cons :count 2))))
;;     ;; (setf user (list (cons :phone "145982") (cons :email "wolforus@gmail.com")))
;;     ;;;;;
;;     ;;если кукисы не пустые
;;     (when (and (not (null cart))
;;                (not (null user)))
;;       (multiple-value-bind (lst cnt sm) (newcart-cart-products cart)
;;         (setf products (remove-if #'null lst))
;;         ;; (print products)
;;         (setf count cnt)
;;         (setf pricesum sm)))
;;     (if (and (not (null products))
;;              (not (null (newcart-get-data-from-alist :phone user))))
;;         ;; если в заказе есть валидные товары и телефон пользователя
;;         ;; генерация идентификатора заказа происходит только если заказ валиден
;;         (let ((order-id (get-order-id)) ;; генерируем ID заказа
;;               (deliverysum 0) ;;цена доставки
;;               (client-mail) ;; текст письма с информацие о заказе для клиента
;;               (mail-file) ;; информация для ТКС
;;               (tks-mail) ;; файл с информацией о заказе для ТКС
;;               (filename)) ;;
;;           (multiple-value-bind (phone delivery-type name email city addr courier_comment pickup pickup_comment payment bankaccount ekk family)
;;               (newcart-user user)
;;             (declare (ignore city))
;;             ;; Временно доставка 300 на все
;;             ;; существует два вида доставки: курьером и самовывоз (express | pickup)
;;             (if  (string= delivery-type "express")
;;                  (setf deliverysum (yml.get-delivery-price (newcart-cart-products cart))))
;;             (format t "EKK: ~a" ekk)
;;             (setf client-mail
;;                   (sendmail:clientmail
;;                    (list :datetime (time.get-date-time)
;;                          :order_id order-id
;;                          :name (format nil "~a ~a" name family)
;;                          :family "" ;; Фамилия не передается отдельно
;;                          :paytype (cond ((string= payment "payment_method-1") "Наличными")
;;                                         ((string= payment "payment_method-2") "Кредитной картой")
;;                                         ((string= payment "payment_method-3") "Безналичный расчет")
;;                                         ((string= payment "payment_method-4") "Банковским переводом")
;;                                         (t payment))
;;                          :deliverytype (cond ((string= delivery-type "express") "Курьер")
;;                                              ((string= delivery-type "pickup") "Самовывоз")
;;                                              (t delivery-type))
;;                          :addr addr
;;                          :bankaccount (if (string= payment "payment_method-4")
;;                                           bankaccount)
;;                          :phone phone
;;                          :ekk ekk
;;                          :email email
;;                          :comment (cond  ((string= delivery-type "express") courier_comment)
;;                                          ((string= delivery-type "pickup") pickup_comment)
;;                                          (t ""))
;;                          :products products
;;                          :deliverysum deliverysum
;;                          :itogo (+ pricesum deliverysum))))
;;             (setf mail-file (list :order_id order-id
;;                                   :ekk ekk
;;                                   :name (format nil "~a ~a" name family)
;;                                   :family ""
;;                                   :addr addr
;;                                   :isdelivery (cond ((string= delivery-type "express") "Доставка")
;;                                                     ((string= delivery-type "pickup") "Самовывоз")
;;                                                     (t delivery-type)) ;;"Доставка" ;; Самовывоз
;;                                   :phone phone
;;                                   :email email
;;                                   :date (time.get-date)
;;                                   :time (time.get-time)
;;                                   :comment (cond  ((string= delivery-type "express") courier_comment)
;;                                                   ((string= delivery-type "pickup") pickup_comment)
;;                                                   (t ""))
;;                                   :products (append products
;;                                                     (if (string= delivery-type "express")
;;                                                         (list (list :articul "107209"
;;                                                                     :cnt "1"))))))
;;             (setf filename (format nil "~a_~a.txt" (time.get-short-date) order-id))
;;             ;;сорханение заказа
;;             (save-order-text order-id client-mail)
;;             ;; удаление страных символов
;;             (setf client-mail (remove-if #'(lambda(c) (< 10000 (char-code c))) client-mail))
;;             (setf tks-mail (remove-if #'(lambda(c) (< 10000 (char-code c))) (sendmail:mailfile mail-file)))
;;             (mapcar #'(lambda (email)
;;                         (send-mail (list email) client-mail filename tks-mail order-id))
;;                     *conf.emails.cart*)
;;                         ;; артикул 099999 и доставка 107209
;;             ;; сделать валидацию пользовательского email
;;             (if (not (string= email ""))
;;                 (send-client-mail (list email) client-mail order-id))
;;             (soy.newcart:fullpage
;;              (list :head (root:newcart-head)
;;                    :header (soy.newcart:header-linked)
;;                    :footer (root:newcart-footer)
;;                    :leftcells (soy.newcart:thanks
;;                                (list :sum pricesum
;;                                      :deliverysum deliverysum
;;                                      :comment  (let ((comment (format nil "~a"
;;                                                                       (cond  ((string= delivery-type "express") courier_comment)
;;                                                                              ((string= delivery-type "pickup") pickup_comment)
;;                                                                              (t "")))))
;;                                                  (if (equal comment "") nil comment))
;;                                      :email (if (equal email "") nil email)
;;                                      :name (if (equal name "") nil name)
;;                                      :courier (equal delivery-type "express")
;;                                      :oplata (cond ((string= payment "payment_method-1")
;;                                                     "<p class=\"h2\">Оплата наличными</p><p>Вы получите кассовый товарный чек</p>")
;;                                                    ((string= payment "payment_method-2")
;;                                                     "<p class=\"h2\">Оплата кредитной картой</p>")
;;                                                    ((string= payment "payment_method-3")
;;                                                     "<p class=\"h2\">Покупка в кредит</p>")
;;                                                    ((string= payment "payment_method-4")
;;                                                     (format nil "<p class=\"h2\">Оплата по безналичному расчету</p>
;; 						                             <p>Реквизиты:<br/>~a</p>" bankaccount))
;;                                                    (t nil))
;;                                      :addr (if (string= delivery-type "pickup")
;;                                                addr
;;                                                "Левашовский пр., д.12")
;;                                      :ekk ekk
;;                                      :map (if (and (equal delivery-type "pickup")
;;                                                    (equal pickup "pickup-2"))
;;                                               (soy.newcart:map-botanicheskaya-img)
;;                                               (soy.newcart:map-levashovskii-img))
;;                                      :order_id order-id))
;;                    :rightcells (soy.newcart:rightcells
;;                                 (list :pricesum pricesum
;;                                       :deliverysum deliverysum
;;                                       :productscount count
;;                                       :tovar (newcart-tovar count)
;;                                       :products (mapcar #'soy.newcart:product-item  products)))))
;;             ))
;;         (progn
;;           (soy.newcart:fullpage (list :head (soy.newcart:head-redirect (list :timeout 5
;;                                                                              :url "/"))
;;                                       :header (soy.newcart:header)
;;                                       :footer (root:newcart-footer)
;;                                       :leftcells (soy.newcart:leftcells-empty)))))))

