(in-package #:product)

;; special in now know as sale

(defclass product ()
  ((articul        :initarg :articul         :initform nil       :reader   articul) ;; nil запрещен!
   (parent         :initarg :parent          :initform nil       :accessor parent)

   (name           :initarg :name            :initform ""        :accessor name)
   (realname       :initarg :realname        :initform ""        :accessor realname)

   (price          :initarg :price           :initform 0         :accessor price)
   (siteprice      :initarg :siteprice       :initform 0         :accessor siteprice)
   (ekkprice       :initarg :ekkprice        :initform 0         :accessor ekkprice)

   (active         :initarg :active          :initform t         :accessor active)
   (newbie         :initarg :newbie          :initform t         :accessor newbie)
   (sale           :initarg :sale            :initform t         :accessor sale)

   (descr          :initarg :descr           :initform ""        :accessor descr)
   (shortdescr     :initarg :shortdescr      :initform ""        :accessor shortdescr)

   (count-transit  :initarg :count-transit   :initform 0         :accessor count-transit)
   (count-total    :initarg :count-total     :initform 0         :accessor count-total)

   (options        :initarg :options         :initform nil       :accessor options)
   ))


(defun get-procent (base real)
  (when (equal 0 base)
    (return-from get-procent (values 0 0)))
  (if (or (null base) (null real))
	  (return-from get-procent 0))
  (values (format nil "~$"  (- base real))
          (format nil "~1$" (- 100 (/ (* real 100) base)))))


(defun get-pics (articul)
  (let ((path (format nil "~a/big/~a/*.jpg" *path-to-pics* articul)))
    (loop
       :for pic
       :in (ignore-errors (directory path))
       :collect (format nil "~a.~a"
                        (pathname-name pic)
                        (pathname-type pic)))))


(defmethod show ((object product))
  "[X] Big – большая картинка на карточке товара
   [X] Goods ( размеры длина до 225, высота - 195) – изображение на карточке товара
   [X] Minigoods (70х70)– изображение товара на карточке товара (маленькое, в ленте фоток под основной картинкой)
   [ ] Middle (длина до 200, высота 160)– картинка товара, использующаяся для превью товаров (разделы каталога, ленты Лучшие цены, Новинки)
   [ ] Small (100х120) – картинка товара, использующаяся для превью (блок Хиты продаж, блок Вы смотрели)"
  (multiple-value-bind (diffprice procent)
      (get-procent (price object) (siteprice object))
    (let ((pics (get-pics (articul object))))
      (service::default-page
        (product:content (list :menu (service:menu object)
                               :breadcrumbs (product:breadcrumbs (service:breadcrumbs object))
                               :articul (articul object)
                               :group_id "999" ;;(group:id (group object))
                               :name (realname object)
                               :siteprice (siteprice object)
                               :storeprice (price object)
                               :diffprice diffprice
                               :procent procent
                               :subst (format nil "/~a" (articul object))
                               ;; :goods (if (not goods) "" (car goods))
                               ;; :minigoods (loop
                               ;;               :for minigood
                               ;;               :in minigoods
                               ;;               :collect (list :mini minigood))
                               :pics pics
                               :firstpic (if (null pics) "" (car pics))
                               :optlist (if (null (options object))
                                            ""
                                            (optlist:show (options object)))
                               :accessories (product:accessories)
                               :reviews (product:reviews)
                               :simular (product:simulars)
                               :others (product:others)
                               :keyoptions (get-keyoptions object)
                               :active (active object)
                               :descr (descr object)
                               :shortdescr (shortdescr object)
                               ))))))


(defmethod get-keyoptions ((object product))
  (let ((parent (parent object)))
    (when (null parent)
      (return-from get-keyoptions nil))
    (mapcar #'(lambda (pair)
                (let ((optgroup (getf pair :optgroup))
                      (optname  (getf pair :optname))
                      (optvalue))
                  (mapcar #'(lambda (option)
                              (if (string= (optgroup::name option) optgroup)
                                  (let ((options (optgroup::options option)))
                                    (mapcar #'(lambda (opt)
                                                (if (string= (option::name opt) optname)
                                                    (setf optvalue (option::value opt))))
                                            options))))
                          (optlist:optlist (product:options object)))
                  (list :optgroup optgroup
                        :optname optname
                        :optvalue optvalue)
                  ))
            (group:keyoptions parent))))


(defmethod view ((object product))
  (let ((pics (get-pics (articul object))))
    (let ((group (parent object)))
      ;; (when (not (null group))
      (list :articul (articul object)
            :name (realname object)
            :groupname (if (null group)
                           "group not found"
                           (group:name group))
            :groupkey  (if (null group)
                           ""
                           (group:key  group))
            :price (price object)
            :firstpic (car pics)
            :group_id (if (null group)
                          ""
                          (group:id group))
            ))))


;; Внутреннее условие, сигнализирующее о неверном значении поля класса
(define-condition wrong-type-of-slot (error) ())


;; Внешнее условие некорректного поля
(define-condition wrong-product-slot-value (error)
  ((text      :initarg :text     :accessor text)
   (value     :initarg :value    :accessor value)
   (product   :initarg :product  :accessor product))
  (:report (lambda (condition stream)
             (format stream "Incorrect value of product slot ~a ~%[value: '~a']~%[type '~a']"
                     (text condition)
                     (value condition)
                     (type-of (value condition))))))


;; Внешнее условие ошибки десериализации
(define-condition wrong-product-file (error)
  ((filepath       :initarg  :filepath       :accessor filepath)
   (in-condition   :initarg  :in-condition   :accessor in-condition))
  (:report (lambda (condition stream)
             (format stream "Unable unserialize product:  ~a ~% Reason: ~a"
                     (filepath condition)
                     (format nil "Incorrect value of product slot ~a ~%   [value: '~a']~%   [type '~a']"
                             (text (in-condition condition))
                             (value (in-condition condition))
                             (type-of (value (in-condition condition))))))))


;; Метод ввода числовых значений при коррекции ошибок
(defun read-new-integer-value ()
  (format t "Enter a new integer value: ")
  (multiple-value-list (eval (read))))


;; Макрос, создающий WRITER-ы для числовых полей
(defmacro make-integer-writer (field)
  `(defmethod (setf ,field) (value (item product))
     (with-slots (,field) item
       (handler-case
           (cond ((typep value 'integer) (setf (slot-value item ',field) value))
                 ((typep value 'string)  (setf (slot-value item ',field)
                                               (handler-case
                                                   (parse-integer value)
                                                 (SB-INT:SIMPLE-PARSE-ERROR ()
                                                   (error 'WRONG-TYPE-OF-SLOT)))))
                 (t (error 'WRONG-TYPE-OF-SLOT )))
         (WRONG-TYPE-OF-SLOT ()
           (restart-case
               (error 'wrong-product-slot-value
                      :text (symbol-name ',field)
                      :value value
                      :product item)
             (enter-correct-value (new)
               :report "Enter a new value"
               :interactive read-new-integer-value
               (setf (,field item) new))
             (ignore ()
               :report "Ignore error, save current value"
               (setf (slot-value item ',field) value))
             (set-null ()
               :report "Set value as NIL"
               (setf (slot-value item ',field) nil))
             ))))))


;; Создаем WRITER-ы для числовых полей
(make-integer-writer articul)
(make-integer-writer count-transit)
(make-integer-writer count-total)


;; Макрос, создающий WRITER-ы для логических полей
(defmacro make-boolean-writer (field)
  `(defmethod (setf ,field) (value (item product))
     (with-slots (,field) item
       (unless (or (equal ,field nil)
                   (equal ,field t))
         (restart-case
             (error 'wrong-product-slot-value
                    :text (symbol-name ',field)
                    :value value
                    :product item)
           (set-null ()
             :report "Set value as NIL"
             (setf (slot-value item ',field) nil))
           (set-t ()
             :report "Set value as T"
             (setf (slot-value item ',field) nil))
           (ignore ()
             :report "Ignore error, save current value"
             (setf (slot-value item ',field) value))
           )))))


;; Создаем WRITER-ы для логических полей
(make-boolean-writer active)
(make-boolean-writer newbie)
(make-boolean-writer sale)
(make-boolean-writer presence)


;; Проверка корректности полей после инициализации instance
(defmethod initialize-instance :after ((item product) &key)
  (with-slots ((articul articul)
               (group group)
               (count-transit count-transit)
               (count-total count-total))
      item
    (unless (typep articul 'integer)       (setf (articul item) articul))
    (unless (typep count-transit 'integer) (setf (count-transit item) count-transit))
    (unless (typep count-total 'integer)   (setf (count-total item) count-total))))


(defun unserialize (pathname)
  (handler-bind ((PRODUCT::WRONG-PRODUCT-SLOT-VALUE
                  #'(lambda (in-condition)
                      (restart-case
                          (error 'wrong-product-file
                                 :filepath pathname
                                 :in-condition in-condition)
                        (ignore ()
                          :report "Ignore error, save current value"
                          (invoke-restart 'ignore))
                        (set-null ()
                          :report "Set value as NIL"
                          (invoke-restart 'set-null))))))
    (let* ((file-content (alexandria:read-file-into-string pathname))
           (raw (decode-json-from-string file-content))
           (articul (cdr (assoc :articul raw)))
           (count-total (cdr (assoc :count-total raw)))
           (parent (gethash (nth 1 (reverse (split-sequence #\/ pathname))) trans:*group*))
           (name (cdr (assoc :name raw)))
           (realname (cdr (assoc :realname raw)))
           (new (make-instance 'product
                               :articul articul
                               :parent parent
                               :name name
                               :realname (if (or (null realname)
                                                 (string= "" realname))
                                             name
                                             realname)
                               :price (cdr (assoc :price raw))
                               :siteprice (cdr (assoc :siteprice raw))
                               :ekkprice (cdr (assoc :ekkprice raw))
                               :active (let ((active (cdr (assoc :active raw))))
                                         ;; Если количество товара равно нулю то флаг active сбрасывается
                                         (if (or (null count-total)
                                                 (= count-total 0))
                                             (setf active nil))
                                         active)
                               :newbie (cdr (assoc :newbie raw))
                               :sale (cdr (assoc :sale raw))
                               :descr (cdr (assoc :descr raw))
                               :shortdescr (cdr (assoc :shortdescr raw))
                               :count-transit (cdr (assoc :count-transit raw))
                               :count-total count-total
                               :options (optlist:unserialize (cdr (assoc :options raw))))))
      ;; Если родитель продукта — группа, связать группу с этим продуктом
      (when (equal 'group:group (type-of parent))
        (push new (group:products parent)))
      ;; Сохраняем продукт в хэш-таблице продуктов
      (setf (gethash articul trans:*product*) new)
      ;; Возвращаем артикул продукта
      articul)))


(defmethod serialize ((object product))
  (let* ((raw-breadcrumbs (service:breadcrumbs object))
         (path-list (mapcar #'(lambda (elt)
                                (getf elt :key))
                            (getf raw-breadcrumbs :breadcrumbelts)))
         (current-dir (format nil "~a~a/" *path-to-bkps*
                              (format nil "~{/~a~}" path-list)))
         (pathname (format nil "~a~a" current-dir (articul object))))
    ;; Создаем директорию, если ее нет
    (ensure-directories-exist current-dir)
    ;; Сохраняем файл продукта
    (let* ((json-string (format nil "{~%   \"articul\": ~a,~%   \"name\": ~a,~%   \"realname\": ~a,~%   \"price\": ~a,~%   \"siteprice\": ~a,~%   \"ekkprice\": ~a,~%   \"active\": ~a,~%   \"newbie\": ~a,~%   \"sale\": ~a,~%   \"descr\": ~a,~%   \"shortdescr\": ~a,~%   \"countTransit\": ~a,~%   \"countTotal\": ~a,~%   \"options\": ~a~%}"
                                (encode-json-to-string (articul object))
                                (format nil "\"~a\"" (stripper (name object)))
                                (format nil "\"~a\"" (stripper (realname object)))
                                (encode-json-to-string (price object))
                                (encode-json-to-string (siteprice object))
                                (encode-json-to-string (ekkprice object))
                                (encode-json-to-string (active object))
                                (encode-json-to-string (newbie object))
                                (encode-json-to-string (sale object))
                                (format nil "\"~a\"" (stripper (descr object)))
                                (format nil "\"~a\""(stripper (shortdescr object)))
                                (encode-json-to-string (count-transit object))
                                (encode-json-to-string (count-total object))
                                (if (null (product:options object))
                                    (format nil " null")
                                    (optlist:serialize (options object)))
                                )))
      ;; (print (descr object))
      (with-open-file (file pathname
                            :direction :output
                            :if-exists :supersede
                            :external-format :utf-8)
        ;; (format t json-string)
        (format file "~a" json-string)))
    pathname))


(defmethod plist-representation ((object product) &rest fields)
  (let ((result))
    (loop :for item :in fields do
       (let ((method (intern (symbol-name item) 'product)))
         (push item result)
         (push (funcall method object) result)))
    (reverse result)))


(funcall *dispatcher*
         `((let ((articul (cadr (service:request-list))))
             (and (not (null articul))
                  (not (null (parse-integer articul :junk-allowed t)))
                  (string= articul
                           (format nil "~a" (parse-integer articul :junk-allowed t)))))
           ,#'(lambda ()
                (let ((product (gethash (parse-integer (cadr (service:request-list))) trans:*product*)))
                  (if (null product)
                      "product not found"
                      (product:show product))))))

