(in-package #:eshop)

;;макрос для создания класса по списку параметров
(defmacro new-classes.make-class (name class-fields)
  `(defclass ,name ()
     ,(mapcar #'(lambda (field)
                  `(,(getf field :name)
                     :initarg ,(intern (format nil "~a" (getf field :name)) :keyword)
                     :initform ,(getf field :initform)
                     :accessor ,(getf field :name)))
              class-fields)))


;;макрос для создания методов просмотра по списку параметров
(defmacro new-classes.make-view-method (name class-fields)
  `(defmethod new-classes.make-fields ((object ,name))
     ,(cons
       `list
       (mapcar #'(lambda (field)
                   `(,(intern (string-upcase
                               (format nil "object-fields.~a-field-view" (getf field :type))))
                      (,(getf field :name)  object)
                      ,(format nil "~a" (getf field :name))
                      ,(getf field :disabled)))
               class-fields))))


;;макрос для создания методов редактирования
(defmacro new-classes.make-edit-method (name class-fields)
  `(defmethod new-classes.edit-fields ((object ,name) post-data-plist)
     ,(cons
       `progn
       (mapcar #'(lambda (field)
                   (when (not (getf field :disabled))
                     `(setf (,(getf field :name) object)
                            (,(intern (string-upcase
                                       (format nil "object-fields.~a-field-get-data" (getf field :type))))
                              (getf post-data-plist ,(intern (string-upcase (format nil "~a" (getf field :name))) :keyword))))))
               class-fields))))


;;декодирование fullfilter
(defmethod new-classes.decode (in-string (dummy group-filter))
  (if (null in-string)
      nil
      (let* ((tmp (read-from-string in-string)))
        (make-instance 'group-filter
                       :name (getf tmp :name)
                       :base (getf tmp :base)
                       :advanced (getf tmp :advanced)))))

;;макрос для создания метода десериализации класса из файла, по данным имени класса и списку полей
(defmacro new-classes.make-unserialize-method (name class-fields)
  `(defmethod unserialize (filepath (dummy ,name))
       ;;читаем из файла и декодируем json
       (let* ((file-content (alexandria:read-file-into-string filepath))
              (raw (decode-json-from-string file-content))
              ;;создаем объект с прочитанными из файла полями
              (item
               ,(let ((res (append (list `make-instance) (list `(quote ,name)))))
                     (mapcar
                      #'(lambda (field)
                          (setf res (append res (let ((name (intern (string-upcase (format nil "~a" (getf field :name))) :keyword)))
                                                  `(,name (cdr (assoc ,name raw)))))))
                      class-fields)
                     res)))
         ;;пост-обработка
         (new-classes.post-unserialize item)
         ;; Возвращаем десериализованный объект
         item)))


;;вызывается после десереализации продукта
(defmethod new-classes.post-unserialize ((item product))
  ;; после десериализации в parent лежит список key родительских групп
  (setf (parents item)
        (mapcar #'(lambda (parent-key)
                    (when (not (null parent-key))
                      (let ((parent (gethash parent-key (storage *global-storage*))))
                        ;;Если родитель продукта — группа, связать группу с этим продуктом
                        (when (equal 'group (type-of parent))
                          (push item (products parent))
                          parent))))
                (parents item)))
  ;;active - если имеется в наличии и цена > 0
  (setf (active item) (and (> (count-total item) 0) (> (siteprice item) 0)))
  ;;преобразуем optgroups из списка alist в список plist
  (let ((optgroups (optgroups item)))
    ;;преобразуем optgroups (1 уровень)
    (setf optgroups
          (mapcar #'(lambda (optgroup)
                      (alist-to-plist optgroup))
                  optgroups))
    ;;преобразуем значение :options в plist (2 уровень)
    (mapcar #'(lambda (optgroup)
                (setf (getf optgroup :options)
                      (mapcar #'(lambda (option)
                                  (alist-to-plist option))
                              (getf optgroup :options))))
            optgroups)
    (setf (optgroups item) optgroups)))


;;вызывается после десереализации группы
(defmethod new-classes.post-unserialize ((item group))
  ;; после десериализации в parent лежит список key родительских групп
  (setf (parents item)
        (mapcar #'(lambda (parent-key)
                    (when (not (null parent-key))
                      (let ((parent (gethash parent-key (storage *global-storage*))))
                        ;;Если родитель — группа, связать группу с данной
                        (when (equal 'group (type-of parent))
                          (push item (groups parent))
                          parent))))
                (parents item)))
  (mapcar #'(lambda (child)
              (setf (empty item) (or (empty item) (active child)))) (products item))
  (setf (keyoptions item) (mapcar #'(lambda (pair)
                                      (list :optgroup (cdr (assoc :optgroup pair))
                                            :optname (cdr (assoc :optname pair))))
                                  (keyoptions item)))
  (setf (fullfilter item) (new-classes.decode (fullfilter item) (make-instance 'group-filter))))



;;макрос для создания метода сериализации
(defmacro new-classes.make-serialize-method (name class-fields)
  `(list (defmethod serialize-entity ((object ,name))
     (format nil "{~%~{~a~^, ~%~}~%}"
             ,(cons
               `list
               (mapcar #'(lambda (field)
                           `(format nil "~a : ~a"
                                    (encode-json-to-string (quote ,(getf field :name)))
                                    (,(intern (string-upcase
                                               (format nil "object-fields.~a-field-serialize" (getf field :type))))
                                      (,(getf field :name)  object))))
                       (remove-if-not #'(lambda (field)
                                          (getf field :serialize))
                                      class-fields)))))
  (defmethod serialize-to-file ((object ,name) pathname)
     (with-open-file (file pathname
                           :direction :output
                           :if-exists :supersede
                           :external-format :utf-8)
       (format file "~a" (serialize-entity object))))))




;; (defmacro new-classes.make-serialize-method (name class-fields)
;;   `(defmethod serialize ((object ,name))
;;      (let* ((raw-breadcrumbs (new-classes.breadcrumbs object))
;;             (path-list (mapcar #'(lambda (elt)
;;                                    (getf elt :key))
;;                                (getf raw-breadcrumbs :breadcrumbelts)))
;;             (current-dir (format nil "~a~a/" *path-to-bkps*
;;                                  (format nil "~{/~a~}" path-list)))
;;          (pathname (format nil "~a~a" current-dir (articul object))))
;;        ;; Создаем директорию, если ее нет
;;        (ensure-directories-exist current-dir)
;;        ;; Сохраняем файл продукта
;;        (let* ((json-string (format nil "{~%   \"articul\": ~a,~%   \"name\": ~a,~%   \"realname\": ~a,~%   \"price\": ~a,~%   \"siteprice\": ~a,~%   \"date-modified\": ~a,~%  \"date-created\": ~a,~%  \"bonuscount\": ~a,~% \"predzakaz\": ~a,~%   \"active\": ~a,~%   \"newbie\": ~a,~%   \"sale\": ~a,~%   \"descr\": ~a,~%   \"shortdescr\": ~a,~%   \"countTransit\": ~a,~%   \"countTotal\": ~a,~%   \"optgroups\": ~a, \"delivery-price\": ~a~%}"
;;                                 (encode-json-to-string (articul object))
;;                                 (format nil "\"~a\"" (stripper (name object)))
;;                                 (format nil "\"~a\"" (stripper (realname object)))
;;                                 (encode-json-to-string (price object))
;;                                 (encode-json-to-string (siteprice object))
;;                                 (encode-json-to-string (date-modified object))
;;                                 (encode-json-to-string (date-created object))
;;                                 (encode-json-to-string (bonuscount object))
;;                                 (encode-json-to-string (predzakaz object))
;;                                 (encode-json-to-string (active object))
;;                                 (encode-json-to-string (newbie object))
;;                                 (encode-json-to-string (sale object))
;;                                 (format nil "\"~a\"" (stripper (descr object)))
;;                                 (format nil "\"~a\""(stripper (shortdescr object)))
;;                                 (encode-json-to-string (count-transit object))
;;                                 (encode-json-to-string (count-total object))
;;                                 (if (null (optgroups object))
;;                                     (format nil " null")
;;                                     (format nil " [    ~{~a~^,~}~%   ]~%"
;;                                             (mapcar #'(lambda (optgroup)
;;                                                         (serialize optgroup))
;;                                                     (optgroups object)))
;;                                     )
;;                                 (encode-json-to-string (delivery-price object))
;;                                 )))
;;       ;; (print (descr object))
;;       (with-open-file (file pathname
;;                             :direction :output
;;                             :if-exists :supersede
;;                             :external-format :utf-8)
;;         ;; (format t json-string)
;;         (format file "~a" json-string)))
;;     pathname))


;; (defun new-classes.breadcrumbs (in &optional out)
;;   (cond ((equal (type-of in) 'product)
;;          (progn
;;            (push (list :key (articul in) :val (name in)) out)
;;            (setf in (car (parents in)))))
;;         ((equal (type-of in) 'group)
;;          (progn
;;            (push (list :key (key in) :val (name in)) out)
;;            (setf in (parent in))))
;;         ((equal (type-of in) 'filter)
;;          (progn
;;            (push (list :key (key in) :val (name in)) out)
;;            (setf in (parent in))))
;;         (t (if (null in)
;;                ;; Конец рекурсии
;;                (return-from new-classes.breadcrumbs
;;                  (list :breadcrumbelts (butlast out)
;;                        :breadcrumbtail (car (last out))))
;;                ;; else - Ищем по строковому значению
;;                (let ((parent (gethash in *storage*)))
;;                  (cond ((equal 'group (type-of parent)) (setf in parent))
;;                        ((null parent) (return-from new-classes.breadcrumbs (list :breadcrumbelts (butlast out)
;;                                                                      :breadcrumbtail (car (last out)))))
;;                        (t (error "breadcrumb link error")))))))
;;   (breadcrumbs in out))

;;создание класса и методов отображения (в админке), изменения (из админки),
;;сереализации (в файл) и десеарелизации (из файла)
(defun new-classes.make-class-and-methods (name list-fields)
  (eval `(new-classes.make-class ,name ,list-fields))
  (eval `(new-classes.make-view-method ,name ,list-fields))
  (eval `(new-classes.make-edit-method ,name ,list-fields))
  (eval `(new-classes.make-unserialize-method ,name ,list-fields))
  (eval `(new-classes.make-serialize-method ,name ,list-fields)))

