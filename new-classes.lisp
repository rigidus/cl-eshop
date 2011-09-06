(in-package #:eshop)

;;набор функций для показа и изменения различных типов полей в админке
;;new-classes.view-*-field - просмотр
;;new-classes.*-field-get-data - декодирование из строки post-запроса
;;тип поля указывается в :type при создании класса (см. classes.lisp)

;;string - универсальный тип, так же используется как undefined
(defun new-classes.view-string-field (value name disabled)
  (soy.class_forms:string-field
   (list :name name :disabled disabled :value value)))

(defun new-classes.string-field-get-data (string)
  string)

;;int
(defun new-classes.view-int-field (value name disabled)
  (new-classes.view-string-field (format nil "~a" value) name disabled))

(defun new-classes.int-field-get-data (string)
  (parse-integer string))


;;textedit, онлайновый WYSIWYG редактор текста
(defun new-classes.view-textedit-field (value name disabled)
  (if disabled
      (new-classes.view-string-field value name disabled)
      (soy.class_forms:texteditor
       (list :name name :value value))))

(defun new-classes.textedit-field-get-data (string)
  string)


;;time, человекопонятное время
(defun new-classes.view-time-field (value name disabled)
  (new-classes.view-string-field (time.decode-date-time value) name disabled))

(defun new-classes.time-field-get-data (string)
  string)


;;bool
(defun new-classes.view-bool-field (value name disabled)
  (soy.class_forms:bool-field
   (list :name name :checked value :disabled disabled)))

(defun new-classes.bool-field-get-data (string)
  (string= string "T"))


;;group, список групп, генерируется из списка с проставленными уровнями глубины
(defun new-classes.view-group-field (value name disabled)
  (soy.class_forms:group-field (list :name name
                                     :tree (new-classes.group-tree value name)
                                     :disabled disabled)))

;;генерация ветви дерева с корнем в данной группе
;;group - корень ветви, open-group - группа, до которой нужно раскрыть дерево
;;field-name - имя поля, нужно для проставления в name в radio
(defun new-classes.group-branch (group open-group field-name)
  (let ((open (eq group open-group)) (children))
    ;;строим список дочерних ветвей и проверяем нужно ли открыть данный узел
    (setf children
          (mapcar #'(lambda (child)
                      (multiple-value-bind (branch branch-opened)
                          (new-classes.group-branch child open-group field-name)
                        (setf open (or open branch-opened))
                        branch))
                  (storage.get-group-children group)))
    (values-list (list
                  (soy.class_forms:group-tree-branch (list :opened open
                                                           :hashkey (key group)
                                                           :name (name group)
                                                           :checked (eq group open-group)
                                                           :children children
                                                           :fieldname field-name))
                  open))))

;;генерация дерева групп
;;open - группа до которой нужно раскрыть дерево
;;field-name - название поля, нужно для проставления в name в radio
(defun new-classes.group-tree (open field-name)
  (let ((roots (root-groups *global-storage*)))
    (soy.class_forms:group-tree (list :roots
                                      (mapcar #'(lambda (child)
                                                  (new-classes.group-branch child open field-name))
                                              roots)))))


(defun new-classes.group-field-get-data (string)
  (gethash string (storage *global-storage*)))





;;макрос для создания класса по списку параметров
(defmacro new-classes.make-class (name class-fields)
  `(defclass ,name ()
     ,(mapcar #'(lambda (field)
                  `(,(getf field :name)
                     :initarg ,(getf field :initarg)
                     :initform ,(getf field :initform)
                     :accessor ,(getf field :accessor)))
              class-fields)))


;;макрос для создания методов просмотра по списку параметров
(defmacro new-classes.make-view-method (name class-fields)
  `(defmethod new-classes.make-fields ((object ,name))
     ,(cons
       `list
       (mapcar #'(lambda (field)
                   `(,(intern (string-upcase
                               (format nil "new-classes.view-~a-field" (getf field :type))))
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
                                       (format nil "new-classes.~a-field-get-data" (getf field :type))))
                              (decode-uri (getf post-data-plist ,(intern (string-upcase (format nil "~a" (getf field :name))) :keyword)))))))
               class-fields))))

;;декодирование опций из списка опций
(defmethod new-classes.decode (in-list (dummy option))
  (make-instance 'option
                 :name (cdr (assoc :name in-list))
                 :value (format nil "~a" (cdr (assoc :value in-list)))
                 :optype (cdr (assoc :optype in-list))
                 :boolflag (cdr (assoc :boolflag in-list))))

;;декодирование optgroup из списка
(defmethod new-classes.decode (in-list (dummy optgroup))
  (make-instance 'optgroup
                 :name (cdr (assoc :name in-list))
                 :options (loop
                             :for option-elt
                             :in (cdr (assoc :options in-list))
                             :collect (unserialize option-elt (make-instance 'option)))))

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
       ;;хендл ошибок при десереализации
       (handler-bind ((WRONG-PRODUCT-SLOT-VALUE
                       #'(lambda (in-condition)
                           (restart-case
                               (error 'WRONG-PRODUCT-FILE
                                      :filepath filepath
                                      :in-condition in-condition)
                             (ignore ()
                               :report "Ignore error, save current value"
                               (invoke-restart 'ignore))
                             (set-null ()
                               :report "Set value as NIL"
                               (invoke-restart 'set-null))))))
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
           ;; Сохраняем продукт в хэш-таблице
           ;; Возвращаем key продукта
           (setf (key item) (storage.add-new item))))))


;;вызывается после десереализации продукта
(defmethod new-classes.post-unserialize ((item product))
  ;; после десериализации в parent лежит key родительской группы
  (when (not (null (parent item)))
      (setf (parent item) (gethash (parent item) (storage *global-storage*))))
  ;; Если родитель продукта — группа, связать группу с этим продуктом
  (when (equal 'group (type-of (parent item)))
    (push item (products (parent item))))
  ;;active - если имеется в наличии и цена > 0
  (setf (active item) (and (> (count-total item) 0) (> (siteprice item) 0)))
  ;;парсим опции продукта
  (setf (optgroups item)
        (loop :for optgroup-elt :in (optgroups item) :collect
           (new-classes.decode optgroup-elt (make-instance 'optgroup)))))

;;вызывается после десереализации группы
(defmethod new-classes.post-unserialize ((item group))
  (setf (keyoptions item) (mapcar #'(lambda (pair)
                                      (list :optgroup (cdr (assoc :optgroup pair))
                                            :optname (cdr (assoc :optname pair))))
                                  (keyoptions item)))
  (when (not (null (parent item)))
    (setf (parent item) (gethash (parent item) (storage *global-storage*))))
  (setf (fullfilter item) (new-classes.decode (fullfilter item) (make-instance 'group-filter)))
  (when (equal 'group (type-of (parent item)))
    (push item (childs (parent item)))
    (setf (childs (parent item)) (remove-duplicates (childs (parent item))))))


;;создание класса и методов отображения (в админке), изменения (из админки),
;;сереализации (в файл) и десеарелизации (из файла)
(defun new-classes.make-class-and-methods (name list-fields)
  (eval `(new-classes.make-class ,name ,list-fields))
  (eval `(new-classes.make-view-method ,name ,list-fields))
  (eval `(new-classes.make-edit-method ,name ,list-fields))
  (eval `(new-classes.make-unserialize-method ,name ,list-fields)))

