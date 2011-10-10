(in-package #:eshop)


(defun new-classes.view-string-field (value name disabled)
  (soy.class_forms:string-field
   (list :name name :disabled disabled :value value)))

(defun new-classes.string-field-get-data (string)
  string)

(defun new-classes.view-int-field (value name disabled)
  (new-classes.view-string-field (format nil "~a" value) name disabled))

(defun new-classes.int-field-get-data (string)
  (parse-integer string))

(defun new-classes.view-textedit-field (value name disabled)
  (if disabled
      (new-classes.view-string-field value name disabled)
      (soy.class_forms:texteditor
       (list :name name :value value))))

(defun new-classes.textedit-field-get-data (string)
  string)


(defun new-classes.view-time-field (value name disabled)
  (new-classes.view-string-field (time.decode-date-time value) name disabled))

(defun new-classes.time-field-get-data (string)
  string)

(defun new-classes.view-bool-field (value name disabled)
  (soy.class_forms:bool-field
   (list :name name :checked value :disabled disabled)))


(defun new-classes.bool-field-get-data (string)
  (string= string "T"))

(defun new-classes.view-group-field (value name disabled)
  (let ((leveled-groups (storage.get-groups-leveled-tree)))
    (soy.class_forms:group-form
     (list :name name :disabled disabled
           :grouplist (mapcar #'(lambda (group-and-level)
                                  (let ((group (car group-and-level))
                                        (level (cdr group-and-level)))
                                    (list :hashkey (key group)
                                          :selected (eq value group)
                                          :name (name group)
                                          :indent (let ((indent ""))
                                                    (loop for x from 1 to level
                                                       do (setf indent (concatenate 'string indent "---")))
                                                    indent))))
                              leveled-groups)))))

(defun new-classes.group-field-get-data (string)
  (gethash string (storage *global-storage*)))






(defmacro new-classes.make-class (name class-fields)
  `(defclass ,name ()
     ,(mapcar #'(lambda (field)
                  `(,(getf field :name)
                     :initarg ,(getf field :initarg)
                     :initform ,(getf field :initform)
                     :accessor ,(getf field :accessor)))
              class-fields)))

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
  `(list
    (defmethod unserialize (raw (dummy ,name))
      ;;читаем из файла и декодируем json
      (let
          ;;создаем объект с прочитанными из файла полями
          ((item
            ,(let ((res (append (list `make-instance) (list `(quote ,name)))))
                  (mapcar
                   #'(lambda (field)
                       (setf res
                             (append res
                                     (let ((name (intern (string-upcase (format nil "~a" (getf field :name))) :keyword))
                                           (initform (getf field :initform)))
                                       `(,name
                                         (let ((val (cdr (assoc ,name raw))))
                                            (if val
                                                val
                                                ,initform)))))))
                   class-fields)
                  res)))
        item))
    (defmethod unserialize-from-file (filepath (dummy ,name))
      (with-open-file (file filepath)
        (loop for line = (read-line file nil 'EOF)
           until (eq line 'EOF)
           do
             (let ((item (unserialize (decode-json-from-string line)
                                      dummy)))
               (storage.add-new-object item (key item))))))))


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
  ;;adding newlines instead of #Newline
  (setf (seo-text item) (object-fields.string-add-newlines (seo-text item)))
  ;;преобразуем optgroups из списка alist в список plist
  (let ((optgroups))
    ;;преобразуем optgroups (1 уровень)
    (setf optgroups
          (mapcar #'(lambda (optgroup)
                      (alist-to-plist optgroup))
                  (optgroups item)))
    ;;преобразуем значение :options в plist (2 уровень)
    (setf optgroups (mapcar #'(lambda (optgroup)
                          (let ((optgroup-plist
                                 (mapcar #'(lambda (option)
                                             (alist-to-plist option))
                                         (getf optgroup :options))))
                            (list :name (getf optgroup :name) :options optgroup-plist)))
                      optgroups))
    (setf (optgroups item) optgroups))
  )


;;вызывается после десереализации группы
(defmethod new-classes.post-unserialize ((item group))
  ;;adding newlines instead of #Newline
  (setf (seo-text item) (object-fields.string-add-newlines (seo-text item)))
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

(defmethod new-classes.post-unserialize ((item filter))
  ;;adding newlines instead of #Newline
  (setf (func-string item) (object-fields.string-add-newlines (func-string item)))
  ;;evaling func-string to func
  (setf (func item) (eval (read-from-string (func-string item))))
  ;; после десериализации в parent лежит список key родительских групп
  (setf (parents item)
        (mapcar #'(lambda (parent-key)
                    (when (not (null parent-key))
                      (let ((parent (gethash parent-key (storage *global-storage*))))
                        ;;Если родитель — группа, связать группу с фильтром
                        (when (equal 'group (type-of parent))
                          (push item (filters parent))
                          parent))))
                (parents item))))




;;макрос для создания метода сериализации
(defmacro new-classes.make-serialize-method (name class-fields)
  `(list
    (defmethod serialize-entity ((object ,name))
      (format nil "{~{~a~^,~}}"
              (remove-if #'null
                         ,(cons
                           `list
                           (mapcar #'(lambda (field)
                                       `(let ((field-value (,(getf field :name) object)))
                                          (when (and field-value
                                                     (string/= (format nil "~a" field-value) "")
                                                     (not (equal field-value ,(getf field :initform))))
                                            (format nil "~a:~a"
                                                    (encode-json-to-string (quote ,(getf field :name)))
                                                    (,(intern (string-upcase
                                                               (format nil "object-fields.~a-field-serialize" (getf field :type))))
                                                      field-value)))))
                                   (remove-if-not #'(lambda (field)
                                                      (getf field :serialize))
                                                  class-fields))))))
    (defmethod serialize-to-file ((object ,name) pathname)
      (with-open-file (file pathname
                            :direction :output
                            :if-exists :supersede
                            :external-format :utf-8)
        (format file "~a" (serialize-entity object))))))

(defun new-classes.serialize-list-to-file (object-list filepath)
  (with-open-file (file filepath
                              :direction :output
                              :if-exists :supersede
                              :external-format :utf-8)
    (mapcar #'(lambda (object)
                (format file "~a~%" (serialize-entity object)))
            object-list)))


(defun new-classes.unserialize-all ()
  (unserialize-from-file #P"/home/eviltosha/serialize_test/products_old" (make-instance 'product))
  (unserialize-from-file #P"/home/eviltosha/serialize_test/groups" (make-instance 'group))
  (unserialize-from-file #P"/home/eviltosha/serialize_test/filters" (make-instance 'filter))
  (storage.make-lists)
  (maphash #'(lambda (key value)
               (declare (ignore key))
               (new-classes.post-unserialize value))
           (storage *global-storage*)))

(defun new-classes.parent (item)
  "Returns main parent of item"
  (car (parents item)))

(defun new-classes.breadcrumbs (in &optional out)
  "Processing parents until nil, creating breadcrumbs"
  (if (not (null in))
      (progn
        (if (equal (type-of in) 'product)
            (push (list :key (articul in) :val (name-seo in)) out)
            (push (list :key (key in) :val (name in)) out))
        (setf in (new-classes.parent in))
        (new-classes.breadcrumbs in out))
      ;; else -  end of recursion
      (list :breadcrumbelts (butlast out)
            :breadcrumbtail (car (last out)))))

(defun new-classes.get-root-parent (item)
  (let ((parent (new-classes.parent item)))
    (if (or (null item) (null parent))
        item
        (new-classes.get-root-parent parent))))


(defun new-classes.menu-sort (a b)
  "Function for sorting groups by order field"
  (if (or (null (order a))
          (null (order b)))
      nil
      ;; else
      (< (order a)
         (order b))))


(defun new-classes.menu (&optional current-object)
  "Creating left menu"
  (let* ((root-groups (root-groups *global-storage*))
        (current-root (new-classes.get-root-parent current-object))
        (divider-list (list "setevoe-oborudovanie" "foto-and-video" "rashodnye-materialy"))
        (src-lst
         (mapcar #'(lambda (val)
                     (if (equal (key val) (key current-root))
                         ;; This is current
                         (leftmenu:selected
                          (list :divider (notevery #'(lambda (divider)
                                                       (string/= (key val) divider))
                                                   divider-list)
                                :key (key val)
                                :name (name val)
                                :icon (icon val)
                                :subs (loop
                                         :for child
                                         :in (sort
                                              (remove-if #'(lambda (g)
                                                             (or
                                                              (empty g)
                                                              (not (active g))))
                                                         (groups val))
                                              #'menu-sort)
                                         :collect
                                         (list :key  (key child) :name (name child)))
                                ))
                         ;; else - this is ordinal
                         (leftmenu:ordinal (list :divider (notevery #'(lambda (divider)
                                                                        (string/= (key val) divider))
                                                                    divider-list)
                                                 :key  (key val)
                                                 :name (name val)
                                                 :icon (icon val)))
                         ))
                 (sort root-groups #'new-classes.menu-sort))))
      (leftmenu:main (list :elts src-lst))))


;;создание класса и методов отображения (в админке), изменения (из админки),
;;сереализации (в файл) и десеарелизации (из файла)
(defun new-classes.make-class-and-methods (name list-fields)
  (eval `(new-classes.make-class ,name ,list-fields))
  (eval `(new-classes.make-view-method ,name ,list-fields))
  (eval `(new-classes.make-edit-method ,name ,list-fields)))

