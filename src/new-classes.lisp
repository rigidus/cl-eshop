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
  (if (or (string= in-string "") (null in-string))
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
      (let ((num 0))
        (with-open-file (file filepath)
          (loop for line = (read-line file nil 'EOF)
             until (eq line 'EOF)
             do
               (let ((item (unserialize (decode-json-from-string line)
                                        dummy)))
                 (incf num)
                 (if (equal (mod num 1000) 0)
                     (wlog (key item)))
                 (storage.add-new-object item (key item)))))))))

(defmethod new-classes.get-transform-optgroups ((item product))
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
    optgroups))


(defun new-classes.bind-product-to-group (product group)
  "Bind product to group, and push product to group's children"
  (when (every #'(lambda (parent)
                   (string/= (key group) (key parent)))
               (parents product))
    (pushnew group (parents product)))
  (when (every #'(lambda (child)
                   (string/= (key product) (key child)))
               (products group))
    (pushnew product (products group))))


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
  ;; проверка цены, если цена в ИМ ноль, а дельта положительная нужно изменить цену
  (when  (and (= (siteprice item) 0)
              (> (delta-price item) 0))
    (setf (siteprice item) (delta-price item))
    (setf (delta-price item) 0))
  ;;active - если имеется в наличии и цена > 0
  (setf (active item) (and (> (count-total item) 0) (> (siteprice item) 0)))
  ;;adding newlines instead of #Newline
  (setf (seo-text item) (object-fields.string-add-newlines (seo-text item)))
  ;;преобразуем optgroups из списка alist в список plist
  (setf (optgroups item) (new-classes.get-transform-optgroups item))
  ;; setting product vendor
  (with-option1 item "Общие характеристики" "Производитель"
                (setf (vendor item) (getf option :value))))


;;вызывается после десереализации группы
(defmethod new-classes.post-unserialize ((item group))
  ;;adding newlines instead of #Newline
  (when (seo-text item)
    (setf (seo-text item) (object-fields.string-add-newlines (seo-text item))))
  (when (equal (type-of (vendors-seo item)) 'cons)
    (setf (vendors-seo item) (mapcar #'object-fields.string-add-newlines
                                     (copy-list (vendors-seo item))))
    ;;convert vendors key to downcase
    (setf (vendors-seo item) (let ((num 0))
                               (mapcar #'(lambda (v)
                                           (incf num)
                                           (if (oddp num)
                                               (string-downcase v)
                                               v))
                                       (vendors-seo item))))
    ;;convert vendors-seo from list to hashtable
    (setf (vendors-seo item) (servo.list-to-hashtasble
                              (copy-list (vendors-seo item)))))
  ;; после десериализации в parent лежит список key родительских групп
  (let ((parents (copy-list (parents item))))
    (setf (parents item)
          (mapcar #'(lambda (parent-key)
                      ;;в случае если на месте ключей уже лежат группы
                      (if (equal (type-of parent-key) 'group)
                          parent-key
                          (if parent-key
                              (gethash parent-key (storage *global-storage*)))))
                  parents))
    ;; удаляем nil для битых ключей
    ;; TODO обрабатывать исключение если ключи не были найдены
    (setf (parents item) (remove-if #'null (parents item)))
    ;; проставление ссылок у родителей на данную группу
    (let ((is-parent-link (remove-if-not #'(lambda (v) (equal item v))
                                         (parents item))))
      (when (not is-parent-link)
        (mapcar #'(lambda (parent)
                    (when parent
                      (push item (groups parent))))
                (parents item)))))
  (setf (empty item) (not (null (remove-if-not #'active (products item)))))
  (let ((keys (keyoptions item)))
    ;;проверка на то нужно ли перерабатывать ключеве опции
    (if (and keys
             (not (atom keys))
             (not (atom (car keys)))
             (not (atom (caar keys))))
        (setf (keyoptions item) (mapcar #'(lambda (pair)
                                            (list :optgroup (cdr (assoc :optgroup pair))
                                                  :optname (cdr (assoc :optname pair))))
                                        (keyoptions item)))))
  ;;TODO эта проверка нужна для постобработки групп дессериализованных их старых быкапов, когда фулфильтры хранились прямо в fullfilter
  (when (and (null (raw-fullfilter item))
             (fullfilter item)
             (atom (fullfilter item)))
    (setf (raw-fullfilter item) (concatenate 'string "" (fullfilter item)))
    (setf (fullfilter item) nil))
  (when (and (raw-fullfilter item)
             (null (fullfilter item)))
    (setf (fullfilter item) (object-fields.string-add-newlines (raw-fullfilter item)))
    (setf (fullfilter item) (new-classes.decode (fullfilter item) (make-instance 'group-filter)))))

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


(defmethod new-classes.post-unserialize ((item article)))


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
  (sb-ext:gc :full t)
  (unserialize-from-file (pathname (format nil "~atest/products.bkp" (user-homedir-pathname))) (make-instance 'product))
  (unserialize-from-file (pathname (format nil "~atest/groups.bkp" (user-homedir-pathname))) (make-instance 'group))
  (unserialize-from-file (pathname (format nil "~atest/filters" (user-homedir-pathname))) (make-instance 'filter))
  (wlog "Making lists")
  (storage.make-lists)
  (wlog "Post unserialize!")
  (maphash #'(lambda (key value)
               (declare (ignore key))
               (new-classes.post-unserialize value))
           (storage *global-storage*)))

(defun new-classes.parent (item)
  "Returns main parent of item"
  (car (parents item)))

(defun new-classes.DBG-unserialize-products ()
  "Одноразовый перенос олдовых характеристик поверх хранилища продуктов и проставление производителя"
  (let ((original-storage (storage *global-storage*))
        (*global-storage* (make-instance 'global-storage)))
    (unserialize-from-file (pathname (format nil "~atest/products" (user-homedir-pathname))) (make-instance 'product))
    ;; на данном этапе в *global-storage* только продукты
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (and (equal (type-of v) 'product)
                            (optgroups v))
                   (let ((item (gethash (key v) original-storage)))
                     (when item
                       ;;преобразуем optgroups из списка alist в список plist
                       (setf (optgroups item)
                             (new-classes.get-transform-optgroups v))
                       (with-option1 item "Общие характеристики" "Производитель"
                                     (setf (vendor item) (getf option :value)))))))
             (storage *global-storage*)))
  ;;необходимо освободить память от уже не нужных продуктов
  (sb-ext:gc :full t))

(defun new-classes.DBG-unserialize-groups ()
  "Одноразовый перенос олдовых характеристик поверх хранилища продуктов и проставление производителя"
  (let ((original-storage (storage *global-storage*))
        (*global-storage* (make-instance 'global-storage)))
    (unserialize-from-file (pathname (format nil "~atest/groups" (user-homedir-pathname))) (make-instance 'group))
    ;; на данном этапе в *global-storage* только группы
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (and (equal (type-of v) 'group)
                            (fullfilter v))
                   (let ((item (gethash (key v) original-storage)))
                     (when item
                       (let ((filter (fullfilter v)))
                         (setf filter (object-fields.string-add-newlines filter))
                         (setf filter (new-classes.decode filter (make-instance 'group-filter)))
                         (setf (fullfilter item) filter))))))
             (storage *global-storage*)))
  ;;необходимо освободить память от уже не нужных продуктов
  (sb-ext:gc :full t))



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
  (if (and item
           (not (equal "" item)))
      (let ((parent (new-classes.parent item)))
        (if (or (null item) (null parent))
            item
            (new-classes.get-root-parent parent)))))


(defun new-classes.menu-sort (a b)
  "Function for sorting groups by order field"
  (if (or (null (order a))
          (null (order b)))
      nil
      ;; else
      (< (order a)
         (order b))))


;;TODO временно убрана проверка на пустые группы, тк это поле невалидно
(defun new-classes.menu (&optional current-object)
  "Creating left menu"
  (let* ((root-groups (storage.get-root-groups-list) );;(root-groups *global-storage*))
         (current-root (new-classes.get-root-parent current-object))
         (divider-list (list "setevoe-oborudovanie" "foto-and-video" "rashodnye-materialy"))
         (src-lst
          (mapcar #'(lambda (val)
                      (if (and current-root
                               (equal (key val) (key current-root)))
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
                                                               ;; (empty g)
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
  (eval `(new-classes.make-edit-method ,name ,list-fields))
  (eval `(new-classes.make-unserialize-method ,name ,list-fields))
  (eval `(new-classes.make-serialize-method ,name ,list-fields)))


(new-classes.make-class-and-methods
 'product
 '((:name key               :initform ""                     :disabled t     :type string      :serialize t)
   (:name articul           :initform nil                    :disabled t     :type int         :serialize t)
   (:name name-provider     :initform ""                     :disabled nil   :type string      :serialize t)
   (:name name-seo          :initform ""                     :disabled nil   :type string      :serialize t)
   (:name siteprice         :initform 0                      :disabled nil   :type int         :serialize t)
   (:name delta-price       :initform 0                      :disabled nil   :type int         :serialize t)
   (:name bonuscount        :initform 0                      :disabled nil   :type int         :serialize t)
   (:name delivery-price    :initform 0                      :disabled nil   :type int         :serialize t)
   (:name active            :initform t                      :disabled nil   :type bool        :serialize nil)
   (:name preorder          :initform nil                    :disabled nil   :type bool        :serialize t)
   (:name newbie            :initform t                      :disabled nil   :type bool        :serialize t)
   (:name sale              :initform t                      :disabled nil   :type bool        :serialize t)
   (:name parents           :initform nil                    :disabled nil   :type group-list  :serialize t)
   (:name date-modified     :initform (get-universal-time)   :disabled t     :type time        :serialize t)
   (:name date-created      :initform (get-universal-time)   :disabled t     :type time        :serialize t)
   (:name seo-text          :initform ""                     :disabled nil   :type textedit    :serialize t)
   (:name count-transit     :initform 0                      :disabled t     :type int         :serialize t)
   (:name count-total       :initform 0                      :disabled t     :type int         :serialize t)
   (:name optgroups         :initform nil                    :disabled t     :type optgroups   :serialize t)
   (:name vendor            :initform ""                     :disabled nil   :type string      :serialize t)))


;; для того чтобы работали фильтры
(defmethod price ((object product))
  (+ (siteprice object) (delta-price object)))


(new-classes.make-class-and-methods
 'group
 '((:name key                 :initform nil                             :disabled t     :type string                    :serialize t)
   (:name parents             :initform nil                             :disabled nil   :type group-list                :serialize t)
   (:name name                :initform nil                             :disabled nil   :type string                    :serialize t)
   (:name active              :initform nil                             :disabled nil   :type bool                      :serialize t)
   (:name empty               :initform nil                             :disabled t     :type bool                      :serialize nil)
   (:name order               :initform 1000                            :disabled nil   :type int                       :serialize t)
   (:name ymlshow             :initform nil                             :disabled t     :type bool                      :serialize t)
   (:name pic                 :initform nil                             :disabled nil   :type string                    :serialize t)
   (:name icon                :initform nil                             :disabled nil   :type string                    :serialize t)
   (:name delivery-price      :initform 0                               :disabled nil   :type int                       :serialize t)
   (:name groups              :initform nil                             :disabled t     :type group-list                :serialize nil)
   (:name products            :initform nil                             :disabled t     :type product-list              :serialize nil)
   (:name filters             :initform nil                             :disabled t     :type string                    :serialize nil)
   (:name fullfilter          :initform nil                             :disabled t     :type string                    :serialize nil)
   (:name raw-fullfilter      :initform nil                             :disabled t     :type string                    :serialize t)
   (:name vendors-seo         :initform (make-hash-table :test #'equal) :disabled t     :type textedit-hashtable        :serialize t)
   (:name seo-text            :initform nil                             :disabled nil   :type textedit                  :serialize t)
   (:name keyoptions          :initform nil                             :disabled nil   :type keyoptions                :serialize t)
   (:name catalog-keyoptions  :initform nil                             :disabled nil   :type catalog-keyoptions        :serialize t)))


(new-classes.make-class-and-methods
 'filter
 '((:name key               :initform ""       :disabled t    :type string)
   (:name parents           :initform nil      :disabled t    :type group-list)
   (:name name              :initform ""       :disabled nil  :type string)
   (:name func              :initform ""       :disabled t    :type string)
   (:name func-string       :initform ""       :disabled t    :type textedit)))

