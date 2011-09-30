(in-package #:eshop)

(defun object-fields.string-escaping (string)
  "Processing string and escapes quotes and backslashes"
  (let ((chars-for-escape (list #\\ #\")))
    (format nil "~{~a~}"
            (map 'list #'(lambda (char)
                           (if (notevery #'(lambda (char-for-escape)
                                             (char-not-equal char char-for-escape))
                                         chars-for-escape)
                               (format nil "\\~a" char)
                               (format nil "~a" char)))
                 string))))

(defun object-fields.string-replace-newlines (string)
  "Processing string and replace newline characters with #Newline"
  (format nil "~{~a~}"
          (map 'list #'(lambda (char)
                         (if (char-equal #\Newline char)
                             (format nil "#Newline")
                             (format nil "~a" char)))
               string)))

;;набор функций для показа, изменения и сериализации различных типов полей в админке
;;object-fields.*-field-view - просмотр
;;object-fields.*-field-get-data - декодирование из списка post-запроса
;;object-fields.*-field-serialize - сериализация поля
;;тип поля указывается в :type при создании класса (см. classes.lisp)

;;string - универсальный тип, так же используется как undefined
(defun object-fields.string-field-view (value name disabled)
  (soy.class_forms:string-field
   (list :name name :disabled disabled :value value)))

(defun object-fields.string-field-get-data (string)
  string)


(defun object-fields.string-field-serialize (string)
  (format nil "\"~a\"" (object-fields.string-escaping string)))

;;int
(defun object-fields.int-field-view (value name disabled)
  (object-fields.string-field-view (format nil "~a" value) name disabled))

(defun object-fields.int-field-get-data (string)
  (parse-integer string))

(defun object-fields.int-field-serialize (int)
  (encode-json-to-string int))


;;textedit, онлайновый WYSIWYG редактор текста
(defun object-fields.textedit-field-view (value name disabled)
  (if disabled
      (object-fields.string-field-view value name disabled)
      (soy.class_forms:texteditor
       (list :name name :value value))))

(defun object-fields.textedit-field-get-data (string)
  string)

(defun object-fields.textedit-field-serialize (text)
  (object-fields.string-field-serialize text))


;;time, человекопонятное время
(defun object-fields.time-field-view (value name disabled)
  (object-fields.string-field-view (time.decode-date-time value) name disabled))

(defun object-fields.time-field-get-data (string)
  string)

(defun object-fields.time-field-serialize (time)
  (object-fields.int-field-serialize time))


;;bool
(defun object-fields.bool-field-view (value name disabled)
  (soy.class_forms:bool-field
   (list :name name :checked value :disabled disabled)))

(defun object-fields.bool-field-get-data (string)
  (string= string "T"))

(defun object-fields.bool-field-serialize (bool)
  (encode-json-to-string bool))


;;генерация дерева групп
;;open - список групп до которых нужно раскрыть дерево
;;field-name - название поля, нужно для проставления в name в radio
(defun object-fields.group-tree (open-groups field-name)
  (let ((roots (root-groups *global-storage*)))
    (soy.class_forms:group-tree
     (list :roots
           (mapcar #'(lambda (child)
                       (object-fields.group-branch child open-groups field-name))
                   roots)))))


;;group, список групп, генерируется из списка с проставленными уровнями глубины
(defun object-fields.group-list-field-view (value name disabled)
  (soy.class_forms:group-field (list :name name
                                     :tree (object-fields.group-tree value name)
                                     :disabled disabled)))

;;генерация ветви дерева с корнем в данной группе
;;group - корень ветви, open-groups - список групп, до которых нужно раскрыть дерево
;;field-name - имя поля, нужно для проставления в name в radio
(defun object-fields.group-branch (group open-groups field-name)
  (let ((open nil)
        (child-open nil)
        (children)
        (checked nil))
    ;;выясняем нужно ли открывать группу
    (mapcar #'(lambda (open-group)
                (when (eq (key group) (key open-group))
                    (setf open t)
                    (setf checked t)))
            open-groups)
    ;;строим список дочерних ветвей и проверяем нужно ли открыть данный узел
    (setf children
          (mapcar #'(lambda (child)
                      (multiple-value-bind (branch branch-opened)
                          (object-fields.group-branch child open-groups field-name)
                        (setf child-open (or child-open branch-opened))
                        branch))
                  (storage.get-group-children group)))
    (values-list (list
                  (soy.class_forms:group-tree-branch (list :opened child-open
                                                           :hashkey (key group)
                                                           :name (name group)
                                                           :checked checked
                                                           :children children
                                                           :fieldname field-name))
                  (or open child-open)))))


(defun object-fields.group-list-field-get-data (string-list)
  (if (equal (type-of string-list) 'cons)
      (mapcar #'(lambda (parent)
                  (log5:log-for debug-console "~a~%" parent)
                  (gethash parent (storage *global-storage*)))
              string-list)
      (list (gethash string-list (storage *global-storage*)))))


(defun object-fields.group-list-field-serialize (groups)
  (format nil "[ ~{\"~a\"~^, ~}]"
          (mapcar #'(lambda (group)
                      (key group))
                  groups)))

;;optgroups
(defun object-fields.optgroups-field-view (value name disabled)
  (object-fields.string-field-view value name disabled))

(defun object-fields.optgroups-field-get-data (string)
  (object-fields.string-field-get-data string))

(defun object-fields.optgroups-field-serialize (optgroups)
  (format nil "[~{~a~^, ~%~%~}]"
          (mapcar #'(lambda (optgroup)
                           (format nil "{\"name\" : \"~a\", ~%  \"options\" : ~%[~{~a~^, ~%~}]}"
                                   (getf optgroup :name)
                                   (mapcar #'(lambda (option)
                                               (format nil "{\"name\" : \"~a\" ,~%\"value\" : \"~a\"}"
                                                       (getf option :name)
                                                       (getf option :value)))
                                           (getf optgroup :options))))
                  optgroups)))



;;products, список продуктов(-детей)
(defun object-fields.product-list-field-view (value name disabled)
  (object-fields.string-field-view name value disabled))


(defun object-fields.product-list-field-get-data (string-list)
  (if (equal (type-of string-list) 'cons)
      (mapcar #'(lambda (parent)
                  (log5:log-for debug-console "~a~%" parent)
                  (gethash parent (storage *global-storage*)))
              string-list)
      (list (gethash string-list (storage *global-storage*)))))

(defun object-fields.product-list-field-serialize (products)
  (format nil "[ ~{\"~a\"~^, ~}]"
          (mapcar #'(lambda (product)
                      (key product))
                  products)))


;;keyoptions
(defun object-fields.keyoptions-field-view (value name disabled)
  (object-fields.string-field-view name value disabled))

(defun object-fields.keyoptions-field-get-data (string)
  (object-fields.string-field-get-data string))

(defun object-fields.keyoptions-field-serialize (keyoptions)
  (format nil "[~{~a~^, ~%~%~}]"
          (mapcar #'(lambda (keyoption)
                           (format nil "{\"optgroup\" : \"~a\", ~%  \"optname\" : \"~a\"}"
                                   (getf keyoption :optgroup)
                                   (getf keyoption :optname)))
                  keyoptions)))
