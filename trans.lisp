(in-package #:trans)

(defun strip ($string)
  (when (null $string)
    (return-from strip ""))
  (let (($ret))
    (loop :for x :across $string do
       (if (not (or
                 (equal x #\")
                 (equal x #\Newline)))
           (push x $ret)))
    (coerce (reverse $ret) 'string)))


(defun trans-groups ()
  ;; (with-open-file (file-stream "tmp.csv" :direction :output :if-exists :overwrite :if-does-not-exist :create)
  ;;   (format file-stream "~a;~a;~a;~a;~a;~a~%"
  ;;           "ID group (old)"
  ;;           "ID group (new)"
  ;;           "Parent"
  ;;           "filter"
  ;;           "name"
  ;;           "active")
    (let ((h-group    (cl-store:restore (format nil "~a/h-group" *path-to-bkps*)))
          (n-group    (xls:parse-xls (format nil "~a/url.xls" *path-to-conf*)
                                   (list :id-old :id-new :parent :filter :name :active) 2))
          (r-group    (make-hash-table :test #'equal))
          (r-group-id (make-hash-table :test #'equal))
          (r-filter   (make-hash-table :test #'equal)))
      (loop :for item :in n-group do
         (let ((id-old (parse-integer (getf item :id-old) :junk-allowed t))
               (id-new (getf item :id-new))
               (filter (getf item :filter)))
           (if (not id-old)
               ;; В файле нет старого айдишника - можно создавать новый чистый объект или фильтр
               (if (not id-new)
                   ;; Ошибка - новый айдишник должен быть в любом случае!
                   (format t "not new-old: ~a~%" item)
                   ;; Все ок, создаем новый объект или фильтр
                   (if (string= "" (string-trim '(#\Space #\Tab #\Newline) filter))
                       ;; Это новый объект группы - сохраняем его в r-group
                       (let ((object (make-instance 'group:group
                                                    :key    id-new
                                                    :parent (getf item :parent)
                                                    :name   (getf item :name))))
                         (format t "G: [~a]=>~a : ~a ~%" id-new (group:parent object) (group:name object))
                         (setf (gethash id-new r-group) object))
                       ;; Это новый объект фильтра - сохраняем его в r-filter
                       (let ((object (make-instance 'filter:filter
                                                    :key    filter
                                                    :parent (getf item :parent)
                                                    :name   (getf item :name))))
                         (format t "F: [~a]=>~a : ~a ~%" filter (filter:parent object) (filter:name object))
                         (setf (gethash filter r-filter) object))
                       ))
               ;; В файле есть старый айдишник - часть полей возьмем из старых данных
               (let ((old (gethash id-old h-group)))
                 (if (not old)
                     ;; Ошибка - нет старых данных
                     (format t "not old: ~A~%" item)
                     ;; Старые данные на месте - создаем объект
                     (if (not (string= "" (getf item :filter)))
                         ;; Ошибка - сочетание старого айдишника и фильтра
                         (format t "not filter id: ~A~%" item)
                         ;; Все в порядке
                         (let ((object (make-instance 'group:group
                                                  :id         (getf old :id)
                                                  :key        id-new
                                                  :parent-id  (getf old :parent_id)
                                                  :parent     (getf item :parent)
                                                  ;; :name       (getf old :name)
                                                  :name       (getf item :name)
                                                  ;; :active     (if (string= "Y" (getf old :active)) t nil)
                                                  :active     (if (string= "Y" (getf item :active)) t nil)
                                                  ;; :empty      (if (string= "Y" (getf old :empty)) t nil)
                                                  )))
                           (setf (getf item :name)   (getf old :name))
                           (setf (getf item :active) (getf old :active))
                           (setf (gethash id-new r-group) object)
                           (setf (gethash (getf old :id)  r-group-id) object)
                           (format t "old [~a]=>~a : ~a ~%" id-new (group:parent object) (group:name object))
                           ))
                     ))
               ))
         ;; (format file-stream "~a;~a;~a;~a;~a;~a~%"
         ;;         (strip (getf item :id-old))
         ;;         (strip (getf item :id-new))
         ;;         (strip (getf item :parent))
         ;;         (strip (getf item :filter))
         ;;         (strip (getf item :name))
         ;;         (strip (getf item :active)))
         )
      (values r-group r-group-id r-filter)))


(defun trans-options (result-options)
  (make-instance 'optlist:optlist
                 :optlist (loop :for optgroup :in result-options
                             collect (make-instance 'optgroup:optgroup
                                                    :name (getf optgroup :optgroup_name)
                                                    :options (loop :for option :in (getf optgroup :options)
                                                                collect (make-instance 'option:option
                                                                                       :name  (getf option :name)
                                                                                       :value (getf option :value)))))))

(defun trans-products (r-group-id)
  (let ((h-product (cl-store:restore (format nil "~a/h-product" *path-to-bkps*)))
        (r-product (make-hash-table :test #'equal)))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 ;; Привязываем продукты к их группам
                 (let* ((articul  (getf v :articul))
                        (group    (gethash (getf v :group_id) r-group-id))
                        ;; (presence (getf v :presence))
                        (object (make-instance 'product:product
                                                :articul articul
                                                :parent group
                                                :name (getf v :name)
                                                :realname (getf v :realname)
                                                :price (getf v :price)
                                                :siteprice (getf v :siteprice)
                                                :ekkprice (getf v :ekkprice)
                                                :active (getf v :active)
                                                :newbie (getf v :newbie)
                                                :sale (getf v :special)
                                                :descr (getf v :descr)
                                                :shortdescr (getf v :shortdescr)
                                                :options (trans-options (getf v :result-options))
                                                )))
                   (setf (gethash articul r-product) object) ;; теперь адресуемся к продуктам по артикулу
                   ;; Если у продукта есть группа-владелец - привязываем группу-владельца к продукту
                   (when (equal 'group:group (type-of group))
                     (push object (group:products group)))
                   ))
             h-product)
    r-product))


(defparameter *group* (make-hash-table :test #'equal))
(defparameter *group-id* (make-hash-table :test #'equal))
(defparameter *product* (make-hash-table :test #'equal))
(defparameter *filter* (make-hash-table :test #'equal))


(defun processing ()
  ;; Обрабатываем группы опираясь на htconf/uri.xls и бэкап.
  (multiple-value-bind (r-group r-group-id r-filter)
      (trans-groups)
    ;; Связываем группы друг с другом в дерево в обе стороны
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (let ((parent (gethash (group:parent val) r-group)))
                   ;; Сначала связываем каждую группу с ее родительской группой
                   (if (or (equal (type-of parent) 'group:group)
                           (null parent))
                       ;; if parent is 'GROUP or NIL - save
                       (setf (group:parent val) parent)
                       ;; else (parent is 'FILTER) - error
                       (error "Parent for 'GROUP must be 'GROUP or NIL"))
                   ;; Потом добавляем в слот CHILDS родительской группы текущую группу
                   (when (not (null parent))
                     (push val (group:childs parent)))
                 ))
             r-group)
    ;; Связываем фильтры с их родительскими группами и родительские группы с фильтрами
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (let ((parent (gethash (filter:parent val) r-group)))
                   ;; Сначала связываем каждый фильтр с его родительской группой
                   (if (or (equal (type-of parent) 'group:group)
                           (null parent))
                       ;; if parent is 'GROUP or NIL - save
                       (progn
                         (when (null parent)
                           (format t "~%WARN-FILTER-PARENT-IS-NULL:~a [~a]" (filter:key val) (filter:parent val)))
                         (setf (filter:parent val) parent))
                       ;; else (parent is 'FILTER) - error
                       (error "Parent for 'FILTER must be 'GROUP"))
                   ;; Потом добавляем в слот CHILDS родительской группы текущую группу
                   (when (not (null parent))
                     (push val (group:filters parent)))
                   ))
             r-filter)
    ;; Выкладываем в глобальную область видимости
    (setf *product*   (trans-products r-group-id))
    (setf *group*     r-group)
    (setf *group-id*  r-group-id)
    (setf *filter*    r-filter)
    ))

;; (processing)

(defun dbg (object &rest fields)
  (cond ((equal (type-of object) 'group:group)    (eval `(group:plist-representation ,object ,@fields)))
        ((equal (type-of object) 'filter:filter)  (eval `(filter:plist-representation ,object ,@fields)))
        (t nil)))

(defun to-files ()
  (let ((bug (gethash "http:/www.320-8080.ru/akkumulyatory-ncm" trans:*group*)))
    (unless (null bug)
      (setf (group:key bug) "akkumulyatory-ncm")
      (setf (gethash "akkumulyatory-ncm" trans:*group*) bug)
      (remhash "http:/www.320-8080.ru/akkumulyatory-ncm" trans:*group*)))
  (maphash #'(lambda (key group)
               (declare (ignore key))
               (group:serialize group))
           trans:*group*)
  (maphash #'(lambda (key product)
               (declare (ignore key))
               (product:serialize product))
           trans:*product*))


;; (to-files)


(defun explore-dir (path)
  (let ((dirs) (files))
    (mapcar #'(lambda (x)
                (if (cl-fad:directory-pathname-p x)
                    (push x dirs)
                    (push x files)))
                (directory (format nil "~a/*" path)))
    (values dirs files (directory (format nil "~a/*.filter" path)))))


(defun process-filter (file)
  (filter:unserialize (format nil "~a" file)))


(defun process-file (file)
  (let* ((name (pathname-name file))
         (candidat (parse-integer name :junk-allowed t)))
    (if (string= (format nil "~a" candidat) name)
        (product:unserialize (format nil "~a" file))
        nil
        )))


(defun process-dir (file)
  (let* ((string-filename (format nil "~a" file))
         (name (car (last (split-sequence #\/ string-filename) 2)))
         (target (format nil "~a~a" string-filename name))
         (target-file (file-exists-p target)))
    (when (null target-file)
      (let ((dymmy (make-instance 'group:group
                                  :key name
                                  :parent (gethash (car (last (split-sequence #\/ string-filename) 3)) trans:*group*)
                                  :name name)))
        (format t "~%warn: NO-FILE-GROUP: ~a" string-filename)
        (group:serialize dymmy)))
    (group:unserialize target)
    (recursive-explore file)))


(defun recursive-explore (path)
  (multiple-value-bind (dirs files filters)
      (explore-dir path)
    (values
     (mapcar #'process-dir dirs)
     (mapcar #'process-file files)
     (mapcar #'process-filter filters))))


(defun store-unlinked-products ()
  (let ((a))
    (maphash #'(lambda (k v)
                 (when (null (product:parent v))
                   (push v a)))
             trans:*product*)
    (mapcar #'product:serialize
            a)))

(defun realname-from-name ()
  (let ((cnt 0))
    (maphash #'(lambda (k v)
                 (when (string= "" (product:realname v))
                   (incf cnt)
                   (setf (product:realname v) (product:name v))
                   ))
             trans:*product*)
    cnt))


(defun restore-from-files ()
  (handler-bind ((PRODUCT::WRONG-PRODUCT-FILE
                  #'(lambda (e)
                      (format t "~%warn: WRONG-PRODUCT-FILE: ~a"
                              (product::filepath e))
                      (invoke-restart 'ignore)
                      )))
    (print "start restore....{")
    (sb-ext:gc :full t)
    (recursive-explore *path-to-bkps*)
    (sb-ext:gc :full t)
    (print "...} finish ok")))

(restore-from-files)


;; Кое-какие заготовки для переноса данных - удалить после завершения
;; (let ((data (cl-store:restore "#h-product"))
;;       (old 0)
;;       (new 0)
;;       (grp (make-hash-table :test #'equal)))
;;   (maphash #'(lambda (k v)
;;                (let* ((articul (getf v :articul))
;;                       (product (gethash articul *product*)))
;;                  (if product
;;                      (progn
;;                        (incf old)
;;                        (setf (product:descr product)
;;                              (getf v :descr))
;;                        (setf (product:shortdescr product)
;;                              (getf v :shortdescr))
;;                        (setf (product:options product)
;;                              (trans-options (getf v :result-options))))
;;                      (let* ((group-id (getf v :group_id))
;;                             (grp-lst (gethash group-id grp nil)))
;;                        (product::serialize2
;;                         ;; (ignore-errors
;;                         (make-instance 'product:product
;;                                        :id (getf v :id)
;;                                        :articul articul
;;                                        :parent group-id
;;                                        :name (getf v :name)
;;                                        :realname (getf v :realname)
;;                                        :price (getf v :price)
;;                                        :siteprice (getf v :siteprice)
;;                                        :ekkprice (getf v :ekkprice)
;;                                        :active (getf v :active)
;;                                        :newbie (getf v :newbie)
;;                                        :sale (getf v :special)
;;                                        :descr (getf v :descr)
;;                                        :shortdescr (getf v :shortdescr)
;;                                        :options (trans-options (getf v :result-options))
;;                                        ))
;;                        (push articul grp-lst)
;;                        (setf (gethash group-id grp) grp-lst)
;;                        (incf new)))))
;;            data)
;;   (print (list new old (hash-table-count grp))))

;; [v.2]
(let ((tmp (cl-store:restore "#h-product")))
  (maphash #'(lambda (k v)
               (let* ((articul (getf v :articul))
                      (new-opt (trans-options (getf v :result-options))))
                 (when (equal articul 142715)
                   (print "YES")
                   (print (optlist:optlist new-opt))
                   (print v)
                   )
                 ;; name
                 (setf (product:name (gethash articul trans:*product*))
                       (getf v :name))
                 ;; realname
                 (setf (product:realname (gethash articul trans:*product*))
                       (getf v :realname))
                 ;; descr
                 (setf (product:descr (gethash articul trans:*product*))
                       (getf v :descr))
                 ;; shortdescr
                 (setf (product:shortdescr (gethash articul trans:*product*))
                       (getf v :shortdescr))
                 ;; options
                 (if (and (equal 'optlist:optlist (type-of new-opt))
                          (not (null (gethash articul trans:*product*))))
                     (progn
                       (setf (product:options (gethash articul trans:*product*))
                             new-opt))
                     ;; else
                     (print "err!"))
                 ;; save
                 (product:serialize (gethash articul trans:*product*))
                 articul))
           tmp)
  (sb-ext:gc :full t)
  'finish)
