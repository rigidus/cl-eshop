(in-package #:group)


(defclass group ()
  ((id                :initarg :id             :initform nil       :reader   id)
   (key               :initarg :key            :initform nil       :accessor key)
   (parent-id         :initarg :parent-id      :initform nil       :accessor parent-id)
   (parent            :initarg :parent         :initform nil       :accessor parent)
   (name              :initarg :name           :initform nil       :accessor name)
   (active            :initarg :active         :initform nil       :accessor active)
   (empty             :initarg :empty          :initform nil       :accessor empty)
   (order             :initarg :order          :initform nil       :accessor order)
   (keyoptions        :initarg :keyoptions     :initform nil       :accessor keyoptions)
   (ymlshow           :initarg :ymlshow        :initform nil       :accessor ymlshow)
   (pic               :initarg :pic            :initform nil       :accessor pic)
   (childs            :initarg :childs         :initform nil       :accessor childs)
   (filters           :initarg :filters        :initform nil       :accessor filters)
   (fullfilter        :initarg :fullfilter     :initform nil       :accessor fullfilter)
   (products          :initarg :products       :initform nil       :accessor products)
  ))

(defparameter *trade-hits-1*
  (list :pic "/img/temp/s1.jpg"
        :name "Lenovo E43-4S-B"
        :price 19990
        :ico "/img/temp/u2.jpg"
        :user "Борис"
        :text "Отличный ноутбук для работы, здорово помогает! Главное - мощная батарея, хватает на 4 часа"
        :more "Еще 12 отзывов о ноутбуке"))

(defparameter *trade-hits-2*
  (list :pic "/img/temp/s3.jpg"
        :name "ASUS UL30Vt"
        :price 32790
        :ico "/img/temp/u3.jpg"
        :user "Тамара"
        :text "Не плохой ноутбук, рабочая лошадка. Хорошие углы обзора, шустрый проц, без подзарядки работает часа 3, легкий и удобный в переноске. В общем, советую."
        :more "Еще 12 отзывов о ноутбуке"))


(defmethod get-recursive-products ((object group))
  (let ((products (products object)))
    (loop :for child :in (childs object) :do
       (setf products (append products (group:get-recursive-products child))))
    products))


(defun product-sort (products operation getter)
  (sort (copy-list products) #'(lambda (a b)
                                 (if (funcall operation
                                              (funcall getter a)
                                              (funcall getter b))
                                     t
                                     nil))))


(defmethod show ((object group) request-get-plist)
  (list :name (name object)
        :breadcrumbs (catalog:breadcrumbs (service:breadcrumbs object))
        :menu (service:menu object)
        :rightblocks (list (make-full-filter object request-get-plist)
                           (catalog:rightblock1)
                           (catalog:rightblock2)
                           (catalog:rightblock3))
        :tradehits (catalog:tradehits (list :reviews (list *trade-hits-1*
                                                           *trade-hits-2*
                                                           *trade-hits-1*
                                                           *trade-hits-2*)))
        :subcontent (if (and (null (group:products object))
                             (null (getf request-get-plist :fullfilter))
                             (null (getf request-get-plist :vendor)))
                        ;; Отображаем группы
                        (catalog:centergroup
                         (list
                          :producers (make-vendor-filter object)
                          :accessories (catalog:accessories)
                          :groups (remove-if ;; удаляем пустые группы
                                   #'(lambda (x)
                                       (equal 0 (getf x :cnt)))
                                   (loop :for child :in (sort (copy-list (childs object)) #'service:menu-sort) :collect
                                      (list :name (name child)
                                            :key (key child)
                                            :group_id (group:id child)
                                            :cnt (let ((products (group:products child)))
                                                   (if (null products)
                                                       "-"
                                                       (length (remove-if-not #'(lambda (product)
                                                                                  (product:active product))
                                                                              (group:products child)))))
                                            :pic (pic child)
                                            :filters (loop :for filter :in (filters child) :collect
                                                        (list :name (filter:name filter)
                                                              :groupkey (group:key child)
                                                              :key (filter:key filter)
                                                              :cnt 999)))))))
                        ;; else
                        (let* ((products (remove-if-not #'(lambda (product)
                                                            (product:active product))
                                                        (cond
                                                          ((getf request-get-plist :fullfilter)
                                                           (filter-controller object request-get-plist))
                                                          ((getf request-get-plist :vendor)
                                                           (vendor-controller object request-get-plist))
                                                          (t (copy-list (products object))))))
                               (sorting  (getf request-get-plist :sort))
                               (sorted-products   (cond ((string= sorting "pt")
                                                (product-sort products #'< #'product:price))
                                               ((string= sorting "pb")
                                                (product-sort products #'> #'product:price))
                                               ((string= sorting "pt1") products)
                                               (t products))))
                          (multiple-value-bind (paginated pager)
                              (service:paginator request-get-plist sorted-products)
                            (catalog:centerproduct
                             (list
                              :sorts (let ((variants '(:pt "увеличению цены" :pb "уменьшению цены")))
                                       (loop :for sort-field :in variants :by #'cddr :collect
                                          (if (string= (string-downcase (format nil "~a" sort-field))
                                                       (getf request-get-plist :sort))
                                              (list :key (string-downcase (format nil "~a" sort-field))
                                                    :name (getf variants sort-field)
                                                    :active t)
                                              (list :key (string-downcase (format nil "~a" sort-field))
                                                    :name (getf variants sort-field)))))
                              :producers (make-vendor-filter object)
                              :accessories (catalog:accessories)
                              :pager pager
                              :products
                              (loop
                                 :for product :in  paginated :collect (product:view product)))))))))


(defun unserialize (pathname)
  (let* ((file-content (alexandria:read-file-into-string pathname))
         (raw (decode-json-from-string file-content))
         (key (cdr (assoc :key raw)))
         (keyoptions (mapcar #'(lambda (pair)
                                 (list :optgroup (cdr (assoc :optgroup pair))
                                       :optname (cdr (assoc :optname pair))))
                             (cdr (assoc :keyoptions raw))))
         (parent (gethash (nth 2 (reverse (split-sequence #\/ pathname))) trans:*group*))
         (new (make-instance 'group
                             :id (cdr (assoc :id raw))
                             :key key
                             :parent parent
                             :name (cdr (assoc :name raw))
                             :active (cdr(assoc :active raw))
                             :empty (cdr (assoc :empty raw))
                             :order (cdr (assoc :order raw))
                             :fullfilter (let ((tmp (cdr (assoc :fullfilter raw))))
                                           (if (null tmp)
                                               nil
                                               (read-from-string (cdr (assoc :fullfilter raw)))))
                             :keyoptions keyoptions
                             :pic (cdr (assoc :pic raw))
                             :ymlshow (cdr (assoc :ymlshow raw)))))
    (when (equal 'group:group (type-of parent))
        (push new (group:childs parent)))
    (setf (gethash key trans:*group*) new)
    key))


(defmethod serialize ((object group))
  (let* ((raw-breadcrumbs (service:breadcrumbs object))
         (path-list (mapcar #'(lambda (elt)
                                (getf elt :key))
                            (getf raw-breadcrumbs :breadcrumbelts)))
         (current-dir (format nil "~a~a/~a/" *path-to-bkps*
                              (format nil "~{/~a~}" path-list)
                              (key object)))
         (pathname (format nil "~a~a" current-dir (key object)))
         (dumb '((:id . 0)
                 (:key . "-")
                 (:name . "-")
                 (:active . nil)
                 (:empty . nil)
                 (:keyoptions . nil)
                 (:fullfilter . nil)
                 (:order . 1)
                 (:ymlshow . 1)
                 (:pic . "")))
         (json-string))
    ;; Создаем директорию, если ее нет
    (ensure-directories-exist current-dir)
    ;; Подтягиваем данные из файла, если он есть
    (when (probe-file pathname)
      (setf dumb (union dumb (decode-json-from-string (alexandria:read-file-into-string pathname)) :key #'car)))
    ;; Сохраняем только те поля, которые нам известны, неизвестные сохраняем без изменений
    (setf (cdr (assoc :id dumb)) (id object))
    (setf (cdr (assoc :key dumb)) (key object))
    (setf (cdr (assoc :name dumb)) (name object))
    (setf (cdr (assoc :active dumb)) (active object))
    (setf (cdr (assoc :empty dumb)) (empty object))
    (setf (cdr (assoc :pic dumb)) (pic object))
    ;; keyoptions - json array or null
    (setf (cdr (assoc :keyoptions dumb))
          (if (null (keyoptions object))
              "null"
              (let ((json-string (format nil "~{~a~}"
                                         (loop :for item :in (keyoptions object) :collect
                                            (format nil "{\"optgroup\":\"~a\",\"optname\":\"~a\"},~%"
                                                    (getf item :optgroup)
                                                    (getf item :optname))))))
                (format nil "[~a]" (subseq  json-string 0 (- (length json-string) 2))))))
    ;; assembly json-string
    (setf json-string
          (format nil "~{~a~}"
                  (remove-if #'null
                             (loop :for item :in dumb :collect
                                (let ((field (string-downcase (format nil "~a" (car item))))
                                      (value (cdr item)))
                                  (cond ((equal t   value) (format nil "\"~a\":true,~%" field))
                                        ((equal nil value) (format nil "\"~a\":null,~%" field))
                                        ((or (subtypep (type-of value) 'number)
                                             (equal 'null (type-of value)))
                                         (format nil "\"~a\":~a,~%" field value))
                                        ((string= "keyoptions" field)
                                         (format nil "\"~a\":~a,~%" field value))
                                        ((subtypep (type-of value) 'string)
                                         (format nil "\"~a\":\"~a\",~%"
                                                 field
                                                 (my:replace-all value "\"" "\\\"")))
                                        ;; for debug
                                        ;; (t (format nil "\"~a\":[~a][~a],~%"
                                        ;;            field
                                        ;;            (type-of value)
                                        ;;            value))
                                        ))))))
    ;; correction
    (setf json-string (subseq  json-string 0 (- (length json-string) 2)))
    (setf json-string (format nil "{~%~a~%}~%" json-string))
    ;; for debug
    ;; (format t "~a" json-string)
    ;; save file
    (with-open-file (file pathname
                          :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
      (format file json-string))
    ;; return pathname
    pathname))


;; test for serialize
;; (serialize (gethash "netbuki" trans:*group*))
;; (unserialize "/home/webadmin/Dropbox/htbkps/noutbuki-i-netbuki/netbuki/netbuki")
;; (print (keyoptions (gethash "netbuki" trans:*group*)))


(defmethod plist-representation ((object group) &rest fields)
  (let ((result))
    (loop :for item :in fields do
       (let ((method (intern (symbol-name item) 'group)))
         (push item result)
         (push (funcall method object) result)))
    (reverse result)))


(defmethod filter-controller ((object group) request-get-plist)
  (let ((functions (mapcar #'(lambda (elt)
                               (eval (car (last elt))))
                           (getf (fullfilter object) :base))))
    (mapcar #'(lambda (filter-group)
                (let ((advanced-filters (cadr filter-group)))
                  (mapcar #'(lambda (advanced-filter)
                              (nconc functions (list (eval (car (last advanced-filter))))))
                          advanced-filters)))
            (getf (fullfilter object) :advanced))
    (mapcar #'(lambda (filter-group)
                (let ((advanced-filters (cadr filter-group)))
                  (mapcar #'(lambda (advanced-filter)
                              (nconc functions (list (eval (car (last advanced-filter))))))
                          advanced-filters)))
            (getf (fullfilter object) :advanced))
    ;; processing
    (let ((result-products))
      (mapcar #'(lambda (product)
                  (when (loop
                           :for function :in functions
                           :finally (return t)
                           :do (unless (funcall function product request-get-plist)
                                 (return nil)))
                    (push product result-products)))
              (remove-if-not #'(lambda (product)
                                 (product:active product))
                             (get-recursive-products object)))
      result-products)))


(defmethod filter-test ((object group) url)
  (let* ((request-full-str url)
         (request-parted-list (split-sequence:split-sequence #\? request-full-str))
         (request-get-plist (let ((result))
                              (loop :for param :in (split-sequence:split-sequence #\& (cadr request-parted-list)) :do
                                 (let ((split (split-sequence:split-sequence #\= param)))
                                   (setf (getf result (intern (string-upcase (car split)) :keyword))
                                       (if (null (cadr split))
                                           ""
                                           (cadr split)))))
                              result)))
    (filter-controller object request-get-plist)))


(defun filter-element (elt request-get-plist)
  (let* ((key (string-downcase (format nil "~a" (nth 0 elt))))
         (name (nth 1 elt))
         (contents
          (cond ((equal :range (nth 2 elt))
                 (fullfilter:range
                  (list :unit (nth 3 elt)
                        :key key
                        :name name
                        :from (getf request-get-plist
                                    (intern (string-upcase (format nil "~a-f" key)) :keyword))
                        :to (getf request-get-plist
                                  (intern (string-upcase (format nil "~a-t" key)) :keyword)))))
                ((equal :radio (nth 2 elt))
                 (fullfilter:box
                  (list :key key
                        :name name
                        :elts (let ((elts (nth 3 elt)))
                                (loop :for nameelt :in elts
                                      :for i from 0 :collect
                                   (fullfilter:radioelt
                                    (list :key key
                                          :value i
                                          :name nameelt
                                          :checked (string= (format nil "~a" i)
                                                            (getf request-get-plist (intern
                                                                                     (string-upcase key)
                                                                                     :keyword)))
                                          )))))))
                ((equal :checkbox (nth 2 elt))
                 (fullfilter:box
                  (list :key key
                        :name name
                        :elts (let ((values (nth 3 elt)))
                                (loop :for value :in values
                                      :for i from 0 :collect
                                   (fullfilter:checkboxelt
                                    (list :value value
                                          :key key
                                          :i i
                                          :checked (string= "1" (getf request-get-plist (intern
                                                                                           (string-upcase
                                                                                            (format nil "~a-~a" key i))
                                                                                           :keyword)))
                                          )))))))
                (t ""))))
    (if (search '(:hidden) elt)
        (fullfilter:hiddencontainer (list :key key
                                          :name name
                                          :contents contents))
        contents)))


(defmethod make-full-filter ((object group) request-get-plist)
  (if (null (group:fullfilter object))
      ""
      (fullfilter:container
       (list :name (getf (group:fullfilter object) :name)
             :base (format nil "~{~a~}"
                           (mapcar #'(lambda (elt)
                                       (filter-element elt request-get-plist))
                                   (getf (group:fullfilter object) :base)))
             :advanced (format nil "~{~a~}"
                               (mapcar #'(lambda (elt)
                                           (fullfilter:group
                                            (list :name (car elt)
                                                  :elts (mapcar #'(lambda (inelt)
                                                                    (filter-element inelt request-get-plist))
                                                                (cadr elt))
                                                  )))
                                       (getf (group:fullfilter object) :advanced)))))))



(defmethod make-vendor-filter ((object group))
  (let ((vendors (make-hash-table :test #'equal)))
    (loop :for product :in (remove-if-not #'(lambda (product)
                                              (product:active product))
                                          (get-recursive-products object)) :do
       (mapcar #'(lambda (option)
                   (if (string= (optgroup::name option) "Общие характеристики")
                       (let ((options (optgroup::options option)))
                         (mapcar #'(lambda (opt)
                                     (if (string= (option::name opt) "Производитель")
                                         (let ((val (option::value opt)))
                                           (if (null (gethash val vendors))
                                               (setf (gethash val vendors) 1)
                                               (incf (gethash val vendors))))))
                                 options))))
               (optlist:optlist (product:options product))))
    (let ((vendor-list))
      (maphash #'(lambda (key val)
                   (push (list key val) vendor-list))
               vendors)
      (multiple-value-bind (base hidden)
          (my:cut 12 (mapcar #'(lambda (x)
                              (list :vendor (car x) :cnt (cadr x) :link (format nil "?vendor=~a" (car x))))
                          (sort vendor-list #'(lambda (a b) (> (cadr a) (cadr b))))))
        (catalog:producers (list :vendorblocks base
                                 :vendorhiddenblocks hidden))
      ))))


(defmethod vendor-controller ((object group) request-get-plist)
  (let* ((result-products))
    (mapcar #'(lambda (product)
                (let ((vendor))
                  (mapcar #'(lambda (option)
                              (if (string= (optgroup::name option) "Общие характеристики")
                                  (let ((options (optgroup::options option)))
                                    (mapcar #'(lambda (opt)
                                                (if (string= (option::name opt) "Производитель")
                                                    (setf vendor (option::value opt))))
                                            options))))
                          (optlist:optlist (product:options product)))
                  ;; (format t "~%[~a] : [~a] : [~a]"
                  ;;         (string-downcase (string-trim '(#\Space #\Tab #\Newline) vendor))
                  ;;         (string-downcase (ppcre:regex-replace-all "%20" (getf request-get-plist :vendor) " "))
                  ;;         ;; (loop :for ch :across (ppcre:regex-replace-all "%20" (getf request-get-plist :vendor) " ") :do
                  ;;         ;;    (format t "~:c." ch))
                  ;;         )
                          ;; vendor (getf request-get-plist :vendor))
                  (if (string=
                       (string-downcase
                        (string-trim '(#\Space #\Tab #\Newline) vendor))
                       (string-downcase
                        (string-trim '(#\Space #\Tab #\Newline)
                                     (ppcre:regex-replace-all "%20" (getf request-get-plist :vendor) " "))))
                      (push product result-products))))
            (remove-if-not #'(lambda (product)
                               (product:active product))
                           (get-recursive-products object)))
    result-products))


(funcall *dispatcher*
         `((equal 'group:group (type-of (gethash (cadr (service:request-list)) trans:*group*)))
           ,#'(lambda ()
                (let* ((request-list (service:request-list))
                       (request-get-plist (service:request-get-plist))
                       (object (gethash (cadr request-list) trans:*group*))
                       (type   (type-of object))
                       (filter-1-click-name (caddr request-list)))
                  (cond ((and (not (null filter-1-click-name))
                              (equal 'filter:filter (type-of (gethash filter-1-click-name trans:*filter*)))
                              (equal object (filter:parent (gethash filter-1-click-name trans:*filter*))))
                         (service:default-page
                             (catalog:content
                              (filter:show (gethash filter-1-click-name trans:*filter*) request-get-plist))))
                        ;; group
                        ((equal type 'group:group)
                         (service:default-page
                             (catalog:content (group:show object request-get-plist))))
                        (t "dispatcher: unk object"))))))
