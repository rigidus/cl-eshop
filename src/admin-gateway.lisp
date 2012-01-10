(in-package #:eshop)


;; ADMIN ROUTE
(restas:define-route admin-route ("/administration-super-panel")
  (show-admin-page))

(restas:define-route admin/-route ("/administration-super-panel/")
  (show-admin-page))

(restas:define-route admin-actions-key-route ("/administration-super-panel/actions" :method :post)
  (show-admin-page "actions"))

(restas:define-route admin-edit-key-route ("/administration-super-panel/edit" :method :post)
  (show-admin-page "edit"))

(restas:define-route admin-make-key-route ("/administration-super-panel/make" :method :post)
  (show-admin-page "make"))

(restas:define-route admin-parenting-key-route ("/administration-super-panel/parenting" :method :post)
  (show-admin-page "parenting"))

(restas:define-route admin-key-route ("/administration-super-panel/:key")
  (show-admin-page key))

(restas:define-route admin-test-get-post-request-route ("/administration-super-panel/test-get-post" :method :post)
  (admin.test-get-post-parse))

(restas:define-route admin-test-get-request-route ("/administration-super-panel/test-get-post" :method :get)
  (admin.test-get-post-parse))

(restas:define-route admin-content-table-route ("/administration-super-panel/content-table" :method :get)
  (admin.content-table))

(restas:define-route admin-content-table-post-route ("/administration-super-panel/content-table" :method :post)
  (admin.content-table))

(restas:define-route admin-get-json ("/administration-super-panel/getjson" :method :get)
  (list-filters.get-json))

;;for correctly working of ExtJS tables
(restas:define-route admin-resources-route ("/resources/*")
  (let ((full-uri (format nil "~a" (restas:request-full-uri))))
    (pathname (concatenate 'string *path-to-dropbox* "/htimgs/"
                           (subseq full-uri (search "/resources/" full-uri))))))

;;for correctly working of ExtJS tables
(restas:define-route admin-ux-route ("/ux/*")
  (let ((full-uri (format nil "~a" (restas:request-full-uri))))
    (pathname (concatenate 'string *path-to-dropbox* "/htimgs/resources/"
                           (subseq full-uri (search "/ux/" full-uri) (search "?" full-uri))))))

;;шаблоны
(defun admin-compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("admin.soy"
            "class_forms.soy"
            "admin-table.soy")))

;;обновление главной страницы
(defun admin-update ()
  (admin-compile-templates))

(defun show-gateway-history ()
  (let ((history-list
         (mapcar #'(lambda (data)
                     (car data))
                 *history*)))
    history-list))

(defun show-admin-menu ()
  (admin:menu
   (list :elts
         (list "<li><a href=\"/administration-super-panel\"><b>MAIN ADMIN</b></a></li>"
               "<li><a href=\"/administration-super-panel/history\">gateway</a></li>"
               "<li><a href=\"/administration-super-panel/actions\">actions</a></li>"
               "<li><a href=\"/administration-super-panel/info\">info</a></li>"))))

(defun admin.test-get-post-parse ()
  "parsing get & post parameters for testing"
  (soy.admin:main
   (list :content
         (let* ((get-params (servo.alist-to-plist (hunchentoot:get-parameters hunchentoot:*request*)))
                (post-params (servo.alist-to-plist (hunchentoot:post-parameters hunchentoot:*request*))))
           (format nil "raw GET params: ~a <br />list to unique keys: ~a <br />raw POST params: ~a<br />list to unique keys: ~a"
                   (print get-params) (servo.plist-to-unique get-params)
                   (print post-params) (servo.plist-to-unique post-params))))))

(defun admin.group-content-column (dataindex)
  (soy.admin-table:table-column
   (cond
     ((equal dataindex "checkbox")
      (list :text "Selected"
            :width 30
            :hideable "false"
            :dataIndex dataindex))
     ((equal dataindex "name")
      (list :text "Name"
            :width 100
            :hideable "false"
            :dataIndex dataindex))
     ((equal dataindex "key")
      (list :text "Key"
            :width 150
            :dataIndex dataindex))
     ((equal dataindex "numprod")
      (list :text "Number of products"
            :flex 1
            :dataIndex dataindex))
     ((equal dataindex "order")
      (list :text "Order"
            :width 150
            :dataIndex "order"))
     ((equal dataindex "active")
      (list :text "Active"
            :width 150
            :sortable "false"
            :dataIndex "active"))
     (t nil))))

(defun admin.product-content-column (dataindex)
  (soy.admin-table:table-column
   (cond
     ((equal dataindex "checkbox")
      (list :text "Selected"
            :width 30
            :hideable "false"
            :dataIndex dataindex))
     ((equal dataindex "name")
      (list :text "Name"
            :width 100
            :hideable "false"
            :dataIndex dataindex))
     ((equal dataindex "key")
      (list :text "Key"
            :width 150
            :dataIndex dataindex))
     ((equal dataindex "active")
      (list :text "Active"
            :width 150
            :sortable "false"
            :dataIndex "active"))
     (t nil))))

(defun admin.content-table ()
  (let* ((params (servo.alist-to-plist (hunchentoot:get-parameters hunchentoot:*request*)))
         (type (getf params :type)))
    (cond
      ((equal type "groups")
       (let ((fields (list "checkbox" "name" "key" "numprod" "order" "active")))
         (soy.admin-table:test-html
          (list
           :title "Group table"
           :script (soy.admin-table:table-js
                    (list
                     :name "Groups"
                     :type "groups"
                     :pagesize 50
                     :remotesort "true"
                     :fields fields
                     :columns (mapcar #'admin.group-content-column fields)))))))
      ((equal type "products")
       (let ((fields (list "checkbox" "name" "key" "active")))
         (soy.admin-table:test-html
          (list
           :title "Product table"
           :backlink (getf params :parent)
           :script (soy.admin-table:table-js
                    (list
                     :name "Products"
                     :type "products"
                     :parent (getf params :parent)
                     :pagesize 50
                     :remotesort "false"
                     :fields fields
                     :columns (mapcar #'admin.product-content-column fields)))))))
      (t "Ololo?"))))

(defun admin-gateway.get-info ()
  (list (format nil "~{~a<br>~}" (mapcar #'(lambda (v) (sb-thread:thread-name v)) (sb-thread:list-all-threads)))
        (regex-replace-all "\\n" (with-output-to-string (*standard-output*) (room)) "<br>")))


(defun admin-gateway.edit-content (&optional new-post-data)
  (let* ((key (getf (request-get-plist) :key))
         (item (gethash key (storage *global-storage*)))
         (item-fields (when item (new-classes.make-fields item)))
         (post-data new-post-data))
    (when (and item post-data)
      (setf post-data (admin.post-data-preprocessing (servo.plist-to-unique post-data)))
      (new-classes.edit-fields item post-data)
      ;; need to fix
      (if (and (equal (type-of item) 'group) (not (null (getf post-data :fullfilter))))
          (setf (fullfilter item) (getf post-data :fullfilter)))
      (object-fields.product-groups-fix item)
      (setf item-fields (new-classes.make-fields item)))
    (if item
        (soy.class_forms:formwindow
         (list :output (format nil "~a" post-data)
               :key key
               :fields item-fields
               :target "edit"))
        "not found")))

(defun admin-gateway.make-content (new-post-data)
  (let* ((key (getf (request-get-plist) :key))
         (type (getf (request-get-plist) :type))
         (item (gethash key (storage *global-storage*)))
         (post-data new-post-data))
    (if item
        ;;if item exist in storage, redirect to edit page (but url will be .../make?...)
        (admin-gateway.edit-content new-post-data)
        ;;else
        (if post-data
            (progn
              (cond
                ((equal "product" type)
                 (setf item (make-instance 'product
                                           :articul (parse-integer key))))
                ((equal "group" type)
                 (setf item (make-instance 'group)))
                ((equal "filter" type)
                 (setf item (make-instance 'filter)))
                (t "Unknown type"))
              (setf (key item) key)
              (if (equal type "product")
                  (setf (date-created item) (get-universal-time)
                        (date-modified item) (get-universal-time)))
              (setf post-data (admin.post-data-preprocessing (servo.plist-to-unique post-data)))
              (new-classes.edit-fields item post-data)
              ;;don't work with filters
              (object-fields.product-groups-fix item)
              (storage.edit-object item) ;;adding item into storage
              (admin-gateway.edit-content))
            ;;else (post-data is nil)
            (let ((empty-item (new-classes.get-instance type)))
              (setf (key empty-item) key)
              (if (equal (type-of empty-item) 'product)
                  (setf (articul empty-item) (parse-integer key)))
              (soy.class_forms:formwindow
               (list :key key
                     :type type
                     :fields (new-classes.make-fields empty-item)
                     :target "make")))))))

(defun admin-gateway.do-action (action)
  (if (not (null action))
      (cond
        ((string= "do-gc" action)
         (progn
           (sb-ext:gc :full t)
           (regex-replace-all
            "\\n"
            (with-output-to-string
                (*standard-output*)
              (room))
            "<br>")))
        ((string= "report-products" action)
         (progn
           (let ((name (format nil "reports/products-report-~a.csv" (time.encode.backup-filename))))
             (create-report name #'write-products-report)
             "DO PRODUCTS REPORT")))
        ((string= "proccess-pictures" action)
         (progn
           (rename-convert-all)
           "DO proccess-pictures"))
        ((string= "dtd" action)
         (progn
           (dtd)
           "DO DTD"))
        ((string= "report" action)
         (progn
           (let ((name (format nil "reports/write-groups-active-product-num-~a.csv" (time.encode.backup-filename))))
             (create-report name #'write-groups-active-product-num)
             "DO REPORT")))
        ((string= "compile" action)
         "DO COMPILE")
        (t (format nil "DON't know action ~a" action)))))

(defun admin-gateway.parenting-content (new-post-data)
  (let ((post-data new-post-data))
    (when post-data
      (setf post-data (servo.plist-to-unique post-data))
      (let ((products (if (equal (type-of (getf post-data :products)) 'cons)
                          (getf post-data :products)
                          (list (getf post-data :products))))
            (groups (if (equal (type-of (getf post-data :groups)) 'cons)
                        (getf post-data :groups)
                        (list (getf post-data :groups)))))
        (mapcar #'(lambda (product)
                    (mapcar #'(lambda (group)
                                (new-classes.bind-product-to-group
                                 (gethash product (storage *global-storage*))
                                 (gethash group (storage *global-storage*))))
                            groups))
                products)))
  (let ((unparented-products (storage.get-filtered-products
                              (storage.get-products-list)
                              #'(lambda (item)
                                  (null (new-classes.parent item))))))
    (soy.class_forms:parenting-page
     (list :products (mapcar #'(lambda (product)
                                 (soy.class_forms:unparented-product-checkbox
                                  (list :key (key product)
                                        :name (name-seo product))))
                             unparented-products)
           :length (length unparented-products)
           :groups (object-fields.group-list-field-view nil "GROUPS" nil))))))

(defun show-admin-page (&optional (key nil))
  (let ((new-post-data (servo.alist-to-plist (hunchentoot:post-parameters hunchentoot:*request*))))
    (soy.admin:main
     (list :header (soy.admin:shortheader)
           :footer (soy.admin:footer)
           :content (admin:content
                     (list :menu (show-admin-menu)
                           :subcontent
                           (cond
                             ((null key)
                              (format nil "<p> Админка в разработке </p>"))
                             ((string= key "history")
                              (format nil "~{~a<br>~}" (show-gateway-history)))
                             ((string= key "info")
                              (soy.admin:info (list :info (admin-gateway.get-info))))
                             ((string= key "actions")
                              (soy.admin:action-buttons (list :post new-post-data
                                                              :info (admin-gateway.do-action (getf new-post-data :action)))))
                             ((string= key "edit")
                              (admin-gateway.edit-content new-post-data))
                             ((string= key "make")
                              (admin-gateway.make-content new-post-data))
                             ((string= key "parenting")
                              (admin-gateway.parenting-content new-post-data))
                             (t (format nil "~a" key)))))))))


(defun admin.post-data-preprocessing (post-data)
  "keyoptions & aliases (catalog-keyoptions) preprocessing"
  (let ((result post-data)
        (keyoptions)
        (catalog-keyoptions)
        (raw-fullfilter (getf post-data :raw-fullfilter)))
    ;;keyoptions
    (loop
       for cnt from 0
       while (getf post-data
                   (intern (string-upcase (format nil "keyoption-og-~a" cnt)) :keyword))
       do
         (let ((optgroup (getf post-data
                               (intern (string-upcase (format nil "keyoption-og-~a" cnt)) :keyword)))
               (optname (getf post-data
                              (intern (string-upcase (format nil "keyoption-on-~a" cnt)) :keyword))))

           (when (and (string/= "" optgroup) (string/= "" optname))
             (push (list :optgroup optgroup :optname optname) keyoptions))))
    (setf result (append result (list :keyoptions (nreverse keyoptions))))
    ;;catalog keyoptions
    (loop
       for cnt from 0
       while (getf post-data
                   (intern (string-upcase (format nil "catalog-keyoption-og-~a" cnt)) :keyword))
       do
         (let ((optgroup (getf post-data
                               (intern (string-upcase (format nil "catalog-keyoption-og-~a" cnt)) :keyword)))
               (optname (getf post-data
                              (intern (string-upcase (format nil "catalog-keyoption-on-~a" cnt)) :keyword)))
               (showname (getf post-data
                               (intern (string-upcase (format nil "catalog-keyoption-sn-~a" cnt)) :keyword)))
               (units (getf post-data
                            (intern (string-upcase (format nil "catalog-keyoption-un-~a" cnt)) :keyword))))
           (when (and (string/= "" optgroup) (string/= "" optname) (string/= "" showname))
             (push (list :optgroup optgroup :optname optname :showname showname :units units) catalog-keyoptions))))
    (setf result (append result (list :catalog-keyoptions (nreverse catalog-keyoptions))))
    ;; fullfilter decode
    (if raw-fullfilter
        (let ((new-raw (getf result :raw-fullfilter))
              (new-full))
          (setf new-full (new-classes.decode new-raw (make-instance 'group-filter)))
          (setf (getf result :fullfilter) new-full)
          (setf (getf result :raw-fullfilter) new-raw)))
    result))
