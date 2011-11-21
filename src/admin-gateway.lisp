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

(restas:define-route admin-content-table-route ("/administration-super-panel/content-table" :method :get)
  (admin.content-table))

(restas:define-route admin-content-table-post-route ("/administration-super-panel/content-table" :method :post)
  (admin.content-table))

(restas:define-route admin-get-json ("/administration-super-panel/getjson" :method :get)
  (list-filters.get-json))

(restas:define-route admin-test-get-post-request-route ("/administration-super-panel/test-get-post" :method :post)
  (admin.test-get-post-parse))

(restas:define-route admin-test-get-request-route ("/administration-super-panel/test-get-post" :method :get)
  (admin.test-get-post-parse))

(restas:define-route admin-testeditor-key-route ("/administration-super-panel/testeditor" :method :post)
  (show-admin-page "testeditor"))

(restas:define-route admin-parenting-key-route ("/administration-super-panel/parenting" :method :post)
  (show-admin-page "parenting"))

(restas:define-route admin-json-test-route ("/administration-super-panel/users.json")
  (admin.show-json-test))

(restas:define-route admin-key-route ("/administration-super-panel/:key")
  (show-admin-page key))

(restas:define-route admin-resources-route ("/resources/*")
  (let ((full-uri (format nil "~a" (restas:request-full-uri))))
    (pathname (concatenate 'string *path-to-dropbox* "/htimgs/"
                           (subseq full-uri (search "/resources/" full-uri))))))

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
            "admin-table.soy"
            )))

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
               "<li><a href=\"/administration-super-panel/info\">info</a></li>"
               ))))

(defun admin.test-get-post-parse ()
  (soy.admin:main
   (list :content
         (let* ((get-params (servo.alist-to-plist (hunchentoot:get-parameters hunchentoot:*request*)))
                (post-params (servo.alist-to-plist (hunchentoot:post-parameters hunchentoot:*request*))))
           (format nil "raw GET params: ~a <br />list to unique keys: ~a <br />raw POST params: ~a<br />list to unique keys: ~a"
                   (print get-params) (servo.plist-to-unique get-params)
                   (print post-params) (servo.plist-to-unique post-params))))))

(defun admin.test-post-parse ()
  (soy.admin:main
   (list :content
         (let* ((params (servo.alist-to-plist (hunchentoot:post-parameters hunchentoot:*request*))))
           (format nil "raw POST params: ~a <br />list to unique keys: ~a" (print params) (servo.plist-to-unique params))))))


(defun admin.content-table ()
  (let* ((params (servo.alist-to-plist (hunchentoot:get-parameters hunchentoot:*request*)))
         (type (getf params :type)))
    (cond
      ((equalp type "groups")
       (soy.admin-table:test-html
        (list
         :title "Group table"
         :script (soy.admin-table:group-table-js))))
      (t "Ololo?"))))

(defun admin-gateway.get-info ()
  (list (format nil "~{~a<br>~}" (mapcar #'(lambda (v) (sb-thread:thread-name v)) (sb-thread:list-all-threads)))
   (regex-replace-all "\\n" (with-output-to-string (*standard-output*) (room)) "<br>")))

(defun show-admin-page (&optional (key nil))
  (let* ((post-data (hunchentoot:raw-post-data))
         (new-post-data (servo.alist-to-plist (hunchentoot:post-parameters hunchentoot:*request*)))
         (post-data-plist))
    (when (not (null post-data))
      (setf post-data (sb-ext:octets-to-string post-data :external-format :utf8))
      (setf post-data-plist  (let ((result))
                               (loop :for param :in (split-sequence:split-sequence #\& post-data) :do
                                  (let ((split (split-sequence:split-sequence #\= param)))
                                    (setf (getf result (intern (string-upcase (car split)) :keyword))
                                          (if (null (cadr split))
                                              ""
                                              (cadr split)))))
                               result))
      (let ((action (getf post-data-plist :action)))
        (if (not (null action))
            (cond
              ((string= "do-gc" action)
               (progn
                 (setf post-data (regex-replace-all "\\n" (with-output-to-string (*standard-output*) (room)) "<br>"))
                 (sb-ext:gc :full t)))
              ((string= "report-products" action)
               (progn
                 (let ((name (format nil "reports/products-report-~a.csv" (time.encode.backup-filename))))
                   (setf post-data "DO PRODUCTS REPORT")
                   (create-report name #'write-products-report))))
              ((string= "proccess-pictures" action)
               (progn
                 (setf post-data "DO proccess-pictures")
                 (rename-convert-all)))
              ((string= "dtd" action)
               (progn
                 (setf post-data "DO DTD")
                 (dtd)))
              ((string= "report" action)
               (progn
                 (let ((name (format nil "reports/write-groups-active-product-num-~a.csv" (time.encode.backup-filename))))
                 (setf post-data "DO REPORT")
                 (create-report name #'write-groups-active-product-num))))
              ((string= "restore" action)
               (progn (setf post-data "DO RESTORE")))
              ((string= "compile" action)
               (setf post-data "DO COMPILE"))
              (t (setf post-data (format nil "DON't know action ~a" action)))))))
    (soy.admin:main (list :header (soy.admin:shortheader)
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
                                                                             :info post-data)))
                                            ((string= key "edit")
                                             (let* ((key (getf (request-get-plist) :key))
                                                    (item (gethash key (storage *global-storage*)))
                                                    (item-fields (when item (new-classes.make-fields item)))
                                                    (post-data new-post-data))
                                               (when (and item post-data)
                                                 (log5:log-for debug-console "~a~%" new-post-data)
                                                 ;; (log5:log-for debug-console "#####   ~a~%"
                                                 ;;               (getf post-data
                                                 ;;                     (intern (string-upcase (format nil "keyoption-og-~a" 0)) :keyword)))
                                                 (setf post-data (admin.post-data-preprocessing (servo.plist-to-unique post-data)))
                                                 (log5:log-for debug-console "~a~%" post-data)
                                                 (log5:log-for debug-console "----> ~a~%" (getf post-data :parents))
                                                 (new-classes.edit-fields item post-data)
                                                 (object-fields.product-groups-fix item)
                                                 (setf item-fields (new-classes.make-fields item)))
                                               (if item
                                                   (soy.class_forms:formwindow
                                                    (list :output (format nil "~a" post-data)
                                                          :key key
                                                          :fields item-fields))
                                                   "not found")))
                                            ((string= key "testeditor")
                                             (soy.class_forms:texteditor (list :output (format nil "~a" new-post-data)
                                                                               :name "testeditor"
                                                                               :value "<p>This is some <strong>sample text</strong>.
                                                                                       You are using <a href=\"http://ckeditor.com/\">CKEditor</a>.</p>")))
                                            ((string= key "tabletest")
                                             (admin.show-table-test))
                                            ((string= key "parenting")
                                             ;; (log5:log-for debug-console "~a~%" (hunchentoot:post-parameters hunchentoot:*request*))
                                             (when new-post-data
                                               ;; (log5:log-for debug-console "~a~%" (hunchentoot:post-parameters hunchentoot:*request*))
                                               (setf new-post-data (servo.plist-to-unique new-post-data))
                                               ;; (log5:log-for debug-console "---->#######  ~a~%" (getf post-data :groups))
                                               (let ((products (if (equal (type-of (getf new-post-data :products)) 'cons)
                                                                   (getf new-post-data :products)
                                                                   (list (getf new-post-data :products))))
                                                     (groups (if (equal (type-of (getf new-post-data :groups)) 'cons)
                                                                 (getf new-post-data :groups)
                                                                   (list (getf new-post-data :groups)))))
                                                 (mapcar #'(lambda (product)
                                                             (mapcar #'(lambda (group)
                                                                         (new-classes.bind-product-to-group
                                                                          (gethash product (storage *global-storage*))
                                                                          (gethash group (storage *global-storage*))))
                                                                     groups))
                                                         products))
                                               ;; (log5:log-for debug-console "----> ~a~%" post-data)
                                               )
                                             (let ((unparented-products (storage.get-filtered-products (products *global-storage*)
                                                                                                       #'(lambda (item)
                                                                                                           (null (new-classes.parent item))))))
                                               (soy.class_forms:parenting-page
                                                (list :products (mapcar #'(lambda (product)
                                                                            (soy.class_forms:unparented-product-checkbox
                                                                             (list :key (key product)
                                                                                   :name (name-seo product))))
                                                                        unparented-products)
                                                      :length (length unparented-products)
                                                      :groups (object-fields.group-list-field-view nil "GROUPS" nil)))))
                                            (t (format nil "~a" key)))))))))


(defun admin.post-data-preprocessing (post-data)
  (let ((result post-data)
        (keyoptions)
        (catalog-keyoptions))
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
    result))
