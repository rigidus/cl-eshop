(in-package #:eshop)


;; ADMIN ROUTE
(restas:define-route admin-route ("/admin")
  (show-admin-page))

(restas:define-route admin/-route ("/admin/")
  (show-admin-page))

(restas:define-route admin-actions-key-route ("/admin/actions" :method :post)
  (show-admin-page "actions"))

(restas:define-route admin-edit-key-route ("/admin/edit" :method :post)
  (show-admin-page "edit"))

(restas:define-route admin-testeditor-key-route ("/admin/testeditor" :method :post)
  (show-admin-page "testeditor"))

(restas:define-route admin-parenting-key-route ("/admin/parenting" :method :post)
  (show-admin-page "parenting"))

(restas:define-route admin-table-test-route ("/admin/tabletest")
  (admin.show-table-test))

(restas:define-route admin-key-route ("/admin/:key")
  (show-admin-page key))

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
         (list "<li><a href=\"/admin\"><b>MAIN ADMIN</b></a></li>"
               "<li><a href=\"/admin/history\">gateway</a></li>"
               "<li><a href=\"/admin/actions\">actions</a></li>"
               ))))


(defun show-admin-page (&optional (key nil))
  (let* (
         (post-data (hunchentoot:raw-post-data))
         (new-post-data (alist-to-plist (hunchentoot:post-parameters hunchentoot:*request*)))
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
            (cond ((string= "restore" action)
                   (progn (setf post-data "DO RESTORE")
                          (safely-restore)))
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
                                            ((string= key "actions")
                                             (soy.admin:action-buttons (list :post new-post-data)))
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
                                            ;; ((string= key "tabletest")
                                            ;;  (admin.show-table-test))
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
                                                      :groups (object-fields.group-list-field-view nil "GROUPS" nil)
                                                      :length (length unparented-products)))))
                                            (t (format nil "~a" key)))))))))

(defun admin.show-table-test ()
  (soy.admin-table:test-html (list
                              :script (soy.admin-table:test-js (list :data
                                                                     (admin.get-test-data))))))

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
             (push (list :optgroup optgroup :optname optname :showname showname) catalog-keyoptions))))
         (setf result (append result (list :catalog-keyoptions (nreverse catalog-keyoptions))))
    result))



(sb-thread:make-thread (lambda () (format t "Hello, world")))


