(in-package #:eshop)


;; ADMIN ROUTE
(restas:define-route admin-route ("/admin")
  (show-admin-page))

(restas:define-route admin/-route ("/admin/")
  (show-admin-page))

(restas:define-route admin-key-route ("/admin/:key")
  (show-admin-page key))

(restas:define-route admin-actions-key-route ("/admin/actions" :method :post)
  (show-admin-page "actions"))

(restas:define-route admin-edit-key-route ("/admin/edit" :method :post)
  (show-admin-page "edit"))

(restas:define-route admin-testeditor-key-route ("/admin/testeditor" :method :post)
  (show-admin-page "testeditor"))

;;обновление главной страницы
(defun admin-update ()
  (admin-compile-templates))

;;шаблоны
(defun admin-compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("admin.soy"
            "class_forms.soy"
            )))

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
  (let* ((post-data (hunchentoot:raw-post-data))
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
                                             (soy.admin:action-buttons (list :post post-data)))
                                            ((string= key "edit")
                                             (let* ((key (getf (request-get-plist) :key))
                                                    (item (gethash key (storage *global-storage*)))
                                                    (item-fields (new-classes.make-fields item)))
                                               (when new-post-data
                                                 (new-classes.edit-fields item new-post-data)
                                                 (setf item-fields (new-classes.make-fields item)))
                                               (if item
                                                   (soy.class_forms:formwindow
                                                    (list :output (format nil "~a" new-post-data)
                                                          :key key
                                                          :fields item-fields))
                                                   "not found")))
                                            ((string= key "testeditor")
                                             (soy.class_forms:texteditor (list :output (format nil "~a" new-post-data)
                                                                               :name "testeditor"
                                                                               :value "<p>This is some <strong>sample text</strong>.
                                                                                       You are using <a href=\"http://ckeditor.com/\">CKEditor</a>.</p>")))
                                            (t (format nil "~a" key)))))))))



(sb-thread:make-thread (lambda () (format t "Hello, world")))
