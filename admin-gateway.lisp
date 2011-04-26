(in-package #:eshop)


;; ADMIN ROUTE
(restas:define-route admin-route ("/admin")
  (show-admin-page))

(restas:define-route admin/-route ("/admin/")
  (show-admin-page))

(restas:define-route admin-key-route ("/admin/:key")
  (show-admin-page key))


(defun show-gateway-history ()
  (let ((history-list
         (mapcar #'(lambda (data)
                     (cdr (last (car data))))
                 *history*)))
    history-list))

(defun show-admin-menu ()
  (admin:menu
   (list :elts
         (list "<li><a href=\"/admin/history\">выгрузка</a></li>"
                                "<li>test2</li>"))))

(defun show-admin-page (&optional (key nil))
  (admin:main (list :header (admin:shortheader)
                    :footer (admin:footer)
                    :content (admin:content (list :menu (show-admin-menu)
                                                  :subcontent (cond
                                                                ((null key)
                                                                 (format nil "<p> Админка в разработке </p>"))
                                                                ((string= key "history")
                                                                 (show-gateway-history))
                                                                (t (format nil "~a" key))))))))

