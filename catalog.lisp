(in-package #:eshop)

(defun alphabet-group-sort-f (a b)
  (if (or (null (name a))
          (null (name b)))
      nil
      ;; else
      (STRING< (name a)
         (name b))))

(defun all-leafs (root)
  (sort (let ((children (childs root))
              (res))
          (if (null children)
              (list root)
              (progn
                (mapcar #'(lambda (root)
                            (setf res (append res (all-leafs root))))
                        children)
                res))) #'alphabet-group-sort-f))


(defun all-leafs-not-sort (root)
  (let ((children (childs root))
        (res))
    (if (null children)
        (list root)
        (progn
          (mapcar #'(lambda (root)
                      (setf res (append res (all-leafs root))))
                  children)
          res))))

;;шаблоны
(defun catalog-update ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("new-catalog.soy")))

(defun sitemap-entity (error404  &optional col1 col2)
  (soy.new-catalog:catalog-main
   (list
    :error404 error404
    :numproducts (wolfor-stuff::num-active-product (wolfor-stuff::get-products-list))
    :menu (menu)
    :items (let* ((res) (roots))
             (maphash #'(lambda (k node)
                          (declare (ignore k))
                          (when (and (equal (type-of node) 'group)
                                     (null (parent node))
                                     (not (equal "bytovaya-technika" (key node))))
                            (pushnew node roots)))
                      *storage*)
             (mapcar #'(lambda (node)
                         (pushnew node roots))
                     (childs (gethash "bytovaya-technika" *storage*)))
             (setf roots (nreverse (sort roots #'menu-sort)))
             (mapcar #'(lambda (node)
                         (pushnew
                          (format nil "~a"
                                  (soy.new-catalog:catalog-item
                                   (let ((style
                                          (multiple-value-bind (width height)
                                              (images-get-dimensions
                                               (format nil "~a/htimgs~a" *path-to-dropbox* (pic node)))
                                            (images-style-for-resize width height 70))))

                                     (list
                                      :maingrouplink (format nil "<a href=\"/~a\">~a</a>~%" (key node) (name node))
                                      :maingroupimg (pic node)
                                      :imgstyle style
                                      :groups (mapcar #'(lambda (g)
                                                          (format nil "<a href=\"/~a\">~a</a>~%" (key g) (name g)))
                                                      (all-leafs (gethash (key node) *storage*)))))))
                                  res)) roots)
             (when col1
               (nconc res (list col1)))
             (when col2
               (nconc res (list col2)))
             (print res)
             res))))





(defun catalog-entity ()
  (sitemap-entity nil))


(defun sitemap-page (&optional error404)
  (sitemap-entity
   error404
   (soy.new-catalog:catalog-item
    (list
     :maingrouplink "Покупателям"
     :maingroupimg nil
     :groups (list "<a href=\"/servicecenter\">Наш Сервисный Центр</a>"
                   "<a href=\"/faq\">F.A.Q.</a>"
                   "<a href=\"/articles/reviews\">Обзоры</a>"
                   "<a href=\"/articles/news\">Новости</a>"
                   "<a href=\"/articles/papers\">Статьи</a>"
                   "<a href=\"/pricesc\">Услуги</a>"
                   "<a href=\"/delivery\">Доставка и Самовывоз</a>"
                   "<a href=\"/warranty\">Гарантия</a>"
                   "<a href=\"/corporate\">Корпоративным клиентам</a>"
                   )))))

