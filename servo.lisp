;;;; servo.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop)

(defmacro re-assoc (alist key val)
  `(progn
     (when (assoc ,key ,alist)
       (setf ,alist (remove-if #'(lambda (x)
                                   (equal x (assoc ,key ,alist)))
                               ,alist)))
     (push (cons ,key  ,val) ,alist)))

;; (macroexpand-1 '(re-assoc dumb :nameless (name object)))



(defmacro with-sorted-paginator (get-products request-get-plist body)
  `(let* ((products ,get-products)
          (sorting  (getf ,request-get-plist :sort))
          (sorted-products   (cond ((string= sorting "pt")
                                    (product-sort products #'< #'siteprice))
                                   ((string= sorting "pb")
                                    (product-sort products #'> #'siteprice))
                                   (t products))))
     (multiple-value-bind (paginated pager)
         (paginator ,request-get-plist sorted-products)
       ,body)))


(defmacro sorts (request-get-plist)
  `(let ((variants '(:pt "увеличению цены" :pb "уменьшению цены"))
         (url-parameters request-get-plist))
     (remf url-parameters :page)
     (remf url-parameters :sort)
     (loop :for sort-field :in variants :by #'cddr :collect
        (let ((key (string-downcase (format nil "~a" sort-field))))
          (setf (getf url-parameters :sort) key)
          (if (string= (string-downcase (format nil "~a" sort-field))
                       (getf request-get-plist :sort))
              (list :key key
                    :name (getf variants sort-field)
                    :url (make-get-str url-parameters)
                    :active t)
              (list :key key
                    :url (make-get-str url-parameters)
                    :name (getf variants sort-field)))))))


(defmacro rightblocks ()
  `(list (catalog:rightblock1)
         (catalog:rightblock2)
         (if (not (equal 'group (type-of object)))
             ""
             (progn
               (let ((vndr (getf (request-get-plist) :vendor)))
                 (if (null vndr)
                     ;; show group descr
                     (let ((descr (descr object)))
                       (if (null descr)
                           ""
                           (catalog:seotext (list :text descr))))
                     ;; show vendor descr
                     (let ((descr (gethash vndr (vendors object))))
                       (if (null descr)
                           ""
                           (catalog:seotext (list :text descr))))))))))

;; (maphash #'(lambda (k v ) (print k)) (vendors (gethash "monobloki" *storage*)))




(defmacro with-option (product optgroup-name option-name body)
  `(mapcar #'(lambda (optgroup)
               (if (string= (name optgroup) ,optgroup-name)
                   (let ((options (options optgroup)))
                     (mapcar #'(lambda (option)
                                 (if (string= (name option) ,option-name)
                                     ,body))
                             options))))
           (optgroups ,product)))

(defmacro f-price ()
  `(lambda (product request-plist filter-options)
     (let ((value-f (getf request-plist :price-f))
           (value-t (getf request-plist :price-t))
           (value-x (siteprice product)))
       (when (null value-f)
         (setf value-f "0"))
       (when (or (null value-t)
                 (string= value-t ""))
         (setf value-t "99999999"))
       (setf value-f (arnesi:parse-float (format nil "~as" value-f)))
       (setf value-t (arnesi:parse-float (format nil "~as" value-t)))
       (and (<= value-f value-x)
            (>= value-t value-x)))))

(defmacro with-range (key optgroup-name option-name)
  `(lambda (product request-plist filter-options)
     (let ((value-f (getf request-plist (intern (string-upcase (format nil "~a-f" (symbol-name ,key))) :keyword)))
           (value-t (getf request-plist (intern (string-upcase (format nil "~a-t" (symbol-name ,key))) :keyword)))
           (value-x 0))
       (with-option product
         ,optgroup-name ,option-name
         (setf value-x (value option)))
       (when (null value-x)
         (setf value-x "0"))
       (when (null value-f)
         (setf value-f "0"))
       (when (or (null value-t)
                 (string= value-t ""))
         (setf value-t "99999999"))
       (setf value-f (arnesi:parse-float (format nil "~as" value-f)))
       (setf value-t (arnesi:parse-float (format nil "~as" value-t)))
       (setf value-x (arnesi:parse-float (format nil "~as" value-x)))
       (and (<= value-f value-x)
            (>= value-t value-x)))))

(defun sklonenie (name skl)
  (setf name (string-downcase name)))


;;Фильтруем по наличию опции
(defun filter-with-check-options (key-name option-group-name product request-plist filter-options)
  (let ((number 0)
        (result-flag t))
    (mapcar #'(lambda (option-name)
                (let ((value-p (getf request-plist
                                     (intern (string-upcase
                                              (format nil "~a-~a"
                                                      key-name
                                                      number))
                                             :keyword))))
                  (incf number)
                  (when (equal value-p "1")
                    (let ((value-x))
                      (mapcar #'(lambda (optgroup)
                                  (if (string= (name optgroup) option-group-name)
                                      (progn
                                        (let ((options (options optgroup)))
                                          (mapcar #'(lambda (option)
                                                      (if (string= (name option) option-name)
                                                          (setf value-x (value option))))
                                                  options)))))
                              (optgroups product))
                      (if (not (string= value-x "Есть"))
                          (setf result-flag nil))))))
            filter-options)
    result-flag))

;; (filter-test (gethash "noutbuki" *storage*) "http://dev.320-8080.ru/noutbuki?fullfilter=1")
;; (filter-test (gethash "noutbuki" *storage*)  "http://dev.320-8080.ru/noutbuki?price-f=&price-t=&producer-13=1&producer-14=1&screen-size-f=&screen-size-t=&work-on-battery-f=&work-on-battery-t=&weight-f=&weight-t=&harddrive-f=&harddrive-t=&screen-resolution-f=&screen-resolution-t=&ram-f=&ram-t=&fullfilter=1#producer")
;; (filter-test (gethash "noutbuki" *storage*)  "http://dev.320-8080.ru/noutbuki?price-f=&price-t=&producer-13=1&producer-14=1&screen-size-f=&screen-size-t=&work-on-battery-f=&work-on-battery-t=&weight-f=&weight-t=&harddrive-f=&harddrive-t=&screen-resolution-f=&screen-resolution-t=&ram-f=&ram-t=&os-0=1&os-1=1&fullfilter=1#producer")
;; ( make-get-str "http://dev.320-8080.ru/noutbuki?price-f=&price-t=&producer-13=1&producer-14=1&screen-size-f=&screen-size-t=&work-on-battery-f=&work-on-battery-t=&weight-f=&weight-t=&harddrive-f=&harddrive-t=&screen-resolution-f=&screen-resolution-t=&ram-f=&ram-t=&os-0=1&os-1=1&fullfilter=1#producer")
;; (filter-test (gethash "noutbuki" *storage*) "http://www.320-8080.ru/noutbuki?&warranty-1=1&fullfilter=1")

;;фильтрация по значениям опции
(defun filter-with-check-values (key-name option-group-name option-name product request-plist filter-options)
  (let ((number 0)
        (result-flag nil)
        (request-flag t)
        (value-x nil))
    (with-option product
      option-group-name option-name
      (setf value-x (value option)))
    ;; (format t "~&Значение опции: ~a ключ: ~a " value-x key-name)
    (mapcar #'(lambda (option-value)
                (let ((value-p (getf request-plist
                                     (intern (string-upcase
                                              (format nil "~a-~a"
                                                      key-name
                                                      number))
                                             :keyword))))
                  (incf number)
                  ;; (format t "~&Опция в запросе: ~a ~a" option-value value-p)
                  (when (equal value-p "1")
                    (setf request-flag nil)
                    ;; (format t "~&Опция в запросе: ~a" option-value)
                    (if (string= value-x option-value)
                        (setf result-flag t)))))
            filter-options)
    ;; DBG
    ;; (if (string= (format nil "~a" key-name) "WARRANTY")
    ;;     (progn
    ;;       (print filter-options) ;;158712
    ;;       (format t "~a-- ~a : ~a" (articul product)  result-flag request-flag)))
    (or result-flag
        request-flag)))

(defmacro with-check (key optgroup-name dummy-var)
  `(lambda (product request-plist filter-options)
     (let ((option-group-name ,optgroup-name)
           (key-name (symbol-name ,key)))
       (if (string= ,dummy-var "")
           (filter-with-check-options key-name option-group-name product request-plist filter-options)
           (filter-with-check-values key-name option-group-name ,dummy-var product request-plist filter-options)))))


     ;; (let ((value-p (getf request-plist (intern (string-upcase (format nil "~a" (symbol-name ,key))) :keyword)))
     ;;       (value-x ""))
     ;;   (with-option product
     ;;     ,optgroup-name ,dummy-var
     ;;     (setf value-x (value option)))
     ;;   (print filter-options)
     ;;   (cond
     ;;     ((null value-p)
     ;;      t)
     ;;     ((null value-x)
     ;;      nil)
     ;;     (t
     ;;      (progn
     ;;        (setf value-p (parse-integer value-p))
     ;;        (let ((opt-val (nth value-p filter-options)))
     ;;          (if (string= opt-val "Любой")
     ;;              t
     ;;              (string= value-x opt-val)))))))))


(defmacro with-radio (key optgroup-name option-name)
  `(lambda (product request-plist filter-options)
     (let ((value-p (getf request-plist (intern (string-upcase (format nil "~a" (symbol-name ,key))) :keyword)))
           (value-x ""))
       (with-option product
         ,optgroup-name ,option-name
         (setf value-x (value option)))
       (cond
         ((null value-p)
          t)
         ((null value-x)
          nil)
         (t
          (progn
            (setf value-p (parse-integer value-p))
            (let ((opt-val (nth value-p filter-options)))
              (if (string= opt-val "Любой")
                  t
                  (string= value-x opt-val)))))))))


(defun paginator-page-line (request-get-plist start stop current)
  (loop :for i from start :to stop :collect
     (let ((plist request-get-plist)
           (is-current-page nil))
       (setf (getf plist :page) (format nil "~a" i))
       (setf is-current-page (= current i))
       (format nil "<a href=\"?~a\">~:[~;<big><b>~]~a~:[~;</b></big>~]</a>"
               (make-get-str plist)
               is-current-page
               i
               is-current-page))))

(defun paginator (request-get-plist sequence &optional (pagesize 15))
  (let ((page (getf request-get-plist :page))
        (page-count (ceiling (length sequence) pagesize)))
    (when (null page)
      (setf page "1"))
    (setf page (parse-integer page :junk-allowed t))
    (unless (and (numberp page)
                 (plusp page))
      (setf page 1))
    (if (> page page-count)
        (setf page page-count))
    (let* ((result (let ((tmp (ignore-errors (subseq sequence (* pagesize (- page 1))))))
                     (when (> (length tmp) pagesize)
                       (setf tmp (subseq tmp 0 pagesize)))
                     tmp))
           (start-page-line nil)
           (cur-page-line nil)
           (stop-page-line nil)
           (start-number 1)
           (stop-number page-count)
           (page-line-string ""))
      (if (> page 5)
          (progn
            (setf start-number (- page 2))
            (setf start-page-line (paginator-page-line request-get-plist 1 2 0))))
      (if (> (- page-count page) 5)
          (progn
            (setf stop-number (+ page 2))
            (setf stop-page-line (paginator-page-line request-get-plist (- page-count 1) page-count 0))))
      (setf cur-page-line (paginator-page-line request-get-plist start-number stop-number page))
      (if (> page-count 1)
          (setf page-line-string
                (format nil "~@[~{~a~}...~] ~{~a ~} ~@[...~{~a~}~]"
                        start-page-line
                        cur-page-line
                        stop-page-line)))
      (values result page-line-string)
      )))


    ;; (loop :for elt :in sequence :do
    ;;    (if (< i (* (- page 1) pagesize))
    ;;        (progn
    ;;          (setf head (cdr head))
    ;;          (incf i))
    ;;        (return)))
    ;; (setf i 0)
    ;; (loop :for elt :in head :do
    ;;    (if (> pagesize i)
    ;;        (progn
    ;;          (push elt ret)
    ;;          (incf i))
    ;;        (return)))
    ;; (let* ((size (floor (length sequence) pagesize))
    ;;        (show-pages
    ;;         (sort
    ;;          (remove-if #'(lambda (x)
    ;;                         (or (not (plusp x))
    ;;                             (< size  x)))
    ;;                     (remove-duplicates
    ;;                      (append '(1 2 3) (list (- page 1) page (+ page 1)) (list (- size 2) (- size 1) size))))
    ;;          #'(lambda (a b)
    ;;              (< a b))))
    ;;        (tmp 0)
    ;;        (res))
    ;;   (loop :for i :in show-pages :do
    ;;      (let ((plist request-get-plist))
    ;;        (when (not (equal tmp (- i 1)))
    ;;          (push "<span>&hellip;</span>" res))
    ;;        (setf tmp i)
    ;;        (setf (getf plist :page) (format nil "~a" i))
    ;;        (push (if (equal page i)
    ;;                  (format nil "<a class=\"active\" href=\"?~a\">~a</a>"
    ;;                          (make-get-str plist)
    ;;                          i)
    ;;                  ;; else
    ;;                  (format nil "<a href=\"?~a\">~a</a>"
    ;;                          (make-get-str plist)
    ;;                          i))
    ;;              res)))
    ;;   (values
    ;;    (reverse ret)
    ;;    (format nil "~{~a&nbsp;&nbsp;&nbsp;&nbsp;~}" (reverse res))
    ;;    ))))


(defun menu-sort (a b)
  (if (or (null (order a))
          (null (order b)))
      nil
      ;; else
      (< (order a)
         (order b))))


(defun menu (&optional current-object)
  (let ((root-groups)
        (current-key (let* ((breadcrumbs (breadcrumbs current-object))
                            (first       (getf (car (getf breadcrumbs :breadcrumbelts)) :key)) )
                       (if (not (null first))
                           first
                           (getf (getf breadcrumbs :breadcrumbtail) :key)
                           ))))
    (maphash #'(lambda (key val)
                 (when (and
                        (equal 'group (type-of val))
                        (null (parent val))
                        (active val)
                        (not (empty val))
                        ;;проверка на реальное наличие активных товаров
                        (not (= 0
                                (length (remove-if-not #'active (get-recursive-products val))))))
                   (push val root-groups)))
             *storage*)
    (let ((src-lst (mapcar #'(lambda (val)
                               (if (string= (format nil "~a" current-key) (key val))
                                   ;; This is current
                                   (leftmenu:selected
                                    (list :divider (or
                                                    (string= (key val) "setevoe-oborudovanie")
                                                    (string= (key val) "foto-and-video")
                                                    (string= (key val) "rashodnye-materialy"))
                                          :key (key val)
                                          :name (name val)
                                          :icon (icon val)
                                          :subs (loop
                                                   :for child
                                                   :in (sort
                                                        (remove-if #'(lambda (g)
                                                                       (or
                                                                        (empty g)
                                                                        (not (active g))
                                                                        ;;проверка на реальное наличие активных товаров
                                                                        (= 0
                                                                           (length
                                                                            (remove-if-not #'active
                                                                                           (get-recursive-products g))))
                                                                        ))
                                                                   (childs val)) #'menu-sort)
                                                   :collect
                                                   (list :key  (key child) :name (name child)))
                                          ))
                                   ;; else - this is ordinal
                                   (leftmenu:ordinal (list :divider (or
                                                                     (string= (key val) "setevoe-oborudovanie")
                                                                     (string= (key val) "foto-and-video")
                                                                     (string= (key val) "rashodnye-materialy"))
                                                           :key  (key val)
                                                           :name (name val)
                                                           :icon (icon val)))
                                   ))
                           (sort root-groups #'menu-sort)
                           ;; root-groups
                           )))
      (leftmenu:main (list :elts src-lst)))))


(defun breadcrumbs (in &optional out)
  (cond ((equal (type-of in) 'product)
         (progn
           (push (list :key (articul in) :val (name in)) out)
           (setf in (parent in))))
        ((equal (type-of in) 'group)
         (progn
           (push (list :key (key in) :val (name in)) out)
           (setf in (parent in))))
        ((equal (type-of in) 'filter)
         (progn
           (push (list :key (key in) :val (name in)) out)
           (setf in (parent in))))
        (t (if (null in)
               ;; Конец рекурсии
               (return-from breadcrumbs
                 (list :breadcrumbelts (butlast out)
                       :breadcrumbtail (car (last out))))
               ;; else - Ищем по строковому значению
               (let ((parent (gethash in *storage*)))
                 (cond ((equal 'group (type-of parent)) (setf in parent))
                       ((null parent) (return-from breadcrumbs (list :breadcrumbelts (butlast out)
                                                                     :breadcrumbtail (car (last out)))))
                       (t (error "breadcrumb link error")))))))
  (breadcrumbs in out))



(defun default-page (&optional (content nil) &key keywords description title no-need-cart)
  (root:main (list :keywords keywords
                   :description description
                   :title title
                   :header (root:header (append (list :logged (root:notlogged)
                                                      :cart (if (not no-need-cart)
                                                                (root:cart)))
                                                (main-page-show-banner "line" (banner *main-page.storage*))))
                   :footer (root:footer)
                   :content (if content
                                content
                                (format nil "<pre>'~a' ~%'~a' ~%'~a'</pre>"
                                        (request-str)
                                        (hunchentoot:request-uri *request*)
                                        (hunchentoot:header-in* "User-Agent"))))))


(defun checkout-page (&optional (content nil))
  (root:main (list :header (root:shortheader)
                   :footer (root:footer)
                   :content (if content
                                content
                                "test page"))))

(defun checkout-thankes-page (&optional (content nil))
  (root:main (list :header (root:short-linked-header)
                   :footer (root:footer)
                   :content (if content
                                content
                                "test page"))))

(defun static-page ()
  (let ((∆ (find-package (intern (string-upcase (subseq (request-str) 1)) :keyword))))
    (default-page
        (static:main
         (list :menu (menu)
               :breadcrumbs (funcall (find-symbol (string-upcase "breadcrumbs") ∆))
               :subcontent  (funcall (find-symbol (string-upcase "subcontent") ∆))
               :rightblock  (funcall (find-symbol (string-upcase "rightblock") ∆)))))))


(defun request-str ()
  (let* ((request-full-str (hunchentoot:url-decode (hunchentoot:request-uri hunchentoot:*request*)))
         (request-parted-list (split-sequence:split-sequence #\? request-full-str))
         (request-str (string-right-trim "\/" (car request-parted-list)))
         (request-list (split-sequence:split-sequence #\/ request-str))
         (request-get-plist (if (null (cadr request-parted-list))
                                nil
                                ;; else
                                (let ((result))
                                  (loop :for param :in (split-sequence:split-sequence #\& (cadr request-parted-list)) :do
                                     (let ((split (split-sequence:split-sequence #\= param)))
                                       (setf (getf result (intern (string-upcase (car split)) :keyword))
                                             (if (null (cadr split))
                                                 ""
                                                 (cadr split)))))
                                  result))))
    (values request-str request-list request-get-plist)))


(defun request-get-plist ()
  (multiple-value-bind (request-str request-list request-get-plist)
      (request-str)
    request-get-plist))


(defun request-list ()
  (multiple-value-bind (request-str request-list request-get-plist)
      (request-str)
    request-list))



(defun make-get-str (request-get-plist)
  (format nil "~{~a~^&~}"
          (loop :for cursor :in request-get-plist by #'cddr collect
              (format nil "~a=~a" (string-downcase cursor) (getf request-get-plist cursor))
             )))



(defun parse-id (id-string)
  (let ((group_id (handler-bind ((SB-INT:SIMPLE-PARSE-ERROR
								  #'(lambda (c)
									  (declare (ignore c))
									  (invoke-restart 'set-nil)))
								 (TYPE-ERROR
								  #'(lambda (c)
									  (declare (ignore c))
									  (invoke-restart 'set-nil)))
								 )
					(restart-case (parse-integer id-string)
					  (set-nil ()
						nil)))))
	group_id))

(defun strip ($string)
  (cond ((vectorp $string) (let (($ret nil))
							 (loop
								for x across $string collect x
								do (if (not
										(or
										 (equal x #\')
										 (equal x #\")
										 (equal x #\!)
										 (equal x #\%)
										 (equal x #\\)
										 (equal x #\/)
										 ))
									   (push x $ret)))
							 (coerce (reverse $ret) 'string)))
		((listp $string)   (if (null $string)
							   ""
							   $string))))

(defun stripper ($string)
  (cond ((vectorp $string) (let (($ret nil))
							 (loop
								for x across $string collect x
								do (if (not
										(or
										 (equal x #\')
										 (equal x #\")
										 (equal x #\\)
                                         (equal x #\~)
										 ))
									   (push x $ret)))
                             (let ((ret (coerce (reverse $ret) 'string)))
                               (when (equal 0 (length ret))
                                 (return-from stripper ""))
                               ret)))))


(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))


(defun merge-plists (a b)
  (let* ((result (copy-list a)))
	(loop while (not (null b)) do
		 (setf (getf result (pop b)) (pop b)))
	result))


(defun reverse-plist (inlist)
  (let ((result))
    (loop :for i :in inlist by #'cddr do
       (setf (getf result i) (getf inlist i)))
    result))


(defun numerizable (param)
  (coerce (loop for i across param when (parse-integer (string i) :junk-allowed t) collect i) 'string))


(defun slice (cnt lst)
  (let ((ret))
    (tagbody re
       (push (loop
                :for elt :in lst
                :repeat cnt
                :collect
                (pop lst)) ret)
       (unless (null lst)
         (go re)))
    (reverse ret)))


(defun cut (cnt lst)
  (values (loop
             :for elt :in lst
             :repeat cnt
             :collect
             (pop lst))
          lst))




(defun get-procent (base real)
  (when (equal 0 base)
    (return-from get-procent (values 0 0)))
  (if (or (null base) (null real))
	  (return-from get-procent 0))
  (values (format nil "~$"  (- base real))
          (format nil "~1$" (- 100 (/ (* real 100) base)))))


(defun get-pics (articul)
  (let ((path (format nil "~a/big/~a/*.jpg" *path-to-pics* articul)))
    (loop
       :for pic
       :in (ignore-errors (directory path))
       :collect (format nil "~a.~a"
                        (pathname-name pic)
                        (pathname-type pic)))))


(defmethod get-keyoptions ((object product))
  (let ((parent (parent object)))
    (when (null parent)
      (return-from get-keyoptions nil))
    (mapcar #'(lambda (pair)
                (let ((optgroup (getf pair :optgroup))
                      (optname  (getf pair :optname))
                      (optvalue))
                  (mapcar #'(lambda (option)
                              (if (string= (name option) optgroup)
                                  (let ((options (options option)))
                                    (mapcar #'(lambda (opt)
                                                (if (string= (name opt) optname)
                                                    (setf optvalue (value opt))))
                                            options))))
                          (optgroups object))
                  (list :optgroup optgroup
                        :optname optname
                        :optvalue optvalue)
                  ))
            (keyoptions parent))))

(defun get-format-price (p)
  (let ((rs (format nil "~a" p))
        (str (reverse (format nil "~a" p))))
    (when (< 3 (length str))
      (let ((st1 (reverse (subseq str 0 3)))
            (st2 (reverse (subseq str 3))))
        (setf rs (format nil "~a ~a" st2 st1))))
    rs))


(defmethod view ((object product))
  (let ((pics (get-pics (articul object))))
    (let ((group (parent object)))
      ;; (when (not (null group))
      (list :articul (articul object)
            :name (realname object)
            :groupname (if (null group)
                           "group not found"
                           (name group))
            :groupkey  (if (null group)
                           ""
                           (key  group))
            :price (siteprice object)
            :formatprice (get-format-price (siteprice object))
            :bestprice (> (price object) (siteprice object))
            :firstpic (car pics)
            :keyopts (get-keyoptions object)
            :oneclickbutton  (if (not (predzakaz object))
                                 (soy.buttons:add-one-click (list :articul (articul object))))
            :addbutton (if (predzakaz object)
                           (soy.buttons:add-predzakaz (list :articul (articul object)))
                           (soy.buttons:add-product-cart (list :articul (articul object)
                                                               :name (realname object)
                                                               :pic (if (null pics) nil (car pics))
                                                               :siteprice (price object)
                                                               :price (siteprice object))))
            ))))


;; выбор нескольких случайных элементов из списка
;; если количество не указано то возвращается список из одного элемента
;; если количество больше длинны входного списка, то возвращается перемешанный входной список
(defun get-randoms-from-list (input-list &optional (count 1))
  (let ((result)
        (current-list input-list))
    ;;уменьшаем count до длинны списка если надо
    (if (< (length input-list)
           count)
        (setf count (length input-list)))
    (setf result (loop
                    :for n
                    :from 1 to count
                    :collect (let* ((pos (random (length current-list)))
                                    (element (nth pos current-list)))
                               (setf current-list (remove-if #'(lambda (v)
                                                                 (equal v element))
                                                             current-list))
                               element)))
    result))

(defmethod relink ((object product))
  (let ((rs (list nil nil nil nil))
        (temp-rs1)
        (temp-rs2))
    (when (not (equal 'group (type-of (parent object))))
      (print object)
      (return-from relink rs))
    ;;2 случайных товара из списка
    (setf temp-rs1 (get-randoms-from-list
                   ;; список активных товаров той же группы и того же производителя
                   ;; кроме его самого
                   (let* ((base-vendor))
                     (with-option object "Общие характеристики" "Производитель"
                                  (setf base-vendor (value option)))
                     (remove-if-not
                      #'(lambda (x)
                          (and (not (equal x object))
                               (active x)
                               (let ((vendor))
                                 (with-option x "Общие характеристики" "Производитель"
                                              (setf vendor (value option)))
                                 (equal vendor base-vendor))))
                      (products (parent object))))
                   2))
    ;;4 случайных товара из списка
    (setf temp-rs2 (get-randoms-from-list
                    ;;список всех активных товаров кроме object
                    (let ((all))
                      (maphash #'(lambda (k v)
                                   (when (and (equal 'product (type-of v))
                                              (active v)
                                              (not (equal v object)))
                                     ;; (print v)
                                     (push v all)))
                               *storage*)
                      all)
                    4))
    ;; (print temp-rs1)
    ;; (print temp-rs2)
    (loop
       :for item in (append temp-rs1 temp-rs2)
       :for n
       :from 1 to 4
       :collect item)))



(defun product-sort (products operation getter)
  (sort (copy-list products) #'(lambda (a b)
                                 (if (funcall operation
                                              (funcall getter a)
                                              (funcall getter b))
                                     t
                                     nil))))

(defmethod get-filter-function-option (malformed-filter-list)
(let ((option nil))
  (maplist #'(lambda (val)
              (when (or (equal (car val) :radio)
                        (equal (car val) :checkbox))
                (setf option (cadr val))))
          malformed-filter-list)
  option))

;; (let ((functions (mapcar #'(lambda (elt)
;;                              (cons (eval (car (last elt)))
;;                                      (get-filter-function-option elt)))
;;                              (base (fullfilter (gethash "noutbuki" *storage*))))))
;;   (mapcar #'(lambda (filter-group)
;;               (let ((advanced-filters (cadr filter-group)))
;;                 (mapcar #'(lambda (advanced-filter)
;;                             (nconc functions (list (cons (eval (car (last advanced-filter)))
;;                                                    (get-filter-function-option advanced-filter)))))
;;                         advanced-filters)))
;;           (advanced (fullfilter (gethash "noutbuki" *storage*))))
;;   ;; (format t "~&~{~a~%~}" functions)
;;   (loop
;;      :for function :in functions
;;      :finally (return t)
;;      :do (format t "~&~a ~a" (car function) (cdr function))))

;; TODO: удалить из кода
(defmethod filter-controller ((object group) request-get-plist)
  (let ((functions (mapcar #'(lambda (elt)
                               (cons (eval (car (last elt)))
                                     (get-filter-function-option elt)))
                           (base (fullfilter object)))))
    (mapcar #'(lambda (filter-group)
                (let ((advanced-filters (cadr filter-group)))
                  (mapcar #'(lambda (advanced-filter)
                              (nconc functions (list (cons (eval (car (last advanced-filter)))
                                                           (get-filter-function-option advanced-filter)))))
                          advanced-filters)))
            (advanced (fullfilter object)))
    ;; processing
    (let ((result-products))
      (mapcar #'(lambda (product)
                  (when (loop
                           :for function :in functions
                           :finally (return t)
                           :do (unless (funcall (car function)
                                                product
                                                request-get-plist
                                                (cdr function))
                                 (return nil)))
                    (push product result-products)))
              (remove-if-not #'(lambda (product)
                                 (active product))
                             (get-recursive-products object)))
      result-products)))


(defmethod fullfilter-controller (product-list (object group) request-get-plist)
  (let ((functions (mapcar #'(lambda (elt)
                               (cons (eval (car (last elt)))
                                     (get-filter-function-option elt)))
                           (base (fullfilter object)))))
    (mapcar #'(lambda (filter-group)
                (let ((advanced-filters (cadr filter-group)))
                  (mapcar #'(lambda (advanced-filter)
                              (nconc functions (list (cons (eval (car (last advanced-filter)))
                                                           (get-filter-function-option advanced-filter)))))
                          advanced-filters)))
            (advanced (fullfilter object)))
    ;; processing
    (let ((result-products))
      (mapcar #'(lambda (product)
                  (when (loop
                           :for function :in functions
                           :finally (return t)
                           :do (unless (funcall (car function)
                                                product
                                                request-get-plist
                                                (cdr function))
                                 (return nil)))
                    (push product result-products)))
              product-list)
      result-products)))

(defun url-to-request-get-plist (url)
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
    request-get-plist))



;; (fullfilter (gethash "noutbuki" *storage*))
(defun if-need-to-show-hidden-block (elt request-get-plist)
  (let ((key (string-downcase (format nil "~a" (nth 0 elt))))
        (showflag nil))
    ;; проверку нужно ли раскрывать hidden блока
      (cond
        ((equal :radio (nth 2 elt))
         (loop
            :for nameelt
            :in  (nth 3 elt)
            :for i from 0
            :do (if (string= (format nil "~a" i)
                             (getf request-get-plist (intern
                                                      (string-upcase key))))
                    (setf showflag t))))
        ((equal :checkbox (nth 2 elt))
         (loop
            :for nameelt
            :in  (nth 3 elt)
            :for i from 0
            :do (if  (string= "1" (getf request-get-plist (intern
                                                           (string-upcase
                                                            (format nil "~a-~a" key i))
                                                           :keyword)))
                     (setf showflag t)))))
      showflag))

;; (is-need-to-show-advanced (fullfilter (gethash "noutbuki" *storage*)) (url-to-request-get-plist "http://dev.320-8080.ru/noutbuki?price-f=&price-t=&producer-0=1&screen-size-f=&screen-size-t=&work-on-battery-f=&work-on-battery-t=&weight-f=&weight-t=&frequency-f=&frequency-t=&ram-f=&ram-t=&harddrive-f=&harddrive-t=&videoram-f=&videoram-t=&fullfilter=1#producer"))

(defun is-need-to-show-advanced (fullfilter request-get-plist)
  (let ((flag nil))
    (mapcar #'(lambda (elt)
                (mapcar #'(lambda (inelt)
                            (setf flag (or flag
                                           (if-need-to-show-hidden-block inelt request-get-plist))))
                        (cadr elt)))
            (advanced fullfilter))
    flag))

(defun filter-element (elt request-get-plist)
  (let* ((key (string-downcase (format nil "~a" (nth 0 elt))))
         (name (nth 1 elt))
         (showflag nil)
         (ishidden (search '(:hidden) elt))
         (contents
          (cond ((equal :range (nth 2 elt))
                 (fullfilter:range
                  (list :unit (nth 3 elt)
                        :key key
                        :name name
                        :ishidden ishidden
                        :from (getf request-get-plist
                                    (intern (string-upcase (format nil "~a-f" key)) :keyword))
                        :to (getf request-get-plist
                                  (intern (string-upcase (format nil "~a-t" key)) :keyword)))))
                ((equal :radio (nth 2 elt))
                 (fullfilter:box
                  (list :key key
                        :name name
                        :ishidden ishidden
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
                        :ishidden ishidden
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
    (if ishidden
        (fullfilter:hiddencontainer (list :key key
                                          :name name
                                          :contents contents
                                          :isshow (if-need-to-show-hidden-block elt request-get-plist)))
        contents)))


;; (url-to-request-get-plist "http://www.320-8080.ru/komputery?vendor=%D0%A6%D0%B8F%D1%80%D1%8B")

(defmethod vendor-controller ((object group) request-get-plist)
  (let* ((result-products))
    (mapcar #'(lambda (product)
                (let ((vendor))
                  (with-option product "Общие характеристики" "Производитель"
                               (setf vendor (value option)))
                  ;; (format t "~%[~a] : [~a] : [~a]"
                  ;;         (string-downcase (string-trim '(#\Space #\Tab #\Newline) vendor))
                  ;;         (string-downcase (ppcre:regex-replace-all "%20" (getf request-get-plist :vendor) " "))
                  ;;         ;; (loop :for ch :across (ppcre:regex-replace-all "%20" (getf request-get-plist :vendor) " ") :do
                  ;;         ;;    (format t "~:c." ch))
                  ;;         )
                  ;; vendor (getf request-get-plist :vendor))

                  ;; Временно отключил унификацию по регистру
                  ;; (if (string=
                  ;;      (string-downcase
                  ;;       (string-trim '(#\Space #\Tab #\Newline) vendor))
                  ;;      (string-downcase
                  ;;       (string-trim '(#\Space #\Tab #\Newline)
                  ;;                    (ppcre:regex-replace-all "%20" (getf request-get-plist :vendor) " "))))
                  ;;     (push product result-products))))

                  (if (string=
                        (string-trim '(#\Space #\Tab #\Newline) vendor)
                        (string-trim '(#\Space #\Tab #\Newline)
                                     (ppcre:regex-replace-all "%20" (getf request-get-plist :vendor) " ")))
                      (push product result-products))
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ))

            ;; (remove-if-not #'(lambda (product)
            ;;                    (active product))
                           ;; (get-recursive-products object))
            (get-recursive-products object)
           )
    result-products))

(defmethod vendor-filter-controller (product request-get-plist)
  (let ((vendor))
       (with-option product "Общие характеристики" "Производитель"
                    (setf vendor (value option)))
       (string=
            (string-trim '(#\Space #\Tab #\Newline) vendor)
            (string-trim '(#\Space #\Tab #\Newline)
                         (ppcre:regex-replace-all "%20" (getf request-get-plist :vendor) " ")))))

;;; Функция, добавляющая в хлебные крошки вендора, если он присутствует
;;; в get запросе.
(defun breadcrumbs-add-vendor (breadcrumbs)
  (let ((belts (getf breadcrumbs :breadcrumbelts))
        (tail (getf breadcrumbs :breadcrumbtail))
        (vendor (getf (request-get-plist) :vendor))
        (result breadcrumbs))
    (when (not (null vendor))
      (setf result (list :breadcrumbelts (append belts (list tail))
                         :breadcrumbtail (list :key vendor
                                               :val (format nil "~a ~a"
                                                            (getf tail :val)
                                                            vendor)))))
    result))


;; Сделать строку начинающейся с заглавной буквы.
(defun string-convertion-for-title (title)
  (if (not (null title))
      (format nil "~a~a"
              (string-upcase (subseq title 0 1))
              (subseq title 1))))
