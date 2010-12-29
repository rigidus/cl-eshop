(in-package #:service)


(defun paginator (request-get-plist sequence &optional (pagesize 15))
  (let ((page (getf request-get-plist :page))
        (head sequence)
        (ret)
        (i 1))
    (when (null page)
      (setf page "1"))
    (setf page (parse-integer page :junk-allowed t))
    (unless (and (numberp page)
                 (plusp page))
      (setf page 1))
    (loop :for elt :in sequence :do
       (if (< i (* (- page 1) pagesize))
           (progn
             (setf head (cdr head))
             (incf i))
           (return)))
    (setf i 0)
    (loop :for elt :in head :do
       (if (> pagesize i)
           (progn
             (push elt ret)
             (incf i))
           (return)))
    (let* ((size (floor (length sequence) pagesize))
           (show-pages
            (sort
             (remove-if #'(lambda (x)
                            (or (not (plusp x))
                                (< size  x)))
                        (remove-duplicates
                         (append '(1 2 3) (list (- page 1) page (+ page 1)) (list (- size 2) (- size 1) size))))
             #'(lambda (a b)
                 (< a b))))
           (tmp 0)
           (res))
      (loop :for i :in show-pages :do
         (let ((plist request-get-plist))
           (when (not (equal tmp (- i 1)))
             (push "<span>&hellip;</span>" res))
           (setf tmp i)
           (setf (getf plist :page) (format nil "~a" i))
           (push (if (equal page i)
                     (format nil "<a class=\"active\" href=\"?~a\">~a</a>"
                             (make-get-str plist)
                             i)
                     ;; else
                     (format nil "<a href=\"?~a\">~a</a>"
                             (make-get-str plist)
                             i))
                 res)))
      (values
       (reverse ret)
       (format nil "~{~a&nbsp;&nbsp;&nbsp;&nbsp;~}" (reverse res))
       ))))


(defun menu-sort (a b)
  (if (or (null (group:order a))
          (null (group:order b)))
      nil
      ;; else
      (< (group:order a)
         (group:order b))))


(defun menu (&optional current-object)
  (let ((root-groups)
        (current-key (let* ((breadcrumbs (breadcrumbs current-object))
                            (first       (getf (car (getf breadcrumbs :breadcrumbelts)) :key)) )
                       (if (not (null first))
                           first
                           (getf (getf breadcrumbs :breadcrumbtail) :key)
                           ))))
    (maphash #'(lambda (key val)
                 (when (null (group:parent val))
                   (push val root-groups)))
             trans:*group*)
    (let ((src-lst (mapcar #'(lambda (val)
                               (if (string= current-key (group:key val))
                                   ;; This is current
                                   (leftmenu:selected
                                    (list :key (group:key val)
                                          :name (group:name val)
                                          :subs (loop
                                                   :for child
                                                   :in (sort (copy-list (group:childs val)) #'menu-sort)
                                                   :collect
                                                   (list :key  (group:key child) :name (group:name child)))
                                          ))
                                   ;; else - this is ordinal
                                   (leftmenu:ordinal (list :key  (group:key val) :name (group:name val)))
                                   ))
                           (sort root-groups #'menu-sort)
                           ;; root-groups
                           )))
      (leftmenu:main (list :elts src-lst)))))


(defun breadcrumbs (in &optional out)
  (cond ((equal (type-of in) 'product:product)
         (progn
           (push (list :key (product:articul in) :val (product:name in)) out)
           (setf in (product:parent in))))
        ((equal (type-of in) 'group:group)
         (progn
           (push (list :key (group:key in) :val (group:name in)) out)
           (setf in (group:parent in))))
        ((equal (type-of in) 'filter:filter)
         (progn
           (push (list :key (filter:key in) :val (filter:name in)) out)
           (setf in (filter:parent in))))
        (t (if (null in)
               ;; Конец рекурсии
               (return-from breadcrumbs
                 (list :breadcrumbelts (butlast out)
                       :breadcrumbtail (car (last out))))
               ;; else - Ищем по строковому значению
               (let ((parent (gethash in trans:*group*)))
                 (cond ((equal 'group:group (type-of parent)) (setf in parent))
                       ((null parent) (return-from breadcrumbs (list :breadcrumbelts (butlast out)
                                                                     :breadcrumbtail (car (last out)))))
                       (t (error "breadcrumb link error")))))))
  (breadcrumbs in out))


(defun default-page (&optional (content nil))
  (root:main (list :header (root:header (list :logged (root:notlogged)
                                              :cart (root:cart)))
                   :footer (root:footer)
                   :content (if content
                                content
                                (format nil  "<pre>'~a' ~%'~a' ~%'~a'</pre>"
                                        (get-request-str)
                                        (hunchentoot:request-uri *request*)
                                        (hunchentoot:header-in* "User-Agent"))))))


(defun checkout-page (request request-str &optional (content nil))
  (root:main (list :header (root:shortheader)
                   :footer (root:footer)
                   :content (if content
                                content
                                (format nil  "<pre>'~a' ~%'~a' ~%'~a'</pre>"
                                        request-str
                                        (hunchentoot:request-uri request)
                                        (hunchentoot:header-in* "User-Agent"))))))


(defun static-page ()
  (let ((∆ (find-package (intern (string-upcase (subseq (service:request-str) 1)) :keyword))))
    (service:default-page
        (static:main
         (list :menu (service:menu)
               :breadcrumbs (funcall (find-symbol (string-upcase "breadcrumbs") ∆))
               :subcontent  (funcall (find-symbol (string-upcase "subcontent") ∆))
               :rightblock  (funcall (find-symbol (string-upcase "rightblock") ∆)))))))

(defun request-str ()
  (let* ((request-full-str (hunchentoot:request-uri hunchentoot:*request*))
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
             (string-downcase (format nil "~a=~a" cursor (getf request-get-plist cursor))))))

