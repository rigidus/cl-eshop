;;;; service.lisp
;;;;
;;;; This file is part of the eshop project,
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(in-package #:service)


(defun get-date-time ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second))
    (format nil
            "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
            year
            month
            date
            hour
            minute)))

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
                 (when (and
                        (null (group:parent val))
                        (group:active val)
                        (not (group:empty val)))
                   (push val root-groups)))
             trans:*group*)
    (let ((src-lst (mapcar #'(lambda (val)
                               (if (string= (format nil "~a" current-key) (group:key val))
                                   ;; This is current
                                   (leftmenu:selected
                                    (list :key (group:key val)
                                          :name (group:name val)
                                          :subs (loop
                                                   :for child
                                                   :in (sort
                                                        (remove-if #'(lambda (g)
                                                                       (or
                                                                        (group:empty g)
                                                                        (not (group:active g)) ))
                                                                   (group:childs val)) #'menu-sort)
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


(defun checkout-page (&optional (content nil))
  (root:main (list :header (root:shortheader)
                   :footer (root:footer)
                   :content (if content
                                content
                                "test page"))))


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

