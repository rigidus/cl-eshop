;;;; trans.lisp
;;;;
;;;; This file is part of the eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop)


;;;;;;;;;;;;;;;;;;;;;;;;;;; LOAD FROM FILES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun explore-dir (path)
  (let ((dirs) (files))
    (mapcar #'(lambda (x)
                (if (cl-fad:directory-pathname-p x)
                    (push x dirs)
                    (push x files)))
            (directory (format nil "~a/*" path)))
    (values dirs
            files
            (directory (format nil "~a/*.filter" path))
            (directory (format nil "~a/*.vendor" path))
            )))


(defun recursive-explore (path)
  (multiple-value-bind (dirs files filters vendors)
      (explore-dir path)
    (values
     (mapcar #'process-dir dirs)
     (mapcar #'process-file files)
     (mapcar #'process-filter filters)
     (mapcar #'process-vendor vendors)
     )))

(defun process-vendor (file)
  (let* ((string-filename (format nil "~a" file))
         (group (car (last (split-sequence #\/ string-filename) 2)))
         (vendor (regex-replace-all ".vendor" (pathname-name file) "")))
    (setf (gethash vendor (vendors (gethash group *storage*)))
        (read-file-into-string file))))


;; (maphash

(defun process-filter (file)
  (unserialize (format nil "~a" file) (make-instance 'filter)))


(defun process-file (file)
  (let* ((name (pathname-name file))
         (candidat (parse-integer name :junk-allowed t)))
    (if (string= (format nil "~a" candidat) name)
        (unserialize (format nil "~a" file) (make-instance 'product))
        nil)))


(defun process-dir (file)
  (let* ((string-filename (format nil "~a" file))
         (name (car (last (split-sequence #\/ string-filename) 2)))
         (target (format nil "~a~a" string-filename name))
         (target-file (file-exists-p target)))
    (when (null target-file)
      (let ((dymmy (make-instance 'group
                                  :key name
                                  :parent (gethash (car (last (split-sequence #\/ string-filename) 3)) *storage*)
                                  :name name)))
        (format t "~%warn: NO-FILE-GROUP: ~a" string-filename)
        (serialize dymmy)))
    (unserialize target (make-instance 'group))
    (recursive-explore file)))


(defparameter *storage* (make-hash-table :test #'equal))


(defun restore-from-files ()
  (let ((t-storage))
    (handler-bind ((WRONG-PRODUCT-FILE
                    #'(lambda (e)
                        (format t "~%warn: WRONG-PRODUCT-FILE: ~a"
                                (filepath e))
                        (invoke-restart 'ignore)
                        )))
      (print "start restore....{")
      (sb-ext:gc :full t)
      (let ((*storage* (make-hash-table :test #'equal)))
        (recursive-explore *path-to-bkps*)
        (setf t-storage *storage*))
      (setf *storage* t-storage)
      (sb-ext:gc :full t)
      (print "...} finish restore"))))


(restore-from-files)


;; Кое-какие заготовки для переноса данных - удалить после завершения
;; (let ((data (cl-store:restore "#h-product"))
;;       (old 0)
;;       (new 0)
;;       (grp (make-hash-table :test #'equal)))
;;   (maphash #'(lambda (k v)
;;                (let* ((articul (getf v :articul))
;;                       (product (gethash articul *product*)))
;;                  (if product
;;                      (progn
;;                        (incf old)
;;                        (setf (product:descr product)
;;                              (getf v :descr))
;;                        (setf (product:shortdescr product)
;;                              (getf v :shortdescr))
;;                        (setf (product:options product)
;;                              (trans-options (getf v :result-options))))
;;                      (let* ((group-id (getf v :group_id))
;;                             (grp-lst (gethash group-id grp nil)))
;;                        (product::serialize2
;;                         ;; (ignore-errors
;;                         (make-instance 'product
;;                                        :id (getf v :id)
;;                                        :articul articul
;;                                        :parent group-id
;;                                        :name (getf v :name)
;;                                        :realname (getf v :realname)
;;                                        :price (getf v :price)
;;                                        :siteprice (getf v :siteprice)
;;                                        :ekkprice (getf v :ekkprice)
;;                                        :active (getf v :active)
;;                                        :newbie (getf v :newbie)
;;                                        :sale (getf v :special)
;;                                        :descr (getf v :descr)
;;                                        :shortdescr (getf v :shortdescr)
;;                                        :options (trans-options (getf v :result-options))
;;                                        ))
;;                        (push articul grp-lst)
;;                        (setf (gethash group-id grp) grp-lst)
;;                        (incf new)))))
;;            data)
;;   (print (list new old (hash-table-count grp))))

;; ;; [v.2]
;; (let ((tmp (cl-store:restore "#h-product")))
;;   (maphash #'(lambda (k v)
;;                (let* ((articul (getf v :articul))
;;                       (new-opt (trans-options (getf v :result-options))))
;;                  (when (equal articul 142715)
;;                    (print "YES")
;;                    (print (optlist:optlist new-opt))
;;                    (print v)
;;                    )
;;                  ;; name
;;                  (setf (product:name (gethash articul trans:*product*))
;;                        (getf v :name))
;;                  ;; realname
;;                  (setf (product:realname (gethash articul trans:*product*))
;;                        (getf v :realname))
;;                  ;; descr
;;                  (setf (product:descr (gethash articul trans:*product*))
;;                        (getf v :descr))
;;                  ;; shortdescr
;;                  (setf (product:shortdescr (gethash articul trans:*product*))
;;                        (getf v :shortdescr))
;;                  ;; options
;;                  (if (and (equal 'optlist:optlist (type-of new-opt))
;;                           (not (null (gethash articul trans:*product*))))
;;                      (progn
;;                        (setf (product:options (gethash articul trans:*product*))
;;                              new-opt))
;;                      ;; else
;;                      (print "err!"))
;;                  ;; save
;;                  (product:serialize (gethash articul trans:*product*))
;;                  articul))
;;            tmp)
;;   (sb-ext:gc :full t)
;;   'finish)
