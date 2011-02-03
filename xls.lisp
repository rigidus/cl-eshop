;;;; xls.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(in-package #:eshop)


(defclass nko ()
  ((folder  :initarg :folder  :initform nil :accessor folder)
   (xls2csv :initarg :xls2csv :initform nil :accessor xls2csv)))


(defmethod initialize-instance :after ((obn nko) &key)
  (unless (file-exists-p (xls2csv obn))
    (error "xls2csv not found"))
  (unless (directory-exists-p (folder obn))
    (error "folder not found")))

(defparameter px (make-instance 'nko
                                :folder  (format nil "~aDropbox/xls" (user-homedir-pathname))
                                :xls2csv "/usr/bin/xls2csv"))

(defmethod ƒ ((isg string) (obn nko))
  (let ((bin))
    (values
     (mapcar #'(lambda (y) (string-trim '(#\Space #\Tab) y))
             (mapcar #'(lambda (y) (regex-replace-all "\\s+" y " "))
                     (mapcar #'(lambda (y) (string-trim '(#\Space #\Tab #\") y))
                             (let ((inp) (sv) (ac) (rs))
                               (loop :for cr :across isg do
                                  (if (null inp)
                                      (cond ((equal #\" cr) (setf inp t))
                                            ((equal #\, cr) (push "" rs)))
                                      (cond ((and (null sv) (equal #\" cr)) (setf sv t))
                                            ((and sv (equal #\" cr)) (progn (setf sv nil)
                                                                            (push #\" ac)))
                                            ((and sv (equal #\, cr)) (progn (setf sv nil)
                                                                            (setf inp nil)
                                                                            (push (coerce (reverse ac) 'string) rs)
                                                                            (setf ac nil)))
                                            ((equal #\Return cr) nil)
                                            (t (push cr ac)))))
                               (when ac
                                 (if (and inp (null sv))
                                     (setf bin t))
                                 (push (coerce (reverse ac) 'string) rs))
                               (reverse rs)))))
     bin)))


(defmethod ƒ ((prm list) (obn nko))
  (let* ((line (getf prm :line))
         (optgroups (getf prm :optgroups))
         (fields (getf prm :fields))
         (flt)
         (rs)
         (mx (max (length line) (length optgroups) (length fields)))
         (cur-optgroup)
         (cur-options))
    (loop :for i :from 0 :to (- mx 1) :do
       (let ((val       (if (nth i line)       (nth i line) ""))
             (optgroup  (if (nth i optgroups)  (nth i optgroups) ""))
             (field     (if (nth i fields)     (nth i fields) "")))
         (cond ((equal i 0) (setf (getf flt :articul)
                                  ;; (handler-case
                                  (parse-integer val)
                                  ;; (SB-INT:SIMPLE-PARSE-ERROR (se) (setf *tmp* line)))
                                  ))
               ((equal i 1) (setf (getf flt :realname) val))
               (t (progn (unless (equal 0 (length optgroup))
                           (unless (null cur-optgroup)
                             (push (list :optgroup_name cur-optgroup :options (reverse cur-options)) rs))
                           (setf cur-optgroup optgroup)
                           (setf cur-options nil))
                         (push (list :name field :value val) cur-options))))))
    (push (list :optgroup_name cur-optgroup :options (reverse cur-options)) rs)
    (append flt (list :rs-options (reverse rs)))))


(defmethod ƒ ((ifl pathname) (obn nko))
  (let ((rs) (otp))
    (setf otp (with-output-to-string (*standard-output*)
                   (let* ((proc (sb-ext:run-program
                                 (xls2csv obn)
                                 (list "-q3" (format nil "~a" ifl)) :wait nil :output :stream))
                          (optgroups)
                          (fields))
                     (with-open-stream (in (sb-ext:process-output proc))
                       (loop :for i from 1 do
                          (tagbody loop-body
                             (handler-case
                                 (let ((ist (read-line in)))
                                   (tagbody dec
                                      (multiple-value-bind (line esf)
                                          (ƒ ist px)
                                        (when esf
                                          (setf ist (concatenate 'string ist (read-line in)))
                                          (incf i)
                                          (go dec))
                                        (unless (null line)
                                          (cond ((null optgroups) (setf optgroups line))
                                                ((null fields) (setf fields line))
                                                (t (handler-case
                                                       (let ((val (ƒ (list :line line
                                                                           :optgroups optgroups
                                                                           :fields fields)
                                                                     px)))
                                                         (print "")
                                                         (print val)
                                                         (push val rs))
                                                     (SB-INT:SIMPLE-PARSE-ERROR () nil))))))))
                               (END-OF-FILE () (return i)))))))))
    rs))


(defmethod ƒ ((jct nko) (obn nko))
  (format t "~%Processing DTD: {...")
  (let ((cnt 0))
    (loop :for file :in (directory (format nil "~a/*.xls" (folder obn))) :do
       (format t "~%~a. Processing file: ~a" (incf cnt) file)
       (loop :for item :in (ƒ file px) :do
          (let* ((articul (getf item :articul))
                 (realname (getf item :realname))
                 (optgroups (loop :for optgroup :in (getf item :result-options) :collect
                               (make-instance 'optgroup
                                              :name (getf optgroup :optgroup_name)
                                              :options (loop :for option :in (getf optgroup :options) :collect
                                                          (make-instance 'option
                                                                         :name (getf option :name)
                                                                         :value (getf option :value))))))
                 (product (gethash (format nil "~a" articul) *storage*)))
            (if (null product)
                (format nil "warn: product ~a (articul ~a) not found, ignore (file: ~a)" realname articul file)
                (setf (optgroups product) optgroups)))))
    (format t "~%...} successfully processed ~a files" cnt)))


(defun dtd ()
  (ƒ px px))

(export 'dtd)
