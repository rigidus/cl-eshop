;;;; serializers.lisp
;;;;
;;;; This file is part of the eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop)


(defgeneric serialize (object)
  (:documentation ""))

(defgeneric unserialize (filepath object)
  (:documentation ""))


;; GROUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod unserialize (filepath (dummy group))
  (let* ((file-content (alexandria:read-file-into-string filepath))
         (raw (decode-json-from-string file-content))
         (key (pathname-name filepath))
         (keyoptions (mapcar #'(lambda (pair)
                                 (list :optgroup (cdr (assoc :optgroup pair))
                                       :optname (cdr (assoc :optname pair))))
                             (cdr (assoc :keyoptions raw))))
         (parent (gethash (nth 2 (reverse (split-sequence #\/ filepath))) *storage*))
         (new (make-instance 'group
                             :key key
                             :parent parent
                             :name (cdr (assoc :name raw))
                             :active (cdr (assoc :active raw))
                             :empty (cdr (assoc :empty raw))
                             :order (cdr (assoc :order raw))
                             :fullfilter (unserialize (cdr (assoc :fullfilter raw)) (make-instance 'group-filter))
                             :keyoptions keyoptions
                             :pic (cdr (assoc :pic raw))
                             :icon (cdr (assoc :icon raw))
                             :ymlshow (cdr (assoc :ymlshow raw))
                             :descr (cdr (assoc :descr raw)))))
    (when (equal 'group (type-of parent))
      (push new (childs parent))
      (setf (childs parent) (remove-duplicates (childs parent))))
    (setf (gethash key *storage*) new)
    key))


(defmethod serialize ((object group))
  (let* ((raw-breadcrumbs (breadcrumbs object))
         (path-list (mapcar #'(lambda (elt)
                                (getf elt :key))
                            (getf raw-breadcrumbs :breadcrumbelts)))
         (current-dir (format nil "~a~a/~a/" *path-to-bkps*
                              (format nil "~{/~a~}" path-list)
                              (key object)))
         (pathname (format nil "~a~a" current-dir (key object)))
         (dumb))
    ;; Создаем директорию, если ее нет
    (ensure-directories-exist current-dir)
    ;; Подтягиваем данные из файла, если он есть
    (when (probe-file pathname)
      (setf dumb (decode-json-from-string (alexandria:read-file-into-string pathname))))
    ;; Удаляем :key и :id если он есть
    (setf dumb (remove-if #'(lambda (x)
                              (equal x (assoc :key dumb)))
                          dumb))
    (setf dumb (remove-if #'(lambda (x)
                              (equal x (assoc :id dumb)))
                          dumb))
    ;; keyoptions - json array or null
    (re-assoc dumb :keyoptions
              (if (null (keyoptions object))
                  "null"
                  (let ((json-string (format nil "~{~a~}"
                                             (loop :for item :in (keyoptions object) :collect
                                                (format nil "    {\"optgroup\":\"~a\", \"optname\":\"~a\"},~%"
                                                        (getf item :optgroup)
                                                        (getf item :optname))))))
                    (format nil "[~%~a~%  ]" (subseq  json-string 0 (- (length json-string) 2))))))
    ;; Сохраняем только те поля, которые нам известны, неизвестные сохраняем без изменений
    (re-assoc dumb :pic (pic object))
    (re-assoc dumb :icon (icon object))
    (re-assoc dumb :ymlshow (ymlshow object))
    (re-assoc dumb :order (order object))
    (re-assoc dumb :empty (empty object))
    (re-assoc dumb :active (active object))
    (re-assoc dumb :name (name object))
    ;; assembly json-string
    (let ((json-string
           (format nil "~{~a~}"
                   (remove-if #'null
                              (loop :for item :in dumb :collect
                                 (let ((field (string-downcase (format nil "~a" (car item))))
                                       (value (cdr item)))
                                   (cond ((equal t   value) (format nil "  \"~a\": true,~%" field))
                                         ((equal nil value) (format nil "  \"~a\": null,~%" field))
                                         ((or (subtypep (type-of value) 'number)
                                              (equal 'null (type-of value)))
                                          (format nil "  \"~a\": ~a,~%" field value))
                                         ((string= "keyoptions" field)
                                          (format nil "  \"~a\": ~a,~%" field value))
                                         ((subtypep (type-of value) 'string)
                                          (format nil "  \"~a\": \"~a\",~%"
                                                  field
                                                  (replace-all value "\"" "\\\"")))
                                         ;; for debug
                                         ;; (t (format nil "  \"~a\": [~a][~a],~%"
                                         ;;            field
                                         ;;            (type-of value)
                                         ;;            value))
                                         )))))))
      ;; correction
      (setf json-string (subseq  json-string 0 (- (length json-string) 2)))
      (setf json-string (format nil "{~%~a~%}~%" json-string))
      ;; dbg
      ;; (format t "~a" json-string)
      ;; save file
      (with-open-file (file pathname
                            :direction :output
                            :if-exists :supersede
                            :external-format :utf-8)
        (format file json-string))
      ;; return pathname
      pathname
      )))


;; GROUP-FILTER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod unserialize (in-string (dummy group-filter))
  (if (null in-string)
      nil
      (let ((tmp (read-from-string in-string)))
        (make-instance 'group-filter
                       :name (getf tmp :name)
                       :base (getf tmp :base)
                       :advanced (getf tmp :advanced)))))


;; PRODUCT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod unserialize (filepath (dummy product))
  (handler-bind ((WRONG-PRODUCT-SLOT-VALUE
                  #'(lambda (in-condition)
                      (restart-case
                          (error 'WRONG-PRODUCT-FILE
                                 :filepath filepath
                                 :in-condition in-condition)
                        (ignore ()
                          :report "Ignore error, save current value"
                          (invoke-restart 'ignore))
                        (set-null ()
                          :report "Set value as NIL"
                          (invoke-restart 'set-null))))))
    (let* ((file-content (alexandria:read-file-into-string filepath))
           (raw (decode-json-from-string file-content))
           (articul (cdr (assoc :articul raw)))
           (key (pathname-name filepath))
           (count-total (cdr (assoc :count-total raw)))
           (parent (gethash (nth 1 (reverse (split-sequence #\/ filepath))) *storage*))
           (name (cdr (assoc :name raw)))
           (realname (cdr (assoc :realname raw)))
           (new (make-instance 'product
                               :articul articul
                               :parent parent
                               :key key
                               :name name
                               :realname (if (or (null realname)
                                                 (string= "" realname))
                                             name
                                             realname)
                               :price (cdr (assoc :price raw))
                               :siteprice (cdr (assoc :siteprice raw))
                               :ekkprice (cdr (assoc :ekkprice raw))
                               :active (let ((active (cdr (assoc :active raw))))
                                         ;; Если количество товара равно нулю то флаг active сбрасывается
                                         (if (or (null count-total)
                                                 (= count-total 0))
                                             (setf active nil))
                                         active)
                               :newbie (cdr (assoc :newbie raw))
                               :sale (cdr (assoc :sale raw))
                               :descr (cdr (assoc :descr raw))
                               :shortdescr (cdr (assoc :shortdescr raw))
                               :count-transit (cdr (assoc :count-transit raw))
                               :count-total count-total
                               :optgroups (let* ((optgroups (cdr (assoc :optgroups raw))))
                                            (loop :for optgroup-elt :in optgroups :collect
                                               (unserialize optgroup-elt (make-instance 'optgroup)))))))
      ;; Если родитель продукта — группа, связать группу с этим продуктом
      (when (equal 'group (type-of parent))
        (push new (products parent)))
      ;; Сохраняем продукт в хэш-таблице
      (if (or (null key)
              (string= "" key))
          (setf (gethash (format nil "~a" articul) *storage*) new)
          (setf (gethash key *storage*) new))
      ;; Возвращаем артикул продукта
      articul)))


(defmethod serialize ((object product))
  (let* ((raw-breadcrumbs (breadcrumbs object))
         (path-list (mapcar #'(lambda (elt)
                                (getf elt :key))
                            (getf raw-breadcrumbs :breadcrumbelts)))
         (current-dir (format nil "~a~a/" *path-to-bkps*
                              (format nil "~{/~a~}" path-list)))
         (pathname (format nil "~a~a" current-dir (articul object))))
    ;; Создаем директорию, если ее нет
    (ensure-directories-exist current-dir)
    ;; Сохраняем файл продукта
    (let* ((json-string (format nil "{~%   \"articul\": ~a,~%   \"name\": ~a,~%   \"realname\": ~a,~%   \"price\": ~a,~%   \"siteprice\": ~a,~%   \"ekkprice\": ~a,~%   \"active\": ~a,~%   \"newbie\": ~a,~%   \"sale\": ~a,~%   \"descr\": ~a,~%   \"shortdescr\": ~a,~%   \"countTransit\": ~a,~%   \"countTotal\": ~a,~%   \"optgroups\": ~a~%}"
                                (encode-json-to-string (articul object))
                                (format nil "\"~a\"" (stripper (name object)))
                                (format nil "\"~a\"" (stripper (realname object)))
                                (encode-json-to-string (price object))
                                (encode-json-to-string (siteprice object))
                                (encode-json-to-string (ekkprice object))
                                (encode-json-to-string (active object))
                                (encode-json-to-string (newbie object))
                                (encode-json-to-string (sale object))
                                (format nil "\"~a\"" (stripper (descr object)))
                                (format nil "\"~a\""(stripper (shortdescr object)))
                                (encode-json-to-string (count-transit object))
                                (encode-json-to-string (count-total object))
                                (if (null (optgroups object))
                                    (format nil " null")
                                    (format nil " [    ~{~a~^,~}~%   ]~%"
                                            (mapcar #'(lambda (optgroup)
                                                        (serialize optgroup))
                                                    (optgroups object)))
                                    )
                                )))
      ;; (print (descr object))
      (with-open-file (file pathname
                            :direction :output
                            :if-exists :supersede
                            :external-format :utf-8)
        ;; (format t json-string)
        (format file "~a" json-string)))
    pathname))


;; FILTER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod unserialize (pathname (dummy filter))
  (let* ((file-content (alexandria:read-file-into-string pathname))
         (raw (decode-json-from-string file-content))
         (key (pathname-name pathname))
         (parent (gethash (nth 1 (reverse (split-sequence #\/ pathname))) *storage*))
         (new (make-instance 'filter
                             :key key
                             :parent parent
                             :name (cdr (assoc :name raw))
                             :func (eval (read-from-string (cdr (assoc :func raw))))
                             )))
    (when (equal 'group (type-of parent))
      (setf (filters parent)
            (remove-duplicates (append (filters parent) (list new)))))
    (setf (gethash key *storage*) new)
    key
    ))


;; OPTGROUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod unserialize (in-list (dummy optgroup))
  (let* ((name (nth 1 in-list))
         (options (nth 2 in-list)))
    (make-instance 'optgroup
                   :name (cdr (assoc :name in-list))
                   :options (loop
                               :for option-elt
                               :in (cdr (assoc :options in-list))
                               :collect (unserialize option-elt (make-instance 'option))))))



(defmethod serialize ((object optgroup))
  (format nil "~%      {~%         \"name\": \"~a\",~%         \"options\": [~{~a~^,~}~%         ]~%      }"
          (stripper (name object))
          (mapcar #'(lambda (option)
                      (serialize option))
                  (options object))
          ))


;; OPTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod unserialize (in-list (dummy option))
  (make-instance 'option
                 :name (cdr (assoc :name in-list))
                 :value (format nil "~a" (cdr (assoc :value in-list)))
                 :optype (cdr (assoc :optype in-list))
                 :boolflag (cdr (assoc :boolflag in-list))))


(defmethod serialize ((object option))
  (format nil "~%            {~%               \"name\": \"~a\",~%               \"value\": \"~a\",~%               \"optype\": ~a,~%               \"boolflag\": ~a~%            }"
          (stripper (name object))
          (stripper (value object))
          (encode-json-to-string (optype object))
          (encode-json-to-string (boolflag object))
          ))
