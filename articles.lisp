(in-package #:eshop)

(defclass article ()
  ((key               :initarg :key       :initform nil       :accessor key)
   (body              :initarg :body      :initform nil       :accessor body)
   (date              :initarg :date      :initform nil       :accessor date)
   (tag               :initarg :tag       :initform (make-hash-table :test #'equal) :accessor tag)
   ))

(defmethod unserialize (filepath (dummy article))
    (let* ((file-content (alexandria:read-file-into-string filepath))
           (raw (decode-json-from-string file-content))
           (key (pathname-name filepath))
           (body (cdr (assoc :body raw)))
           (name (cdr (assoc :date raw)))
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
