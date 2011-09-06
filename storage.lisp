(in-package #:eshop)

(defclass global-storage ()
    ((storage :initarg :storage :initform (make-hash-table :test #'equal) :accessor storage)
     (product-storage :initarg :product-storage :initform (make-hash-table :test #'equal) :accessor product-storage)
     (group-storage :initarg :group-storage :initform (make-hash-table :test #'equal) :accessor group-storage)
     (active-products :initarg :active-products :initform (make-hash-table :test #'equal) :accessor active-products)
     (root-groups :initarg :root-groups :initform (make-hash-table :test #'equal) :accessor root-groups)
     (prev-valid-key :initarg :prev-valid-key :initform 0 :accessor prev-valid-key)))


(defvar *global-storage* (make-instance 'global-storage))
(setf (storage *global-storage*) *storage*)
(setf (root-groups *global-storage*) (storage.get-root-groups-list))
(setf (active-products *global-storage*) (storage.get-active-products-list))


;;получение всех дочерних групп
(defun storage.get-group-children (group)
  (childs group))

;;обход storage и составление списка в соответствии с функцией checker. Сортировка с переданным компаратором
(defun storage.round-collect-storage (checker &optional (compare t compare-supplied-p))
  (let ((result))
    (maphash #'(lambda (key node)
                 (declare (ignore key))
                 (when (funcall checker node)
                   (push node result)))
             (storage *global-storage*))
    (if compare-supplied-p
        (stable-sort (copy-list result) compare)
        result)))

;;получение списка активных продуктов
(defun storage.get-active-products-list ()
  (storage.round-collect-storage #'(lambda (obj) (and (equal (type-of obj) 'product) (active obj)))))


;;получение списка корневых групп
(defun storage.get-root-groups-list (&optional (compare #'(lambda (a b)
                                                            (if (or (null (order a)) (null (order b)))
                                                                nil
                                                                (< (order a) (order b))))))
  (storage.round-collect-storage #'(lambda (obj)
                                     (and (equal (type-of obj) 'group)
                                          (null (parent obj))))
                                 compare))




;;генерация следующего уникального ключа в storage
(defun storage.gen-next-valid-key ()
  (let ((key (prev-valid-key *global-storage*)))
    (loop
       :while (gethash key (storage *global-storage*))
       :do (incf key))
    (setf (prev-valid-key *global-storage*) key)
    key))


;;добавление нового элемента в storage
(defun storage.add-new (object)
  (let ((key (storage.gen-next-valid-key)))
    (setf (gethash key (storage *global-storage*)) object)
    (log5:log-for test "Added new element with key ~a" key)
    key))


