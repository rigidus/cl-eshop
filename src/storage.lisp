(in-package #:eshop)

(defclass global-storage ()
    ((storage :initarg :storage :initform (make-hash-table :test #'equal) :accessor storage)
     (products :initarg :products :initform nil :accessor products)
     (groups :initarg :groups :initform nil :accessor groups)
     (filters :initarg :filters :initform nil :accessor filters)
     (actual-groups :initarg :actual-groups :initform nil :accessor actual-groups)
     (active-products :initarg :active-products :initform nil :accessor active-products)
     (root-groups :initarg :root-groups :initform nil :accessor root-groups)))


(defvar *global-storage* (make-instance 'global-storage))

;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defun storage.get-main-product-parent (product)
  (car (parents product)))

(defmethod storage.main-parent ((object group))
  (car (parents object)))
;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defun storage.round-collect-storage (checker &optional (compare t compare-supplied-p))
  "Processing storage and creating list according to checker function. Sorting with passed comparator"
  (let ((result))
    (maphash #'(lambda (key node)
                 (declare (ignore key))
                 (when (funcall checker node)
                   (push node result)))
             (storage *global-storage*))
    (if compare-supplied-p
        (stable-sort (copy-list result) compare)
        result)))


(defun storage.get-products-list ()
  (storage.round-collect-storage #'(lambda (obj) (equal (type-of obj) 'product))))

(defun storage.get-active-products-list ()
  (storage.round-collect-storage #'(lambda (obj) (and (equal (type-of obj) 'product) (active obj)))))

(defun storage.get-groups-list ()
  (storage.round-collect-storage #'(lambda (obj) (equal (type-of obj) 'group))))

(defun storage.get-filters-list ()
  (storage.round-collect-storage #'(lambda (obj) (equal (type-of obj) 'filter))))

(defun storage.get-actual-groups-list ()
  (storage.round-collect-storage #'(lambda (obj) (and (equal (type-of obj) 'group) (not (empty obj)) (active obj)))))



(defun storage.get-root-groups-list (&optional (compare #'(lambda (a b)
                                                            (if (or (null (order a)) (null (order b)))
                                                                nil
                                                                (< (order a) (order b))))))
  (storage.round-collect-storage #'(lambda (obj)
                                     (and (equal (type-of obj) 'group)
                                          (null (parents obj))))
                                 compare))




(defun storage.add-new-object (object &optional (key nil key-supplied-p))
  "Adding exactly new object to storage but not pushing it in any list"
  (when (not key-supplied-p)
    (setf key (key object)))
  (setf (gethash key (storage *global-storage*)) object))
  ;; (log5:log-for test "Added new element with key ~a" key))

(defun storage.edit-in-list (list object &optional (key nil key-supplied-p))
  "Editing or adding (if not exist) object in given list"
  (when (not key-supplied-p)
    (setf key (key object)))
  (let* ((found nil)
         (result-list (mapcar #'(lambda (list-obj)
                                  (if (equal (key list-obj) key)
                                      (progn
                                        (setf found t)
                                        object)
                                      list-obj))
                              list)))
    (if (null found)
        (push object list)
        result-list)))


(defun storage.edit-object (object &optional (key nil key-supplied-p))
  "Editing or adding object to storage and edit it in appropriate lists"
  (when (not key-supplied-p)
    (setf key (key object)))
  (setf (gethash key (storage *global-storage*)) object)
  (when (equal (type-of object) 'product)
    (setf (products *global-storage*) (storage.edit-in-list (products *global-storage*) object key))
    (when (active object)
      (setf (active-products *global-storage*) (storage.edit-in-list (active-products *global-storage*) object key))))
  (when (equal (type-of object) 'group)
    (setf (groups *global-storage*) (storage.edit-in-list (groups *global-storage*) object key))
    (when (and (active object) (not (empty object)))
      (setf (actual-groups *global-storage*) (storage.edit-in-list (actual-groups *global-storage*) object key)))
    (when (null (parent object))
      (setf (root-groups *global-storage*) (storage.edit-in-list (root-groups *global-storage*) object key))))
  (when (equal (type-of object) 'filter)
    (setf (filters *global-storage*) (storage.edit-in-list (filters *global-storage*) object key))))


(defun storage.make-lists ()
  (setf (groups *global-storage*) (storage.get-groups-list))
  (setf (actual-groups *global-storage*) (storage.get-actual-groups-list))
  (setf (root-groups *global-storage*) (storage.get-root-groups-list))
  (setf (products *global-storage*) (storage.get-products-list))
  (setf (active-products *global-storage*) (storage.get-active-products-list))
  (setf (filters *global-storage*) (storage.get-filters-list)))


;; (setf (storage *global-storage*) *storage*)

