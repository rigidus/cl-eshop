(in-package #:eshop)

(defclass global-storage ()
    ((storage :initarg :storage :initform (make-hash-table :test #'equal) :accessor storage)
     (product-storage :initarg :product-storage :initform (make-hash-table :test #'equal) :accessor product-storage)
     (group-storage :initarg :group-storage :initform (make-hash-table :test #'equal) :accessor group-storage)
     (active-products :initarg :active-products :initform (make-hash-table :test #'equal) :accessor active-products)
     (root-groups :initarg :root-groups :initform (make-hash-table :test #'equal) :accessor root-groups)))


(defvar *global-storage* (make-instance 'global-storage))
(setf (storage *global-storage*) *storage*)
(setf (root-groups *global-storage*) (storage.get-root-groups-list))

(defun storage.get-root-groups-list (&optional (compare #'(lambda (a b)
                                                            (if (or (null (order a)) (null (order b)))
                                                                nil
                                                                (< (order a) (order b))))))
  (let ((result))
    (maphash #'(lambda (key node)
                 (declare (ignore key))
                 (if (and (equal (type-of node) 'group)
                          (null (parent node)))
                     (push node result)))
             (storage *global-storage*))
    (stable-sort result compare)))

(defun storage.get-groups-leveled-tree (&optional (compare #'(lambda (a b)
                                                               (if (or (null (order a)) (null (order b)))
                                                                   nil
                                                                   (< (order a) (order b))))))
  (let ((roots (stable-sort (copy-list (root-groups *global-storage*)) compare))
          (result))
    (mapcar #'(lambda (root)
                (setf result
                      (append result
                              (storage.get-leveled-branch root 0))))
            roots)
    result))

(defun storage.get-leveled-branch (node level &optional (compare #'(lambda (a b)
                                                                     (if (or (null (order a)) (null (order b)))
                                                                         nil
                                                                         (< (order a) (order b))))))
  (let ((children (stable-sort (copy-list (childs node)) compare)) (result (list (cons node level))))
    (mapcar #'(lambda (child)
                (setf result
                      (append result
                              (storage.get-leveled-branch child (+ 1 level)))))
            children)
    result))

