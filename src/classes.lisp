;;;; classes.lisp

(in-package #:eshop)

(defclass group ()
  ((key               :initarg :key             :initform nil       :accessor key)
   (parent            :initarg :parent          :initform nil       :accessor parent)
   (name              :initarg :name            :initform nil       :accessor name)
   (active            :initarg :active          :initform nil       :accessor active)
   (empty             :initarg :empty           :initform nil       :accessor empty)
   (order             :initarg :order           :initform nil       :accessor order)
   (keyoptions        :initarg :keyoptions      :initform nil       :accessor keyoptions)
   (ymlshow           :initarg :ymlshow         :initform nil       :accessor ymlshow)
   (pic               :initarg :pic             :initform nil       :accessor pic)
   (delivery-price    :initarg :delivery-price  :initform nil       :accessor delivery-price)
   (icon              :initarg :icon            :initform nil       :accessor icon)
   (childs            :initarg :childs          :initform nil       :accessor childs)
   (filters           :initarg :filters         :initform nil       :accessor filters)
   (fullfilter        :initarg :fullfilter      :initform nil       :accessor fullfilter)
   (products          :initarg :products        :initform nil       :accessor products)
   (vendors           :initarg :vendors         :initform (make-hash-table :test #'equal) :accessor vendors)
   (descr             :initarg :descr           :initform nil       :accessor descr)
   (raw-fullfilter    :initarg :raw-fullfilter  :initform ""        :accessor raw-fullfilter)
   ))


(defclass product ()
  ((articul           :initarg :articul         :initform nil                          :accessor articul)
   (parent            :initarg :parent          :initform nil                          :accessor parent)
   (key               :initarg :key             :initform ""                           :accessor key)
   (name              :initarg :name            :initform ""                           :accessor name)
   (realname          :initarg :realname        :initform ""                           :accessor realname)
   (price             :initarg :price           :initform 0                            :accessor price)
   (siteprice         :initarg :siteprice       :initform 0                            :accessor siteprice)
   (delivery-price    :initarg :delivery-price  :initform nil                          :accessor delivery-price)
   (bonuscount        :initarg :bonuscount      :initform 0                            :accessor bonuscount)
   (date-modified     :initarg :date-modified   :initform (get-universal-time)         :accessor date-modified)
   (date-created      :initarg :date-created    :initform (get-universal-time)         :accessor date-created)
   (active            :initarg :active          :initform t                            :accessor active)
   (predzakaz         :initarg :predzakaz       :initform nil                          :accessor predzakaz)
   (newbie            :initarg :newbie          :initform t                            :accessor newbie)
   (sale              :initarg :sale            :initform t                            :accessor sale)
   (descr             :initarg :descr           :initform ""                           :accessor descr)
   (shortdescr        :initarg :shortdescr      :initform ""                           :accessor shortdescr)
   (count-transit     :initarg :count-transit   :initform 0                            :accessor count-transit)
   (count-total       :initarg :count-total     :initform 0                            :accessor count-total)
   (optgroups         :initarg :optgroups       :initform nil                          :accessor optgroups)))


(defclass optgroup ()
  ((name              :initarg :name            :initform ""        :accessor name)
   (options           :initarg :options         :initform nil       :accessor options)))


(defclass option ()
  ((name              :initarg :name            :initform ""        :accessor name)
   (value             :initarg :value           :initform ""        :accessor value)
   (optype            :initarg :optype          :initform nil       :accessor optype)
   (boolflag          :initarg :boolflag        :initform nil       :accessor boolflag)))


(defclass group-filter ()
  ((name              :initarg :name            :initform nil       :accessor name)
   (base              :initarg :base            :initform nil       :accessor base)
   (advanced          :initarg :advanced        :initform nil       :accessor advanced)))


(defclass filter ()
  ((key               :initarg :key             :initform nil       :accessor key)
   (parent            :initarg :parent          :initform nil       :accessor parent)
   (name              :initarg :name            :initform nil       :accessor name)
   (func              :initarg :func            :initform nil       :accessor func)
   (func-string       :initarg :func-string     :initform nil       :accessor func-string)))


(defclass producers ()
  ((producers         :initarg :producers         :initform nil       :accessor producers)
   (producersall      :initarg :producersall      :initform nil       :accessor producersall)))


(defmacro make-integer-writer (field)
  `(defmethod (setf ,field) (value (item product))
     (with-slots (,field) item
       (handler-case
           (cond ((typep value 'integer) (setf (slot-value item ',field) value))
                 ((typep value 'string)  (setf (slot-value item ',field)
                                               (handler-case
                                                   (parse-integer value)
                                                 (SB-INT:SIMPLE-PARSE-ERROR ()
                                                   (error 'WRONG-TYPE-OF-SLOT)))))
                 (t (error 'WRONG-TYPE-OF-SLOT )))
         (WRONG-TYPE-OF-SLOT ()
           (restart-case
               (error 'WRONG-PRODUCT-SLOT-VALUE
                      :text (symbol-name ',field)
                      :value value
                      :product item)
             (enter-correct-value (new)
               :report "Enter a new value"
               :interactive read-new-integer-value
               (setf (,field item) new))
             (ignore ()
               :report "Ignore error, save current value"
               (setf (slot-value item ',field) value))
             (set-null ()
               :report "Set value as NIL"
               (setf (slot-value item ',field) nil))
             ))))))


(make-integer-writer count-transit)
(make-integer-writer count-total)


(defmacro make-boolean-writer (field)
  `(defmethod (setf ,field) (value (item product))
     (with-slots (,field) item
       (handler-case
           (cond ((typep value 'boolean) (setf (slot-value item ',field) value))
                 (t (error 'WRONG-TYPE-OF-SLOT )))
         (WRONG-TYPE-OF-SLOT ()
           (restart-case
               (error 'wrong-product-slot-value
                      :text (symbol-name ',field)
                      :value value
                      :product item)
             (ignore ()
               :report "Ignore error, save current value"
               (setf (slot-value item ',field) value))
             (set-null ()
               :report "Set value as NIL"
               (setf (slot-value item ',field) nil))
             (set-t ()
               :report "Set value as T"
               (setf (slot-value item ',field) t))
             ))))))


(make-boolean-writer active)
(make-boolean-writer newbie)
(make-boolean-writer sale)
(make-boolean-writer presence)


(defmethod initialize-instance :after ((item product) &key)
  (with-slots ((articul articul)
               (count-transit count-transit)
               (count-total count-total))
      item
    (if (not (date-modified item))
        (setf (date-modified item) (get-universal-time)))
    (if (not (date-created item))
        (setf (date-created item) (get-universal-time)))
    (unless (typep articul 'integer)       (setf (articul item) articul))
    (unless (typep count-transit 'integer) (setf (count-transit item) count-transit))
    (unless (typep count-total 'integer)   (setf (count-total item) count-total))))


;; PRODUCERS -----------------------------------------------------------------------------------


(defmethod initialize-instance :after ((item producers) &key)
  (with-slots ((producers producers))
      item
    (setf (producers item) (sort producers #'(lambda (a b)
                                               (> (cadr a) (cadr b)))))))


(defmethod get-recursive-products ((object group))
  (let ((products (products object)))
    (loop :for child :in (childs object) :do
       (setf products (append products (get-recursive-products child))))
    products))


(defmethod get-list-of-producers ((object group) &optional (filter #'(lambda (product) (active product))))
  (let ((producers (make-hash-table :test #'equal)))
    (loop :for product :in (remove-if-not filter (get-recursive-products object)) :do
       (mapcar #'(lambda (optgroup)
                   (if (string= (name optgroup) "Общие характеристики")
                       (let ((options (options optgroup)))
                         (mapcar #'(lambda (opt)
                                     (if (string= (name opt) "Производитель")
                                         (let ((val (value opt)))
                                           (if (null (gethash val producers))
                                               (setf (gethash val producers) 1)
                                               (incf (gethash val producers))))))
                                 options))))
               (optgroups product)))
    (let ((producer-list))
      (maphash #'(lambda (key val)
                   (push (list key val) producer-list))
               producers)
      producer-list)))

;; (get-list-of-producers (gethash "noutbuki-i-netbuki" *storage*))


(defmethod make-producers ((object group))
  (make-instance 'producers
                 :producers (get-list-of-producers object)
                 :producersall (get-list-of-producers object  #'(lambda (product) t))
                 ))


;; -----------------------------------------------------------------------------------------
