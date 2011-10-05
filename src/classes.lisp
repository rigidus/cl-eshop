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


;; (new-classes.make-class-and-methods
;;  'product
;;  '((:name key               :initform ""                     :disabled t     :type string      :serialize t)
;;    (:name articul           :initform nil                    :disabled t     :type int         :serialize t)
;;    (:name name-provider     :initform ""                     :disabled nil   :type string      :serialize t)
;;    (:name name-seo          :initform ""                     :disabled nil   :type string      :serialize t)
;;    (:name siteprice         :initform 0                      :disabled nil   :type int         :serialize t)
;;    (:name delta-price       :initform 0                      :disabled nil   :type int         :serialize t)
;;    (:name bonuscount        :initform 0                      :disabled nil   :type int         :serialize t)
;;    (:name delivery-price    :initform 0                      :disabled nil   :type int         :serialize t)
;;    (:name active            :initform t                      :disabled nil   :type bool        :serialize nil)
;;    (:name preorder          :initform nil                    :disabled nil   :type bool        :serialize t)
;;    (:name newbie            :initform t                      :disabled nil   :type bool        :serialize t)
;;    (:name sale              :initform t                      :disabled nil   :type bool        :serialize t)
;;    (:name parents           :initform nil                    :disabled nil   :type group-list  :serialize t)
;;    (:name date-modified     :initform (get-universal-time)   :disabled t     :type time        :serialize t)
;;    (:name date-created      :initform (get-universal-time)   :disabled t     :type time        :serialize t)
;;    (:name seo-text          :initform ""                     :disabled nil   :type textedit    :serialize t)
;;    (:name count-transit     :initform 0                      :disabled t     :type int         :serialize t)
;;    (:name count-total       :initform 0                      :disabled t     :type int         :serialize t)
;;    (:name optgroups         :initform nil                    :disabled t     :type optgroups   :serialize t)))




;; (new-classes.make-class-and-methods
;;  'group
;;  '((:name key               :initform nil                             :disabled t   :type string       :serialize t)
;;    (:name parents           :initform nil                             :disabled nil :type group-list   :serialize t)
;;    (:name name              :initform nil                             :disabled nil :type string       :serialize t)
;;    (:name active            :initform nil                             :disabled nil :type bool         :serialize t)
;;    (:name empty             :initform nil                             :disabled t   :type bool         :serialize nil)
;;    (:name order             :initform 1000                            :disabled nil :type int          :serialize t)
;;    (:name ymlshow           :initform nil                             :disabled t   :type bool         :serialize t)
;;    (:name pic               :initform nil                             :disabled nil :type string       :serialize t)
;;    (:name icon              :initform nil                             :disabled nil :type string       :serialize t)
;;    (:name delivery-price    :initdorm 0                               :disabled nil :type int          :serialize t)
;;    (:name groups            :initform nil                             :disabled t   :type group-list   :serialize t)
;;    (:name products          :initform nil                             :disabled t   :type product-list :serialize nil)
;;    (:name filters           :initform nil                             :disabled t   :type string       :serialize nil)
;;    (:name fullfilter        :initform nil                             :disabled t   :type string       :serialize nil)
;;    (:name vendors           :initform (make-hash-table :test #'equal) :disabled t   :type string       :serialize nil)
;;    (:name seo-text          :initform nil                             :disabled nil :type textedit     :serialize t)
;;    (:name keyoptions        :initform nil                             :disabled t   :type keyoptions   :serialize t)))


;; (new-classes.make-class-and-methods
;;  'filter
;;  '((:name key               :initform ""       :disabled t    :type string)
;;    (:name parents           :initform nil      :disabled t    :type group-list)
;;    (:name name              :initform ""       :disabled nil  :type string)
;;    (:name func              :initform ""       :disabled t    :type string)
;;    (:name func-string       :initform ""       :disabled t    :type textedit)))

;; (new-classes.make-class-and-methods
;;  'optgroup
;;  '((:name name              :initarg :name            :initform ""        :accessor name :disabled nil :type string)
;;    (:name key               :initarg :key             :initform ""                           :accessor key :disabled t :type string)
;;    (:name options           :initarg :options         :initform nil       :accessor options :disabled t :type string)))

;; (new-classes.make-class-and-methods
;;  'option
;;  '((:name name              :initarg :name            :initform ""        :accessor name :disabled nil :type string)
;;    (:name key               :initarg :key             :initform ""                           :accessor key :disabled t :type string)
;;    (:name value             :initarg :value           :initform ""        :accessor value :disabled nil :type string)
;;    (:name optype            :initarg :optype          :initform nil       :accessor optype :disabled t :type string)
;;    (:name boolflag          :initarg :boolflag        :initform nil       :accessor boolflag :disabled nil :type bool)))

;; (new-classes.make-class-and-methods
;;  'group-filter
;;  '((:name name              :initarg :name            :initform nil       :accessor name :disabled nil :type string)
;;    (:name key               :initarg :key             :initform ""                           :accessor key :disabled t :type string)
;;    (:name base              :initarg :base            :initform nil       :accessor base :disabled t :type string)
;;    (:name advanced          :initarg :advanced        :initform nil       :accessor advanced :disabled t :type string)))





;; (new-classes.make-class-and-methods
;;  'producers
;;  '((:name producers         :initarg :producers         :initform nil       :accessor producers :disabled t :type string)
;;    (:name key               :initarg :key             :initform ""          :accessor key :disabled t :type string)
;;    (:name producersall      :initarg :producersall      :initform nil       :accessor producersall :disabled t :type string)))


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
