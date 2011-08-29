(in-package #:eshop)


(defun new-classes.view-string-field (value name disabled)
  (format nil "<p style=\"margin-top: 0px; margin-bottom: 0px;\">
                   ~a
                </p><input type=\"text\" name=\"~a\" ~a
                value=\"~a\" /><br/><br/>"
          name name (if disabled
                        "disabled=\"disabled\""
                        "")
          value))

(defun new-classes.string-field-get-data (string)
  string)

(defun new-classes.view-int-field (value name disabled)
  (new-classes.view-string-field (format nil "~a" value) name disabled))

(defun new-classes.int-field-get-data (string)
  (parse-integer string))

(defun new-classes.view-bool-field (value name disabled)
  (soy.class_forms:bool-field
   (list :name name :checked value :disabled disabled)))


(defun new-classes.bool-field-get-data (string)
  (string= string "T"))

(defun new-classes.view-group-field (value name disabled)
  (let ((leveled-groups (storage.get-groups-leveled-tree))
        (indent "--------------------------------------"))
    (soy.class_forms:group-form
     (list :name name :disabled disabled
           :grouplist (mapcar #'(lambda (group-and-level)
                                  (let ((group (car group-and-level))
                                        (level (cdr group-and-level)))
                                    (list :hashkey (key group)
                                          :selected (eq value group)
                                          :name (name group)
                                          :indent (subseq indent 0 (* 3 level)))))
                              leveled-groups)))))

(defun new-classes.group-field-get-data (string)
  (gethash string (storage *global-storage*)))






(defmacro new-classes.make-class (name class-fields)
  `(defclass ,name ()
     ,(mapcar #'(lambda (field)
                  `(,(getf field :name)
                     :initarg ,(getf field :initarg)
                     :initform ,(getf field :initform)
                     :accessor ,(getf field :accessor)))
              class-fields)))

(defmacro new-classes.make-view-method (name class-fields)
  `(defmethod new-classes.make-fields ((object ,name))
     ,(cons
       `list
       (mapcar #'(lambda (field)
                   `(,(intern (string-upcase
                               (format nil "new-classes.view-~a-field" (getf field :type))))
                                      (,(getf field :name)  object)
                                      ,(format nil "~a" (getf field :name))
                                      ,(getf field :disabled)))
               class-fields))))

(defmacro new-classes.make-edit-method (name class-fields)
  `(defmethod new-classes.edit-fields ((object ,name) post-data-plist)
     ,(cons
       `progn
       (mapcar #'(lambda (field)
                   (when (not (getf field :disabled))
                     `(setf (,(getf field :name) object)
                            (,(intern (string-upcase
                                       (format nil "new-classes.~a-field-get-data" (getf field :type))))
                              (decode-uri (getf post-data-plist ,(intern (string-upcase (format nil "~a" (getf field :name))) :keyword)))))))
               class-fields))))


(defun new-classes.make-class-and-methods (name list-fields)
  (eval `(new-classes.make-class ,name ,list-fields))
  (eval `(new-classes.make-view-method ,name ,list-fields))
  (eval `(new-classes.make-edit-method ,name ,list-fields)))

