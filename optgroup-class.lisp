(in-package #:optgroup)


;; Класс OPTGROUP (список опций)
(defclass optgroup ()
  ((name           :initarg :name            :initform ""        :accessor name)
   (options        :initarg :options         :initform nil       :accessor options)))

(defmethod show ((object optgroup))
  (product:optgroup (list :name (name object)
                          :options (mapcar #'(lambda (option)
                                               (option:show option))
                                           (options object)))))

(defmethod serialize ((object optgroup))
  (format nil "~%         \"optgroup\" : {~%            \"name\": \"~a\",~%            \"options\": [~{~a~^,~}~%            ]~%         }"
          (stripper (name object))
          (mapcar #'(lambda (option)
                      (option:serialize option))
                  (options object))
          ))


(defun unserialize (in)
  (let ((name  (nth 1 in))
        (options (nth 2 in)))
    (pop options)
    (make-instance 'optgroup
                   :name (cdr name)
                   :options (loop :for option :in options :collect (option:unserialize option)))))
