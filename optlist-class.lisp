(in-package #:optlist)


;; Класс OPTLIST (список оптрупп)
(defclass OPTLIST ()
  ((optlist        :initarg :optlist         :initform nil       :accessor optlist)))

(defmethod show ((object optlist))
  (product:optlist (list :optgroups (mapcar #'(lambda (optgroup)
                                                (optgroup:show optgroup))
                                            (optlist object)))))

(defmethod serialize ((object optlist))
  (format nil " {~%      \"optlist\": {~{~a~^,~}~%      }~%   }"
          (mapcar #'(lambda (optgroup)
                      (optgroup:serialize optgroup))
                  (optlist object))))


(defun unserialize (in)
  (make-instance 'optlist
                 :optlist (loop :for optgroup :in (cdr (assoc :optlist in)) :collect
                             (optgroup:unserialize optgroup))))
