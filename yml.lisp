(in-package #:yml)

(defun get-date-time ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second))
    (format nil
            "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
            year
            month
            date
            hour
            minute)))

(funcall cl-user:*dispatcher*
         `((string= "/yml" (service:request-str))
           ,#'(lambda ()
                (setf (hunchentoot:content-type*) "application/xml; charset=utf-8")
                (yml:xml (list :datetime (get-date-time)
                               :marketname "ЦиFры 320-8080"
                               :marketcompany "ЦиFры 320-8080"
                               :marketurl "http://www.320-8080.ru/"
                               :categoryes
                               (append
                                (list
                                 (list :id 9999999
                                       :name "root"
                                       :parent 0))
                                (loop
                                   :for group
                                   :being :the hash-values
                                   :in trans:*group*
                                   :when (and (equal (type-of group) 'group:group)
                                              (group:ymlshow group))
                                   :collect (list :id (group:id group)
                                                  :name (group:name group)
                                                  :parent (if (null (group:parent group))
                                                              9999999
                                                              (group:id (group:parent group))))))

                               :offers (format nil "~{~a~}"
                               (loop
                                  ;; :repeat 50
                                  :for product
                                  :being :the hash-values
                                  :in trans:*product*
                                  :when (and (equal (type-of (product:parent product)) 'group:group)
                                             (group:ymlshow (product:parent product)))
                                  :collect (yml:offer (list :articul (product:articul product)
                                                            :price (product:price product)
                                                            :category (group:id (product:parent product))
                                                            :picture  (let ((pics (product:get-pics product)))
                                                                        (if (null pics) nil (car pics)))
                                                            :name (product:name product)
                                                            :description (product:descr product)
                                                            )))))))))
