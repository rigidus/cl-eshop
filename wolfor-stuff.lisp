(in-package #:wolfor-stuff)

(defun range-filter (product request-get-plist opt-label opt-group-name opt-name)
  (let ((value-f (getf request-get-plist (read-from-string (format nil ":~a-f" opt-label))))
        (value-t (getf request-get-plist (read-from-string (format nil ":~a-t" opt-label))))
        (product-value 0))
     (mapcar #'(lambda (option)
                (if (string= (optgroup::name option) opt-group-name)
                    (let ((options (optgroup::options option)))
                      (mapcar #'(lambda (opt)
                                  (if (string= (option::name opt) opt-name)
                                      (setf product-value
                                             (format nil "~a" (option::value opt)))))
                              options))))
            (optlist:optlist (product:options product)))
    (if (null product-value)
      (setf product-value "0"))
    (if (null value-f)
      (setf value-f "0"))
    (if (null value-t)
      (setf value-t "99999999"))
    (setf value-f (arnesi:parse-float (format nil "~as" value-f)))
    (setf value-t (arnesi:parse-float (format nil "~as" value-t)))
    (setf product-value (arnesi:parse-float (format nil "~as" product-value)))
    (if (and (<= value-f product-value)
         (>= value-t product-value))
          (print (list value-f value-t product-value opt-label opt-group-name opt-name)))
   ;; (arnesi:parse-float "")
    (and (<= value-f product-value)
         (>= value-t product-value))))

(defun checkbox-filter (p g) t)

(defun price-filter (product request-get-plist)
  (LET ((PRICE-F
         (PARSE-INTEGER
          (GETF REQUEST-GET-PLIST :PRICE-F)
          :JUNK-ALLOWED T))
        (PRICE-T
         (PARSE-INTEGER
          (GETF REQUEST-GET-PLIST :PRICE-T)
          :JUNK-ALLOWED T))
        (PRODUCT-PRICE
         (PRODUCT:PRICE PRODUCT)))
    (UNLESS (INTEGERP PRICE-F)
      (SETF PRICE-F 0))
    (UNLESS (INTEGERP PRICE-T)
      (SETF PRICE-T 999999999999))
    (PRINT (LIST PRICE-F PRICE-T))
    (AND
     (<= PRICE-F PRODUCT-PRICE)
     (>= PRICE-T
         PRODUCT-PRICE))))

(defun radio-filter (p g opts og o) t)
