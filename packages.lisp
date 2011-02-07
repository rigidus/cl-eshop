(defpackage #:eshop
  (:use #:cl
        #:cl-user
        #:json
        #:split-sequence
        #:cl-fad)
  (:export :compile-templates
		   :*storage*
           :name
           :unserialize
           :plist-representation))


(defpackage #:wolfor-stuff
  (:use #:cl)
  (:export :range-filter
           :checkbox-filter
           :price-filter
           :radio-filter
           ))
