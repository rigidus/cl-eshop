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


(defpackage #:eshop-test
  (:use #:cl
        #:eshop
        #:wolfor-stuff))
