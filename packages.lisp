(defpackage #:group
  (:use #:cl
		#:cl-user
        #:json
        #:split-sequence)
  (:export #:group
           #:parent
           #:plist-representation
           #:id
           #:key
           #:name
           #:keyoptions
           #:childs
           #:show
           #:products
           #:ymlshow
           #:filters
           #:fullfilter
           #:order
           #:get-recursive-products
           #:dispatcher
           #:serialize
           #:unserialize
           #:make-vendor-filter))

(defpackage #:service
  (:use #:cl
        #:cl-user
        #:hunchentoot)
  (:export :paginator
           :menu
           :menu-sort
           :breadcrumbs
           :default-page
           :checkout-page
           :main-page
           :static-page
           :make-get-str
           :request-str
           :request-list
           :request-get-plist))
