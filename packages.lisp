(defpackage #:my
  (:use #:cl
        #:cl-user)
  (:export :setvar
           :sql-start
		   :sql-query
		   :sql-rows
		   :to-hash
		   :hash-elt
		   :foreach
		   :iterate
		   :cross
		   :parse-id
		   :strip
           :stripper
		   :get-file-length
		   :get-file-content
		   :replace-all
		   :merge-plists
           :reverse-plist
           :get-date-time
           :numerizable
           :slice
           :cut))


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



(defpackage #:option
  (:use #:cl
		#:cl-user
        #:json
        #:my)
  (:export #:option
           #:show
           #:name
           #:value
           #:optype
           #:boolflag
           #:serialize
           #:unserialize))


(defpackage #:optgroup
  (:use #:cl
		#:cl-user
        #:json
        #:my)
  (:export #:optgroup
           #:show
           #:serialize
           #:name
           #:options
           #:unserialize))


(defpackage #:optlist
  (:use #:cl
		#:cl-user)
  (:export #:optlist
           #:show
           #:serialize
           #:unserialize))


(defpackage #:product
  (:use #:cl
		#:cl-user
        #:json
        #:my
        #:split-sequence)
  (:export #:product
           #:articul
           #:parent
           #:name
           #:realname
           #:price
           #:siteprice
           #:ekkprice
           #:active
           #:newbie
           #:sale
           #:descr
           #:shortdescr
           #:count-transit
           #:count-total
           #:options
           #:plist-representation
           ;; Отображение
           #:get-pics
           #:show
           #:view
           #:serialize
           #:unserialize))


(defpackage #:filter
  (:use #:cl
		#:cl-user
        #:json
        #:split-sequence
        #:alexandria)
  (:export #:filter
           #:parent
           #:func
           #:key
           #:name
           #:plist-representation
           #:show
           #:serialize
           #:unserialize))


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
           #:active
           #:empty
           #:ymlshow
           #:filters
           #:fullfilter
           #:order
           #:get-recursive-products
           #:dispatcher
           #:serialize
           #:unserialize
           #:make-vendor-filter))


(defpackage #:xls
  (:use #:cl
        #:cl-user
        #:split-sequence
		#:cl-ppcre)
  (:export #:parse-xls))


(defpackage #:trans
  (:use #:cl
		#:cl-user
        #:split-sequence
        #:cl-fad)
  (:export #:*product*
           #:*group*
           #:*filter*
           #:restore-from-files
           #:store-unlinked-products
           ))


(defpackage #:cart
  (:use #:cl
		#:cl-user
        #:hunchentoot)
  (:export #:dispatcher))


(defpackage #:checkout
  (:use #:cl
		#:cl-user
        #:hunchentoot)
  (:export #:dispatcher0
           #:dispatcher1
           #:dispatcher2
           #:dispatcher3
           #:dispatcher4
           ))


(defpackage #:gateway
  (:use #:cl
        #:cl-user)
  (:export :dispatcher
           :load-from-conf))


(defpackage #:search
  (:use #:cl
		#:cl-user
        #:split-sequence)
  (:export #:dispatcher))


(defpackage #:html
  (:use #:cl
		#:cl-user)
  (:export #:select))


(defpackage #:conditions
  (:use #:cl
		#:cl-user)
  (:export :c-test
		   :c-equal
		   :c-notequal
		   :c-contains
		   :c-and-codelen5-equal
		   :c-and-oe-vendor-contains
		   :c-equal-length
		   :c-and-febi-vendor-firstsym
		   :c-and-kyb-vendor-firstsym
		   :c-and-ruville-vendor-firstsym
		   :c-and-nk-vendor-firstsym
		   :c-and-dph-vendor-firstsym
		   :c-and-suplex-vendor-firstsym
		   :c-and-intermotor-vendor-firstsym
		   :c-and-pilenga-vendor-firstsym))


(defpackage #:actions
  (:use #:cl
		#:cl-user)
  (:export :a-test
		   :a-head-substr
		   :a-str-append
		   :a-toeuro
		   :a-copy
		   :a-cross-avangard
		   :a-cross-alessio
		   :a-trimspace
		   :a-numerizable
		   :a-replace
		   :a-skip
		   :a-skidka
		   :a-natsenka
		   ))


(defpackage #:data
  (:use #:cl
		#:cl-user)
  (:export #:*options*
           #:*typefile*
           #:*agents*
           #:*conditio*
           #:*actions*
           #:make-example-rule
           #:make-example-agent))


(defpackage #:rule
  (:use #:cl
		#:cl-user)
  (:export #:rule
           #:show
           #:get-lambda)
  )


(defpackage #:agent
  (:use #:cl
		#:cl-user
        #:split-sequence
		#:cl-ppcre
		#:hunchentoot
        ;; #:rule
        )
  (:export #:agent
           #:name
           #:rules
           #:controller)
  )


(defpackage #:update
  (:use #:cl
        #:hunchentoot
		#:cl-user)
  (:export :update))


(defpackage #:outload
  (:use #:cl
		#:cl-user
		#:split-sequence
		#:cl-ppcre
		#:hunchentoot
		#:clsql
        #:bordeaux-threads)
  (:export :outload))


(defpackage #:admin
  (:use #:cl
        #:cl-user
		#:hunchentoot
		#:clsql)
  (:export :dispatcher
           :admin-main-page
		   :admin-page
           :product-page))


(defpackage #:yml
  (:use #:cl
        #:cl-user
		#:my)
  (:export :dispatcher))


(defpackage #:wolfor-stuff
  (:use #:cl
        #:my)
  (:export :range-filter
           :checkbox-filter
           :price-filter
           :radio-filter
           ))
