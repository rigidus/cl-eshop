;; TODO fix exports

;; (print "restas:define-module CL-ESHOP")
(restas:define-module #:eshop
    (:use :cl
          :closure-template
          :anaphora
          :split-sequence
          :cl-ppcre
          :json
          :cl-fad)
  (:import-from :arnesi :parse-float)
  (:import-from :alexandria :read-file-into-string)
  (:export :compile-templates
		   :*storage*
           :name
           :unserialize
           :plist-representation
           :*path-to-bkps*
           :*path-to-pics*
           :*path-to-conf*
           :*path-to-tpls*))


(defpackage #:wolfor-stuff
  (:use #:cl
        #:eshop))

(defpackage #:eshop-test
  (:use #:cl
        #:eshop
        #:wolfor-stuff))



;; (defpackage #:product
;;   (:use #:cl
;;         ;; #:cl-user
;;         #:json
;;         #:split-sequence)
;;   (:export
;;    ;; #:product
;;    ;; #:articul
;;    :parent
;;    ;; #:name
;;    ;; #:realname
;;    :price
;;    ;; #:siteprice
;;    ;; #:ekkprice
;;    :active
;;    ;; #:newbie
;;    ;; #:sale
;;    ;; #:descr
;;    ;; #:shortdescr
;;    ;; #:count-transit
;;    ;; #:count-total
;;    :options
;;    ;; #:plist-representation
;;    ;; ;; Отображение
;;    ;; #:get-pics
;;    ;; #:show
;;    ;; #:view
;;    ;; #:serialize
;;    ;; #:unserialize
;;    )
;;   )

;; (defpackage #:optgroup
;;   (:use #:cl
;;         #:cl-user
;;         #:json)
;;   (:export
;;    ;; #:optgroup
;;    ;; #:show
;;    ;; #:serialize
;;    #:name
;;    #:options
;;    ;; #:unserialize
;;    ))

;; (defpackage #:option
;;   (:use #:cl
;;         #:cl-user
;;         #:json)
;;   (:export
;;    ;; #:option
;;    ;; #:show
;;    #:name
;;    #:value
;;    ;; #:optype
;;    ;; #:boolflag
;;    ;; #:serialize
;;    ;; #:unserialize
;;    ))

;; (defpackage #:optlist
;;   (:use #:cl
;;         #:cl-user)
;;   (:export #:optlist))
