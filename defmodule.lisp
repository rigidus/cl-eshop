(restas:define-module #:eshop
    (:use #:cl
          #:closure-template
          #:anaphora
          #:split-sequence
          #:cl-ppcre
          #:json
          #:cl-fad)
  (:import-from #:arnesi      #:parse-float)
  (:import-from #:alexandria  #:read-file-into-string)
  (:export #:compile-templates
		   #:*storage*
           #:name
           #:unserialize
           #:plist-representation
           #:*path-to-bkps*
           #:*path-to-pics*
           #:*path-to-conf*
           #:*path-to-tpls*))

(defpackage #:wolfor-stuff
  (:use #:cl
        #:eshop))

(defpackage #:eshop-test
  (:use #:cl
        #:eshop
        #:wolfor-stuff))


(in-package #:eshop)


(defconstant +system-name+ :eshop)
(defconstant +dbg-username+ "rigidus")
