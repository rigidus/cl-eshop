(in-package #:eshop)

(log5:defcategory test)
(log5:defcategory debug-console)
(log5:defcategory debug-file)
(log5:defcategory info)
(log5:defcategory warning)
(log5:defcategory error)
(log5:defcategory warn+ (or error warning))





(log5:defoutput human-time (time.get-full-human-time))


(log5:start-sender 'general-file
                   (log5:stream-sender :location (format nil "~a/general.log" *path-to-logs*))
                   :category-spec '(or info warn+)
                   :output-spec '(human-time log5:category log5:message))

(log5:start-sender 'general-console
                   (log5:stream-sender :location *standard-output*)
                   :category-spec '(or info warn+)
                   :output-spec '(human-time log5:category log5:message))

(log5:start-sender 'debug-console
                   (log5:stream-sender :location *standard-output*)
                   :category-spec '(or test debug-console)
                   :output-spec '(log5:message))


(log5:start-sender 'debug-file
                   (log5:stream-sender :location (format nil "~a/debug.log" *path-to-logs*))
                   :category-spec '(or debug-file)
                   :output-spec '(log5:message))


;;example of call
;;(log5:log-for info "First log msg")
