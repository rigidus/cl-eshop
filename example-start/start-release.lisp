;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START RELEASE server:4244 swank:4444
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "START RELEASE server:4244 swank:4444")

;; загрузка модулей и файлов
(load (format nil "~a~a~a" (user-homedir-pathname) "cl-restas-eshop/" #P"load.lisp"))

;; старт сервера swank
(setq swank:*use-dedicated-output-stream* nil)
(swank:create-server :coding-system "utf-8-unix" :dont-close t :port 4444)

;;запуск Restas
(restas:start '#:eshop :port 4244)
;; (restas:debug-mode-on)
(restas:debug-mode-off)
;; (setf hunchentoot:*catch-errors-p* nil)
(setf hunchentoot:*catch-errors-p* t)
(setf hunchentoot:*log-lisp-errors-p* nil)
