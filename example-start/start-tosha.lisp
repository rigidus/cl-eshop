;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START TOSHA-DEV server:4247 swank:8888
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "START TOSHA-DEV server:4247 swank:8888")

;; загрузка модулей и файлов
(load (format nil "~a~a~a" (user-homedir-pathname) "cl-restas-eshop/" #P"load.lisp"))

;; старт сервера swank
(setq swank:*use-dedicated-output-stream* nil)
(swank:create-server :coding-system "utf-8-unix" :dont-close t :port 8888)

;; нумерация заказов
(setf eshop::*order-id* 1)
(setf eshop:*path-order-id-file* "tosha-order-id.txt")

;; запуск Restas
(restas:start '#:eshop :port 4247)
(restas:debug-mode-on)
(setf hunchentoot:*catch-errors-p* nil)


