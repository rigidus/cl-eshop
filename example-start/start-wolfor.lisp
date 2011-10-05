;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START WOLFOR-DEV server:4246 swank:7777
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "START WOLFOR-DEV server:4246 swank:7777")

;; загрузка модулей и файлов
(load (format nil "~a~a~a" (user-homedir-pathname) "cl-restas-eshop/" #P"load-new.lisp"))

;; старт сервера swank
(require :swank)
(setq swank:*use-dedicated-output-stream* nil)
(swank:create-server :coding-system "utf-8-unix" :dont-close t :port 7777)


(asdf:load-system :eshop)

;; нумерация заказов
(setf eshop::*order-id* 1)
(setf eshop:*path-order-id-file* "wolfor-order-id.txt")

;; запуск Restas
(restas:start '#:eshop :port 4246)
(restas:debug-mode-on)
(setf hunchentoot:*catch-errors-p* nil)

(eshop::restore-skls-from-files)
(eshop::restore-articles-from-files)
(eshop::main-page-restore)
(eshop::restore-from-files)
