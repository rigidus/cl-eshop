;; загрузка модулей и файлов
(load (format nil "~a~a~a" (user-homedir-pathname) "cl-restas-eshop/" #P"load-new.lisp"))

;; старт сервера swank
(require :swank)
 ;; для того чтобы загружался esrap
(load (format nil "~a~a~a" (user-homedir-pathname) "cl-restas-eshop/" #P"libs/slime-archimag/contrib/swank-indentation.lisp"))

(setq swank:*use-dedicated-output-stream* nil)
(swank:create-server :coding-system "utf-8-unix" :dont-close t :port 4005)

(require 'asdf)
(asdf:load-system :eshop)

;;нумерация заказов
(setf eshop::*order-id* 1)
(setf eshop:*path-order-id-file* "tosha-order-id.txt")

;; запуск Restas
(restas:start '#:eshop :port 8080)
(restas:debug-mode-on)
(setf hunchentoot:*catch-errors-p* nil)


(eshop::restore-skls-from-files)
(eshop::restore-articles-from-files)
(eshop::main-page-restore)
;;(eshop::restore-from-files)
