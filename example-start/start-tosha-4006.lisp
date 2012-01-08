;; загрузка модулей и файлов
(load (format nil "~a~a~a" (user-homedir-pathname) "cl-restas-eshop/" #P"load-new.lisp"))

;; старт сервера swank
(asdf:load-system :swank)
;; для того чтобы загружался esrap
(load (format nil "~a~a~a" (user-homedir-pathname) "cl-restas-eshop/" #P"libs/slime-archimag/contrib/swank-indentation.lisp"))

(print swank::*application-hints-tables*)
(setq swank:*use-dedicated-output-stream* nil)
(swank:create-server :coding-system "utf-8-unix" :dont-close t :port 4006)


;; (asdf:load-system :restas)
;; (asdf:load-system :cl-json)
;; (asdf:load-system :arnesi)
;; (asdf:load-system :closure-template)
;; (asdf:load-system :log5)

(asdf:load-system :eshop)

;; нумерация заказов
(setf eshop::*order-id* 1)
(setf eshop:*path-order-id-file* "wolfor-order-id.txt")
(setf eshop:*path-sitemap* "wolfor-sitemap.xml")

;; запуск Restas
(restas:start '#:eshop :port 8081)
(restas:debug-mode-on)
(setf hunchentoot:*catch-errors-p* nil)

(eshop::restore-skls-from-files)
(eshop::restore-articles-from-files)
(eshop::main-page-restore)
;; (eshop::restore-from-files)
;; (eshop::static-pages.restore)
;; (let ((*package* (find-package :eshop)))
;;   (eshop::new-classes.unserialize-all))
;; (eshop::new-classes.dbg-unserialize-products)
(eshop::static-pages.restore)
