;; загрузка модулей и файлов
(defvar *path-to-eshop-home* "cl-restas-eshop")
(defvar *swank-port* 4005)
(defvar *server-port* 8080)

;; регестрация путей для asdf
(load (format nil "~a~a/~a" (user-homedir-pathname) *path-to-eshop-home* "load.lisp"))
(load.register-libs *path-to-eshop-home*)

;; старт сервера swank
(asdf:load-system :swank)
;; для того чтобы загружался esrap
(load (format nil "~a~a~a" (user-homedir-pathname) "cl-restas-eshop/" #P"libs/slime-archimag/contrib/swank-indentation.lisp"))

(print swank::*application-hints-tables*)
(setq swank:*use-dedicated-output-stream* nil)
(swank:create-server :coding-system "utf-8-unix" :dont-close t :port *swank-port*)

(asdf:load-system :eshop)

;; нумерация заказов
(setf eshop::*order-id* 1)
(setf eshop:*path-order-id-file* "wolfor-order-id.txt")
;; адрес для карты сайта
(setf eshop:*path-sitemap* "wolfor-sitemap.xml")
;; Список email для рассылки писем от ошибках выгрузки 1с
(setf eshop::*conf.emails.gateway.warn* (list "wolforus@gmail.com"))
;; Список email для отправки заказов
(setf eshop::*conf.emails.cart* (list "wolforus@gmail.com"
                                      "slamly@gmail.com"))

;; запуск Restas
(restas:start '#:eshop :port *server-port*)
(restas:debug-mode-on)
(setf hunchentoot:*catch-errors-p* nil)

(eshop::restore-skls-from-files)
(eshop::articles.restore)
(eshop::main-page.restore)
;; (eshop::restore-from-files)
;; (eshop::static-pages.restore)
(let ((*package* (find-package :eshop)))
  (eshop::new-classes.unserialize-all))
  ;;(eshop::new-classes.dbg-unserialize-products))
(eshop::static-pages.restore)
