(asdf:load-system :drakma)

(defpackage :eshop-test
  (:use :cl :drakma))

(in-package :eshop-test)

(setf *header-stream* nil) ;;*standard-output*)
(setf *drakma-default-external-format* :utf-8)

;; (defvar *eshop-test.server* "www.320-8080.ru")
(defvar *eshop-test.server* "localhost:8080")

(defun test-main-page ()
  (let ((*header-stream* *standard-output*)
        (url (format nil "http://~a/" *eshop-test.server*))
        (result)
        (header))
    (setf header (with-output-to-string (*standard-output*)
                   (setf result (http-request url
                                              :method :get
                                              ;; :parameters (create-register-post user secret-value)
                                              ;; :cookie-jar *cookie*
                                              ))))
    header))


(defun test-gateway ()
  )


;; (defun test-filter(group)
;;   (mapcar #'(lambda(p)
;;               (eshop::with-option pр
;;                 ;; "Общие характеристики"
;;                 ;; "Тип компьютера"
;;                 ;; "Сетевые интерфейсы"
;;                 ;; "WiMAX"
;;                 "Видеоадаптер"
;;                 "Объем видеопамяти, Мб"
;;                 (if (eshop::active p)
;;                     ;; (string= (string-downcase
;;                     ;;           (format nil "~a" (eshop::value eshop::option)))
;;                     ;;          "спецпредложение")
;;                     (format t "~&~a: ~a | ~a | ~a"
;;                             (eshop::articul p)
;;                             (eshop::name p)
;;                             (eshop::value eshop::option)
;;                             (eshop::active p)
;;                             ))))
;;           (eshop::products group)))

;; (test-filter (gethash "noutbuki" *storage*))
;; (test-filter (gethash "komputery" *storage*))



;; (let* ((articul 97115)
;;        (cnt     2)
;;        (plist   (eshop::plist-representation (gethash (format nil "~a" articul) eshop::*storage*)
;;                                       :articul
;;                                       :name
;;                                       :price
;;                                       :siteprice))
;;          (product-real-price (if
;;                               (= (getf plist :siteprice)
;;                                  0)
;;                               (getf plist :price)
;;                               (getf plist :siteprice)))
;;        (sum (* product-real-price cnt)))
;;   (list* :cnt cnt
;;          :sum sum
;;          plist))

;; (defun get-producter (product)
;;   (let ((result))
;;    (mapcar #'(lambda (optgroup)
;;                (if (string= (eshop::name optgroup) "Общие характеристики")
;;                    (let ((options (eshop::options optgroup)))
;;                      (mapcar #'(lambda (opt)
;;                                  (if (string= (eshop::name opt) "Производитель")
;;                                      (setf result (eshop::value opt))))
;;                              options))))
;;            (eshop::optgroups product))
;;    result))

;; (get-producter (gethash "138938" eshop::*storage*))

;; (defmethod filter-test ((object group) url)
;;   (let* ((request-full-str url)
;;          (request-parted-list (split-sequence:split-sequence #\? request-full-str))
;;          (request-get-plist (let ((result))
;;                               (loop :for param :in (split-sequence:split-sequence #\& (cadr request-parted-list)) :do
;;                                  (let ((split (split-sequence:split-sequence #\= param)))
;;                                    (setf (getf result (intern (string-upcase (car split)) :keyword))
;;                                          (if (null (cadr split))
;;                                              ""
;;                                              (cadr split)))))
;;                               result)))
;;     (filter-controller object request-get-plist)))



;; (defun test-is-empty1 (input-list)
;;   (let ((is-empty))
;;     (mapcar #'(lambda (v)
;;                 (setf is-empty (or is-empty (null v))))
;;             input-list)
;;     is-empty))


;; (defun test-is-empty2 (input-list)
;;  (not (null (remove-if #'null input-list))))

;; (time (let ((l))
;;   (loop
;;      :for i
;;      :from 1 to 5000000
;;      :do (progn
;;            (if (oddp i)
;;                (push i l)
;;                (push nil l))))
;;   ;; (print (length l))
;;   (test-is-empty2 l)))

