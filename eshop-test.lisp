(in-package #:eshop-test)




(defun test-filter(group)
  (mapcar #'(lambda(p)
              (eshop::with-option p
                ;; "Общие характеристики"
                ;; "Тип компьютера"
                ;; "Сетевые интерфейсы"
                ;; "WiMAX"
                "Видеоадаптер"
                "Объем видеопамяти, Мб"
                (if (eshop::active p)
                    ;; (string= (string-downcase
                    ;;           (format nil "~a" (eshop::value eshop::option)))
                    ;;          "спецпредложение")
                    (format t "~&~a: ~a | ~a | ~a"
                            (eshop::articul p)
                            (eshop::name p)
                            (eshop::value eshop::option)
                            (eshop::active p)
                            ))))
          (eshop::products group)))

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