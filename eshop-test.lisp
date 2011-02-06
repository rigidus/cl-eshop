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

(test-filter (gethash "noutbuki" *storage*))
;; (test-filter (gethash "komputery" *storage*))

