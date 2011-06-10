(in-package #:eshop)

;;категория для логирования
(log5:defcategory :main-page-log)

;;старт логирования ошибок в стандартный поток ошибок
(log5:start-sender 'main-page-sender
              (log5:stream-sender :location *error-output*)
              :category-spec 'log5:warn+
              :output-spec '("WARN:: " log5:message))

;;обновление главной страницы
(defun main-page-update ()
  (main-page-compile-templates))

;;шаблоны
(defun main-page-compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (pathname (format nil "~a/~a" *path-to-tpls* fname))))
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("index.html"
            "main-page.html"
            )))

(main-page-update)

(defun main-page-button-add-card (articul)
  (let ((product (gethash articul *storage*)))
    (if (not (null articul))
        (main-page:button-add-card
         (list :articul ""
               :groupid "0" ;;id группы -- это поле устарело
               :name ""
               :price ""  ;;цена вида 8999.00
               :number ""  ;;количество
               :productlink ""  ;;cсылка на товар сейчас это и есть артикул
               :pic ""))
        ;; (log5:log-for (or log5::test-warr
        ;;                   :main-page-log
        ;;                   log5:error+) "trying to show button for articul:~a" articul)
        )))


;;отображение товаров дня
(defun main-page-daily-show ()
  (main-page:daily))

;;отображение главной страницы
(defun main-page-show (&optional (request-str ""))
  (default-page (root:content (list :menu (menu request-str)
                                    :dayly  (root:dayly);;(main-page-daily-show)
                                    :banner (root:banner)
                                    :olist (root:olist)
                                    :lastreview (root:lastreview)
                                    :best (root:best)
                                    :hit (root:hit)
                                    :new (root:new)
                                    :post (root:post)
                                    :plus (root:plus)))
    :KEYWORDS "компьютеры, купить компьютер, компьютерная техника, Петербург, Спб, Питер, Санкт-Петербург, продажа компьютеров, магазин компьютерной техники, магазин компьютеров, интернет магазин компьютеров, интернет магазин компьютерной техники, продажа компьютерной техники, магазин цифровой техники, цифровая техника, Цифры, 320-8080"
    :DESCRIPTION "Купить компьютер и другую технику вы можете в Цифрах. Цифровая техника в Интернет-магазине 320-8080.ru"
    :TITLE "Интернет-магазин: купить компьютер, цифровую технику, комплектующие в Санкт-Петербурге"))



;; (defparameter *main-page-daily-products* (make-hash-table :test #'equal))
;; (defparameter *main-page-daily-products* (make-hash-table :test #'equal))


(let ((daily-products))
  (defun main-page-daily-products (func)
    (funcall func daily-products)))


(defun main-page-restore-from-files ()
  (let ((t-storage))
    (print "start load skls....{")
    ;;(sb-ext:gc :full t)
    (let ((*group-skls* (make-hash-table :test #'equal)))
      (load-sklonenie)
      (print *group-skls*)
      (setf t-storage  *group-skls*))
    (setf *group-skls* t-storage)
    ;;(sb-ext:gc :full t)
    (print "...} finish load skls")))

(defun main-page-load-sklonenie ()
  (let ((proc (sb-ext:run-program
               "/usr/bin/xls2csv"
               (list "-q3" (format nil "~a/seo/~a" *path-to-dropbox* "sklonenija.xls")) :wait nil :output :stream)))
    (with-open-stream (stream (sb-ext:process-output proc))
      (loop
         :for line = (read-line stream nil)
         :until (or (null line)
                    (string= "" (string-trim "#\," line)))
         :do (let* ((words (split-sequence:split-sequence #\, line))
                    (skls (mapcar #'(lambda (w) (string-trim "#\""  w))
                                  words))
                    (key (string-downcase (car skls))))
               (setf (gethash key *group-skls*) skls)
               ;; (format t "~&~a: ~{~a~^,~}" key skls)
               )
         ))))

(defun main-page-sklonenie (name skl)
  (let* ((key (string-downcase name))
         (words (gethash key *group-skls*)))
    (if (null words)
        key
        (nth (- skl 1) words))))


