(in-package #:eshop)


(defun write-products-report (stream)
  (format stream "~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~%"
          "артикул" "цена магазина" "цена сайта" "имя" "имя real" "имя yml" "is-yml-show" "seo текст"
          "фотографии" "характеристики" "активный" "группа" "родительская группа"
          "secret" "DTD" "vendor" "доставка")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               ;; (wlog stream)
               (let ((id "нет") (name "нет") (name-real "нет") (name-yml "нет")
                     (desc "нет") (img "нет") (options "нет") (active "нет")
                     (group-name "нет") (parent-group-name "нет") (secret "нет"))
                 (when (equal (type-of v)
                              'product)
                   (setf id (articul v))
                   (setf name (stripper (name-provider v)))
                   (setf name-real (stripper (name-seo v)))
                   (with-option1 v "Secret" "Yandex"
                                 (setf name-yml (stripper (getf option :value))))
                   (setf desc (if (and (not (null (seo-text v)))
                                       (not (string= "" (stripper (seo-text v)))))
                                  "есть"
                                  "нет"))
                   (setf img (length (get-pics (articul v))))
                   (setf options (if (is-valide-option v)
                                     "есть"
                                     "нет"))
                   (setf active (if (active v)
                                    "да"
                                    "нет"))
                   (setf group-name (if (not (null (new-classes.parent v)))
                                        (stripper (name (new-classes.parent v)))))
                   (setf parent-group-name (if (and (not (null (new-classes.parent v)))
                                                    (not (null (new-classes.parent (new-classes.parent v)))))
                                               (stripper (name (new-classes.parent (new-classes.parent v))))))
                   (setf secret "Нет")
                   (with-option1 v "Secret" "Checked"
                                 (setf secret (getf option :value)))
                   (if (string= (format nil "~a" id) "172466")
                       (wlog v))
                   (format stream "~a;~a;~a;\"~a\";\"~a\";\"~a\";~a;~a;~a;~a;~a;\"~a\";\"~a\";~a;~a;~a;~a~%"
                           id (price v) (siteprice v) name name-real
                           name-yml (yml.is-yml-show v) desc img options active group-name
                           parent-group-name secret
                           (gethash (articul v) *xls.product-table*)
                           (vendor v)
                           (yml.get-product-delivery-price1 v))
                   )))
           (storage *global-storage*)))

(defun is-valide-option (product)
  (let ((flag nil))
    (mapcar #'(lambda (v) (mapcar #'(lambda (l)
                                      (when (and (not (equal (getf v :name) "Secret"))
                                                 l
                                               (getf l :value)
                                               (not (equal (getf l :value) ""))
                                               (not (equal (getf l :value) "Производитель"))
                                               (not (equal (getf l :value) "Модель")))
                                        ;; (print (getf l :value))
                                        (setf flag t)))
                       (getf v :options)))
        (optgroups product))
  flag
  ))

(defun write-groups (stream)
  (format stream "~a;~a;~a;~a;%"
          "Название категории"
          "url страницы"
          "Active"
          "seo-text")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (equal (type-of v)
                              'group)
                 (format stream "\"~a\";http://www.320-8080.ru/~a;~a;~a;~%"
                           (stripper (name v))
                           (key v)
                           (if (active v)
                               "yes"
                               "no")
                           (if (and (not (null (seo-text v)))
                                       (not (string= "" (stripper (seo-text v)))))
                                  "yes"
                                  "no"))
                   ))
           (storage *global-storage*)))

(defun write-groups-active-product-num (stream)
  (format stream "~a;~a;~a;~a;~%"
          "Название категории"
          "url страницы"
          "Active"
          "кол-во товаров")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (equal (type-of v)
                              'group)
                 (format stream "\"~a\";http://www.320-8080.ru/~a;~a;~a;~%"
                           (stripper (name v))
                           (key v)
                           (if (active v)
                               "yes"
                               "no")
                           (length (remove-if-not #'active (get-recursive-products v))))))
           (storage *global-storage*)))



(defun write-products (stream)
  (let ((vendor-name)
        (desc))
    (format stream "~a;~a;~a;~a;~a;~a;~%"
            "Название категории"
            "Брэнд"
            "Название товара"
            "url страницы"
            "Active"
            "seo-text")
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (equal (type-of v)
                              'product)
                   (setf vendor-name "Нет")
                   (setf vendor-name (vendor v))
                   (setf desc (if (and (not (null (seo-text v)))
                                       (not (string= "" (stripper (seo-text v)))))
                                  "yes"
                                  "no"))
                   (format stream "\"~a\";\"~a\";\"~a\";http://www.320-8080.ru/~a;~a;~a;~%"
                           (if (not (null (new-classes.parent v)))
                               (stripper (name (new-classes.parent v)))
                               "Нет категории")
                           (stripper vendor-name)
                           (stripper (name-seo v))
                           (articul v)
                           (if (active v)
                               "yes"
                               "no")
                           desc)))
             (storage *global-storage*))))


(defun write-vendors (stream)
  (format stream "~a;~a;~a;~a;~a;~%"
          "Название категории"
          "Брэнд"
          "url страницы"
          "Active"
          "seo-text")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (and (equal (type-of v)
                            'group)
                          (null (groups v)))

                 (maphash #'(lambda (vendor num)
                              (declare (ignore num))
                              (format stream "\"~a\";\"~a\";http://www.320-8080.ru/~a?vendor=~a;~a;~a;~%"
                                      (stripper (name v))
                                      (stripper vendor)
                                      (key v)
                                      (stripper vendor)
                                       "yes"
                                       (let ((desc (gethash vendor (vendors-seo v))))
                                         (if (and (not (null desc))
                                                  (not (string= "" desc)))
                                             "yes"
                                             "no"))))
                           ;; (producersall (make-producers v)))
                           (storage.get-vendors (storage.get-filtered-products v #'atom)))
                 ))
           (storage *global-storage*)))

(defun create-report (file-name report-func)
  (let ((filename (format nil "~a/~a" *path-to-dropbox* file-name)))
    (with-open-file
        (stream filename :direction :output :if-exists :supersede :external-format :cp1251)
      (print stream)
      (funcall report-func stream)
      )))


(defun check-valid-siteprice ()
  (format t "~&~a;\"~a\";~a;~a;~a;"
          "Артикул"
          "Имя"
          "Активный"
          "Цена магазина"
          "Цена 3208080")
  (maphash #'(lambda(k v)
               (declare (ignore k))
               (when (equal (type-of v)
                            'product)
                 (if (< (price v)
                        (siteprice v))
                     (format t "~&~a;\"~a\";~a;~a;~a;"
                             (articul v)
                             (stripper (name v))
                             (if (active v)
                                 "yes"
                                 "no")
                             (price v)
                             (siteprice v)))))
           (storage *global-storage*)))

(defun show-last-history (stream)
  (when (not (null *history*))
    ;; Делаем все продукты неактивными
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (equal (type-of v) 'product)
                   (setf (active v) nil)))
             (storage *global-storage*))
    (loop :for packet :in (reverse (caddr (car *history*))) :do
       (format stream "~a" (sb-ext:octets-to-string packet :external-format :cp1251)))))





(defun error-price-report ()
  (let ((products (remove-if-not #'(lambda (v) (< (price v) (siteprice v)))
                                 (remove-if-not #'eshop::active (wolfor-stuff::get-products-list)))))
    (when products
      (gateway-send-error-mail (list "web_design@alpha-pc.com"
                                     "wolforus@gmail.com"
                                     "slamly@gmail.com")
                               (format nil "~&Цена на сайте выше цены в магазине: ~a<br/>~{~&~a<br/>~}"
                                       (length products)
                                       (mapcar #'(lambda (v)
                                                   (format nil "~&<a href=\"http://www.320-8080.ru/~a\">~a</a>:~a | siteprice:~a price:~a"
                                                           (articul v)
                                                     (articul v)
                                                     (name v)
                                                     (siteprice v)
                                                     (price v)))
                                               products))
                               "Siteprice > Price"))))


                          ;; (setf (active (gethash "160420" (storage *global-storage*))) nil)
                          ;; (serialize (gethash "160420" (storage *global-storage*)))
                          ;; (setf (active (gethash "165359" (storage *global-storage*))) nil)
                          ;; (serialize (gethash "165359" (storage *global-storage*)))
                          ;; (setf (active (gethash "165360" (storage *global-storage*))) nil)
                          ;; (serialize (gethash "165360" (storage *global-storage*)))
;; (setf (active (gethash "157499" (storage *global-storage*))) nil)
;; (serialize (gethash "157499" (storage *global-storage*)))
;; (setf (active (gethash "153599" (storage *global-storage*))) nil)
;; (serialize (gethash "153599" (storage *global-storage*)))



(defparameter *special-products* (make-hash-table :test #'equal))



(defun post-proccess-gateway ()
    (mapcar #'(lambda (v)
                (let ((p (gethash v (storage *global-storage*)))
                  ;; (p (make-instance 'product
                  ;;                   :key v
                  ;;                   :articul v))
                  )
              ;; (if p1
                  ;; (setf p p1))
              (when (not (null p))
                  (setf (preorder p) t)
                  (setf (active p) t)
                  ;; (serialize p)
                  (setf (gethash v (storage *global-storage*)) p)
                  (setf (gethash v *special-products*) p))))
        (list
         "711265"
         "834786"
         "938111"
         "777888"
         "888777"
         "999111"
         "999777"
         ))
  (mapcar #'(lambda (v)
              (let ((p (gethash v (storage *global-storage*))))
                (when (not (null p))
                  (setf (preorder p) t)
                  ;; (serialize p)
                  )))
          (list "166545"
                ;; "166578"
              "166579"
              ;; "166580"
              "166581"
              "167530"
              "167531"
              "167532"
              "167533"
              "167534"
              "167535"))
  (let ((rs))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (and (equal (type-of v) 'product)
                            (active v)
                            (= (siteprice v) 0))
                   (push v rs)
                   (setf (active v) nil)
                   (wlog (key v))))
             (storage *global-storage*))
    (length rs))

;; (mapcar #'(lambda (v) (setf (active v) nil))
;;  (storage.get-filtered-products (gethash "vinchester" (storage *global-storage*))))
;; (setf (active (gethash "vinchester" (storage *global-storage*))) nil)
;; (mapcar #'(lambda (v) (setf (active v) nil))
;;  (storage.get-filtered-products (gethash "vneshnie-zhostkie-diski" (storage *global-storage*))))
;; (setf (active (gethash "vneshnie-zhostkie-diski" (storage *global-storage*))) nil)


  ;; (maphash #'(lambda (k v)
  ;;              (declare (ignore k))
  ;;              (when (equal (type-of v)
  ;;                             'group)
  ;;                (when (not (delivery-price v))
  ;;                    (wlog (name v))
  ;;                    (setf (delivery-price v) 300))))
  ;;          (storage *global-storage*))
(report.delete-doubles (list 150573
                165780
                147264
                166163
                157054
                157354
                157338
                150571
                142079
                162044
                157855
                131427
                124188
                146660
                163902
                157860
                160412
                149529
                162323
                145931
                146120
                157834
                158270
                141294
                157356
                159725
                164968
                159135
                156834
                157833
                117402
                143482
                145914
                154513
                149588
                157444
                168317
                152414
                161601
                138261
                168478
                142933
                152451
                149519
                157713
                167709
                156715
                168301
                159747
                149580
                152130
                159220
                151929
                170924
                169936
                153608
                170516
                160436
                110296
                156662
                110301
                148486
                158487
                150823
                165008
                152534
                149565
                139486
                156423
                165413
                153662
                161280
                156710
                156712
                166850
                153472
                167079
                155767
                157357
                154780
                152510
                147792
                152352
                143777
                168354
                170916
                158830
                145971
                167231
                132993
                157839
                145825
                159722
                147615
                156228
                157851
                149566
                149504
                156659
                155766
                157840
                144061
                152484
                143521
                155613
                165026
                143393
                171933
                155003
                153367
                162533
                163780
                155422
                154512
                160437
                161964
                150189
                156863
                149535
                143522
                138438
                166852
                159536
                170430
                145952
                168359
                146946
                163689
                153887
                150803
                150892
                152549
                170915
                118235
                150109
                150450
                158269
                149487
                160428
                160978
                145972
                168847
                171473
                157856
                143383
                114374
                145837
                160438
                161693
                156669
                149404
                150046
                167806
                160431
                160423
                153546
                150706
                159880
                158167
                144103
                168137
                148997
                150056
                150595
                147622
                143864
                163774
                153216
                147269
                130134
                167899
                146301
                164859
                147173
                149716
                158417
                153119
                167238
                157831
                153471
                145975
                171005
                149576
                156779
                171501
                171541
                163903
                151930
                150950
                100123
                127214
                121082
                109718
                163465))
(report.delet-from-groups)
)



(defun product-delivery (p)
  (let ((g (parent p))
        (daily (gethash (articul p) (daily *main-page.storage*))))
    (if daily
        0
        (aif (delivery-price p)
             it
             (if (and (not (null g))
                      (delivery-price g))
                 (delivery-price g)
                 300))))
  )


;; (with-open-file (stream "/home/webadmin/Dropbox/htconf/test.csv")
;;     (do ((line (read-line stream nil)
;;                (read-line stream nil)))
;;         ((null line))
;;       ;; (print line)
;;       (let* ((words (split-sequence:split-sequence #\, line))
;;              (article (car words))
;;              (price (parse-integer (cadr words)))
;;              (siteprice (parse-integer (caddr words)))
;;              (prod (gethash article (storage *global-storage*))))
;;         (format t "~&~a: ~a ~a" article price siteprice)
;;         (if prod
;;             (setf (price prod) price)
;;             (setf (siteprice prod) siteprice))
;;       )))




;; (let ((res)
;;       (res1))
;;   (print "test")
;;   (maphash #'(lambda (k v)
;;                (if (and (equal (type-of v) 'product)
;;                         (new-classes.parent v)
;;                         (not (equal (type-of (new-classes.parent v)) 'group)))
;;                    (push v res)))
;;            (storage *global-storage*))
;;   (print (length res))
;;   (car res
;;   ;; (mapcar #'(lambda (v)
;;   ;;             (let ((key (key v)))
;;   ;;               (setf (key v) (format nil "~a" key))
;;   ;;               (storage.edit-object v)
;;   ;;               ;; (remhash key (storage *global-storage*)))
;;   ;;             ))
;;   ;;         res)
;;   ;; (print (length res1))
;;   )


;; (let ((res)
;;       (res1))
;;   (print "test")
;;   (maphash #'(lambda (k v)
;;                (if (equal (type-of (key v))
;;                           (type-of 132345))
;;                    (push v res)))
;;            (storage *global-storage*))
;;   (print (length res))
;;   ;; (mapcar #'(lambda (v)
;;   ;;             (let ((key (key v)))
;;   ;;               (setf (key v) (format nil "~a" key))
;;   ;;               (storage.edit-object v)
;;   ;;               ;; (remhash key (storage *global-storage*)))
;;   ;;             ))
;;   ;;         res)
;;   ;; (print (length res1))
;;   )


;; (let ((res)
;;       (res1))
;;   (print "test")
;;   (maphash #'(lambda (k v)
;;                (when (and (equal (type-of v) 'group)
;;                         (equal (fullfilter v) ""))
;;                  (setf (fullfilter v) nil)
;;                  (format t "~&~a:~a" k (fullfilter v))))
;;            (storage *global-storage*))
;;   (print (length res))
;;   ;; (mapcar #'(lambda (v)
;;   ;;             (let ((key (key v)))
;;   ;;               (setf (key v) (format nil "~a" key))
;;   ;;               (storage.edit-object v)
;;   ;;               ;; (remhash key (storage *global-storage*)))
;;   ;;             ))
;;   ;;         res)
;;   ;; (print (length res1))
;;   )


;; (mapcar #'(lambda (v) (setf (active v) nil))
;;  (storage.get-filtered-products (gethash "vinchester" (storage *global-storage*))))
;; (mapcar #'(lambda (v) (setf (active v) nil))
;;  (storage.get-filtered-products (gethash "vneshnie-zhostkie-diski" (storage *global-storage*))))

;; (create-report "seo/last-gateway-string.txt" #'show-last-history)
;; (time (create-report "xls/products.csv" #'write-products-report))
;; (create-report "seo/report-groups.csv" #'write-groups)
;; (create-report "seo/report-products.csv" #'write-products)
;; (create-report "seo/report-vendors.csv" #'write-vendors)
;; (create-report "seo/write-groups-active-product-num.csv" #'write-groups-active-product-num)


;; (progn
;;   (mapcar #'(lambda (v) (setf (groups v) nil))
;;           (groups *global-storage*))
;;   (mapcar #'(lambda (v)
;;               (mapcar #'(lambda (item)
;;                           (push v (groups item)))
;;                       (parents v)))
;;           (groups *global-storage*)))

;; (let ((rs))
;;   (maphash #'(lambda (k v)
;;                (declare (ignore k))
;;                (when (and (equal (type-of v) 'product)
;;                         (active v)
;;                         (= (siteprice v) 0))
;;                  (push v rs)
;;                  (setf (active v) nil)
;;                  (wlog (key v))))
;;            (storage *global-storage*))
;;   (length rs))


;; (let ((rs))
;;   (maphash #'(lambda (k v)
;;                (declare (ignore k))
;;                (when (and (equal (type-of v) 'article)
;;                           (not (null (title v))))
;;                  (push v rs)
;;                  (wlog (key v))))
;;            *storage-articles*)
;;   (length rs))

(defun report.delet-from-groups ()
  (let ((groups (storage.get-groups-list)))
    (mapcar #'(lambda (group)
                (setf (products group)
                      (remove-if-not #'(lambda (v) (let ((pr (gethash (key v) (storage *global-storage*))))
                                                     (and pr
                                                          (equal group (new-classes.parent pr)))))
                                     (products group))))
            groups)
    "done"))




(defun report.delete-doubles (products)
  (mapcar #'(lambda (v)
              ;; (wlog v)
              ;; (format t "rewrite ^/~a/?$ /~a permanent;~&" v v)
              (let ((pr (gethash (format nil "~a" v) (storage *global-storage*))))
                (when pr
                  (remhash (format nil "~a" v) (storage *global-storage*)))))
          products))



;; (mapcar #'(lambda (v)
;;             (if (not (equal 0 (hash-table-count (vendors-seo v))))
;;                 (maphash #'(lambda (k text)
;;                              (remhash k (vendors-seo v))
;;                              (setf (gethash (string-downcase (format nil "~a" k))
;;                                             (vendors-seo v)) text))
;;                          (vendors-seo v))))
;;         (groups *global-storage*))


;; (length (let ((rs))
;;          (maphash #'(lambda (k v)
;;                       (when (equal (type-of v) 'product)
;;                         (if (and (not (gethash (articul v) *xls.product-table*))
;;                                  (optgroups v))
;;                             (push v rs))))
;;                         (storage *global-storage*))
;;          rs))

;; (mapcar #'(lambda (v) (setf (ymlshow v) nil)) (groups (gethash "uslugi" (storage *global-storage*))))



(defun serials.all-prs ()
  (let ((rs))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (equal (type-of v) 'product)
                       (push v rs)))
             (storage *global-storage*))
         rs))


;; (mapcar #'(lambda (v)
;;             (let ((delta-p (price v))
;;                   (price (price v)))
;;               ((<= 0.2 (float (/ (abs (- siteprice-old siteprice 1)) siteprice-old))))))
;;         (serials.all-prs))



(defun report.add-products-to-group (product-list gr)
  ;; (wlog product-list)
  ;; (wlog gr)
  (mapcar #'(lambda (v)
             (let ((pr (gethash (format nil "~a" v) (storage *global-storage*))))
               (when pr
                  (setf (parents pr) (list gr))
                  (push pr (products gr))
                  (storage.edit-object pr)
                 (wlog pr))))
          product-list)
  "done")





;; (let ((i 0))
;;   (progn
;;   (format t "~&<table border=\"0\"  cellpadding=\"3\" cellspacing=\"3\">")
;;   (format t "<tr style=\"background-color:#FBC348\"><th>Населенный пункт</th><th>Расстояние в км.</th><th>Цена доставки</th>")
;;   (mapcar #'(lambda (v)
;;               (let ((bg-color (if (equal (mod i 2) 0)
;;                                   "F0FFFF"
;;                                   "FAFAD2")))
;;                 (incf i)
;;                 (format t "~&<tr style=\"background-color:#~a\">~{<td>~a~^</td>~} р.</td></tr>" bg-color v)))
;;               (list
;;                (list "Агалатово" "22" "700")
;;                (list "Александровская" "19" "500")
;;                (list "Александровская (юг)" "13" "500")
;;                (list "Аннино" "10" "500")
;;                (list "Аннолово" "27" "700")
;;                (list "Б.Ижора" "37" "900")
;;                (list "Белоостров" "20" "500")
;;                (list "Бугры" "2" "500")
;;                (list "Ваганово" "40" "900")
;;                (list "Васкелово" "44" "1100")
;;                (list "Верево" "24" "700")
;;                (list "Виллози" "30" "700")
;;                (list "Вирки" "14" "500")
;;                (list "Войскорово" "19" "500")
;;                (list "Володарский" "4" "500")
;;                (list "Вырицы" "50" "1100")
;;                (list "Гарболово" "49" "1100")
;;                (list "Горбунки" "16" "500")
;;                (list "Горелово" "7" "500")
;;                (list "Горская" "17" "500")
;;                (list "Гостилицы" "50" "1100")
;;                (list "Грибное" "41" "1100")
;;                (list "Девяткино" "2" "500")
;;                (list "дер. Старая" "10" "500")
;;                (list "Дибуны" "12" "500")
;;                (list "Дранишники" "10" "500")
;;                (list "Жилгородок" "16" "500")
;;                (list "Заводской" "50" "1100")
;;                (list "Зеленогорск" "50" "1100")
;;                (list "им. Морозова" "33" "900")
;;                (list "им. Свердлова" "13" "500")
;;                (list "Кавголово" "27" "700")
;;                (list "Канисты" "17" "500")
;;                (list "Касимово" "20" "500")
;;                (list "Келлози" "28" "700")
;;                (list "Кипень" "27" "700")
;;                (list "Кировск" "31" "900")
;;                (list "Кобринское" "50" "1100")
;;                (list "Колпино" "14" "500")
;;                (list "Колтуши" "10" "500")
;;                (list "Комарово" "32" "900")
;;                (list "Коммунар" "30" "700")
;;                (list "Кондакопшино" "20" "500")
;;                (list "Красная звезда" "9" "500")
;;                (list "Красное село" "12" "500")
;;                (list "Красный бор" "24" "700")
;;                (list "Кронштадт" "33" "900")
;;                (list "Кузьмолово" "13" "500")
;;                (list "Куйвози" "41" "1100")
;;                (list "Лаврики" "11" "500")
;;                (list "Лаголово" "20" "500")
;;                (list "Лебяжье" "49" "1100")
;;                (list "Левашово" "9" "500")
;;                (list "Ленинское" "37" "900")
;;                (list "Ленсоветовский" "10" "500")
;;                (list "Лесколово" "36" "900")
;;                (list "Лесное (племзавод)" "19" "500")
;;                (list "Лисий нос" "21" "700")
;;                (list "Ломоносов" "25" "700")
;;                (list "Лукаши" "38" "900")
;;                (list "Лупполово" "14" "500")
;;                (list "Мартышкино" "25" "700")
;;                (list "Мга" "38" "900")
;;                (list "Медвежий стан" "4" "500")
;;                (list "Медный завод" "19" "500")
;;                (list "Металлострой" "5" "500")
;;                (list "Можайский" "17" "500")
;;                (list "Молодежное" "50" "1100")
;;                (list "Мурино" "2" "500")
;;                (list "Невская Дубровка" "27" "700")
;;                (list "Ненимяки" "38" "900")
;;                (list "Нижние Осельки" "31" "900")
;;                (list "Низино" "25" "700")
;;                (list "Никольское" "20" "500")
;;                (list "Никольское" "50" "1100")
;;                (list "Новое Девяткино" "5" "500")
;;                (list "Новое Токсово" "26" "700")
;;                (list "Новосаратовский" "3" "500")
;;                (list "Новоселье" "13" "500")
;;                (list "Новый Петергоф" "19" "500")
;;                (list "Новый Свет" "34" "900")
;;                (list "Новый учхоз" "43" "1100")
;;                (list "Ольгино" "7" "500")
;;                (list "Оржицы" "34" "900")
;;                (list "Осиная Роща" "6" "500")
;;                (list "Отрадное (юг)" "17" "500")
;;                (list "Павловск" "21" "700")
;;                (list "Парголово" "4" "500")
;;                (list "Пеники" "37" "900")
;;                (list "Первомайское" "50" "1100")
;;                (list "Пески" "9" "500")
;;                (list "Песочный" "10" "500")
;;                (list "Петро-Славянка" "4" "500")
;;                (list "Петродворец" "17" "500")
;;                (list "Петрокрепость" "33" "900")
;;                (list "Понтонный" "6" "500")
;;                (list "пос.Тельмана" "20" "500")
;;                (list "Пудомяги" "34" "900")
;;                (list "Пушкин" "15" "500")
;;                (list "Разлив" "20" "500")
;;                (list "Разметелево" "11" "500")
;;                (list "Рапполово" "20" "500")
;;                (list "Рапполово" "25" "700")
;;                (list "Рахья" "28" "700")
;;                (list "Репино" "30" "700")
;;                (list "Романовка" "14" "500")
;;                (list "Ропша" "32" "900")
;;                (list "Русско-Высоцкое" "20" "500")
;;                (list "Саблино" "31" "900")
;;                (list "Саперное" "9" "500")
;;                (list "Сарженка" "20" "500")
;;                (list "Семрино" "50" "1100")
;;                (list "Сертолово" "10" "500")
;;                (list "Сестрорецк" "20" "500")
;;                (list "Синявино" "32" "900")
;;                (list "Сойкино" "30" "700")
;;                (list "Старопаново" "2" "500")
;;                (list "Старый Петегроф" "20" "500")
;;                (list "Стрельна" "12" "500")
;;                (list "Сярьги" "16" "500")
;;                (list "Тавры" "12" "500")
;;                (list "Тайцы" "21" "700")
;;                (list "Тарховка" "20" "500")
;;                (list "Терволово" "29" "700")
;;                (list "Токсово" "20" "500")
;;                (list "Торики" "9" "500")
;;                (list "Тосно" "50" "1100")
;;                (list "Углово" "19" "500")
;;                (list "Университет" "25" "700")
;;                (list "Формосова" "38" "900")
;;                (list "Хапо-Ое" "15" "500")
;;                (list "Хвойный" "25" "700")
;;                (list "Черная Речка (восток)" "28" "700")
;;                (list "Черная Речка (юг)" "17" "500")
;;                (list "Шапки" "48" "1100")
;;                (list "Шлиссельбург" "32" "900")
;;                (list "Шушары" "2" "500")
;;                (list "Щеглово" "16" "500")
;;                (list "Энколово" "8" "500")
;;                (list "Юкки" "6" "500")
;;                (list "Янино 1" "3" "500")))
;;           (format t "</table>")
;;   ))

;; (progn
;;   (let ((group (gethash "stoly" (storage *global-storage*))))
;;     (report.add-products-to-group (list 169131
;;                                         166108
;;                                         166107
;;                                         166106
;;                                         166105
;;                                         166104
;;                                         166103
;;                                         166097
;;                                         171964
;;                                         171955
;;                                         171948
;;                                         171947
;;                                         171946
;;                                         171945
;;                                         171944
;;                                         171943
;;                                         171942
;;                                         )
;;                                   group))
;;   (let ((group (gethash "doskidlyazapisey" (storage *global-storage*))))
;;     (report.add-products-to-group (list 171936
;;                                         171935
;;                                         )
;;                                   group))
;;   (let ((group (gethash "chasi" (storage *global-storage*))))
;;     (report.add-products-to-group (list 169150
;;                                         169149
;;                                         )
;;                                   group))
;;   (let ((group (gethash "kresla" (storage *global-storage*))))
;;     (report.add-products-to-group (list 169126
;;                                         169125
;;                                         169124
;;                                         169123
;;                                         169122
;;                                         169121
;;                                         169120
;;                                         169119
;;                                         169118
;;                                         169117
;;                                         169116
;;                                         169115
;;                                         169114
;;                                         169113
;;                                         169112
;;                                         169111
;;                                         169110
;;                                         169109
;;                                         169108
;;                                         169107
;;                                         169106
;;                                         169105
;;                                         169104
;;                                         169103
;;                                         169102
;;                                         169101
;;                                         169100
;;                                         169099
;;                                         169098
;;                                         169097
;;                                         169096
;;                                         169095
;;                                         169094
;;                                         169086
;;                                         169085
;;                                         169084
;;                                         169083
;;                                         169082
;;                                         169081
;;                                         169080
;;                                         169079
;;                                         169078
;;                                         169077
;;                                         169076
;;                                         169075
;;                                         169074
;;                                         169073
;;                                         169072
;;                                         169071
;;                                         169070
;;                                         169069
;;                                         169068
;;                                         169067
;;                                         169066
;;                                         169065
;;                                         169064
;;                                         169063
;;                                         169062
;;                                         169061
;;                                         169060
;;                                         169059
;;                                         169058
;;                                         169057
;;                                         169056
;;                                         169055
;;                                         169054
;;                                         169053
;;                                         169052
;;                                         166096
;;                                         166095
;;                                         166094
;;                                         166093
;;                                         166092
;;                                         166090
;;                                         166089
;;                                         166088
;;                                         166087
;;                                         166086
;;                                         166085
;;                                         166084
;;                                         166083
;;                                         166081
;;                                         166080
;;                                         166079
;;                                         166078
;;                                         166077
;;                                         166076
;;                                         166073
;;                                         166072
;;                                         166071
;;                                         166070
;;                                         166069
;;                                         166067
;;                                         166066
;;                                         166065
;;                                         166063
;;                                         166062
;;                                         166061
;;                                         166060
;;                                         166059
;;                                         166058
;;                                         166057
;;                                         166056
;;                                         166055
;;                                         166054
;;                                         166053
;;                                         166052
;;                                         166051
;;                                         166049
;;                                         166048
;;                                         166047
;;                                         166038
;;                                         166037
;;                                         166036
;;                                         166035
;;                                         166034
;;                                         166033
;;                                         166032
;;                                         166030
;;                                         166029
;;                                         166028
;;                                         166027
;;                                         166026
;;                                         166025
;;                                         166024
;;                                         166018
;;                                         166017
;;                                         166016
;;                                         166015
;;                                         166014
;;                                         166013
;;                                         166012
;;                                         166011
;;                                         166010
;;                                         166009
;;                                         166008
;;                                         166007
;;                                         166006
;;                                         166005
;;                                         166004
;;                                         166003
;;                                         166002
;;                                         166001
;;                                         166000
;;                                         165999
;;                                         165998
;;                                         165997
;;                                         165996
;;                                         165995
;;                                         165994
;;                                         165993
;;                                         165992
;;                                         165991
;;                                         165990
;;                                         165989
;;                                         165988
;;                                         165987
;;                                         165986
;;                                         165985
;;                                         165984
;;                                         165983
;;                                         165982
;;                                         165981
;;                                         165980
;;                                         165979
;;                                         165978
;;                                         165977
;;                                         165976
;;                                         165962
;;                                         165961
;;                                         165960
;;                                         165958
;;                                         165956
;;                                         165975
;;                                         165974
;;                                         165973
;;                                         165972
;;                                         165971
;;                                         165970
;;                                         165968
;;                                         165967
;;                                         165966
;;                                         166091
;;                                         166082
;;                                         166074
;;                                         166068
;;                                         166050
;;                                         166031
;;                                         166020
;;                                         166019
;;                                         165969
;;                                         171892
;;                                         171891
;;                                         171890
;;                                         171889
;;                                         171888
;;                                         171887
;;                                         171886
;;                                         171885
;;                                         171884
;;                                         171883
;;                                         171882
;;                                         171881
;;                                         171880
;;                                         171879
;;                                         171878
;;                                         171877
;;                                         171876
;;                                         171875
;;                                         171874
;;                                         171873
;;                                         171872
;;                                         171871
;;                                         171870
;;                                         171869
;;                                         171868
;;                                         171867
;;                                         171866
;;                                         171865
;;                                         171864
;;                                         171863
;;                                         171862
;;                                         171924
;;                                         171923
;;                                         171922
;;                                         171921
;;                                         171920
;;                                         171919
;;                                         171918
;;                                         171917
;;                                         171916
;;                                         171915
;;                                         171914
;;                                         171913
;;                                         171912
;;                                         171911
;;                                         171910
;;                                         171909
;;                                         171908
;;                                         171069
;;                                         )
;;                                   group))
;;   (let ((group (gethash "vewalki" (storage *global-storage*))))
;;     (report.add-products-to-group (list 169140
;;                                         169139
;;                                         169138
;;                                         169137
;;                                         169136
;;                                         166109
;;                                         171929
;;                                         171928
;;                                         171927
;;                                         171926
;;                                         171925
;;                                         )
;;                                   group))
;;   (let ((group (gethash "stulya" (storage *global-storage*))))
;;     (report.add-products-to-group (list 169090
;;                                         166045
;;                                         171904
;;                                         171899
;;                                         169092
;;                                         171901
;;                                         166044
;;                                         171903
;;                                         171907
;;                                         171905
;;                                         165963
;;                                         165964
;;                                         169091
;;                                         166043
;;                                         171902
;;                                         171900
;;                                         166040
;;                                         171895
;;                                         171898
;;                                         171894
;;                                         169088
;;                                         166039
;;                                         169087
;;                                         171893
;;                                         171896
;;                                         171897
;;                                         169089
;;                                         169093
;;                                         171906
;;                                         166046
;;                                         )
;;                                   group))
;;   (let ((group (gethash "stoyki" (storage *global-storage*))))
;;     (report.add-products-to-group (list 169135
;;                                         169134
;;                                         169133
;;                                         171949
;;                                         169127
;;                                         171952
;;                                         171951
;;                                         171950
;;                                         166100
;;                                         166099
;;                                         166098
;;                                         171956
;;                                         171954
;;                                         171953
;;                                         166102
;;                                         166101
;;                                         171958
;;                                         171957
;;                                         171959
;;                                         169129
;;                                         169128
;;                                         171961
;;                                         171960
;;                                         169132
;;                                         171962
;;                                         169130
;;                                         171963
;;                                         )
;;                                   group))
;;   (let ((group (gethash "tumbi" (storage *global-storage*))))
;;     (report.add-products-to-group (list 169146
;;                                         169145
;;                                         169144
;;                                         169143
;;                                         166114
;;                                         171938
;;                                         171937
;;                                         )
;;                                   group))
;;   (let ((group (gethash "aksessuaridlyamebeli" (storage *global-storage*))))
;;     (report.add-products-to-group (list 169142
;;                                         166113
;;                                         166111
;;                                         166110
;;                                         171934
;;                                         )
;;                                   group))
;;   )
















;; (mapcar #'(lambda (v) (setf (parents v) nil))
;;  (remove-if-not #'(lambda (v) (and (parents v)
;;                                  (null (car (parents v)))))
;;  (storage.get-products-list)))
