(in-package #:eshop)


(defun write-products-report (stream)
  (format stream "~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~%"
          "артикул" "цена магазина" "цена сайта" "имя" "имя real" "имя yml" "seo текст"
          "фотографии" "характеристики" "активный" "группа" "родительская группа"
          "secret" "DTD" "vendor")
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
                   (format stream "~a;~a;~a;\"~a\";\"~a\";\"~a\";~a;~a;~a;~a;\"~a\";\"~a\";~a;~a;~a~%"
                           id (price v) (siteprice v) name name-real
                           name-yml desc img options active group-name
                           parent-group-name secret
                           (gethash (articul v) *xls.product-table*)
                           (vendor v))
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
(report.delete-doubles)
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


(defun report.delete-doubles ()
  (mapcar #'(lambda (v)
              ;; (wlog v)
              (let ((pr (gethash (format nil "~a" v) (storage *global-storage*))))
                (when pr
                  (remhash (format nil "~a" v) (storage *global-storage*)))))
          (list 150573
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
                163465)))



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
