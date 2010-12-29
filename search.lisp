(in-package #:search)


(defun search-engine (q size)
  (let* ((wordlist (mapcar #'string-downcase (split-sequence #\Space q)))
		 (results (make-hash-table :test #'equal))
		 (result-list nil)
		 (sorted-list nil))
	(maphash #'(lambda (key val)
                 (when (and (product:active val)
                            (not (null (product:parent val))))
                   (let ((name (string-downcase (format nil "~a" (product:name val)))))
                     (mapcar #'(lambda (word)
                                 (let* ((search-result (search word name))
                                        (tmp (gethash key results (list :name name :rel 0))))
                                   (when (not (null search-result))
                                     (setf (getf tmp :rel) (+ 1 (getf tmp :rel)))
                                     (setf (gethash key results) tmp))))
                             wordlist))))
             trans::*product*)
    ;; Преобразуем хэш с элементами вида (list :name "xxx" :rel "yyy") в список для сортировки
	(maphash #'(lambda (key val)
				 (push (list* :id key val) result-list))
			 results)
    ;; Сортируем список
	(setf sorted-list (sort result-list #'(lambda (a b) (> (getf a :rel) (getf b :rel)))))
    ;; Обрезаем результаты
	(when (< size (length sorted-list))
	  (setf sorted-list (subseq sorted-list 0 size)))
    ;; Возвращаем список продуктов
	(mapcar #'(lambda (x)
				(gethash (getf x :id) trans:*product*))
			sorted-list)))


(defun get-match-products (q)
  (when (string= q "")
	(return-from get-match-products nil))
  (let ((articul (parse-integer q :junk-allowed t)))
	(if (null articul)
        (search-engine q 50)
        ;; else
		(let ((result (gethash articul trans:*product*)))
          (if (null result)
              (search-engine q 50)
              (list result))))))

;; (get-match-products "asus")


(defun get-safe-url-decode-value (param)
  param)


(funcall *dispatcher*
         `((string= "/search" (service:request-str))
           ,#'(lambda ()
                (let* ((q (hunchentoot:get-parameter "q"))
                       (search-string (my:strip q))
                       (url-decoded (get-safe-url-decode-value search-string)))
                  (cond ((string= q "")              (make-output "Введите поисковый запрос!"))
                        ((null url-decoded)          (make-output))
                        ((> 3 (length url-decoded))  (make-output "Слишком короткий поисковый запрос"))
                        (t  (let ((search-result (get-match-products (my:strip url-decoded))))
                              (if (null search-result)
                                  (make-output)
                                  (make-output (prefer search-result))))))))))

(defun prefer (products)
  (setf test products)
  (catalog:centerproduct
   (list
    ;; :producers (group:make-vendor-filter (parent object))
    :accessories (catalog:accessories)
    :products (loop
                 :for product
                 :in (remove-if #'(lambda (x)
                                    (null (product:active x)))
                                products)
                 :collect (product:view product)
                 ))))


;; (product:parent (gethash 149759 trans:*product*))

(defun make-output (&optional (centercontent nil))
  (service::default-page
    (catalog:content
     (list :name "Поиск мысли..."
           :breadcrumbs "<a href=\"/catalog\">Каталог</a> / Поиск"
           :menu (service:menu)
           :rightblocks (list (catalog:rightblock1)
                              (catalog:rightblock2)
                              (catalog:rightblock3))
           :subcontent (if (null centercontent)
                           "Ничего не найдено"
                           (format nil "~a" centercontent))))))

			 ;; (get-safe-url-decode-value (hunchentoot:get-parameter "q"))


