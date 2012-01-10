;;;; prerender.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop)


(defun prerender-str-to-args (string)
  (let* ((res) (prev 0))
    (dotimes (cur (length string))
      (when (equal (char string cur) #\;)
        (pushnew (subseq string prev cur) res)
        (setf prev (+ 1 cur))))
    (nreverse res)))


;;составление строки по данным аргументам
;; (defun prerender-args-to-html (args)
;;   (let* ((type (string-trim '(#\Space) (nth 0 args))))
;;     (cond
;;       ;;вставка картинки
;;       ((string= type "pic")
;;        (let* ((size (string-trim '(#\Space) (nth 1 args)))
;;               (articul (string-trim '(#\Space) (nth 2 args)))
;;               (path-art  (ppcre:regex-replace  "(\\d{1,3})(\\d{0,})"
;;                                                (format nil "~a" articul)
;;                                                "\\1/\\1\\2" ))
;;               (number (- (parse-integer
;;                           (string-trim '(#\Space) (nth 3 args))) 1))
;;               (product (gethash articul (storage *global-storage*)))
;;               (picname (nth number (get-pics articul)))
;;               (height (nth 5 args))
;;               (width (nth 4 args))
;;               (style ""))
;;          (if height
;;              (setf style (format nil "~aheight:~apx;" style
;;                                  (string-trim '(#\Space) height))))
;;          (if width
;;              (setf style (format nil "~awidth:~apx;" style
;;                                  (string-trim '(#\Space) width))))
;;          (if (and (not picname) (get-pics articul))
;;              (setf picname (car (get-pics articul))))
;;          (when (and product (name-seo product) picname)
;;            (let* ((path (format nil "~a/~a/~a" size articul picname))
;;                   (path* (format nil "~a/~a/~a" size path-art picname)))
;;              (if (and (not height) (not width))
;;                  (multiple-value-bind
;;                        (width height)
;;                      (images-get-dimensions (format nil "~a/~a" *path-to-product-pics* path*))
;;                    (setf style (images-style-for-resize width height 600))))
;;              (format nil "<a href=\"/~a\" title=\"~a\">~%
;;                                    <img src=\"/pic/~a\" alt=\"~a\" style=\"~a\"/>~%
;;                                 </a>~%"
;;                      articul (name-seo product) path (name-seo product) style)))))
;;       ;;вставка области для маппинга
;;       ((string= type "area")
;;        (let* ((c1 (nth 1 args))
;;               (c2 (nth 2 args))
;;               (c3 (nth 3 args))
;;               (c4 (nth 4 args))
;;               (articul (nth 5 args))
;;               (product (gethash articul (storage *global-storage*)))
;;               (name (name-seo product))
;;               (siteprice (siteprice product))
;;               ;; (delivery-price (delivery-price product))
;;               (picname (car (get-pics articul))))
;;          (format nil "<area shape=\"rect\" coords=\"~a,~a,~a,~a\"
;;                      href=\"#add\" ~a>"
;;                  c1 c2 c3 c4
;;                  (soy.buttons:add-product-onclick
;;                   (list :articul articul
;;                         :name name
;;                         :siteprice siteprice
;;                         ;; :deliveryprice delivery-price
;;                         :pic picname
;;                         )))))
;;       ;;вставка кнопки покупки
;;       ((string= type "buy")
;;        (let* ((articul (nth 1 args))
;;               (product (gethash articul (storage *global-storage*))))
;;          (when product
;;            (let ((name (name-seo product))
;;                  (siteprice (siteprice product))
;;                  ;; (delivery-price (delivery-price product))
;;                  (picname (car (get-pics articul))))
;;              (format nil "<span class=\"add\" id=\"add-img\"><big class=\"price\"><b>~a</b><var> руб.</var></big>~a"
;;                      (get-format-price siteprice)
;;                      (soy.buttons:add-product-cart
;;                                (list :articul articul
;;                                      :name name
;;                                      :siteprice siteprice
;;                                      ;; :deliveryprice delivery-price
;;                                      :pic picname
;;                                      )))))))
;;       ((string= type "price")
;;        (let* ((articul (nth 1 args))
;;               (product (gethash articul (storage *global-storage*))))
;;          (when product
;;            (let ((siteprice (siteprice product)))
;;              (format nil "~a"
;;                      (get-format-price siteprice))))))
;;       (t
;;        (format nil "<!-- unknown format -->~%")))))

(defun prerender-string-replace (string)
  (let* ((start (search "<!--#" string)) (end (search ";-->" string)))
    (if (null start)
        string
        (concatenate 'string (subseq string 0 start)
                     (prerender-args-to-html
                      (prerender-str-to-args
                       (subseq string (+ 5 start) (+ 1 end))))
                     (prerender-string-replace (subseq string (+ 4 end))))
        )))
