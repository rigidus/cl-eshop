(in-package #:eshop)

;;набор функций для переименования картинок в соответствии с SEO именами

(defun rename-translit-char (char)
  (let ((letters (list "a" "b" "v" "g" "d" "e"
                   "zh" "z" "i" "y" "k" "l" "m"
                   "n" "o" "p" "r" "s" "t" "u"
                   "f" "h" "ts" "ch" "sh" "shch"
                   "" "y" "" "e" "yu" "ya"))
        (code (char-code char)))
    (if (or (< code 1072) (> code 1103))
        (string char)
        (nth (- code 1072) letters))))


;;составление имени картинки по SEO имени товара
(defun rename-new-name (name &optional number)
  (when name
    (setf name
          (ppcre:regex-replace-all                    ;;заменяет цепочки подчеркиваний на одно
           "_{2,}"
           (ppcre:regex-replace-all                   ;;удаляет оставшиеся непечатные символы и подчеркивания в начале и конце
            "\\W|(_$)|(^_)"
            (ppcre:regex-replace-all                  ;;заменяет непечатные символы на подчеркивания
             "\\s|-|/|\\\\|\\[|\\]|\\(|\\)|\\."
             (string-downcase name)
             "_")
            "")
           "_"))
    (let ((result ""))
      (dotimes (i (length name))
        (let ((char (char name i)))
          (setf result (concatenate 'string result (rename-translit-char char)))))
      (if number
          (format nil (if (< number 10)
                          "~a_0~a"
                          "~a_~a")
                  result number)
          result))))


;;проверка имен картинок для конкретного товара
;;при несоответствии хотя бы одной - возвращает nil
(defun rename-check (product)
  (let* ((articul (articul product))
         (name (realname product))
         (pics (get-pics articul))
         (result t))
    (dotimes (i (length pics))
      (when (not (equal (rename-new-name name (+ 1 i))
                      (subseq (nth i pics) 0 (search "." (nth i pics)))))
          (setf result nil)))
          ;;(print (rename-new-name name (+ 1 i)))
          ;;(print (subseq (nth i pics) 0 (search "." (nth i pics))))))
    result))


;;переименовывает все картинки в указанной папке именами для указанного продукта
(defun rename-in-folder (product folder-path)
  (let ((counter 0))
    (loop
       :for pic
       :in (directory (format nil "~a/*.jpg" folder-path))
       :do (incf counter)
       (with-open-file (file pic)
         (rename-file file (rename-new-name
                            (realname product) counter))))))



;;переименовывает все картинки для продукта
(defun rename-product-all-pics (product)
  (when (not (rename-check product))
    (let ((articul (articul product)))
      (loop
         :for folder
         :in (list "big" "goods" "middle" "minigoods" "small")
         :do (rename-in-folder product
                               (format nil "~a/~a/~a" *path-to-pics* folder articul))))))


(defun rename-force-product-all-pics (product)
  (let ((articul (articul product)))
    (loop
       :for folder
       :in (list "big" "goods" "middle" "minigoods" "small")
       :do (rename-in-folder product
                             (format nil "~a/~a/~a" *path-to-pics* folder articul)))))


;; берет картинки из указанной папки и конвертирует в 5 папок для продукта
(defun rename-convert-from-folder (product path-to-folder)
  (let* ((articul (articul product))
         (name (realname product))
         (counter 0))
    (loop
       :for pic
       :in (directory (format nil "~a/*.jpg" path-to-folder))
       :do (incf counter)
       (format t "Converting file ~a ~%" pic)
       (let ((new-name (rename-new-name name counter)))
         (loop
            :for folder
            :in (list "big" "goods" "middle" "minigoods" "small")
            :for size-w
            :in (list nil 225 200 70 100)
            :for size-h
            :in (list nil nil 160 70 120)
            :do
            (rename-convert
             (format nil "~a" pic)
             (format nil "~a/~a/~a/~a.jpg"
                     *path-to-pics*
                     folder
                     articul
                     new-name)
             size-w
             size-h))
         (format t "converted to ~a ~%" new-name)))))


;;конвертирует картинку до указанных размеров и кладет в указанную папку
(defun rename-convert (path-from path-to &optional size-w size-h)
  (let* ((size (if size-w
                  (if size-h
                      (format nil "~ax~a" size-w size-h)
                      (format nil "~a" size-w))
                  nil))
         (proc (sb-ext:run-program "/usr/bin/convert"
                                   (append
                                    (list path-from)
                                    (if size
                                        (append
                                         (list
                                          "-resize"
                                          (format nil "~a\>" size)
                                          "-size"
                                          (format nil "~a" size))
                                         (if size-h
                                             (list
                                              "xc:white"
                                              "+swap"
                                              "-gravity"
                                              "center"
                                              "-composite"))))
                                    (list path-to))
                                   :wait nil :output :stream)))
    (with-open-stream (in (sb-ext:process-output proc))
      (loop
         :for line = (read-line in nil)
         :until (null line)
         :do (print line)))))
