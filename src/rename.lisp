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
    (let* ((articul (articul product))
           (path-art  (ppcre:regex-replace  "(\\d{1,3})(\\d{0,})"  (format nil "~a" articul)  "\\1/\\1\\2" )))
      (loop
         :for folder
         :in (list "big" "goods" "middle" "minigoods" "small")
         :do (rename-in-folder product
                               (format nil "~a/~a/~a" *path-to-product-pics* folder path-art))))))


(defun rename-force-product-all-pics (product)
  (let* ((articul (articul product))
         (path-art  (ppcre:regex-replace  "(\\d{1,3})(\\d{0,})"  (format nil "~a" articul)  "\\1/\\1\\2" )))
    (loop
       :for folder
       :in (list "big" "goods" "middle" "minigoods" "small")
       :do (rename-in-folder product
                             (format nil "~a/~a/~a" *path-to-product-pics* folder path-art)))))


;; берет картинки из указанной папки и конвертирует в 5 папок для продукта
(defun rename-convert-from-folder (product path-to-folder)
  (if (directory-exists-p path-to-folder)
      (let* ((articul (articul product))
             (name (realname product))
             (counter 0)
             (path-art  (ppcre:regex-replace  "(\\d{1,3})(\\d{0,})"  (format nil "~a" articul)  "\\1/\\1\\2" )))
        (wlog (format nil "Start converting images for product ~a from folder ~a~%"
                articul path-to-folder))
        (loop
           :for pic
           :in (directory (format nil "~a/*.jpg" path-to-folder))
           :do (incf counter)
           (wlog (format nil "Converting file ~a ~%" pic))
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
                         *path-to-product-pics*
                         folder
                         path-art
                         new-name)
                 size-w
                 size-h))
             (wlog (format nil "converted to ~a ~%" new-name)))))
      (wlog (format nil "No such directory! ~%"))))


;;конвертирует картинку до указанных размеров и кладет в указанную папку
(defun rename-convert (path-from path-to &optional size-w size-h)
  (ensure-directories-exist path-to)
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


;;возвращает список всех вложенных файлов
(defun rename-recursive-get-files (path)
  (let ((result (list (pathname path))))
    (loop
       :for file-or-dir
       :in (list-directory path)
       :do (if (directory-pathname-p file-or-dir)
               (setf result (append result (rename-recursive-get-files file-or-dir)))
               (setf result (append result (list file-or-dir)))))
    result))

;;копирование папки и всего ее содержимого

(defun rename-copy-folder (from to)
  (when (not (directory-exists-p from))
      (wlog (format nil  "Directory doesn't exist!~%")))
  (ensure-directories-exist to)
  (wlog (format nil "Start copy folder ~a to ~a~%" from to))
  (let ((files-list (rename-recursive-get-files from))
        (len (length from))
        (counter 0))
    (loop
       :for file-or-dir
       :in files-list
       :do
       (let ((new-to (format nil "~a~a" to (subseq (format nil "~a" file-or-dir) len))))
         (ensure-directories-exist new-to)
         (if (and (not (directory-pathname-p new-to))
                    (not (file-exists-p new-to)))
             (progn
               (copy-file file-or-dir new-to)
               (incf counter))
             (when (not (directory-pathname-p new-to))
               (wlog (format nil "File ~a already exists!~%" new-to))))))
    (wlog (format nil "Copying finished! ~a files were copied.~%" counter))))



;;конвертация всех картинок из папок с артикулами
(defun rename-convert-all (&key (from (format nil "~a/big-images/" *path-to-dropbox*))
                           (backup "/home/webadmin/source-big-images-bkps/"))
  (if (and (directory-exists-p from)
           (directory-exists-p backup))
      (progn
        (wlog (format nil "Start converting from \"~a\"~% Backup folder : \"~a\"~%"
                from backup))
        (loop
           :for folder
           :in (directory (format nil "~a*" from))
           :do (let* ((path (format nil "~a" folder)))
                 (if (directory-exists-p path)
                     (let* ((articul (car (last (split "/" path))))
                            (product (gethash articul *storage*)))
                       (rename-convert-from-folder product path)
                       (rename-copy-folder path (format nil "~a~a/" backup articul))
                       (rename-remove-folder path)
                       )))))

      ;;else
      (wlog (format nil "Folder doesn't exist!~%"))))

;;удаление папки
(defun rename-remove-folder (path)
  (wlog (format nil "Start removing folder ~a~%" path))
  (let ((proc (sb-ext:run-program "/bin/rm"
                            (list "-r"
                                  path)
                            :wait nil :output :stream)))
    (with-open-stream (in (sb-ext:process-output proc))
      (loop
         :for line = (read-line in nil)
         :until (null line)
         :do (print line)))))

(defun rename-remove-product-pics (product)
  (let* ((articul (articul product))
         (dirs (list "big" "goods" "middle" "minigoods" "small"))
         (path-art  (ppcre:regex-replace  "(\\d{1,3})(\\d{0,})" (format nil "~a" articul) "\\1/\\1\\2" )))
    (loop
       :for dir
       :in dirs
       :do (rename-remove-folder (format nil "~a/~a/~a" *path-to-product-pics* dir path-art)))))
