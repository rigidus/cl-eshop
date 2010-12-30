(in-package #:my)

(defun setvar (symbol value)
  (proclaim `(special ,symbol))
  (setf (symbol-value symbol) value))

(defun sql-start ()
  (clsql:connect '("localhost" "www320" "root" "bi0Biukou") :database-type :mysql :if-exists :new)
  (clsql:execute-command "SET NAMES utf8"))

(defun sql-query (sql)
  (handler-bind ((CLSQL-SYS:SQL-DATABASE-DATA-ERROR
				  #'(lambda (c)
					  (invoke-restart 'restart-mysql-connection))))
	(restart-case (clsql:query sql)
	  (restart-mysql-connection ()
		(sql-start)
		(clsql:query sql))))
  )

(defun sql-rows (sql)
  (let ((result nil))
	(multiple-value-bind (rows meta)
		(sql-query sql)
	  (eval
	   (read-from-string
		(format nil
				"'~a"
				(mapcar #'(lambda (row)
							(loop
							   for key in meta
							   for val in row
							   collect (typecase val
										 (string  (format nil ":~a \"~a\"" key (my:strip val)))
										 (integer (format nil ":~a ~a"     key val))
										 (real    (format nil ":~a \"~$\"" key val))
										 (t       (format nil ":~a ~a"     key val))
										 )))
						rows)))))))

(defmacro to-hash (-hash -sql)
  `(progn
	 (clrhash ,-hash)
	 (iterate ,-sql
			(lambda ($record)
			  (setf (gethash (car $record) ,-hash) $record)))))

(defmacro hash-elt (-hash -key -field-list)
  `(let ((elt (gethash ,-key ,-hash)))
	 (eval
	  (read-from-string
	   (format nil "'~a"
			   (loop
				  for key in ',-field-list
				  for val in elt
				  collect (typecase val
							(string  (format nil ":~a \"~a\"" key (my:strip val)))
							(integer (format nil ":~a ~a"     key val))
							(real    (format nil ":~a \"~$\"" key val))
							(t       (format nil ":~a ~a"     key val))
							)))))))

(defmacro foreach (-hash -field-list -body)
  (let (($ret nil)
		($i 0))
	(dolist ($field -field-list 'nil)
	  (push `(,$field (nth ,$i $record)) $ret)
	  (setf $i (+ 1 $i)))
	`(maphash #'(lambda ($id $record)
				  (let (,@$ret) ,-body))
			  ,-hash)))

(defun parse-id (id-string)
  (let ((group_id (handler-bind ((SB-INT:SIMPLE-PARSE-ERROR
								  #'(lambda (c)
									  (declare (ignore c))
									  (invoke-restart 'set-nil)))
								 (TYPE-ERROR
								  #'(lambda (c)
									  (declare (ignore c))
									  (invoke-restart 'set-nil)))
								 )
					(restart-case (parse-integer id-string)
					  (set-nil ()
						nil)))))
	group_id))

(defun iterate ($sql $func)
  "Итерирование по таблице базы данных"
  (let (($step 50))
	(do (
		 ($n $step (+ $n $step))
		 ($records
		  (sql-query (format nil "~a LIMIT 0,~a" $sql $step))
		  (sql-query (format nil "~a LIMIT ~a,~a" $sql $n $step))))
		;((or
		;  (null $records)
		;  (>= $n 110)) 'finish)
		((null $records) 'finish)
	  (loop
		 for $record in $records collect $record
		 do (apply $func (list $record))))))

(defun cross ($sql $func)
  (let (($step 50))
	(let (($plist
		   (loop for $n from 0 by $step append
				(multiple-value-bind ($records $fields)
					(my:sql-query (format nil "~a LIMIT ~a,~a" $sql $n $step))
				  ;; (format t "~%~a LIMIT ~a,~a" $sql $n $step)
				  ;; (format t "~%|= ~a,~a" $records $fields)
				  (when (null $records)
					(loop-finish))
				  (loop for $record in $records collect
					   (let (($fld 0))
						 (loop for $value in $record append
							  (prog1
								  (list
								   (intern (string-upcase (nth $fld $fields)) :keyword)
								   $value)
								(setf $fld (+ $fld 1))))))))))
	  (loop for $record in $plist collect $record
		 do (apply $func (list $record))))))

(defun strip ($string)
  (cond ((vectorp $string) (let (($ret nil))
							 (loop
								for x across $string collect x
								do (if (not
										(or
										 (equal x #\')
										 (equal x #\")
										 (equal x #\!)
										 (equal x #\%)
										 (equal x #\\)
										 (equal x #\/)
										 ))
									   (push x $ret)))
							 (coerce (reverse $ret) 'string)))
		((listp $string)   (if (null $string)
							   ""
							   $string))))

(defun stripper ($string)
  (cond ((vectorp $string) (let (($ret nil))
							 (loop
								for x across $string collect x
								do (if (not
										(or
										 (equal x #\')
										 (equal x #\")
										 (equal x #\\)
                                         (equal x #\~)
										 ))
									   (push x $ret)))
                             (let ((ret (coerce (reverse $ret) 'string)))
                               (when (equal 0 (length ret))
                                 (return-from stripper ""))
                               ret)))))


(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))


(defun merge-plists (a b)
  (let* ((result (copy-list a)))
	(loop while (not (null b)) do
		 (setf (getf result (pop b)) (pop b)))
	result))


(defun reverse-plist (inlist)
  (let ((result))
    (loop :for i :in inlist by #'cddr do
       (setf (getf result i) (getf inlist i)))
    result))


(defun get-date-time ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second))
    (format nil
            "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
            year
            month
            date
            hour
            minute)))


(defun numerizable (param)
  (coerce (loop for i across param when (parse-integer (string i) :junk-allowed t) collect i) 'string))


(defun slice (cnt lst)
  (let ((ret))
    (tagbody re
       (push (loop
                :for elt :in lst
                :repeat cnt
                :collect
                (pop lst)) ret)
       (unless (null lst)
         (go re)))
    (reverse ret)))


(defun cut (cnt lst)
  (values (loop
             :for elt :in lst
             :repeat cnt
             :collect
             (pop lst))
          lst))


