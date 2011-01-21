(in-package #:my)

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


