(in-package #:eshop)

(defun store-products ()
  (maphash #'(lambda (p)
               ())
   *storage*))

(defun copy-instance (i)
  (let ((i-class (class-of i)))
    (with-lock (i-class :READ-ALLOWED T :MODIFY-ALLOWED NIL)
      (loop
         with c = (allocate-instance i-class)
         for sd in (mop:class-slots i-class)
         for sn = (mop:slot-definition-name sd)
         when (slot-boundp i sn)
         do (setf (slot-value c sn)
                  (slot-value i sn))
         finally (return c)))))
