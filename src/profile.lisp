(in-package #:eshop)

;; тест по времени работы выборки
(defmacro format-runtime (time-var &rest body)
  `(let ((start-time (get-internal-real-time)))
     (progn ,@body)
     (setf ,time-var (- (get-internal-real-time) start-time))))


(defun start-to-profile ()
  (sb-profile:reset)
  (sb-profile:profile default-page))
