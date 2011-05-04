(in-package #:eshop)


(defparameter *group-skls* (make-hash-table :test #'equal))

(defun group-skls-make ()
  "Строим *yml-group-ids*"
    *group-skls*)

(defun sklonenie (name skl)
  (setf name (string-downcase name))
  name)
