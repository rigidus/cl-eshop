;;;; option.lisp
;;;;
;;;; This file is part of the eshop project,
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(in-package #:option)

;; Класс OPTION
(defclass option ()
  ((name           :initarg :name            :initform ""        :accessor name)
   (value          :initarg :value           :initform ""        :accessor value)
   (optype         :initarg :optype          :initform nil       :accessor optype)
   (boolflag       :initarg :boolflag        :initform nil       :accessor boolflag)
   ))


(defmethod show ((object option))
  (product:option (list :name (name object)
                        :value (if (and (equal (optype object) :bool)
                                        (boolflag object))
                                   (format nil "~a ~a" "<img src=\"img/ok.png\" alt=\"*\"/>" (value object))
                                   (value object)))))


(defmethod serialize ((object option))
  (format nil "~%               {~%                  \"name\": \"~a\",~%                  \"value\": \"~a\",~%                  \"optype\": ~a,~%                  \"boolflag\": ~a~%               }"
          (stripper (name object))
          (stripper (value object))
          (encode-json-to-string (optype object))
          (encode-json-to-string (boolflag object))
          ))


(defun unserialize (in)
  (make-instance 'option
                 :name (cdr (assoc :name in))
                 :value (format nil "~a" (cdr (assoc :value in)))
                 :optype (cdr (assoc :optype in))
                 :boolflag (cdr (assoc :boolflag in))))
