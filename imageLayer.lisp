(in-package #:vision)


(defclass image-layer()
  ((name :accessor layer-name)
   (objects :accessor objects :initform '())))


(defun add-object(object layer)
  (setf (objects layer) (append (objects layer) (list object)))
  (setf (parent-layer object) layer))
