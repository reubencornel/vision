(in-package #:vision)

(defclass image-stack()
  ((layers :accessor layers 
	   :initform '())))


(defun add-layer (layer stack-obj)
  (setf (layers stack-obj) (push layer (layers stack-obj)))
  layer)