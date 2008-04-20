(in-package #:vision)

(defclass image-stack()
  ((layers :accessor layers 
	   :initform '())))

(defun add-layer(stack-obj)
  (let ((new-layer (make-instance 'layer)))
    (push new-layer (layers stack-obj))
    new-layer))

