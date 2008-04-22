(in-package #:vision)


(defclass image-layer()
  ((name :accessor layer-name)
   (objects :accessor objects :initform '())))


(defun add-object(object layer)
  (setf (objects layer) (append (objects layer) (list object)))
  (setf (parent-layer object) layer))

(defun find-object-with-origin(x y layer)
  (find-if #'(lambda(obj)
	       (and (= x (first (origin-pixels obj)))
		    (= y (second (origin-pixels obj)))))
	   (objects layer)))
