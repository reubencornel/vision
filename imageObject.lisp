(in-package #:vision)

(defclass image-object()
  ((image-pixels :accessor image-pixels
		 :initform (make-hash-table :test #'equal))
   (upper-left-coord :accessor upper-left-coord
		     :initform '(0 0))
   (lower-right-coord :accessor lower-right-coord
		      :initform '(0 0))))


(defun add-pixel-to-image(pixel-x pixel-y intensity image-obj)
  (setf (gethash (list pixel-x pixel-y) (image-pixels image-obj)) intensity))

(defun add-pixel-list-to-image(xy intensity image-obj)
  (setf (gethash xy (image-pixels image-obj)) intensity))


(defun pixel-in-image-p(pixel-x pixel-y image-obj)
  (multiple-value-bind (value predicate) (gethash (list pixel-x pixel-y) (image-pixels image-obj))
    (declare (ignore value))
    predicate))

(defun pixel-list(image-obj)
  (let ((pixel-list  '()))
    (maphash #'(lambda(k v)
		 (declare (ignore v))
		 (setf pixel-list (append (list k) pixel-list)))
	     (image-pixels image-obj))
    pixel-list))

(defun intensity-list(image-obj)
  (let ((intensity-list '()))
    (maphash #'(lambda(k v)
		 (declare (ignore k))
		 (setf intensity-list (append (list v) intensity-list)))
	     (image-pixels image-obj))
    intensity-list))
    

(defun sort-pixel-list(pixel-list predicate key)
  (sort pixel-list predicate :key key) )

(defun calc-upper-left-coord(image-obj)
  (let* ((pixel-list (pixel-list image-obj))
	 (x (first (first (sort pixel-list #'< :key #'first))))
	 (y (second (first (sort pixel-list #'< :key #'second)))))
    (setf (upper-left-coord image-obj) (list x y))))

(defun calc-lower-right-coord(image-obj)
  (let* ((pixel-list (pixel-list image-obj))
	 (x (first (first (sort pixel-list #'> :key #'first))))
	 (y (second (first (sort pixel-list #'> :key #'second)))))
    (setf (upper-left-coord image-obj) (list x y))))

