(in-package #:vision)

(defclass image-object()
  ((image-pixels :accessor image-pixels
		 :initform (make-hash-table :test #'equal))
   (scan-status :accessor scan-status
		:initform nil)
   (parent-widget :accessor parent-widget
		  :initform nil)
   (parent-layer :accessor parent-layer)
   (children-widgets :accessor children-widgets
		     :initform nil)
   (upper-left-coord :accessor upper-left-coord
		     :initform '(0 0))
   (origin-pixels :accessor origin-pixels
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

(defmethod add-child-widget((child-widget image-object)
			    (parent-widget image-object))
  (setf (children-widgets parent-widget)
	(cons child-widget (children-widgets parent-widget))))

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
  (let* ;((pixel-list (pixel-list image-obj))
	 ((x (first (first (sort (pixel-list image-obj) #'< :key #'first))))
	 (y (second (first (sort (pixel-list image-obj) #'< :key #'second)))))
    (setf (upper-left-coord image-obj) (list x y))))

(defun calc-lower-right-coord(image-obj)
  (let* ((x (first (first (sort (pixel-list image-obj) #'> :key #'first))))
	 (y (second (first (sort (pixel-list image-obj) #'> :key #'second)))))
    (setf (lower-right-coord image-obj) (list x y))))

(defun merge-objects(to-be-merged parent)
  (let ((new-obj (make-instance 'image-object)))
    (maphash #'(lambda(k v)
		 (setf (gethash k (image-pixels new-obj)) v))
	     (image-pixels parent))
    (maphash #'(lambda(k v)
		 (setf (gethash k (image-pixels new-obj)) v))
	     (image-pixels to-be-merged))
    (calc-lower-right-coord new-obj)
    (calc-upper-left-coord new-obj)))

(defmethod calc-image-object-width((object image-object))
  (abs (- (first (upper-left-coord object)) (first (lower-right-coord object)))))

(defmethod calc-image-object-height((object image-object))
  (abs (- (Second (upper-left-coord object)) (second (lower-right-coord object)))))


(defmethod create-image(object image)
  nil)


(defmethod create-image((object image-object) (image imago:grayscale-image))
  (let* ((h (+ 1 (calc-image-object-height object)))
	 (w (+ 1 (calc-image-object-width object)))
	 (x (first (upper-left-coord object)))
	 (y (second (upper-left-coord object)))
	 (new-image-object (make-instance 'imago:grayscale-image :height h :width w)))
    (imago:copy new-image-object image :dest-x 0 :dest-y 0 :src-x x :src-y y :width w :height h)
    new-image-object))


(defun new-name()
  (format nil "~a" (gensym)))

(defun new-file-name()
  (concatenate 'string "/Users/reuben/images1/" (new-name) ".png"))

;; (let* ((layer (first (last (layers *image-stack*)))))
;;  (mapcar #'(lambda(x)
;; 	     (write-image (new-file-name) 
;; 			   (create-image (find-object-with-origin (first (origin-pixels x))
;; 								  (second (origin-pixels x))
;; 								  layer)
;; 					 *edge*)))
					 
;; 	  (objects layer)))
;									 
; (Read-image "/Users/reuben/img.png")
; (initialize-first-layer *edge*)
;; (let* ((layer (first (layers *image-stack*))))
;;   (mapcar #'(lambda(x)
;; 	      (write-image (new-file-name)
;; 			   (create-image (find-object-with-origin (first (origin-pixels x))
;; 								  (second (origin-pixels x))
;; 								  layer) 
;; 					 *edge*)))
;; 	  (objects layer)))
					 
;;(let* ((layer (first (last (layers *image-stack*)))) 
; ;      (image-obj (find-object-with-origin 62 8 layer))
;  ;     (gray-image (create-image image-obj *edge*)))
;  ;(write-image "/Users/reuben/smallimg.png" gray-image))
