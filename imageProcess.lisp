(in-package #:vision)

(defparameter *original-image* '()) ;(imago:read-png "/Users/reuben/img.png"))
(defparameter *gray-image* '()) ; (imago:convert-to-grayscale *original-image*))
(defparameter *edge* '()) ;(imago:edge-detect *gray-image*))
(defparameter threshold-intensity 67)


(defparameter *north* (list (list #'+ 0) (list #'- 1)))
(defparameter *south* (list (list #'+ 0) (list #'+ 1)))
(defparameter *east* (list (list #'+ 1) (list #'+ 0)))
(defparameter *west* (list (list #'- 1) (list #'+ 0)))
(defparameter *north-east* (list (list #'+ 1) (list #'- 1)))
(defparameter *north-west* (list (list #'- 1) (list #'- 1)))
(defparameter *south-east* (list (list #'+ 1) (list #'+ 1)))
(defparameter *south-west* (list (list #'- 1) (list #'+ 1)))


(defun make-point (x y)
  (list x y))

(defun read-image(filename)
  (setf *original-image* (imago:read-png filename))
  (setf *gray-image* (imago:convert-to-grayscale *original-image*))
  (setf *edge* (imago:edge-detect *gray-image*)))
  
(defun write-image(filename)
  (imago:write-png *edge* filename))

(defun get-x-component(direction)
  (first direction))
(defun get-y-component(direction)
  (second direction))

(defun get-function(component)
  (first component))
(defun get-intensity(component)
  (second component))


(defun can-move-p (direction x y image stack image-obj)
  (let* ((intended-x (funcall (get-function (get-x-component direction)) x (get-intensity (get-x-component direction))))
	 (intended-y (funcall (get-function (get-y-component direction)) y (get-intensity (get-y-component direction)))))
    (if (and (not (< intended-x 0))
	     (not (< intended-y 0))
	     (not (member (list intended-x intended-y) stack :test #'equal))
	     (not (pixel-in-image-p intended-x intended-y image-obj)))
;	     (not (member (list intended-x intended-y) boundary :test #'equal)))
	(let ((intensity (imago:gray-intensity (imago:image-pixel image intended-x intended-y))))
	  (if (> intensity threshold-intensity)
	      t
	      nil)))))

   
(defun get-pixels-in-direction (direction x y)
  (let* ((intended-x (funcall (get-function (get-x-component direction)) x (get-intensity (get-x-component direction))))
	 (intended-y (funcall (get-function (get-y-component direction)) y (get-intensity (get-y-component direction)))))
    (list intended-x intended-y)))

(defun perform-dfs-bookkeeping(direction x y image image-obj stack) 
  (let ((pixels (get-pixels-in-direction direction x y)))
    (add-pixel-list-to-image pixels
			     (imago:gray-intensity (imago:image-pixel image x y))
			     image-obj)
    (push pixels stack)
    stack))

(defun dfs(image x y)
  (let ((stack (list (make-point x y)))
	(image-obj (make-instance 'image-object)))
    (add-pixel-to-image x y (imago:gray-intensity (imago:image-pixel image x y)) image-obj)
    (while-list-not-empty stack
      (let* ((current-point (first stack))
	     (current-x (first current-point))
	     (current-y (second current-point)))
	(cond ((can-move-p *north* current-x current-y image stack image-obj)
	       (setf stack (perform-dfs-bookkeeping *north* current-x current-y image image-obj stack)))
	      ((can-move-p *south* current-x current-y image stack image-obj)
	       (setf stack (perform-dfs-bookkeeping *south* current-x current-y image image-obj stack)))
	      ((can-move-p *east* current-x current-y image stack image-obj)
	       (setf stack (perform-dfs-bookkeeping *east* current-x current-y image image-obj stack)))
	      ((can-move-p *west* current-x current-y image stack image-obj) 
	       (setf stack (perform-dfs-bookkeeping *west* current-x current-y image image-obj stack)))
	      ((can-move-p *north-east* current-x current-y image stack image-obj)
	       (setf stack (perform-dfs-bookkeeping *north-east* current-x current-y image image-obj stack)))
	      ((can-move-p *north-west* current-x current-y image stack image-obj)
	       (setf stack (perform-dfs-bookkeeping *north-west* current-x current-y image image-obj stack)))
	      ((can-move-p *south-east* current-x current-y image stack image-obj)
	       (setf stack (perform-dfs-bookkeeping *south-east* current-x current-y image image-obj stack)))
	      ((can-move-p *south-west* current-x current-y image stack image-obj)
	       (setf stack (perform-dfs-bookkeeping *south-west* current-x current-y image image-obj stack)))
	      (t 
	       (pop stack)))))
    image-obj))

(defun scan(image)
  (with-open-file (op "/Users/reuben/op"
		      :direction :output
		      :if-exists :supersede)    
    (imago:do-image-pixels (image color x y)
      (let ((intensity (imago:gray-intensity (imago:image-pixel image x y))))
	(if (> intensity threshold-intensity)
	   (print (dfs image x y)))))))

;(read-image "/Users/reuben/img.png")

;(let ((image-obj (dfs *edge* 56 5)))
;  (print (sort-pixel-list (pixel-list image-obj) #'>))
;  (print (intensity-list image-obj))
;  (print (calc-upper-left-coord image-obj))
;  (print (calc-lower-right-coord image-obj)))

