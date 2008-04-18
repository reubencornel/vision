(in-package #:vision)

;; (defparameter *original-image* (imago:read-png "/Users/reuben/img.png"))
;; (defparameter *gray-image* (imago:convert-to-grayscale *original-image*))
;; (defparameter *edge* (imago:edge-detect *gray-image*))
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


(defun can-move-p (direction x y image stack boundary)
  (let* ((intended-x (funcall (get-function (get-x-component direction)) x (get-intensity (get-x-component direction))))
	 (intended-y (funcall (get-function (get-y-component direction)) y (get-intensity (get-y-component direction)))))
;	 (key-presence (gethash 
    (if (and (not (< intended-x 0))
	     (not (< intended-y 0))
	     (not (member (list intended-x intended-y) stack :test #'equal))
	     (not (member (list intended-x intended-y) boundary :test #'equal)))
;	     (not (
	(let ((intensity (imago:gray-intensity (imago:image-pixel image intended-x intended-y))))
	  (if (> intensity threshold-intensity)
	      t
	      nil)))))

   
(defun get-pixels-in-direction (direction x y)
  (let* ((intended-x (funcall (get-function (get-x-component direction)) x (get-intensity (get-x-component direction))))
	 (intended-y (funcall (get-function (get-y-component direction)) y (get-intensity (get-y-component direction)))))
    (list intended-x intended-y)))


(defun dfs(image x y)
  (let ((stack (list (make-point x y)))
	(boundary (list (make-point x y))))
    (while-list-not-empty stack
      (let* ((current-point (first stack))
	     (current-x (first current-point))
	     (current-y (second current-point)))
	;; --- very ugly code below ---
	(cond ((can-move-p *north* current-x current-y image stack boundary)
	       (push (get-pixels-in-direction *north* current-x current-y) stack)
	       (push (get-pixels-in-direction *north* current-x current-y) boundary))	      
	      ((can-move-p *south* current-x current-y image stack boundary)
	       (push (get-pixels-in-direction *south* current-x current-y) stack)
	       (push (get-pixels-in-direction *south* current-x current-y) boundary))
	      ((can-move-p *east* current-x current-y image stack boundary)
	       (push (get-pixels-in-direction *east* current-x current-y) stack)
	       (push (get-pixels-in-direction *east* current-x current-y) boundary))
	      ((can-move-p *west* current-x current-y image stack boundary) 
	       (push (get-pixels-in-direction *west* current-x current-y) stack)
	       (push (get-pixels-in-direction *west* current-x current-y) boundary))
	      ((can-move-p *north-east* current-x current-y image stack boundary)
	       (push (get-pixels-in-direction *north-east* current-x current-y) stack)
	       (push (get-pixels-in-direction *north-east* current-x current-y) boundary))
	      ((can-move-p *north-west* current-x current-y image stack boundary)
	       (push (get-pixels-in-direction *north-west* current-x current-y) stack)
	       (push (get-pixels-in-direction *north-west* current-x current-y) boundary))
	      ((can-move-p *south-east* current-x current-y image stack boundary)
	       (push (get-pixels-in-direction *south-east* current-x current-y) stack)
	       (push (get-pixels-in-direction *south-east* current-x current-y) boundary))
	      ((can-move-p *south-west* current-x current-y image stack boundary)
	       (push (get-pixels-in-direction *south-west* current-x current-y) stack)
	       (push (get-pixels-in-direction *south-west* current-x current-y) boundary))
	      (t 
	       (pop stack)))))
    ;; --- end of ugly code ---
    boundary))

(defun scan(image)
  (with-open-file (op "/Users/reuben/op"
		      :direction :output
		      :if-exists :supersede)    
    (imago:do-image-pixels (image color x y)
      (let ((intensity (imago:gray-intensity (imago:image-pixel image x y))))
	(if (> intensity threshold-intensity)
	   (print (dfs image x y)))))))
;;(scan *edge*)

;(read-image "/Users/reuben/img.png")
;(write-image "/Users/reuben/edge.png")

