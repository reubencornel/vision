(in-package #:vision)

(defparameter *original-image* '()) ;(imago:read-png "/Users/reuben/img.png"))
(defparameter *gray-image* '()) ; (imago:convert-to-grayscale *original-image*))
(defparameter *edge* '()) ;(imago:edge-detect *gray-image*))
(defparameter threshold-intensity 120) ;67) ;67)
(defparameter *image-stack* (make-instance 'image-stack)) ;variable to store the image stack
(defparameter *mergable-threshold* 6) ; variable that decides if two given image objects can be merged into one object
(defparameter *mergable-ratio* .4)

(defparameter *default-start-x* 284)
(defparameter *default-start-y* 118)
(defparameter *default-end-x* 955)
(defparameter *default-end-y*  461)


;(read-image
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


(defun edge-detect (image)
  (imago:convolve image #2A(( 0  0  0  0  0)
			    ( 0  2  2  2  0)
			    ( 0  2 -16  2  0)
			    ( 0  2  2  2  0)
			    ( 0  0  0  0  0)) 1 0))


(defun read-image(filename)
  (setf *original-image* (imago:read-png filename))
  (setf *gray-image* (imago:convert-to-grayscale *original-image*))
  (setf *edge* (edge-detect *gray-image*)))
;  (setf *edge* (imago:edge-detect *gray-image*)))
  
(defun write-image(filename image)
  (imago:write-png image filename))

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
    (calc-upper-left-coord image-obj)
    (setf (origin-pixels image-obj) (list x y))
    ;;     (print (calc-upper-left-coord image-obj))
    (calc-lower-right-coord image-obj)
    image-obj))

(defun coordinate-in-any-object-p(x y layer)
  (some #'(lambda(obj)
	    (let ((up (upper-left-coord obj))
		  (low (lower-right-coord obj)))
	      (and (and (>= x (first up)) 
			(<= x (first low)))
		   (and (>= y (second up))
			(<= y (second low))))))
	(objects layer)))


(defun should-perform-dfs-p(image x y &optional parent-widget)
;  (print (length (layers *image-stack*)))
  (if (null parent-widget)
      (and (> (imago:gray-intensity (imago:image-pixel image x y)) threshold-intensity)
	   (not  (coordinate-in-any-object-p x y (first (layers *image-stack*)))))
      (and (> (imago:gray-intensity (imago:image-pixel image x y)) threshold-intensity)
	   (not (coordinate-in-any-object-p x y (first (layers *image-stack*))))
	   (not (member (list x y) (pixel-list parent-widget)  :test #'equal)))))=


(defun scan-region(image start-x start-y end-x end-y &optional parent-widget)
  (let ((image-layer (first (layers *image-stack*))))
    (loop-for y start-y end-y #'< #'1+
	 (loop-for x start-x end-x #'< #'1+ 
	      (if (should-perform-dfs-p image x y parent-widget)
		  (let ((obj (dfs image x y)))
		    (setf (parent-widget obj) parent-widget)	
		    (if (and (mergable-with-parent-p obj)
			     (not-null parent-widget))
			(merge-objects obj parent-widget)
;			(if (< (area (upper-left-coord obj) (lower-right-coord obj)) 0)
			(if (not (equal (upper-left-coord obj) (lower-right-coord obj)))
			    (add-object obj image-layer)))))))))

(defun all-objects-scanned-p(layer)
  (every #'(lambda(x)
	     (scan-status x))
	 (objects layer)))


(defun square (x)
  (* x x))

(defun cartesian-distance (x1 y1 x2 y2)
  (round (sqrt (+ (square (- x2 x1))
		  (square (- y2 y1))))))

(defun proper-ratio(number-1 number-2)
  (* 1.0 (if (> number-1 number-2)
	     (/ number-2 number-1)
	     (/ number-1 number-2))))

(defun area(xy1 xy2)
  (let ((length (abs (- (first xy1) (first xy2))))
	(width (abs (- (second xy1) (second xy2)))))
    (* length width)))

(defun mergable-with-parent-p(img)
  (< (area (upper-left-coord img) (lower-right-coord img)) *mergable-threshold*))

;; ;;; TO DO Not complete yet!!
;(defun mergable-p (img-1 img-2)
 ; (let ((up-1  (upper-left-coord img-1))
;	(up-2 (upper-left-coord img-2)))
 ;   (
;    (and (> *mergable-ratio*   (print (proper-ratio (length (pixel-list img-1)) (length (pixel-list img-2)))))
;	 (> (print (cartesian-distance (first up-1) (second up-1) (first up-2) (second up-2))) *mergable-threshold*))))

(defun initialize-first-layer(image)
  "Initialize the first layer to all the elements in the "
  (setf *image-stack* (make-instance 'image-stack))
  (add-layer (make-instance 'image-layer) *image-stack*)
  (scan-region image *default-start-x* *default-start-y* *default-end-x* *default-end-y*))

(defun list-of-objects-to-be-scanned(layer)
  (remove-if #'(lambda(x)
		 (scan-status x))
	     (objects layer)))

(defun scan-all-objects(list-of-objects image)
  (mapcar #'(lambda(x)
	      (let ((up (upper-left-coord x))
		    (low (lower-right-coord x)))
		(setf (scan-status x) t)
		(scan-region image (first up) (second up) (first low) (second low) x)))
	  list-of-objects))

(defparameter *val* 0)
(defun complete-scan(image)
  (if (or 
       (> (length (objects (first (layers *image-stack*)))) 5) ; If there are about 5 objects on the top of the stack then we are done
      ;; cause the rest would be repetitions.
       (= *val* 0))
      (let ((list-of-objects (list-of-objects-to-be-scanned (first (layers *image-stack*))))
	    (new-layer (make-instance 'image-layer)))
	(setf *val* 1)
	(add-layer new-layer *image-stack*)
	(scan-all-objects list-of-objects image)
	(complete-scan image))))

(defun scan (image)
  (initialize-first-layer image)
  (complete-scan image))

(defun new-file-name(path x-pos y-pos)
  (format nil "~a\/~a ~a.png" path x-pos y-pos))

(defun write-layer-images(path layer)
  (mapcar #'(lambda(x)
	      (let* ((start-x (first (origin-pixels x)))
		     (start-y (second (origin-pixels x)))
		     (file-name (new-file-name path start-x start-y)))
		(write-image file-name 
			     (create-image 
			      (find-object-with-origin start-x start-y layer)
			      *edge*))))
	  
	  (objects layer)))
;(write-layer-images "/Users/reuben/images" (first (layers *image-stack*)))