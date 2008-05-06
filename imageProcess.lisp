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

(defparameter *north* (list (list #'+ 0) (list #'- 1)))
(defparameter *south* (list (list #'+ 0) (list #'+ 1)))
(defparameter *east* (list (list #'+ 1) (list #'+ 0)))
(defparameter *west* (list (list #'- 1) (list #'+ 0)))
(defparameter *north-east* (list (list #'+ 1) (list #'- 1)))
(defparameter *north-west* (list (list #'- 1) (list #'- 1)))
(defparameter *south-east* (list (list #'+ 1) (list #'+ 1)))
(defparameter *south-west* (list (list #'- 1) (list #'+ 1)))

;(read-image "/Users/reuben/img.png")
;(scan *edge*)

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
		    (if (not-null parent-widget)
			(add-child-widget obj parent-widget))
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

(defun number-of-pixels(number image-obj key-function)
  (let ((number-of-pixels 0))
    (maphash #'(lambda(k v)
;		 (declare (ignore v))
		 (if (and (= number (funcall key-function k))
			  (>= v threshold-intensity))
		     (incf number-of-pixels)))
	     (image-pixels image-obj))
    number-of-pixels))

(defun number-of-x-pixels(number image-obj)
  (number-of-pixels number image-obj #'first))

(defun number-of-y-pixels(number image-obj)
  (number-of-pixels number image-obj #'second))

(defun ratio-of-x-isto-y(image-obj)
  (let* ((x-dist (abs (- (first (lower-right-coord image-obj) )
		    (First (upper-left-coord image-obj)))))
	(y-dist (abs (- (second (lower-right-coord image-obj))
		   (second (upper-left-coord image-obj)))))
	 (ratio (/ x-dist y-dist)))
    (cond ((and (> ratio .75)
		(<= ratio 1.5)) 1)
	  ((> ratio 1.5) 2)
	  (t 0))))

(defun data(image-obj)
  (list (list 'left-x (number-of-x-pixels (first (upper-left-coord image-obj)) image-obj))
	(list 'upper-y (number-of-y-pixels (second (upper-left-coord image-obj)) image-obj))
	(list 'right-x (number-of-x-pixels (first (lower-right-coord image-obj)) image-obj))
	(list 'lower-y (number-of-y-pixels (second (lower-right-coord image-obj)) image-obj))
	(list 'x-to-y (ratio-of-x-isto-y image-obj))
	(list 'x-dist (abs (- (first (lower-right-coord image-obj) )
			      (First (upper-left-coord image-obj)))))
	(list 'y-dist (abs (- (second (lower-right-coord image-obj))
			      (second (upper-left-coord image-obj)))))
	(list 'feature (find-if #'(lambda(x)
				    (not (null (identification x))))
				(children-widgets image-obj)))))

(defun generate-training-data (class-name data-list)
  (append (list 'push)
	  (list (append (list 'define-instance class-name) data-list))
	  (list 'inst-list)))

(defun generate-classification-parameters(data-list)
  (append (list 'decisiontree:define-instance '?) data-list))

(defparameter  inst-list '())
(let ()
  (PUSH
   (decisiontree:DEFINE-INSTANCE MAGNIFYING-GLASS (LEFT-X 4) (UPPER-Y 4) (RIGHT-X 3)
				 (LOWER-Y 1) (X-TO-Y 1) (X-DIST 11) (Y-DIST 12) (FEATURE NIL))
   INST-LIST)
  (PUSH
   (decisiontree:DEFINE-INSTANCE TRIANGLE (LEFT-X 1) (UPPER-Y 5) (RIGHT-X 1) (LOWER-Y 1)
				 (X-TO-Y 1) (X-DIST 4) (Y-DIST 3) (FEATURE NIL))
   INST-LIST)
  (PUSH
  (decisiontree:DEFINE-INSTANCE TRIANGLE (LEFT-X 1) (UPPER-Y 1) (RIGHT-X 1) (LOWER-Y 5)
   (X-TO-Y 1) (X-DIST 4) (Y-DIST 3) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE QUESTION-MARK (LEFT-X 2) (UPPER-Y 5) (RIGHT-X 2) (LOWER-Y 2)
   (X-TO-Y 1) (X-DIST 5) (Y-DIST 6) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE BUTTON (LEFT-X 19) (UPPER-Y 51) (RIGHT-X 19) (LOWER-Y 49)
   (X-TO-Y 2) (X-DIST 52) (Y-DIST 21) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE BUTTON (LEFT-X 19) (UPPER-Y 78) (RIGHT-X 19) (LOWER-Y 76)
   (X-TO-Y 2) (X-DIST 79) (Y-DIST 21) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE SLIDE-BAR (LEFT-X 3) (UPPER-Y 13) (RIGHT-X 3) (LOWER-Y 1)
   (X-TO-Y 2) (X-DIST 203) (Y-DIST 17) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE SEARCH-BAR (LEFT-X 9) (UPPER-Y 136) (RIGHT-X 9)
   (LOWER-Y 134) (X-TO-Y 2) (X-DIST 149) (Y-DIST 22) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE DROP-DOWN (LEFT-X 17) (UPPER-Y 118) (RIGHT-X 17)
   (LOWER-Y 118) (X-TO-Y 2) (X-DIST 121) (Y-DIST 20) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE CHECK-BOX (LEFT-X 13) (UPPER-Y 12) (RIGHT-X 13) (LOWER-Y 14)
   (X-TO-Y 1) (X-DIST 13) (Y-DIST 13) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE CHECKED-CHECK-BOX (LEFT-X 12) (UPPER-Y 3) (RIGHT-X 1)
   (LOWER-Y 12) (X-TO-Y 1) (X-DIST 14) (Y-DIST 15) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE CHECKED-CHECK-BOX (LEFT-X 12) (UPPER-Y 3) (RIGHT-X 1)
   (LOWER-Y 12) (X-TO-Y 1) (X-DIST 14) (Y-DIST 15) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE RADIO-BUTTON (LEFT-X 8) (UPPER-Y 6) (RIGHT-X 8) (LOWER-Y 8)
   (X-TO-Y 1) (X-DIST 13) (Y-DIST 14) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE RADIO-BUTTON (LEFT-X 8) (UPPER-Y 6) (RIGHT-X 8) (LOWER-Y 8)
   (X-TO-Y 1) (X-DIST 13) (Y-DIST 14) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE BUTTON (LEFT-X 5) (UPPER-Y 6) (RIGHT-X 5) (LOWER-Y 6)
   (X-TO-Y 1) (X-DIST 13) (Y-DIST 13) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE BUTTON (LEFT-X 6) (UPPER-Y 6) (RIGHT-X 6) (LOWER-Y 6)
   (X-TO-Y 1) (X-DIST 13) (Y-DIST 13) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE BUTTON (LEFT-X 6) (UPPER-Y 6) (RIGHT-X 6) (LOWER-Y 6)
   (X-TO-Y 1) (X-DIST 13) (Y-DIST 13) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE HELP-BUTTON (LEFT-X 8) (UPPER-Y 7) (RIGHT-X 8) (LOWER-Y 7)
   (X-TO-Y 1) (X-DIST 18) (Y-DIST 19) (FEATURE NIL))
  INST-LIST)
(PUSH
  (decisiontree:DEFINE-INSTANCE MAGNIFYING-GLASS (LEFT-X 4) (UPPER-Y 4) (RIGHT-X 3)
   (LOWER-Y 1) (X-TO-Y 1) (X-DIST 11) (Y-DIST 12) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE TRIANGLE (LEFT-X 1) (UPPER-Y 5) (RIGHT-X 1) (LOWER-Y 1)
   (X-TO-Y 1) (X-DIST 4) (Y-DIST 3) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE TRIANGLE (LEFT-X 1) (UPPER-Y 1) (RIGHT-X 1) (LOWER-Y 5)
   (X-TO-Y 1) (X-DIST 4) (Y-DIST 3) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE QUESTION-MARK (LEFT-X 2) (UPPER-Y 5) (RIGHT-X 2) (LOWER-Y 2)
   (X-TO-Y 1) (X-DIST 5) (Y-DIST 6) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE BUTTON (LEFT-X 19) (UPPER-Y 51) (RIGHT-X 19) (LOWER-Y 49)
   (X-TO-Y 2) (X-DIST 52) (Y-DIST 21) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE BUTTON (LEFT-X 19) (UPPER-Y 78) (RIGHT-X 19) (LOWER-Y 76)
   (X-TO-Y 2) (X-DIST 79) (Y-DIST 21) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE SLIDE-BAR (LEFT-X 3) (UPPER-Y 13) (RIGHT-X 3) (LOWER-Y 1)
   (X-TO-Y 2) (X-DIST 203) (Y-DIST 17) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE SEARCH-BAR (LEFT-X 9) (UPPER-Y 136) (RIGHT-X 9)
   (LOWER-Y 134) (X-TO-Y 2) (X-DIST 149) (Y-DIST 22) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE DROP-DOWN (LEFT-X 17) (UPPER-Y 118) (RIGHT-X 17)
   (LOWER-Y 118) (X-TO-Y 2) (X-DIST 121) (Y-DIST 20) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE CHECK-BOX (LEFT-X 13) (UPPER-Y 12) (RIGHT-X 13) (LOWER-Y 14)
   (X-TO-Y 1) (X-DIST 13) (Y-DIST 13) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE CHECKED-CHECK-BOX (LEFT-X 12) (UPPER-Y 3) (RIGHT-X 1)
   (LOWER-Y 12) (X-TO-Y 1) (X-DIST 14) (Y-DIST 15) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE CHECKED-CHECK-BOX (LEFT-X 12) (UPPER-Y 3) (RIGHT-X 1)
   (LOWER-Y 12) (X-TO-Y 1) (X-DIST 14) (Y-DIST 15) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE RADIO-BUTTON (LEFT-X 8) (UPPER-Y 6) (RIGHT-X 8) (LOWER-Y 8)
   (X-TO-Y 1) (X-DIST 13) (Y-DIST 14) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE RADIO-BUTTON (LEFT-X 8) (UPPER-Y 6) (RIGHT-X 8) (LOWER-Y 8)
   (X-TO-Y 1) (X-DIST 13) (Y-DIST 14) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE BUTTON (LEFT-X 5) (UPPER-Y 6) (RIGHT-X 5) (LOWER-Y 6)
   (X-TO-Y 1) (X-DIST 13) (Y-DIST 13) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE BUTTON (LEFT-X 6) (UPPER-Y 6) (RIGHT-X 6) (LOWER-Y 6)
   (X-TO-Y 1) (X-DIST 13) (Y-DIST 13) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE BUTTON (LEFT-X 6) (UPPER-Y 6) (RIGHT-X 6) (LOWER-Y 6)
   (X-TO-Y 1) (X-DIST 13) (Y-DIST 13) (FEATURE NIL))
  INST-LIST)
 (PUSH
  (decisiontree:DEFINE-INSTANCE HELP-BUTTON (LEFT-X 8) (UPPER-Y 7) (RIGHT-X 8) (LOWER-Y 7)
   (X-TO-Y 1) (X-DIST 18) (Y-DIST 19) (FEATURE NIL))
  INST-LIST))

(defparameter *root-node* (decisiontree:create-classifier inst-list))

(defun run-project(filename)
  (read-image filename)
  (scan *edge*)
  (identify (reverse (layers *image-stack*))))


(defun identify(layer-list &optional (stream t))
  (cond ((null layer-list) nil)
	(t (let ((current-layer (first layer-list)))
	     (mapcar #'(lambda(x)
			 (let ((classification (decisiontree:classify *root-node* (eval (generate-classification-parameters (data x))))))
			   (if (not-null classification)
			       (let ()
				 (print classification)
				 (print (origin-pixels x))))))
		     (objects current-layer))
	     (identify (cdr layer-list) stream)))))
		       

;; (decisiontree:classify *root-node* (decisiontree:define-instance ? (left-x 50) (upper-y 3) (right-x 1) (lower-y 12) (x-to-y 1) (x-dist 15) (y-dist 16) (Feature nil)))

;; (print (decisiontree:attribute-name *root-node*))
;; (defparameter *root-node* (print (gethash 8 (decisiontree:subtree-hash *root-node*))))
;; (print (decisiontree:attribute-name *root-node*))

;; (defparameter *root-node* (print (gethash 19 (decisiontree:subtree-hash *root-node*))))
;; (print (decisiontree:label *root-node*))

;; (maphash #'(lambda(k v)
;; ;	     (attribute-name 
;; 	     (print k)
;; 	     (print v))
;; 	 (decisiontree:subtree-hash *root-node*))



