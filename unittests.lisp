(in-package #:vision)
;(use-package 'lisp-unit)

(lisp-unit:define-test addObject 
  (let ((layer (make-instance 'image-layer))
	(object (make-instance 'image-object)))
    (add-object object layer)
    (lisp-unit:assert-true (= (length (objects layer)) 1))
    (lisp-unit:assert-true (equal (parent-layer object) layer))))

(lisp-unit:define-test add-layer-to-stack
  (let ((stack-obj (make-instance 'image-stack)))
    (lisp-unit:assert-equal 'IMAGE-LAYER (type-of (add-layer (make-instance 'image-layer) stack-obj)))
    (lisp-unit:assert-true (= (length (layers stack-obj)) 1))))

(lisp-unit:define-test for-loop-test
  (let ((l '())
	(x 0))
    (loop-for x 0 5 #'< #'1+ 
	 (setf l (cons x l)))
    (lisp-unit:assert-true (equal '(4 3 2 1 0) l))))

(lisp-unit:define-test coordinate-in-object-check-predicate
  (let ((*image-stack* (make-instance 'image-stack))
	(layer (make-instance 'image-layer)))
    (add-layer layer *image-stack*)
    (lisp-unit:assert-false (coordinate-in-any-object-p 10 10 layer ))
    (read-image "/Users/reuben/img.png")
    (add-object (dfs *edge* 56 5) layer)
    (lisp-unit:assert-false (coordinate-in-any-object-p  10 10 layer))
    (lisp-unit:assert-true (coordinate-in-any-object-p 58 10 layer))))

(lisp-unit:define-test image-scan-check-predicate
  (let ((*image-stack* (make-instance 'image-stack))
	(layer (make-instance 'image-layer)))
    (add-layer layer *image-stack*)
    (read-image "/Users/reuben/img.png")
    (add-object (dfs *edge* 2 21) layer)
    (add-object (dfs *edge* 821 17) layer)
    (add-object (dfs *edge* 917 3) layer)
    (lisp-unit:assert-false (should-perform-dfs-p *edge* 0 0)) ; low threshold level
    (lisp-unit:assert-true (should-perform-dfs-p *edge* 58 10)) ; pixel has not been scanned and has is not on layer
    (add-object (dfs *edge* 56 5) layer)
    (lisp-unit:assert-false (should-perform-dfs-p *edge* 917 3))
    (lisp-unit:assert-false (should-perform-dfs-p *edge* 55 6))
    (lisp-unit:assert-false (should-perform-dfs-p *edge* 55 7))
    (lisp-unit:assert-false (should-perform-dfs-p *edge* 822 17))
    (lisp-unit:assert-false (should-perform-dfs-p *edge* 3 21))))

(lisp-unit:run-tests image-scan-check-predicate)


;    (lisp-unit:assert-false (print (should-perform-dfs-p *edge* 2 21)))
;    (lisp-unit:assert-false (should-perform-dfs-p *edge* 289 119)))) ; pixel is with in the bounding box of an object

;; 285 119
;285  122

(lisp-unit:define-test mergable-p-test
  (let ((image-obj (dfs *edge* 21 13))
	(image-obj-1 (dfs *edge* 22 16))
	(image-obj-2 (dfs *edge* 56 5))
	(image-obj-3 (dfs *edge* 56 5))
	(image-obj-4 (dfs *edge* 62  8)))
;    (print (proper-ratio (length (pixel-list image-obj)) (length (pixel-list image-obj-1))))
    (lisp-unit:assert-true (mergable-p image-obj image-obj-1))
    (lisp-unit:assert-false (mergable-p image-obj-3 image-obj-4))
    (print (proper-ratio (length (pixel-list image-obj-4)) (length (pixel-list image-obj-3))))
    (lisp-unit:assert-false (mergable-p image-obj-1 image-obj-2))))

(lisp-unit:define-test proper-ratio-test
  (let ((x 1)
	(y 2))
    (lisp-unit:assert-equal .5 (proper-ratio x y))
    (lisp-unit:assert-equal .5 (proper-ratio y x))))

