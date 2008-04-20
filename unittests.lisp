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
    (lisp-unit:assert-equal 'LAYER (type-of (add-layer stack-obj)))
    (lisp-unit:assert-true (= (length (layers stack-obj)) 1))))

(lisp-unit:run-tests)
  
