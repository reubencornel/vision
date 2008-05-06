(asdf:oos 'asdf:load-op 'decisiontree)
(asdf:oos 'asdf:load-op 'vision)

(in-package #:vision)

(read-image "/Users/reuben/img.png")
(initialize-first-layer *edge*)

;; (defun new-name()
;;   (format nil "~a" (gensym)))

;; (defun new-file-name()
;;   (concatenate 'string "/Users/reuben/images2/" (new-name) ".png"))

(write-layer-images  "/Users/reuben/images2" (first (layers *image-stack*)))

(edge-count (find-object-with-origin 287 119 (first (layers *image-stack*))))

(defparameter *edge-length* 3)
(defun generate-input-vector(image-object)
  )

(defun edge-count(image-object)
  (let ((horizontal-hash (make-hash-table :test #'equal))
	(vertical-hash (make-hash-table :test #'equal))
	(horizontal-edge-count 0)
	(vertical-edge-count 0))
  (maphash #'(lambda(k v)
	       (if (gethash (first k) vertical-hash)
		   (incf (gethash (first k) vertical-hash))
		   (setf (gethash (first k) vertical-hash) 0))
	       (if (gethash (second k) horizontal-hash)
		   (incf (gethash (second k) horizontal-hash))
		   (setf (gethash (second k) horizontal-hash) 0)))
	   (image-pixels image-object))
  (maphash #'(lambda(k v)

	       (if (> v *edge-length*)
		   (incf horizontal-edge-count)))
	   horizontal-hash)
  (maphash #'(lambda(k v)
	       (if (> v *edge-length*)
		   (incf vertical-edge-count)))
	   vertical-hash)
  (list horizontal-edge-count vertical-edge-count)))



