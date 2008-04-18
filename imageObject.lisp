(in-package #:vision)

(defclass image-object()
  ((image-pixels :accessor image-pixels
		 :initform (make-hash-table :test #'equal))
   (upper-left-coord :accessor upper-left-coord
		     :initform '(0 0))
   (lower-right-coord :accessor lower-right-coord
		      :initform '(0 0))))
