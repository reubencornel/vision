(in-package #:vision)

(defmacro while-list-not-empty(list &body body)
  `(loop until (null ,list)
	do (progn
	     ,@body)))
