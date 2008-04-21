(in-package #:vision)

(defmacro while-list-not-empty(list &body body)
  `(loop until (null ,list)
	do (progn
	     ,@body)))

(defmacro loop-for(variable start end predicate step &body body)
  (let ((start-lim (gensym))
	(end-lim (gensym)))
    `(let* ;((,variable ,start)
	   ((,start-lim ,start)
	    (,end-lim ,end)
	    (,variable ,start-lim))
       (loop until (not (funcall ,predicate ,variable ,end-lim))
	    do (let ()
		 ,@body
		 (setf ,variable
		       (funcall ,step ,variable)))))))

