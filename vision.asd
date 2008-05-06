(defpackage #:vision
  (:use #:asdf #:common-lisp))

(in-package #:vision)

(defsystem vision
    :name "vision"
    :depends-on (:imago :lisp-unit :decisiontree)
    :components ((:file "utilities")
		 (:file "imageProcess" :depends-on ("utilities" "imageObject" "imageLayer" "imageStack"))
		 (:file "imageObject" :depends-on ("utilities"))
		 (:file "imageLayer" :depends-on ("utilities" "imageObject"))
		 (:file "imageStack" :depends-on ("utilities" "imageLayer"))
		 (:file "unittests" :depends-on ("utilities" "imageProcess" "imageObject" "imageLayer" "imageStack"))))
