(defpackage #:vision
  (:use #:asdf #:common-lisp))

(in-package #:vision)

(defsystem vision
    :name "vision"
    :depends-on (:imago :lisp-unit)
    :components ((:file "utilities")
		 (:file "imageProcess" :depends-on ("utilities"))
		 (:file "imageObject" :depends-on ("utilities"))
		 (:file "imageLayer" :depends-on ("utilities"))))