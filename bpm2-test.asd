;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-

(in-package :cl-user)

(defpackage :bpm2-system (:use :asdf :cl))
(in-package :bpm2-system)

(defsystem bpm2
  :name "bpm2"
  :description "the \"backslash pattern matching language\""
  :author "Nick Allen <nallen05@gmail.com>"
  :depends-on (:bpm2 :ptester)
  :components
  ((:module :bpm2-test
	    :components ((:file "tests"))
	    :serial t)))
