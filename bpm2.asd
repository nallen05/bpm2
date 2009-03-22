;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-

(in-package :cl-user)

(defpackage :bpm2-system (:use :asdf :cl))
(in-package :bpm2-system)

(defsystem bpm2
  :name "bpm2"
  :description "backslash pattern matching language"
  :author "Nick Allen <nallen05@gmail.com>"
  :depends-on (:cl-ppcre :f-underscore)
  :components
  ((:module :bpm2
	    :components ((:file "core")
			 (:file "definitions"))
	    :serial t)))
