
(defpackage :bpm2-test
  (:use :cl :bpm2))

(in-package :bpm2-test)

(defmacro f_ (&body body)
  "a LAMBDA that takes 1 argument: `_'"
  `(lambda (_) ,@body))

(defmacro m (macro-lambda-list &body body)
  "a LAMBDA that has a macro-lambda-list instead of an ordinary lambda-list"
  (let ((% (gensym "macro-lambda-list")))
    (if (and (stringp (first body))
	     (rest body))

	;; there is a doc string
	`(lambda (&rest ,%)
	   ,(first body)
	   (destructuring-bind ,macro-lambda-list ,%
	     ,@(rest body)))

	;; there is no doc string
	`(lambda (&rest ,%)
	   (destructuring-bind ,macro-lambda-list ,%
	     ,@body)))))

(defmacro testit (/form alt-//name &rest tests)
  `(progn
     ,@(mapcar (m ((in . out))
		 `(ptester:test ,out (funcall ,/form ,in)))
	       tests)
     ,@(if alt-//name
	   (mapcar (m ((in . out))
		     `(ptester:test ,out (,alt-//name ,in ,@(rest /form))))
		   tests))))
	 
; PRIMITIVES

; lispp

(testit (/lispp 'numberp)
	nil
	(5 . t)
	("5" . nil))

(testit (/lispp 'numberp
		'integerp)
	nil
	(5 . t)
	(5.1 . nil))

;; //LISPP doesn't follow normal /FOO vs //FOO naming pattern
(ptester:test t (//lispp (> 2 1)))
(ptester:test nil (//lispp (> 1 2)))
(ptester:test t (//lispp (> 2 1)
			 (= 1 2)))
(ptester:test nil (//lispp (> 2 1)
			   (= 2 1)))

;lisp

;; /LISP and //LISPP are hard to test

(ptester:test '(1 2 3)
	      (let (x)
		(funcall (/lisp (f_ 

	      