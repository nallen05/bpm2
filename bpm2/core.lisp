;
; http://github.com/nallen05/bpm2
;

(defpackage :bpm2
  (:use :cl :f-underscore)
  (:export ;; primitive patterns
	   :/lispp
	   :/lisp
	   :/and
	   :/or

	   ;; primitive clauses
	   ://lispp
	   ://lisp
	   ://and
	   ://or

	   ;; defining new bpm2 macros
           :def-bpm2-macro
	   :bpm2-macroexpand-1
	   :bpm2-macroexpand

	   ;; built-in bpm2 macros are exported from "definitions.lisp"
	   ))

(in-package :bpm2)

; anaphoric IF macro from... all the books on lisp...

(defmacro aif (test &optional (then 'it) else)
  `(let ((it ,test))
     (if it
	 ,then
	 ,else)))

; compiler

(defgeneric bpm2-compile (pattern &key seen-vars
				       next-compiler
				       thing-in-question
				  &allow-other-keys))

; built-in methods

(defmethod bpm2-compile ((symbol symbol)
			 &key seen-vars
			      next-compiler
			      thing-in-question
			 &allow-other-keys)
  (if (not (cl-ppcre:scan "^\\?" (string symbol)))

      ;; then `SYMBOL' is a symbol literal, not a logic var
      `(if (eql ,thing-in-question ',symbol)
	   ,(funcall next-compiler seen-vars))

      (if (cl-ppcre:scan "^\\?$" (string symbol))

	  ;; then `SYMBOL' is the wildcard
	  (funcall next-compiler seen-vars)

	  (if (find symbol seen-vars)

	      ;; then `SYMBOL' is an already-bound logic var
	      `(if (equal ,thing-in-question ,symbol)
		   ,(funcall next-compiler seen-vars))

	      ;; then `SYMBOL' is a logic var that hasn't been seen before
	      `(let ((,symbol ,thing-in-question))
		 ,(funcall next-compiler (cons symbol seen-vars)))))))

(defmethod bpm2-compile ((x null)
			 &key seen-vars
			      thing-in-question
			      next-compiler
			 &allow-other-keys)
  `(unless ,thing-in-question
     ,(funcall next-compiler seen-vars)))

(defmethod bpm2-compile ((list list)
			 &rest kwd-args
			 &key seen-vars
			      thing-in-question
			      next-compiler
			      recursivep
			      expandedp
			 &allow-other-keys)
  (destructuring-bind (1st . rest) list
    (if (not recursivep)
      
	;; then is the beginning of a new list
	(if (not expandedp)
	    
	    ;; try to BPM2-MACROEXPAND and recurse
	    (apply 'bpm2-compile
		   (bpm2-macroexpand list)
		   :expandedp t
		   kwd-args)

	    (case 1st
	      ((/lispp //lispp
		/lisp //lisp
		/and //and
		/or //or) ;; `LIST' is a bpm2 primitive form
	                  (bpm2-compile-primitive list
						  :seen-vars seen-vars
						  :thing-in-question thing-in-question
						  :next-compiler next-compiler))
	      (otherwise ;; `LIST' is not a bpm2 primitive, it's just a pattern literal.
	                 ;; Recurse.
	                 (apply 'bpm2-compile list :recursivep t kwd-args))))

	;; otherwise `LIST' is a pattern literal
	(let ((<1st> (gensym "first"))
	      (<rest> (gensym "rest")))
	  `(if (listp ,thing-in-question)
	       (let ((,<1st> (first ,thing-in-question))
		     (,<rest> (rest ,thing-in-question)))
		 ,(bpm2-compile 1st
				:seen-vars seen-vars
				:next-compiler (f_ (bpm2-compile rest
								 :seen-vars _
								 :next-compiler next-compiler
								 :thing-in-question <rest>
								 :recursivep t))
				:thing-in-question <1st>
				:recursivep t)))))))

(defmethod bpm2-compile ((n number)
			 &key seen-vars
			      thing-in-question
			      next-compiler
			 &allow-other-keys)
  `(if (eql ,thing-in-question ,n)
       ,(funcall next-compiler seen-vars)))

(defmethod bpm2-compile ((s string)
			 &key seen-vars
			      thing-in-question
			      next-compiler
			 &allow-other-keys)
  `(if (equal ,thing-in-question ,s)
       ,(funcall next-compiler seen-vars)))

(defmethod bpm2-compile ((c character)
			 &key seen-vars
			      thing-in-question
			      next-compiler
			 &allow-other-keys)
  `(if (eql ,thing-in-question ,c)
       ,(funcall next-compiler seen-vars)))

; compiling primitives

(defun bpm2-compile-primitive (form
			       &rest kwd-args
			       &key seen-vars thing-in-question next-compiler
			       &allow-other-keys)
  (destructuring-bind (1st . rest) form
    (case 1st
      (/lispp `(and ,@#1=(mapcar (f_ `(funcall ,_ ,thing-in-question))
				 rest)
		    ,#2=(funcall next-compiler seen-vars)))
      (//lispp `(and ,@rest ,#2#))
      (/lisp `(progn ,@#1# ,#2#))
      (//lisp `(progn ,@rest ,#2#))
      (/and (if (null rest)
		#2#
		(apply 'bpm2-compile
		       (first rest)
		       :next-compiler (f_ (apply 'bpm2-compile-primitive
						 `(/and ,@(rest rest))
						 :seen-vars _
						 kwd-args))
		       kwd-args)))
      (//and (apply 'bpm2-compile-primitive
		    `(/and ,@(rest rest))
		    :thing-in-question (first rest)
		    kwd-args))
      (/or `(or ,@(mapcar (f_ (apply 'bpm2-compile _ kwd-args))
			  rest)))
      (//or (apply 'bpm2-compile-primitive
		   `(/or ,(rest rest))
		   :thing-in-question (first rest)
		   kwd-args)))))

; primitives as macros

(defmacro /lispp (&rest lisp-forms)
  (let ((it (gensym "it")))
    `(f (,it) (and ,@(mapcar (f_ `(funcall ,_ ,it))
			     lisp-forms)))))

(defmacro //lispp (&rest lisp-forms)
  `(and ,@lisp-forms))

(defmacro /lisp (&rest lisp-forms)
  (let ((it (gensym "it")))
    `(f (,it)
       ,@(mapcar (f_ `(funcall ,_ ,it))
		 lisp-forms)
	t)))

(defmacro //lisp (&rest lisp-forms)
  `(progn ,@lisp-forms t))

(defmacro /and (&rest /forms)
  (let ((it (gensym "it")))
    `(f (,it)
       ,(bpm2-compile `(/and ,@/forms)
		      :thing-in-question it
		      :next-compiler (constantly t)))))

(defmacro //and (it &rest /forms)
  (bpm2-compile `(//and ,it ,@/forms)
		:next-compiler (constantly t)))

(defmacro /or (&rest /forms)
  (let ((it (gensym "it")))
    `(f (,it)
       ,(bpm2-compile `(/or ,@/forms)
		      :thing-in-question it
		      :next-compiler (constantly t)))))

(defmacro //or (it &rest /forms)
  (bpm2-compile `(//or ,it ,@/forms)
		:next-compiler (constantly t)))

; bpm2 macros

(defmacro def-bpm2-macro (name args &body body)
  `(progn

     (defmacro ,name ,args
       ,@body)

     (eval-when (:load-toplevel :compile-toplevel :execute)
       (setf (get ',name 'bpm2-macro-expander)
	     (m ,args ,@body)))))

(defun bpm2-macroexpand-1 (form)
  (if (atom form)
      form
      (destructuring-bind (1st . rest) form
	(if (symbolp 1st)
	    (aif (get 1st 'bpm2-macro-expander)
		 (values (apply it rest) t)
		 form)
	    form))))

(defun bpm2-macroexpand (form)
  (multiple-value-bind (result expanded-p) (bpm2-macroexpand-1 form)
    (if expanded-p
	(values (bpm2-macroexpand result) t)
	result)))