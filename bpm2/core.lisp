;;
;; OVERVIEW
;;
;; `bpm2' is lisp-embedded pattern-matching language. it was originally designed for
;; doing code transformation [ie, writing compilers] but it is currently trying to find
;; a second life as a general purpose pattern matcher [for validating data objects being
;; passed around inside a web application]
;;
;; A BIRD'S EYE VIEW
;;
;; the `bpm2's language is composed of clauses, patterns, and transformers. transformers
;; are currently unimplemented, so that leaves us with clauses and patterns:
;;
;; a clause looks like a call to a macro thats name starts with 2 backslashes [eg //INT]
;;
;; a pattern looks like a call to a macro thats name starts with 1 backslash [eg /INT]
;;
;; CLAUSES
;;
;; the first argument to a clause is referred to as the Thing In Question. a clause
;; evaluates the Thing In Question then clause returns T or NIL depending on whether or
;; not the Thing In Question fulfills its constraints.
;;
;; so:
;;
;;    CL-USER> (//int 5)
;;    -> T
;;
;; because 5 is an integer
;;
;; but:
;;
;;    CL-USER> (//int "5")
;;    -> NIL
;;
;; because "5" is a string, not an integer
;;
;; PATTERNS
;;
;; a pattern is like a clause except that it is missing the Thing in Question parameter.
;;
;; instead of returning T or NIL, a pattern evaluates to a function that takes one
;; argument and plugs it into the Thing In Question parameter of the related clause.
;;
;; so
;;
;;    (/int) =~ (lambda (x) (//int x))
;;
;; thus
;;
;;    CL-USER> (/int)
;;    -> #<function ..>
;;
;;    CL-USER> (funcall * 5)
;;    -> T
;;
;;    CL-USER> (funcall ** "5")
;;    -> NIL
;;
;; FUNCTORS
;;
;; identically named clauses and patterns are grouped together to form a ``functor''
;; [eg, //INT and /INT are reffered to as the functor INT]
;;
;; the only exceptions to the simmitry between the similarly named clauses and patterns
;; are the LISPP and LISP functors
;;
;; COMBINING FUNCTORS
;;
;; the AND and OR functors allow you to combine multiple patterns/functors.
;;
;; for example:
;;
;;    (/or (/string)
;; 	   (/number))
;;
;; will match a string or a number
;;
;; and:
;;
;;    (/and (/string)
;;          (/number))
;;
;; will match nothing since there's no type of Lisp object that is both a string
;; and a number [that I'm aware of]
;;
;; DROPPING INTO LISP
;;
;; the clause //LISPP behaves similar to lisp's AND. //LISPP takes N forms and returns
;; T if all of them evaluate to a non-NULL value, short-circuiting the first time one of
;; them doesn't
;;
;;    CL-USER> (//lispp 1 2 3 (print "4") 5)
;;    "4"
;;    -> T
;;
;;    CL-USER> (//lispp 1 2 nil (print "4") 5)
;;    -> NIL
;;
;; the pattern /LISPP takes N function indicators and returns a function that returns T
;; if all of the predicates return a non-NULL value when applied to its argument
;;
;;    CL-USER> (/lispp 'numberp 'integerp 'print)
;;    -> #<function ..>
;;
;;    CL-USER> (funcall * 5)
;;    "5"
;;    -> T
;;
;;    CL-USER> (funcall ** 5.0)
;;    -> NIL
;;
;; the clause //LISP is just like //LISPP except that it always returns T and it always
;; tries to evaluate all its forms. the pattern /LISP is like /LISPP except that it
;; always returns T and it always tries to apply all its functions to its argument
;; [/LISP and //LISP are for side effects only]
;;
;; PATTERN LITERALS
;;
;; anything within a pattern or clause that does not look like a pattern or clause [as
;; described above: a call to a macro thats name starts with 1 or 2 slashes] is
;; interpreted as a pattern literal
;;
;; a pattern literal is a pattern that looks like what it should match
;;
;; so
;;
;;    CL-USER> (/and (a b c))
;;    -> #<function ..>
;;
;;    CL-USER> (funcall * '(a b c))
;;    -> T
;;
;;    CL-USER> (funcall ** '(a b 3))
;;    -> NIL
;;
;;    CL-USER> (/and "Foo")
;;    -> #<function>
;;
;;    CL-USER> (funcall * "Foo")
;;    -> T
;;
;;    CL-USER> (funcall ** "foO")
;;    -> NIL
;;
;; LOGIC VARIABLES
;;
;; variables that start with a question mark (?) are percieved as logic variables.
;; bpm2's logic variables behave similarly to logic variables in logic languages such as
;; prolog: identically named logic variables must have identical values [by EQUAL].
;;
;; so
;;
;;    CL-USER> (/and (?x . ?x)
;;                   (//lisp (print ?x)))
;;    -> #<function ..>
;;
;;    CL-USER> (funcall * '(1 . 1))
;;    "1"
;;    -> T
;;
;;    CL-USER> (funcall ** '(1 . 2))
;;    -> NIL
;;
;; a single question mark (?) is treated as the ``wildcard'' that can match anything and
;; never remembers its value.
;;
;;    CL-USER> (/and ?x
;;                   (? . ?)
;;                   (//lisp (print ?x)))
;;    -> #<function ..>
;;
;;    CL-USER> (funcall * '(1 . 2))
;;    "(1 . 2)"
;;    T
;;
;;    CL-USER> (funcall ** 5)
;;    -> NIL
;;
;; IS AND DO
;;
;; one useful functor is IS. IS takes a pattern and some lisp forms. it matches the
;; Thing In Question against the pattern then tries the lisp forms like //LISPP
;;
;;    CL-USER> (/is (?x . ?y)
;;                  (> ?x ?y))
;;    -> #<function ..>
;;
;;    CL-USER> (funcall * '(2 1))
;;    -> T
;;
;;    CL-USER> (funcall ** '(1 2))
;;    -> NIL
;;
;; DO is like IS except that the lisp clauses behave like //LISP instead of //LISPP [DO
;; is for side-effects only]
;;
;; EXTENDING BPM2
;;
;; there are 4 primitive functors
;;
;;    AND
;;    LISP
;;    LISPP
;;    OR
;;
;; all other patterns and clauses are defined with DEF-BPM2-MACRO. DEF-BPM2-MACRO
;; behaves exactly like DEFMACRO except there is some magic that goes on behind the
;; scenes so that the defined bpm2 macros know about the current logic variable envoronment
;;
;; BPM2-MACROEXPAND and BPM2-MACROEXPAND-1 behave like MACROEXPAND and MACROEXPAND-1,
;; respectively
;;

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