 
# BPM2

_Bpm2_ is lisp-embedded pattern-matching language. it was originally designed for
  doing code transformation
 
## A BIRD'S EYE VIEW
 
The _bpm2_ language is composed of clauses, patterns, and transformers. transformers
are currently unimplemented, so that leaves us with clauses and patterns:
 
*  a _clause_ looks like a call to a macro thats name starts with 2 backslashes
   (eg `//INT`)
 
*  a _pattern_ looks like a call to a macro thats name starts with 1 backslash
   (eg `/INT`)
 
## CLAUSES
 
The first argument to a clause is referred to as the _Thing In Question_. A clause
evaluates the Thing In Question then clause returns T or NIL depending on whether or
not the Thing In Question fulfills its constraints.
 
So:
 
    CL-USER> (//int 5)
    -> T
 
because `5` is an integer
 
but:
 
    CL-USER> (//int "5")
    -> NIL
 
because `"5"` is a string, not an integer
 
##  PATTERNS
 
A pattern is like a clause except that it is missing the Thing in Question parameter.
 
Instead of returning `T` or `NIL`, a pattern evaluates to a function that takes one
argument and plugs it into the `Thing In Question` parameter of the related clause.

So:
 
   `(/int)` ~ `(lambda (x) (//int x))`
 
Thus
 
    CL-USER> (/int)
    -> #<function ..>
 
    CL-USER> (funcall * 5)
    -> T
 
    CL-USER> (funcall ** "5")
    -> NIL
 
## FUNCTORS
 
Identically named clauses and patterns are grouped together to form a _functor_
(eg, `//INT` and `/INT` are refered to as the functor `INT`)
 
The only exceptions to the simmitry between the similarly named clauses and patterns
are the LISPP and LISP functors
 
## COMBINING FUNCTORS
 
The `AND` and `OR` functors allow you to combine multiple patterns/functors.
 
For example:
 
    (/or (/string)
 	   (/number))
 
will match a string or a number and:
 
    (/and (/string)
          (/number))
 
will never successfully match anything since there's no type of Lisp object that is
both a string and a number (that I'm aware of)
 
## DROPPING INTO LISP
 
The clause `//LISPP` behaves similar to lisp's `AND`. `//LISPP` takes N forms and
returns `T` if all of them evaluate to a non-`NULL` value, short-circuiting the
first time one of them doesn't
 
    CL-USER> (//lispp 1 2 3 (print "4") 5)
    "4"
    -> T
 
    CL-USER> (//lispp 1 2 nil (print "4") 5)
    -> NIL
 
The pattern `/LISPP` takes N function indicators and returns a function that returns
T if all of the predicates return a non-NULL value when applied to its argument
 
    CL-USER> (/lispp 'numberp 'integerp 'print)
    -> #<function ..>
 
    CL-USER> (funcall * 5)
    "5"
    -> T
 
    CL-USER> (funcall ** 5.0)
    -> NIL
 
The clause `//LISP` is just like `//LISPP` except that it always returns `T` and it
always tries to evaluate all its forms. The pattern `/LISP` is like `/LISPP` except
that it always returns `T` and it always tries to apply all its functions to its
argument (`/LISP` and `//LISP` are for side effects only)
 
## PATTERN LITERALS
 
Anything within a pattern or clause that does not look like a pattern or clause (as
described above: a call to a macro thats name starts with 1 or 2 slashes) is
interpreted as a pattern literal
 
A pattern literal is a pattern that looks like what it should match.
 
So:
 
    CL-USER> (/and (a b c))
    -> #<function ..>
 
    CL-USER> (funcall * '(a b c))
    -> T
 
    CL-USER> (funcall ** '(a b 3))
    -> NIL
 
    CL-USER> (/and "Foo")
    -> #<function>
 
    CL-USER> (funcall * "Foo")
    -> T
 
    CL-USER> (funcall ** "foO")
    -> NIL
 
##  LOGIC VARIABLES
 
Variables that start with a question mark (`#\?`) are percieved as logic variables.
bpm2's logic variables behave similarly to logic variables in logic languages such
as prolog: identically named logic variables must have identical values (by `EQUAL`).
 
So:
 
    CL-USER> (/and (?x . ?x)
                   (//lisp (print ?x)))
    -> #<function ..>
 
    CL-USER> (funcall * '(1 . 1))
    "1"
    -> T
 
    CL-USER> (funcall ** '(1 . 2))
    -> NIL
 
A single question mark (`#\?`) is treated as the _wildcard_ that can match anything
and never remembers its value.
 
    CL-USER> (/and ?x
                   (? . ?)
                   (//lisp (print ?x)))
    -> #<function ..>
 
    CL-USER> (funcall * '(1 . 2))
    "(1 . 2)"
    T
 
    CL-USER> (funcall ** 5)
    -> NIL
 
##  IS AND DO
 
One useful functor is `IS`. `IS` takes a pattern and some lisp forms. it matches
the Thing In Question against the pattern then tries the lisp forms like `//LISPP`
 
    CL-USER> (/is (?x . ?y)
                  (> ?x ?y))
    -> #<function ..>
 
    CL-USER> (funcall * '(2 1))
    -> T
 
    CL-USER> (funcall ** '(1 2))
    -> NIL
 
`DO` is like `IS` except that the lisp clauses behave like `//LISP` instead of
`//LISPP` [`DO` is for side-effects only]
 
## EXTENDING BPM2
 
There are 4 primitive functors
 
* `AND`
* `LISP`
* `LISPP`
* `OR`
 
All other patterns and clauses are defined with `DEF-BPM2-MACRO`. `DEF-BPM2-MACRO`
behaves exactly like `DEFMACRO` except there is some magic that goes on behind the
scenes so that the defined bpm2 macros know about the current logic variable
envoronment.
 
`BPM2-MACROEXPAND` and `BPM2-MACROEXPAND-1` behave like `MACROEXPAND` and
`MACROEXPAND-1` respectively.
 
