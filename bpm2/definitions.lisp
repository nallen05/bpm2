;; it may seem redundent to have to define both /FOO and //FOO by hand, but this way
;; there are sensible parameter sigs in SLIME and well... keep it simple...

(in-package :bpm2)

; is

(def-bpm2-macro /is (pattern &rest lisp-forms)
  `(/and ,pattern (//lispp ,@lisp-forms)))

(def-bpm2-macro //is (it pattern &rest lisp-forms)
  `(//and ,it (/is ,pattern ,@lisp-forms)))

(export '(/is //is))

; do

(def-bpm2-macro /do (pattern &rest lisp-forms)
  `(/and ,pattern (//lisp ,@lisp-forms)))

(def-bpm2-macro //do (it pattern &rest lisp-forms)
  `(//and ,it (/do ,pattern ,@lisp-forms)))

(export '(/do //do))

; n

(def-bpm2-macro /n (&optional low-bound high-bound)
  `(/lispp 'numberp
	   (f_ (let ((low ,low-bound)
		     (high ,high-bound))
		 (and (or (not low)
			  (>= _ low))
		      (or (not high)
			  (> high _)))))))

(def-bpm2-macro //n (it &optional low-bound high-bound)
  `(//and ,it (/n ,low-bound ,high-bound)))

(export '(/n //n))

; int

(def-bpm2-macro /int (&optional low-bound high-bound)
  `(/lispp 'integerp
	   (f_ (let ((low ,low-bound)
		     (high ,high-bound))
		 (and (or (not low)
			  (>= _ low))
		      (or (not high)
			  (> high _)))))))

(def-bpm2-macro //int (it &optional low-bound high-bound)
  `(//and ,it (/int ,low-bound ,high-bound)))

(export '(/int //int))

; string-equal

(def-bpm2-macro /string-equal (string)
  `(/lispp 'stringp
	   (f_ (string-equal _ ,string))))

(def-bpm2-macro //string-equal (it string)
  `(//and ,it (/string-equal ,string)))

(export '(/string-equal //string-equal))

; re

(def-bpm2-macro /re (regex) ;; bind to registers??
  `(/lispp 'stringp
	   (f_ (cl-ppcre:scan ,regex _))))

(def-bpm2-macro //re (it regex)
  `(//and ,it (/re ,regex)))

(export '(/re //re))

; &key / &allow-other-keys

(defun .plist-keys (plist)
  (when plist
    (destructuring-bind (k v . rest) plist
      (declare (ignore v))
      (cons k (.plist-keys rest)))))

(def-bpm2-macro /&key (&rest keys)
  `(/lispp 'listp
	   (f_ (lisp (last _)))
	   (f_ (let ((ks (.plist-keys _)))
		 (and (not (set-difference ks ,keys))
		      (not (set-difference ks ,keys)))))))

(def-bpm2-macro /&allow-other-keys (&rest keys)
  `(/lispp 'listp
	   (f_ (lisp (last _)))
	   (f_ (not (set-difference ,keys (.plist-keys _))))))

(export '(/&key /&allow-other-keys //&key //&allow-other-keys))

; getf

(def-bpm2-macro /getf (key value)
  `(/and (/&allow-other-keys)
	 (/lispp (f_ (equal (getf ,key _) ,value)))))

(export '(/&getf //&getf))	      