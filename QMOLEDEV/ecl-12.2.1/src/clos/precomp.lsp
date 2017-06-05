;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

(eval-when (compile eval)
(defmacro pre-make-caching-discriminating-functions (specs)
  `(progn ,.(mapcar #'(lambda (s)
			`(pre-make-templated-function-constructor
			   caching-discriminating-function
			   ,.s))
		    specs)))

(defmacro pre-make-templated-function-constructor (name
						   &rest template-parameters)
  (let* ((params (get-sysprop name 'TEMPLATED-FN-PARAMS))
	 (template-params (first params))
	 (instance-params (second params))
	 (body (cddr params))
	 (form 
	  (progv template-params
		 template-parameters
	    `(LET ((ENTRY
		    (OR (ASSOC ',template-parameters 
			       (GET-SYSPROP ',name 'TEMPLATED-FN-CONSTRUCTORS)
			       :test #'equal)
			(LET ((NEW-ENTRY
			       (LIST ',template-parameters () () ())))
			  (PUSH NEW-ENTRY
				(GET-SYSPROP ',name 'TEMPLATED-FN-CONSTRUCTORS))
			  NEW-ENTRY))))
	      (SETF (THIRD ENTRY) 'COMPILED)
	      (SETF (SECOND ENTRY)
	       (FUNCTION (LAMBDA ,(eval instance-params)
		 ,(eval (cons 'PROGN body)))))))))
    form))
)

(eval-when (load)
  (pre-make-caching-discriminating-functions
    ((1 NIL (0) 32)
     (2 NIL (0) 32)
     (2 NIL (1) 32)		;setf of accessor gfuns
     (2 NIL (0 1) 32)
     (3 NIL (0) 32)
     (3 NIL (1) 32)
     (3 NIL (0 1) 32)
     (4 NIL (0) 32)
     (4 NIL (1) 32)
     (5 NIL (0) 32)
     (5 NIL (0 1) 32)
     (6 NIL (0) 32)
     (6 NIL (0 1) 32)
     (7 NIL (0) 32)
     
     (1 T (0) 32)
     (2 T (0) 32)
     (2 T (0 1) 32)
     (3 T (0) 32)
     (4 T (0) 32))))

(eval-when (compile eval)
  (defmacro precompile-effective-method-templates (templates)
    `(progn ,@(mapcar #'(lambda (x)
			  `(precompile-effective-method-template ,x))
		      templates))))

(precompile-effective-method-templates 
  (
   (_call-method_)				      ;1 or more :around
						      ;methods with 0 or
						      ;more next methods
						      ;This case happens
						      ;whenever there are
						      ;:around methods

   (PROGN (_call-method_)			      ;1 :before 1 :after
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				(_call-method_)))

   (PROGN ()					      ;0 :before 1 :after
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				(_call-method_)))

   (PROGN (_call-method_)			      ;1 :before 0 :after
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				()))


   (PROGN (_call-method_)			      ;2 :befores 2 :after
	  (_call-method_)
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				(_call-method_)
				(_call-method_)))


   (PROGN (_call-method_)			      ;2 :befores 1 :after
	  (_call-method_)
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				(_call-method_)))

   (PROGN (_call-method_)			      ;1 :before 2 :after
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				(_call-method_)
				(_call-method_)))


   (PROGN (_call-method_)			      ;2 :befores no :after
	  (_call-method_)
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				()))

   (PROGN (_call-method_)			      ;0 :before 2 :after
	  (MULTIPLE-VALUE-PROG1 (_call-method_)
				(_call-method_)
				(_call-method_)))
  
   ))
