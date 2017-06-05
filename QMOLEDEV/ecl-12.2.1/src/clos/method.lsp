;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

;;; ----------------------------------------------------------------------

(defparameter *method-size* 32)		; Size of methods hash tables

;;; This holds fake methods created during bootstrap.
;;; It is  an alist of:
;;;	(method-name {method}+)
(defparameter *early-methods* nil)

;;;
;;; This is used by combined methods to communicate the next methods to
;;; the methods they call.
;;;
(defparameter *next-methods* nil)


;;; ----------------------------------------------------------------------
;;; DEFMETHOD
;;;

(defmacro defmethod (&whole whole &rest args &environment env)
  (multiple-value-bind (name qualifiers specialized-lambda-list body)
      (parse-defmethod args)
    (multiple-value-bind (lambda-list required-parameters specializers)
	(parse-specialized-lambda-list specialized-lambda-list)
      (multiple-value-bind (fn-form doc plist)
	  (expand-defmethod name qualifiers lambda-list
			    required-parameters specializers body env)
	(declare (ignore required-parameters))
	(ext:register-with-pde whole
			       `(install-method ',name ',qualifiers
						,(list 'si::quasiquote specializers)
						',lambda-list ',doc
						',plist ,fn-form))))))


;;; ----------------------------------------------------------------------
;;;                                                  method body expansion

(defun expand-defmethod (generic-function-name qualifiers lambda-list
			 required-parameters specializers body env)
  (declare (ignore qualifiers)
	   (si::c-local))
  (multiple-value-bind (declarations real-body documentation)
      (sys::find-declarations body)
    ;; FIXME!! This deactivates the checking of keyword arguments
    ;; inside methods. The reason is that this checking must be
    ;; supplemented the knowledge of the keyword arguments of all
    ;; applicable methods (X3J13 7.6.5). Therefore, we should insert
    ;; that check, either in the method itself so that it is done
    ;; incrementally, or in COMPUTE-EFFECTIVE-METHOD.
    (when (and (member '&key lambda-list)
	       (not (member '&allow-other-keys lambda-list)))
      (let ((x (position '&aux lambda-list)))
	(setf lambda-list
		(append (subseq lambda-list 0 x)
			'(&allow-other-keys)
			(and x (subseq lambda-list x))
                        nil))))
    (let* ((class-declarations
	    (nconc (loop for name in required-parameters
		      for type in specializers
		      when (and (not (eq type t)) (symbolp type))
		      nconc `((type ,type ,name)
			      (si::no-check-type ,name)))
		   (cdar declarations)))
	   (method-lambda
	    ;; Remove the documentation string and insert the
	    ;; appropriate class declarations.  The documentation
	    ;; string is removed to make it easy for us to insert
	    ;; new declarations later, they will just go after the
	    ;; second of the method lambda.  The class declarations
	    ;; are inserted to communicate the class of the method's
	    ;; arguments to the code walk.
	    `(ext::lambda-block ,generic-function-name
	      ,lambda-list
	      ,@(and class-declarations `((declare ,@class-declarations)))
	      ,@real-body))
	   
	   (aux-bindings ())	; Suffice to say that &aux is one of
				; damndest things to have put in a
				; language.
	   (plist ()))
      (multiple-value-bind (call-next-method-p next-method-p-p in-closure-p)
	  (walk-method-lambda method-lambda required-parameters env)

	(when (or call-next-method-p next-method-p-p)
	  (setf plist '(:needs-next-method-p t)))

	(when in-closure-p
	  (setf plist '(:needs-next-method-p FUNCTION))
	  (setf real-body
		`((let* ((.closed-combined-method-args.
                          (if (listp .combined-method-args.)
                              .combined-method-args.
                              (apply #'list .combined-method-args.)))
			 (.next-methods. *next-methods*))
		    (flet ((call-next-method (&rest args)
			     (unless .next-methods.
			       (error "No next method"))
			     (funcall (car .next-methods.)
				      (or args .closed-combined-method-args.)
				      (rest .next-methods.)))
			   (next-method-p ()
			     .next-methods.))
		      ,@real-body)))))
	(values
	 `#'(ext::lambda-block ,generic-function-name
	      ,lambda-list
	      ,@(and class-declarations `((declare ,@class-declarations)))
	      ,@real-body)
	 documentation
	 plist)))))

(defun environment-contains-closure (env)
  ;;
  ;; As explained in compiler.d (make_lambda()), we use a symbol with name
  ;; "FUNCTION" to mark the beginning of a function. If we find that symbol
  ;; twice, it is quite likely that this form will end up in a closure.
  ;;
  (flet ((function-boundary (s)
	   (and (consp s)
		(symbolp (setf s (first s)))
		(null (symbol-package s))
		(equal (symbol-name s) "FUNCTION"))))
    (> (count-if #'function-boundary (car env)) 1)))

(defun walk-method-lambda (method-lambda required-parameters env)
  (declare (si::c-local)
	   (ignore required-parameters))
  (let ((call-next-method-p nil)
	(next-method-p-p nil)
	(in-closure-p nil))
    (flet ((code-walker (form env)
	     (unless (atom form)
	       (let ((name (first form)))
		 (case name
		   (CALL-NEXT-METHOD
		    (setf call-next-method-p
			  (or call-next-method-p T)
			  in-closure-p
			  (or in-closure-p (environment-contains-closure env))))
		   (NEXT-METHOD-P
		    (setf next-method-p-p t
			  in-closure-p (or in-closure-p (environment-contains-closure env))))
		   (FUNCTION
		    (when (eq (second form) 'CALL-NEXT-METHOD)
		      (setf in-closure-p t
			    call-next-method-p 'FUNCTION))
		    (when (eq (second form) 'NEXT-METHOD-P)
		      (setf next-method-p-p 'FUNCTION
			    in-closure-p t))))))
	     form))
      (let ((si::*code-walker* #'code-walker))
	;; Instead of (coerce method-lambda 'function) we use
        ;; explicitely the bytecodes compiler with an environment, no
        ;; stepping, compiler-env-p = t and execute = nil, so that the
        ;; form does not get executed.
        (si::eval-with-env method-lambda env nil t t)))
    (values call-next-method-p
	    next-method-p-p
	    in-closure-p)))

;;; ----------------------------------------------------------------------
;;;                                                                parsing

(defun legal-generic-function-name-p (name)
  (si::valid-function-name-p name))

(defun parse-defmethod (args)
  (declare (si::c-local))
  ;; This function has to extract the name of the method, a list of
  ;; possible qualifiers (identified by not being lists), the lambda
  ;; list of the method (which might be empty!) and the body of the
  ;; function.
  (let* (name)
    (unless args
      (error "Illegal defmethod form: missing method name"))
    (setq name (pop args))
    (unless (legal-generic-function-name-p name)
      (error "~A cannot be a generic function specifier.~%~
             It must be either a non-nil symbol or ~%~
             a list whose car is setf and whose second is a non-nil symbol."
	     name))
    (do ((qualifiers '()))
	((progn
	   (when (endp args)
	     (error "Illegal defmethod form: missing lambda-list"))
	   (listp (first args)))
	 (values name (nreverse qualifiers) (first args) (rest args)))
      (push (pop args) qualifiers))))

(defun parse-specialized-lambda-list (specialized-lambda-list)
  "This function takes a method lambda list and outputs the list of required
arguments, the list of specializers and a new lambda list where the specializer
have disappeared."
  (declare (si::c-local))
  ;; SI:PROCESS-LAMBDA-LIST will ensure that the lambda list is
  ;; syntactically correct and will output as a first argument the
  ;; list of required arguments. We use this list to extract the
  ;; specializers and build a lambda list without specializers.
  (do* ((arglist (rest (si::process-lambda-list specialized-lambda-list 'METHOD))
		 (rest arglist))
	(lambda-list (copy-list specialized-lambda-list))
	(ll lambda-list (rest ll))
	(required-parameters '())
	(specializers '())
	arg variable specializer)
       ((null arglist)
	(values lambda-list
		(nreverse required-parameters)
		(nreverse specializers)))
    (setf arg (first arglist))
    (cond
      ;; Just a variable
      ((atom arg)
       (setf variable arg specializer T))
      ;; List contains more elements than variable and specializer
      ((not (endp (cddr arg)))
       (si::simple-program-error "Syntax error in method specializer ~A" arg))
      ;; Specializer is NIL
      ((null (setf variable (first arg)
		   specializer (second arg)))
       (si::simple-program-error
	"NIL is not a valid specializer in a method lambda list"))
      ;; Specializer is a class name
      ((atom specializer))
      ;; Specializer is (EQL value)
      ((and (eql (first specializer) 'EQL)
	    (endp (cddr specializer)))
       (let ((value (second specializer)))
	 (setf specializer
	       `(eql ,(if (constantp value)
			  (eval value)
			  (list 'si::unquote value))))))
      ;; Otherwise, syntax error
      (t
       (si::simple-program-error "Syntax error in method specializer ~A" arg)))
    (setf (first ll) variable)
    (push variable required-parameters)
    (push specializer specializers)))

(defun declaration-specializers (arglist declarations)
  (declare (si::c-local))
  (do ((argscan arglist (cdr argscan))
       (declist (when declarations (cdr declarations))))
      ((or
	(null argscan)
	(member (first argscan) '(&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX)))
       `(DECLARE ,@declist))
      (when (listp (first argscan))
	    (push `(TYPE ,(cadar argscan) ,(caar argscan)) declist))))


;;; ----------------------------------------------------------------------
;;;                                                             operations

(defun add-method-keywords (method)
  (multiple-value-bind (reqs opts rest key-flag keywords allow-other-keys)
      (si::process-lambda-list (method-lambda-list method) t)
    (declare (ignore reqs opts rest key-flag))
    (setf (method-keywords method)
          (if allow-other-keys
              't
              (loop for k in (rest keywords) by #'cddddr
                 collect k)))
    method))

(defun make-method (method-class qualifiers specializers lambda-list
				 fun plist options)
  (declare (ignore options))
  (let* ((instance-size (+ #.(length +standard-method-slots+)
			   (if (eq method-class 'standard-method)
			       0 2)))
	 (method (si:allocate-raw-instance nil method-class instance-size)))
    (setf (method-generic-function method) nil
	  (method-lambda-list method) lambda-list
	  (method-function method) fun
	  (method-specializers method) specializers
	  (method-qualifiers method) qualifiers
	  (method-plist method) plist)
    (add-method-keywords method)))

;;; early version used during bootstrap
(defun method-p (x)
  (si::instancep x))

(defun method-needs-next-methods-p (method)
  (getf (method-plist method) :needs-next-methods-p))

;;; early version used during bootstrap
(defun add-method (gf method)
  (let* ((name (generic-function-name gf))
	 (method-entry (assoc name *early-methods*)))
    (unless method-entry
      (setq method-entry (list name))
      (push method-entry *early-methods*))
    (push method (cdr method-entry))
    (push method (generic-function-methods gf))
    (setf (method-generic-function method) gf)
    (unless (si::sl-boundp (generic-function-lambda-list gf))
      (setf (generic-function-lambda-list gf) (method-lambda-list method))
      (setf (generic-function-argument-precedence-order gf)
	    (rest (si::process-lambda-list (method-lambda-list method) t))))
    (compute-g-f-spec-list gf)
    (set-generic-function-dispatch gf)
    (dolist (d (generic-function-dependents gf))
      (update-dependent gf d 'add-method method))
    method))

(defun find-method (gf qualifiers specializers &optional (errorp t))
  (declare (notinline method-qualifiers))
  (let* ((method-list (generic-function-methods gf))
	 found)
    (dolist (method method-list)
      (when (and (equal qualifiers (method-qualifiers method))
		 (equal specializers (method-specializers method)))
	(return-from find-method method)))
    ;; If we did not find any matching method, then the list of
    ;; specializers might have the wrong size and we must signal
    ;; an error.
    (cond ((/= (length specializers)
	       (length (generic-function-argument-precedence-order gf)))
	   (error
	    "The specializers list~%~A~%does not match the number of required arguments in ~A"
	    specializers (generic-function-name gf)))
	  (errorp
	   (error "There is no method on the generic function ~S that agrees on qualifiers ~S and specializers ~S"
		  (generic-function-name gf)
		  qualifiers specializers)))
    nil))


;;; ----------------------------------------------------------------------
;;;                                                             with-slots

(defmacro with-slots (slot-entries instance-form &body body)
  (let* ((temp (gensym))
	 (accessors
	  (do ((scan slot-entries (cdr scan))
	       (res))
	      ((null scan) (nreverse res))
	      (if (symbolp (first scan))
		  (push `(,(first scan) (slot-value ,temp ',(first scan))) res)
		(push `(,(caar scan)
			(slot-value ,temp ',(cadar scan))) res)))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))

;(with-slots (x (y2 y)) inst (setq x y2))

;;; ----------------------------------------------------------------------
;;;                                                         with-accessors

(defmacro with-accessors (slot-accessor-pairs instance-form &body body)
  (let* ((temp (gensym))
	 (accessors (do ((scan slot-accessor-pairs (cdr scan))
			(res))
		       ((null scan) (nreverse res))
		       (push `(,(caar scan) (,(cadar scan) ,temp)) res))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))

;;; Force the compiler into optimizing use of gethash inside methods:
(setf (symbol-function 'SLOT-INDEX) (symbol-function 'GETHASH))