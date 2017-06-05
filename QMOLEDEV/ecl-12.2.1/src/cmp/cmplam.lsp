;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPLAM  Lambda expression.

(in-package "COMPILER")

;;; During Pass1, a lambda-list
;;;
;;; (	{ var }*
;;; 	[ &optional { var | ( var [ initform [ svar ] ] ) }* ]
;;; 	[ &rest var ]
;;; 	[ &key { var | ( { var | ( kwd var ) } [initform [ svar ]])}*
;;; 		[&allow-other-keys]]
;;; 	[ &aux {var | (var [initform])}*]
;;; )
;;;
;;; is transformed into
;;;
;;; (	( { var }* )				; required
;;; 	( { var initform svar }* )		; optional
;;; 	{ var | nil }				; rest
;;; 	allow-other-keys-flag
;;; 	( { kwd-vv-index var initform svar }* )	; key
;;; )
;;;
;;; where
;;; 	svar:	NIL	; means svar is not supplied
;;;	        | var
;;;
;;; &aux parameters will be embedded into LET*.
;;;
;;; c1lambda-expr receives
;;;	( lambda-list { doc | decl }* . body )
;;; and returns
;;;	( lambda info-object lambda-list' doc body' )
;;;
;;; Doc is NIL if no doc string is supplied.
;;; Body' is body possibly surrounded by a LET* (if &aux parameters are
;;; supplied) and an implicit block.

(defun c1lambda-doc (form)
  (second (c1form-args form)))

(defun c1lambda-body (form)
  (third (c1form-args form)))

(defun c1lambda-list (form)
  (first (c1form-args form)))

(defun fun-needs-narg (fun)
  (not (fun-fixed-narg fun)))

(defun fun-fixed-narg (fun)
  "Returns true if the function has a fixed number of arguments and it is not a closure.
The function thus belongs to the type of functions that ecl_make_cfun accepts."
  (let (narg)
    (and (not (eq (fun-closure fun) 'CLOSURE))
	 (= (fun-minarg fun) (setf narg (fun-maxarg fun)))
	 (<= narg si::c-arguments-limit)
	 narg)))

(defun add-referred-variables-to-function (fun var-list)
  (setf (fun-referred-vars fun)
	(set-difference (union (fun-referred-vars fun) var-list)
			(fun-local-vars fun)))
  fun)

(defun c1compile-function (lambda-list-and-body &key (fun (make-fun))
			   (name (fun-name fun)) (CB/LB 'CB))
  (let ((lambda (if name
		    `(ext:lambda-block ,name ,@lambda-list-and-body)
		    `(lambda ,@lambda-list-and-body))))
    (setf (fun-name fun) name
	  (fun-lambda-expression fun) lambda 
	  (fun-parent fun) *current-function*))
  (when *current-function*
    (push fun (fun-child-funs *current-function*)))
  (let* ((*current-function* fun)
	 (*cmp-env* (setf (fun-cmp-env fun) (cmp-env-mark CB/LB)))
	 (setjmps *setjmps*)
	 (decl (si::process-declarations (rest lambda-list-and-body)))
	 (global (and (assoc 'SI::C-GLOBAL decl) (setf (fun-global fun) T)))
	 (no-entry (assoc 'SI::C-LOCAL decl))
	 (lambda-expr (c1lambda-expr lambda-list-and-body
				     (si::function-block-name name)))
	 (children (fun-child-funs fun))
	 cfun exported minarg maxarg)
    (when (and no-entry (policy-debug-ihs-frame))
      (setf no-entry nil)
      (cmpnote "Ignoring SI::C-LOCAL declaration for~%~4I~A~%because the debug level is large" name))
    (unless (eql setjmps *setjmps*)
      (setf (c1form-volatile lambda-expr) t))
    (setf (fun-lambda fun) lambda-expr)
    (if global
	(multiple-value-setq (cfun exported) (exported-fname name))
	(setf cfun (next-cfun "LC~D~A" name) exported nil))
    #+ecl-min
    (when (member name c::*in-all-symbols-functions*)
      (setf no-entry t))
    (if exported
	;; Check whether the function was proclaimed to have a certain
	;; number of arguments, and otherwise produce a function with
	;; a flexible signature.
	(progn
	  (multiple-value-setq (minarg maxarg) (get-proclaimed-narg name))
          (format t "~&;;; Function ~A proclaimed (~A,~A)" name minarg maxarg)
	  (unless minarg
	    (setf minarg 0 maxarg call-arguments-limit)))
	(multiple-value-setq (minarg maxarg)
	  (lambda-form-allowed-nargs lambda-expr)))
    (setf (fun-cfun fun) cfun
	  (fun-exported fun) exported
	  (fun-closure fun) nil
	  (fun-minarg fun) minarg
	  (fun-maxarg fun) maxarg
	  (fun-description fun) name
	  (fun-no-entry fun) no-entry)
    (reduce #'add-referred-variables-to-function
	    (mapcar #'fun-referred-vars children)
	    :initial-value fun)
    (reduce #'add-referred-variables-to-function
	    (mapcar #'fun-referred-vars (fun-referred-funs fun))
	    :initial-value fun)
    (do ((finish nil))
	(finish)
      (setf finish t)
      (dolist (f (fun-child-funs fun))
	(when (compute-fun-closure-type f)
	  (setf finish nil))))
    (compute-fun-closure-type fun)
    (when global
      (if (fun-closure fun)
          (cmpnote "Function ~A is global but is closed over some variables.~%~{~A ~}"
                   (fun-name fun) (mapcar #'var-name (fun-referred-vars fun)))
          (new-defun fun (fun-no-entry fun)))))
  fun)

(defun cmp-process-lambda-list (list)
  (handler-case (si::process-lambda-list list 'function)
    (error (c) (cmperr "Illegal lambda list ~S" list))))

(defun c1lambda-expr (lambda-expr
                      &optional (block-name nil)
                      &aux doc body ss is ts
                           other-decls
		           new-variables
		           (type-checks '())
			   (*permanent-data* t)
			   (old-env *cmp-env*)
                           (*cmp-env* (cmp-env-copy)))
  (declare (si::c-local))

  (cmpck (endp lambda-expr)
         "The lambda expression ~s is illegal." (cons 'LAMBDA lambda-expr))

  (multiple-value-setq (body ss ts is other-decls doc)
    (c1body (cdr lambda-expr) t))

  (when block-name (setq body (list (cons 'BLOCK (cons block-name body)))))

  (multiple-value-bind (requireds optionals rest key-flag keywords
			allow-other-keys aux-vars)
      (cmp-process-lambda-list (car lambda-expr))

    (do ((specs (setq requireds (cdr requireds)) (cdr specs)))
	((endp specs))
      (let* ((name (first specs))
	     (var (c1make-var name ss is ts)))
	(push var type-checks)
	(setf (first specs) var)
	(push-vars var)))

    (do ((specs (setq optionals (cdr optionals)) (cdddr specs)))
	((endp specs))
      (let* ((name (first specs))
	     (var (c1make-var name ss is ts))
	     (init (second specs))
	     (flag (third specs)))
	(setq init (if init
		       (and-form-type (var-type var) (c1expr init) init
				      :safe "In (LAMBDA ~a...)" block-name)
		       (default-init var)))
	(push var type-checks)
	(push-vars var)
	(when flag
	  (push-vars (setq flag (c1make-var flag ss is ts))))
	(setf (first specs) var
	      (second specs) init
	      (third specs) flag)))

    (when rest
      (push-vars (setq rest (c1make-var rest ss is ts))))

    (do ((specs (setq keywords (cdr keywords)) (cddddr specs)))
	((endp specs))
      (let* ((key (first specs))
	     (name (second specs))
	     (var (c1make-var name ss is ts))
	     (init (third specs))
	     (flag (fourth specs)))
	(setq init (if init
		       (and-form-type (var-type var) (c1expr init) init
				      :safe "In (LAMBDA ~a...)" block-name)
		       (default-init var)))
	(push var type-checks)
	(push-vars var)
	(when flag
	  (push-vars (setq flag (c1make-var flag ss is ts))))
	(setf (second specs) var
	      (third specs) init
	      (fourth specs) flag)))

    ;; Make other declarations take effect right now
    (setf *cmp-env* (reduce #'add-one-declaration other-decls
                            :initial-value *cmp-env*))

    ;; After creating all variables and processing the initalization
    ;; forms, we wil process the body. However, all free declarations,
    ;; that is declarations which do not refer to the function
    ;; arguments, have to be applied to the body. At the same time, we
    ;; replace &aux variables with a LET* form that defines them.
    (let* ((declarations other-decls)
           (type-checks (extract-lambda-type-checks block-name requireds optionals
                                                    keywords ts other-decls))
           (type-check-forms (car type-checks))
           (let-vars (loop for spec on (nconc (cdr type-checks) aux-vars)
                        by #'cddr
                        for name = (first spec)
                        for init = (second spec)
                        collect (list name init)))
           (new-variables (cmp-env-new-variables *cmp-env* old-env))
	   (already-declared-names (set-difference (mapcar #'var-name new-variables)
                                                   (mapcar #'car let-vars))))
      ;; Gather declarations for &aux variables, either special...
      (let ((specials (set-difference ss already-declared-names)))
        (when specials
          (push `(special ,@specials) declarations)))
      ;; ...ignorable...
      (let ((ignorables (loop for (var . expected-uses) in is
                           unless (member var already-declared-names)
                           collect var)))
        (when ignorables
          (push `(ignorable ,@ignorables) declarations)))
      ;; ...or type declarations
      (loop for (var . type) in ts
	    unless (member var already-declared-names)
	    do (push `(type ,type ,var) declarations))
      ;; ...create the enclosing LET* form for the &aux variables
      (when (or let-vars declarations)
        (setq body `((let* ,let-vars
                       (declare ,@declarations)
                       ,@body))))
      ;; ...wrap around the optional type checks
      (setq body (nconc type-check-forms body))
      ;; ...now finally compile the body with the type checks
      (let ((*cmp-env* (cmp-env-copy *cmp-env*)))
        (setf body (c1progn body)))
      ;;
      ;; ...and verify whether all variables are used.
      (dolist (var new-variables)
        (check-vref var))
      (make-c1form* 'LAMBDA
                    :local-vars new-variables
                    :args (list requireds optionals rest key-flag keywords
                                allow-other-keys)
                    doc body))))

(defun lambda-form-allowed-nargs (lambda)
  (let ((minarg 0)
	(maxarg call-arguments-limit))
    (destructuring-bind (requireds optionals rest key-flag keywords a-o-k)
	(c1form-arg 0 lambda)
      (when (and (null rest) (not key-flag) (not a-o-k))
	(setf minarg (length requireds)
	      maxarg (+ minarg (/ (length optionals) 3)))))
    (values minarg maxarg)))

#| Steps:
 1. defun creates declarations for requireds + va_alist
 2. c2lambda-expr adds declarations for:
	unboxed requireds
	lexical optionals (+ supplied-p), rest, keywords (+ supplied-p)
    Lexical optionals and keywords can be unboxed if:
	a. there is more then one reference in the body
	b. they are not referenced in closures
 3. binding is performed for:
	special or unboxed requireds
	optionals, rest, keywords
 4. the function name is optionally pushed onto the IHS when
    the caller asks for it.
|#

(defun c2lambda-expr
    (lambda-list body cfun fname use-narg
                 &optional closure-type local-entry-p
		 &aux (requireds (first lambda-list))
		 (optionals (second lambda-list))
		 (rest (third lambda-list)) rest-loc
		 (keywords (fifth lambda-list))
		 (allow-other-keys (sixth lambda-list))
		 (nreq (length requireds))
		 (nopt (/ (length optionals) 3))
		 (nkey (/ (length keywords) 4))
		 (varargs (or optionals rest keywords allow-other-keys))
                 (fname-in-ihs-p (or (policy-debug-variable-bindings)
                                     (and (policy-debug-ihs-frame)
                                          fname)))
		 simple-varargs
		 (*permanent-data* t)
		 (*unwind-exit* *unwind-exit*)
		 (*env* *env*)
		 (last-arg))
  (declare (fixnum nreq nkey))

  (if (and fname ;; named function
	   ;; no required appears in closure,
	   (dolist (var (car lambda-list) t)
	     (declare (type var var))
	     (when (var-ref-ccb var) (return nil)))
	   (null (second lambda-list))	;; no optionals,
	   (null (third lambda-list))	;; no rest parameter, and
	   (null (fourth lambda-list)))	;; no keywords.
    (setf *tail-recursion-info* (cons *tail-recursion-info* (car lambda-list)))
    (setf *tail-recursion-info* nil))

  ;; For local entry functions arguments are processed by t3defun.
  ;; They must have a fixed number of arguments, no optionals, rest, etc.
  (when (and local-entry-p varargs)
    (baboon))

  ;; check arguments
  (unless (or local-entry-p (not (policy-check-nargs)))
    (incf *inline-blocks*)
    (if (and use-narg (not varargs))
	(wt-nl "if (ecl_unlikely(narg!=" nreq ")) FEwrong_num_arguments_anonym();")
	(when varargs
	  (when requireds
	    (wt-nl "if (ecl_unlikely(narg<" nreq ")) FEwrong_num_arguments_anonym();"))
	  (unless (or rest keywords allow-other-keys)
	    (wt-nl "if (ecl_unlikely(narg>" (+ nreq nopt) ")) FEwrong_num_arguments_anonym();"))))
    (wt-nl "{"))

  ;; If the number of required arguments exceeds the number of variables we
  ;; want to pass on the C stack, we pass some of the arguments to the list
  ;; of optionals, which will eventually get passed in the lisp stack.
  (when (> nreq si::c-arguments-limit)
    (setf nopt (+ nopt (- nreq si::c-arguments-limit))
	  nreq si::c-arguments-limit)
    (setf optionals (nconc (reduce #'nconc (mapcar #'(lambda (var) (list var *c1nil* NIL))
						   (subseq requireds si::c-arguments-limit)))
				  (rest optionals))
	  requireds (subseq requireds 0 si::c-arguments-limit)
	  varargs t))

  ;; For each variable, set its var-loc.
  ;; For optional and keyword parameters, and lexical variables which
  ;; can be unboxed, this will be a new LCL.
  ;; The bind step later will assign to such variable.
  (let* ((req0 *lcl*)
	 (lcl (+ req0 nreq)))
    (declare (fixnum lcl))
    (labels ((wt-decl (var)
               (wt-nl)
               (wt *volatile* (rep-type-name (var-rep-type var)) " ")
               (wt-lcl (incf lcl)) (wt ";")
               `(LCL ,lcl))
             (do-decl (var)
	       (when (local var) ; no LCL needed for SPECIAL or LEX
		 (setf (var-loc var) (wt-decl var)))))
      (loop for var in requireds
	 for reqi of-type fixnum from (1+ req0)
	 do (cond (local-entry-p
		   (bind `(LCL ,reqi) var))
		  ((unboxed var) ; create unboxed variable
		   (setf (var-loc var) (wt-decl var)))))
      ;; dont create rest or varargs if not used
      (when (and rest (< (var-ref rest) 1))
	(setq rest nil
              varargs (or optionals keywords allow-other-keys)))
      (do ((opt optionals (cdddr opt)))
	  ((endp opt))
        (do-decl (first opt))
        (when (third opt) (do-decl (third opt))))
      (when rest (setq rest-loc (wt-decl rest)))
      (do ((key keywords (cddddr key)))
	  ((endp key))
        (do-decl (second key))
        (when (fourth key) (do-decl (fourth key)))))

    (when varargs
      (let ((first-arg (cond ((plusp nreq) (format nil "V~d" (+ req0 nreq)))
			     ((eq closure-type 'LEXICAL) (format nil "lex~D" (1- *level*)))
			     (t "narg"))))
	(wt-nl
	  (format nil
	     (if (setq simple-varargs (and (not (or rest keywords allow-other-keys))
					   (< (+ nreq nopt) 30)))
		 "va_list args; va_start(args,~a);"
		 "cl_va_list args; cl_va_start(args,~a,narg,~d);")
	     first-arg nreq))))

    ;; Bind required parameters.
    (unless local-entry-p
      (loop for var in requireds
	 for reqi of-type fixnum from (1+ req0)
	 do (bind `(LCL ,reqi) var)))

    (when fname-in-ihs-p
      (wt-nl "{")
      (setf *ihs-used-p* t)
      (push 'IHS *unwind-exit*)
      (when (policy-debug-variable-bindings)
        (build-debug-lexical-env (reverse requireds) t))
      (wt-nl "ecl_ihs_push(cl_env_copy,&ihs," (add-symbol fname)
             ",_ecl_debug_env);"))

    (setq *lcl* lcl))

  ;; Bind optional parameters as long as there remain arguments.
  (when optionals
    ;; When binding optional values, we use two calls to BIND. This means
    ;; 'BDS-BIND is pushed twice on *unwind-exit*, which results in two calls
    ;; to bds_unwind1(), which is wrong. A simple fix is to save *unwind-exit*
    ;; which is what we do here.
    (let ((va-arg-loc (if simple-varargs 'VA-ARG 'CL-VA-ARG)))
      ;; counter for optionals
      (wt-nl "{int i=" nreq ";")
      (do ((opt optionals (cdddr opt)))
	  ((endp opt))
	(wt-nl "if (i >= narg) {")
	  (bind-init (second opt) (first opt))
	  (when (third opt) (bind nil (third opt)))
	(wt-nl "} else {")
	  (wt-nl "i++;")
	  (let ((*unwind-exit* *unwind-exit*))
	    (bind va-arg-loc (first opt)))
	  (when (third opt) (bind t (third opt)))
	(wt-nl "}"))
      (wt "}")))

  (when (or rest keywords allow-other-keys)
    (cond ((not (or keywords allow-other-keys))
	   (wt-nl rest-loc "=cl_grab_rest_args(args);"))
	  (t
	   (cond (keywords
		  (wt-nl "{ cl_object keyvars[" (* 2 nkey) "];")
		  (wt-nl "cl_parse_key(args," nkey "," cfun "keys,keyvars"))
		 (t
		  (wt-nl "cl_parse_key(args,0,NULL,NULL")))
	   ;; This explicit coercion is required to remove the "volatile"
	   ;; declaration on some variables.
	   (if rest (wt ",(cl_object*)&" rest-loc) (wt ",NULL"))
	   (wt (if allow-other-keys ",TRUE);" ",FALSE);"))))
    (when rest (bind rest-loc rest)))

  (when varargs
    (wt-nl (if simple-varargs "va_end(args);" "cl_va_end(args);")))

  ;;; Bind keywords.
  (do ((kwd keywords (cddddr kwd))
       (all-kwd nil)
       (KEYVARS[i] `(KEYVARS 0))
       (i 0 (1+ i)))
      ((endp kwd)
       (when all-kwd
	 (wt-nl-h "#define " cfun "keys (&" (add-keywords (nreverse all-kwd)) ")")
	 (wt-nl "}")))
    (declare (fixnum i))
    (push (first kwd) all-kwd)
    (let ((key (first kwd))
	  (var (second kwd))
	  (init (third kwd))
	  (flag (fourth kwd)))
      (cond ((and (eq (c1form-name init) 'LOCATION)
		  (null (c1form-arg 0 init)))
	     ;; no initform
	     ;; Cnil has been set in keyvars if keyword parameter is not supplied.
	     (setf (second KEYVARS[i]) i)
	     (bind KEYVARS[i] var))
	    (t
	     ;; with initform
	     (setf (second KEYVARS[i]) (+ nkey i))
	     (wt-nl "if(") (wt-loc KEYVARS[i]) (wt "==Cnil){")
	     (let ((*unwind-exit* *unwind-exit*))
	       (bind-init init var))
	     (wt-nl "}else{")
	     (setf (second KEYVARS[i]) i)
	     (bind KEYVARS[i] var)
	     (wt "}")))
      (when flag
	(setf (second KEYVARS[i]) (+ nkey i))
	(bind KEYVARS[i] flag))))

  (when *tail-recursion-info*
    (push 'TAIL-RECURSION-MARK *unwind-exit*)
    (wt-nl1 "TTL:"))

  ;;; Now the parameters are ready, after all!
  (c2expr body)

  ;;; Closing braces is done i cmptop.lsp
  (when fname-in-ihs-p
    (wt-nl "}"))
  )

(defun optimize-funcall/apply-lambda (lambda-form arguments apply-p
				      &aux body apply-list apply-var
				      let-vars extra-stmts all-keys)
  (multiple-value-bind (requireds optionals rest key-flag keywords
				  allow-other-keys aux-vars)
      (cmp-process-lambda-list (car lambda-form))
    (when apply-p
      (setf apply-list (first (last arguments))
	    apply-var (gensym)
	    arguments (butlast arguments)))
    (setf arguments (copy-list arguments))
    (do ((scan arguments (cdr scan)))
	((endp scan))
      (let ((form (first scan)))
	(unless (constantp form)
	  (let ((aux-var (gensym)))
	    (push `(,aux-var ,form) let-vars)
	    (setf (car scan) aux-var)))))
    (when apply-var
      (push `(,apply-var ,apply-list) let-vars))
    (dolist (i (cdr requireds))
      (push (list i
		  (cond (arguments
			 (pop arguments))
			(apply-p
			 `(if ,apply-var
			   (pop ,apply-var)
			   (si::dm-too-few-arguments)))
			(t
			 (cmperr "Too few arguments for lambda form ~S"
                                 (cons 'LAMBDA lambda-form)))))
	    let-vars))
    (do ((scan (cdr optionals) (cdddr scan)))
	((endp scan))
      (let ((opt-var (first scan))
	    (opt-flag (third scan))
	    (opt-value (second scan)))
	(cond (arguments
	       (setf let-vars
		     (list* `(,opt-var ,(pop arguments))
			    `(,opt-flag t)
			    let-vars)))
	      (apply-p
	       (setf let-vars
		     (list* `(,opt-var (if ,apply-var
					   (pop ,apply-var)
					   ,opt-value))
			    `(,opt-flag ,apply-var)
			    let-vars)))
	      (t
	       (setf let-vars
		     (list* `(,opt-var ,opt-value)
			    `(,opt-flag nil)
			    let-vars))))))
    (when (or key-flag allow-other-keys)
      (unless rest
	(setf rest (gensym))))
    (when rest
      (push `(,rest ,(if arguments
			 (if apply-p
			     `(list* ,@arguments ,apply-var)
			     `(list ,@arguments))
			 (if apply-p apply-var nil)))
	    let-vars))
    (loop while aux-vars
       do (push (list (pop aux-vars) (pop aux-vars)) let-vars))
    (do ((scan (cdr keywords) (cddddr scan)))
	((endp scan))
      (let ((keyword (first scan))
	    (key-var (second scan))
	    (key-value (third scan))
	    (key-flag (or (fourth scan) (gensym))))
	(push keyword all-keys)
	(setf let-vars
	      (list*
	       `(,key-var (if (eq ,key-flag 'si::failed) ,key-value ,key-flag))
	       `(,key-flag (si::search-keyword ,rest ,keyword))
	       let-vars))
	(when (fourth scan)
	  (push `(setf ,key-flag (not (eq ,key-flag 'si::failed)))
		extra-stmts))))
    (when (and key-flag (not allow-other-keys))
      (push `(si::check-keyword ,rest ',all-keys) extra-stmts))
    `(let* ,(nreverse (delete-if-not #'first let-vars))
       ,@(and apply-var `((declare (ignorable ,apply-var))))
      ,@(multiple-value-bind (decl body)
	   (si::find-declarations (rest lambda-form))
	 (append decl extra-stmts body)))))
