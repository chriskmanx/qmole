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

;;;; CMPMULT  Multiple-value-call and Multiple-value-prog1.

(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
(defun c1multiple-value-call (args &aux forms)
  (check-args-number 'MULTIPLE-VALUE-CALL args 1)
  (cond
   ;; (M-V-C #'FUNCTION) => (FUNCALL #'FUNCTION)
   ((endp (rest args))
    (c1funcall args))
   ;; (M-V-C #'FUNCTION (VALUES A ... Z)) => (FUNCALL #'FUNCTION A ... Z)
   ((and (= (length args) 2)
	 (consp (setq forms (second args)))
	 (eq 'VALUES (first forms)))
    (c1funcall (list* (first args) (rest forms))))
   ;; More complicated case.
   (t
    (let ((function (gensym))
	  (frame (gensym)))
      `(with-stack ,frame
	 (let* ((,function ,(first args)))
	   ,@(loop for i in (rest args)
		collect `(stack-push-values ,frame ,i))
	   (si::apply-from-stack-frame ,frame ,function)))))))

(defun c1multiple-value-prog1 (args)
  (check-args-number 'MULTIPLE-VALUE-PROG1 args 1)
  (let ((frame (gensym)))
    `(with-stack ,frame
       (stack-push-values ,frame ,(first args))
       ,@(rest args)
       (stack-pop ,frame))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Beppe:
;;; this is the WRONG way to handle 1 value problem.
;;; should be done in c2values, so that (values (truncate a b)) can
;;; be used to restrict to one value, so we would not have to warn
;;; if this occurred in a proclaimed fun.

(defun c1values (args)
  (make-c1form* 'VALUES :args (c1args* args)))

(defun c2values (c1form forms)
  (declare (ignore c1form))
  (when (and (eq *destination* 'RETURN-OBJECT)
             (rest forms)
             (consp *current-form*)
             (eq 'DEFUN (first *current-form*)))
    (cmpwarn "Trying to return multiple values. ~
              ~%;But ~a was proclaimed to have single value.~
              ~%;Only first one will be assured."
             (second *current-form*)))
  (cond
   ;; When the values are not going to be used, then just
   ;; process each form separately.
   ((eq *destination* 'TRASH)
    (mapc #'c2expr* forms)
    ;; We really pass no value, but we need UNWIND-EXIT to trigger all the
    ;; frame-pop, stack-pop and all other exit forms.
    (unwind-exit 'VALUE0)
    )
   ;; For (VALUES) we can replace the output with either NIL (if the value
   ;; is actually used) and set only NVALUES when the value is the output
   ;; of a function.
   ((endp forms)
    (cond ((eq *destination* 'RETURN)
	   (wt-nl "value0=Cnil; cl_env_copy->nvalues=0;")
	   (unwind-exit 'RETURN))
	  ((eq *destination* 'VALUES)
	   (wt-nl "cl_env_copy->values[0]=Cnil; cl_env_copy->nvalues=0;")
	   (unwind-exit 'VALUES))
	  (t
	   (unwind-exit 'NIL))))
   ;; For a single form, we must simply ensure that we only take a single
   ;; value of those that the function may output.
   ((endp (rest forms))
    (let ((form (first forms)))
      (if (or (not (member *destination* '(RETURN VALUES)))
              (c1form-single-valued-p form))
          (c2expr form)
          (progn
            (let ((*destination* 'VALUE0)) (c2expr* form))
            (unwind-exit 'VALUE0)))))
   ;; In all other cases, we store the values in the VALUES vector,
   ;; and force the compiler to retrieve anything out of it.
   (t
    (let* ((nv (length forms))
	   (*inline-blocks* 0)
           (*temp* *temp*)
	   (forms (nreverse (coerce-locs (inline-args forms)))))
      ;; By inlining arguments we make sure that VL has no call to funct.
      ;; Reverse args to avoid clobbering VALUES(0)
      (wt-nl "cl_env_copy->nvalues=" nv ";")
      (do ((vl forms (rest vl))
	   (i (1- (length forms)) (1- i)))
	  ((null vl))
	(declare (fixnum i))
	(wt-nl "cl_env_copy->values[" i "]=" (first vl) ";"))
      (unwind-exit 'VALUES)
      (close-inline-blocks)))))

(defun c1multiple-value-setq (args &aux (vars nil) (temp-vars nil)
			      (late-bindings nil))
  (check-args-number 'MULTIPLE-VALUE-SETQ args 2 2)
  (dolist (var (reverse (first args)))
    (cmpck (not (symbolp var)) "The variable ~s is not a symbol." var)
    (let* ((var-or-form (chk-symbol-macrolet var))
	   (type t))
      (unless (when (symbolp var-or-form)
		(cmpck (constantp var-or-form)
		       "The constant ~s is being assigned a value." var-or-form)
		(when (or (not (policy-type-assertions))
			  (trivial-type-p
			   (setf type (variable-type-in-env var-or-form))))
		  (push var-or-form vars)
		  t))
	(let ((new-var (gensym)))
	  (push new-var vars)
	  (push new-var temp-vars)
	  (push `(setf ,var-or-form (checked-value ,type ,new-var)) late-bindings)))))
  (let ((value (second args)))
    (cond (temp-vars
	   `(let* (,@temp-vars)
	      (multiple-value-setq ,vars ,value)
	      ,@late-bindings))
	  ((endp vars)
	   `(values ,value))
	  ((= (length vars) 1)
	   `(setq ,(first vars) ,value))
	  (t
	   (setq value (c1expr value)
		 vars (mapcar #'c1vref vars))
	   (add-to-set-nodes-of-var-list
	    vars (make-c1form* 'MULTIPLE-VALUE-SETQ :args vars value))))))

(defun do-m-v-setq-fixed (nvalues vars form use-bind &aux (output (first vars)))
  ;; This routine should evaluate FORM and store the values (whose amount
  ;; is known to be NVALUES) into the variables VARS. The output is a
  ;; place from where the first value can be retreived.
  ;; INV: There is at least one variable.
  ;;
  (if (or (> nvalues 1) use-bind)
      (let ((*destination* 'VALUES))
	(c2expr* form)
	(loop for i from 0 below nvalues
	   while vars
	   do (funcall (if use-bind #'bind-var #'set-var)
		       (values-loc i) (pop vars))))
      (let ((*destination* (pop vars)))
	(c2expr* form)))
  (dolist (v vars)
    (if use-bind
	(bind (c1form-arg 0 (default-init v)) v)
	(set-var '(C-INLINE (:object) "Cnil" () t nil) v)))
  output)

(defun do-m-v-setq-any (min-values max-values vars use-bind)
  ;; This routine moves values from the multiple-value stack into the
  ;; variables VARS. The amount of values is not known (or at least we only
  ;; know that there is some number between MIN-VALUES and MAX-VALUES).  If
  ;; the values are to be created with BIND, then USED-BIND=T.  The output of
  ;; this routine is a location containing the first value (typically, the
  ;; name of the first variable).
  ;;
  (let* ((*lcl* *lcl*)
         (nr (make-lcl-var :type :int))
	 (output (first vars))
	 (labels '()))
    ;; We know that at least MIN-VALUES variables will get a value
    (dotimes (i min-values)
      (when vars
	(let ((v (pop vars))
	      (loc (values-loc i)))
	  (if use-bind (bind loc v) (set-var loc v)))))
    ;; If there are more variables, we have to check whether there
    ;; are enough values left in the stack.
    (when vars
      (wt-nl "{int " nr "=cl_env_copy->nvalues-" min-values ";")
      ;;
      ;; Loop for assigning values to variables
      ;;
      (do (;; We call BIND twice for each variable. Hence, we need to
	   ;; remove spurious BDS-BIND from the list. See also C2LAMBDA.
	   (*unwind-exit* *unwind-exit*)
	   (vs vars (rest vs))
	   (i min-values (1+ i)))
	  ((or (endp vs) (= i max-values)))
	(declare (fixnum i))
	(let ((loc (values-loc i))
	      (v (first vs))
	      (label (next-label)))
	  (wt-nl "if (" nr "--<=0) ") (wt-go label)
	  (push label labels)
	  (if use-bind (bind loc v) (set-var loc v))))
      ;;
      ;; Loop for setting default values when there are less output than vars.
      ;;
      (let ((label (next-label)))
	(wt-nl) (wt-go label) (wt "}")
	(push label labels)
	(setq labels (nreverse labels))
	(dolist (v vars)
	  (when labels (wt-label (pop labels)))
	  (if use-bind
	      (bind '(C-INLINE (:object) "Cnil" () t nil) v)
	      (set-var '(C-INLINE (:object) "Cnil" () t nil) v)))
	(when labels (wt-label label))))
    output))

(defun c2multiple-value-setq (c1form vars form)
  (declare (ignore c1form))
  (multiple-value-bind (min-values max-values)
      (c1form-values-number form)
    (unwind-exit 
     (if (= min-values max-values)
	 (do-m-v-setq-fixed min-values vars form nil)
	 (let ((*destination* 'VALUES))
	   (c2expr* form)
	   (do-m-v-setq-any min-values max-values vars nil))))))

(defun c1multiple-value-bind (args)
  (check-args-number 'MULTIPLE-VALUE-BIND args 2)
  (let* ((*cmp-env* (cmp-env-copy))
         (variables (pop args))
         (init-form (pop args)))
    (when (= (length variables) 1)
      (return-from c1multiple-value-bind
        `(let* ((,(first variables) ,init-form))
	   ,@args)))
    (multiple-value-bind (body ss ts is other-decls)
        (c1body args nil)
      (c1declare-specials ss)
      (let* ((vars (loop for name in variables
                      collect (c1make-var name ss is ts))))
        (setq init-form (c1expr init-form))
        (mapc #'push-vars vars)
        (check-vdecl variables ts is)
        (setq body (c1decl-body other-decls body))
        (mapc #'check-vref vars)
        (make-c1form* 'MULTIPLE-VALUE-BIND :type (c1form-type body)
                      :local-vars vars
                      :args vars init-form body)))))

(defun c2multiple-value-bind (c1form vars init-form body)
  (declare (ignore c1form))
  ;; 0) Compile the form which is going to give us the values
  (let ((*destination* 'VALUES)) (c2expr* init-form))

  (let* ((*unwind-exit* *unwind-exit*)
	 (*env-lvl* *env-lvl*)
	 (*env* *env*)
	 (*lcl* *lcl*)
	 (labels nil)
	 (env-grows nil)
	 (nr (make-lcl-var :type :int))
	 min-values max-values)
    ;; 1) Retrieve the number of output values
    (wt-nl "{")
    (multiple-value-setq (min-values max-values)
      (c1form-values-number init-form))

    ;; 2) For all variables which are not special and do not belong to
    ;;    a closure, make a local C variable.
    (dolist (var vars)
      (declare (type var var))
      (let ((kind (local var)))
	(if kind
	  (progn
	    (bind (next-lcl) var)
	    (wt-nl *volatile* (rep-type-name kind) " " var ";")
	    (wt-comment (var-name var)))
	  (unless env-grows (setq env-grows (var-ref-ccb var))))))

    ;; 3) If there are closure variables, set up an environment.
    (when (setq env-grows (env-grows env-grows))
      (let ((env-lvl *env-lvl*))
	(wt-nl "volatile cl_object env" (incf *env-lvl*)
	       " = env" env-lvl ";")))

    ;; 4) Assign the values to the variables
    (do-m-v-setq-any min-values max-values vars t)

    ;; 5) Compile the body. If there are bindings of special variables,
    ;;    these bindings are undone here.
    (c2expr body)

    ;; 6) Close the C expression.
    (wt "}"))
  )

