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
;;;                                                                  slots

#|
(defclass effective-slot-definition (slot-definition))

(defclass direct-slot-definition (slot-definition))

(defclass standard-slot-definition (slot-definition))

(defclass standard-direct-slot-definition (standard-slot-definition direct-slot-definition))

(defclass standard-effective-slot-definition (standard-slot-definition direct-slot-definition))
|#

(defun convert-one-class (class)
  (let* ((direct-slots (class-direct-slots class))
	 (effective-slots (class-slots class))
	 (new-direct-slots
	  (loop for x in direct-slots
		collect (if (consp x)
			    (apply #'make-instance 'standard-direct-slot-definition
				   (slot-definition-to-plist x))
			    x)))
	 (new-effective-slots
	  (loop for x in effective-slots
		collect (if (consp x)
			    (apply #'make-instance 'standard-effective-slot-definition
				   (slot-definition-to-plist x))
			    x))))
    (map-into direct-slots #'identity new-direct-slots)
    (map-into effective-slots #'identity new-effective-slots)
    (when (typep class 'std-class)
      (std-create-slots-table class)))
  (mapc #'convert-one-class (class-direct-subclasses class)))

;;;
;;; We cannot redefine the class for slot definitions because this
;;; causes an infinite loop. Hence, we avoid evaluating the following
;;; forms at compile time.
;;;
(eval-when (:load-toplevel :execute)
  (eval
   `(progn
     (defclass slot-definition ()
       ,(mapcar #'(lambda (x) (butlast x 2)) +slot-definition-slots+))
     (defclass standard-slot-definition (slot-definition) ())
     (defclass direct-slot-definition (slot-definition) ())
     (defclass effective-slot-definition (slot-definition) ())
     (defclass standard-direct-slot-definition (standard-slot-definition direct-slot-definition) ())
     (defclass standard-effective-slot-definition (standard-slot-definition effective-slot-definition) ())))
  (make-instances-obsolete (find-class 't))
  (convert-one-class (find-class 't)))

(defmethod reader-method-class ((class std-class)
				(direct-slot direct-slot-definition)
				&rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-reader-method))

(defmethod writer-method-class ((class std-class)
				(direct-slot direct-slot-definition)
				&rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-writer-method))

;;; ----------------------------------------------------------------------
;;; Fixup

(dolist (method-info *early-methods*)
  (let* ((method-name (car method-info))
	 (gfun (fdefinition method-name))
	 (standard-method-class (find-class 'standard-method)))
    (when (eq 'T (class-id (si:instance-class gfun)))
      ;; complete the generic function object
      (si:instance-class-set gfun (find-class 'STANDARD-GENERIC-FUNCTION))
      (si::instance-sig-set gfun)
      (setf (generic-function-method-class gfun) standard-method-class)
      (setf (slot-value gfun 'documentation) nil)
      )
    (dolist (method (cdr method-info))
      ;; complete the method object
      (let ((old-class (si::instance-class method)))
	(si::instance-class-set method
				(cond ((null old-class)
				       (find-class 'standard-method))
				      ((symbolp old-class)
				       (find-class (truly-the symbol old-class)))
				      (t
				       old-class))))
      (si::instance-sig-set gfun)
      )
    (makunbound '*EARLY-METHODS*)))


;;; ----------------------------------------------------------------------
;;;                                                              redefined

(defun method-p (method) (typep method 'METHOD))

(defun make-method (method-class qualifiers specializers arglist
				 function plist options)
  (apply #'make-instance
	 method-class
	 :generic-function nil
	 :qualifiers qualifiers
	 :lambda-list arglist
	 :specializers specializers
	 :function function
	 :plist plist
	 :allow-other-keys t
	 options))

(defun all-keywords (l)
  (declare (si::c-local))
  (let ((all-keys '()))
    (do ((l (rest l) (cddddr l)))
	((null l)
	 all-keys)
      (push (first l) all-keys))))

(defun congruent-lambda-p (l1 l2)
  (multiple-value-bind (r1 opts1 rest1 key-flag1 keywords1 a-o-k1)
      (si::process-lambda-list l1 'FUNCTION)
    (declare (ignore a-o-k1))
    (multiple-value-bind (r2 opts2 rest2 key-flag2 keywords2 a-o-k2)
	(si::process-lambda-list l2 'FUNCTION)
	(and (= (length r2) (length r1))
	     (= (length opts1) (length opts2))
	     (eq (and (null rest1) (null key-flag1))
		 (and (null rest2) (null key-flag2)))
	     ;; All keywords mentioned in the genericf function
	     ;; must be accepted by the method.
	     (or (null key-flag1)
		 (null key-flag2)
		 a-o-k2
		 (null (set-difference (all-keywords keywords1)
				       (all-keywords keywords2))))
	     t))))

(defun add-method (gf method)
  (declare (notinline method-qualifiers)) ; during boot it's a structure accessor
  ;;
  ;; 1) The method must not be already installed in another generic function.
  ;;
  (let ((other-gf (method-generic-function method)))
    (unless (or (null other-gf) (eq other-gf gf))
      (error "The method ~A belongs to the generic function ~A ~
and cannot be added to ~A." method other-gf gf)))
  ;;
  ;; 2) The method and the generic function should have congruent lambda
  ;;    lists. That is, it should accept the same number of required and
  ;;    optional arguments, and only accept keyword arguments when the generic
  ;;    function does.
  ;;
  (let ((new-lambda-list (method-lambda-list method)))
    (if (slot-boundp gf 'lambda-list)
	(let ((old-lambda-list (generic-function-lambda-list gf)))
	  (unless (congruent-lambda-p old-lambda-list new-lambda-list)
	    (error "Cannot add the method ~A to the generic function ~A because ~
their lambda lists ~A and ~A are not congruent."
		   method gf old-lambda-list new-lambda-list)))
	(reinitialize-instance gf :lambda-list new-lambda-list)))
  ;;
  ;; 3) Finally, it is inserted in the list of methods, and the method is
  ;;    marked as belonging to a generic function.
  ;;
  (when (generic-function-methods gf)
    (let* ((method-qualifiers (method-qualifiers method)) 
	   (specializers (method-specializers method))
	   found)
      (when (setq found (find-method gf method-qualifiers specializers nil))
	(remove-method gf found))))
  ;;
  ;; We install the method by:
  ;;  i) Adding it to the list of methods
  (push method (generic-function-methods gf))
  (setf (method-generic-function method) gf)
  ;;  ii) Updating the specializers list of the generic function. Notice that
  ;;  we should call add-direct-method for each specializer but specializer
  ;;  objects are not yet implemented
  #+(or)
  (dolist (spec (method-specializers method))
    (add-direct-method spec method))
  ;;  iii) Computing a new discriminating function... Well, since the core
  ;;  ECL does not need the discriminating function because we always use
  ;;  the same one, we just update the spec-how list of the generic function.
  (compute-g-f-spec-list gf)
  (set-generic-function-dispatch gf)
  ;;
  ;; Finally update the dependent objects
  (map-dependents gf #'(lambda (dep) (update-dependents gf dep 'add-method method)))
  ;;
  gf)

(defun function-to-method (name signature)
  (let* ((aux-name 'temp-method)
         (method (eval `(defmethod ,aux-name ,signature)))
         (generic-function (fdefinition aux-name)))
    (setf (method-function method) (fdefinition name))
    (setf (fdefinition name) generic-function)
    (setf (generic-function-name generic-function) name)
    (fmakunbound aux-name)))

(function-to-method 'add-method '((gf standard-generic-function)
                                  (method standard-method)))

(defun remove-method (gf method)
  (setf (generic-function-methods gf)
	(delete method (generic-function-methods gf))
	(method-generic-function method) nil)
  (si:clear-gfun-hash gf)
  (map-dependents gf #'(lambda (dep) (update-dependents gf dep 'remove-method method)))
  gf)

;;; ----------------------------------------------------------------------
;;; Error messages

(defmethod no-applicable-method (gf args)
  (error "No applicable method for ~S with arguments of types~{~& ~A~}" 
	 (generic-function-name gf)
         (mapcar #'type-of args)))

(defmethod no-next-method (gf method &rest args)
  (declare (ignore gf))
  (error "In method ~A~%No next method given arguments ~A" method args))

(defun no-primary-method (gf &rest args)
  (error "Generic function: ~A. No primary method given arguments: ~S"
	 (generic-function-name gf) args))

;;; Now we protect classes from redefinition:
(eval-when (compile load)
(defun setf-find-class (new-value name &optional errorp env)
  (declare (ignore errorp))
  (let ((old-class (find-class name nil env)))
    (cond
      ((typep old-class 'built-in-class)
       (error "The class associated to the CL specifier ~S cannot be changed."
	      name))
      ((member name '(CLASS BUILT-IN-CLASS) :test #'eq)
       (error "The kernel CLOS class ~S cannot be changed." name))
      ((classp new-value)
       (setf (gethash name si:*class-name-hash-table*) new-value))
      ((null new-value) (remhash name si:*class-name-hash-table*))
      (t (error "~A is not a class." new-value))))
  new-value)
)

;;; ----------------------------------------------------------------------
;;; DEPENDENT MAINTENANCE PROTOCOL
;;;

(function-to-method 'map-dependents '((c standard-generic-function) function))

(defmethod map-dependents ((c class) function)
  (dolist (d (class-dependents c))
    (funcall function c)))

(function-to-method 'add-dependent '((c standard-generic-function) function))

(defmethod add-dependent ((c class) dep)
  (pushnew dep (class-dependents c)))

(defmethod remove-dependent ((c standard-generic-function) dep)
  (setf (generic-function-dependents c)
        (remove dep (generic-function-dependents c))))

(defmethod remove-dependent ((c class) dep)
  (setf (class-dependents c)
        (remove dep (class-dependents c))))

(defgeneric update-dependents (object dependents &rest initargs))

(defclass initargs-updater ()
  ())

(defun recursively-update-classes (a-class)
  (slot-makunbound a-class 'valid-initargs)
  (mapc #'recursively-update-classes (class-direct-subclasses a-class)))

(defmethod update-dependents ((object generic-function) (dep initargs-updater)
                               &rest initargs)
  (declare (ignore dep initargs object))
  (recursively-update-classes +the-class+))

(setf *clos-booted* t)

(let ((x (make-instance 'initargs-updater)))
  (add-dependent #'shared-initialize x)
  (add-dependent #'initialize-instance x)
  (add-dependent #'allocate-instance x))
