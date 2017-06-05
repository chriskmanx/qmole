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
;;; Funcallable object
;;; ----------------------------------------------------------------------

(defclass funcallable-standard-object (standard-object function) ())

;;; ----------------------------------------------------------------------
;;; Generic Functions
;;; ----------------------------------------------------------------------

(defclass generic-function (standard-object function) ()
  (:metaclass 'funcallable-standard-class))

(defclass standard-generic-function (generic-function)
  #.(remove-accessors +standard-generic-function-slots+)
  (:metaclass 'funcallable-standard-class))

;;;----------------------------------------------------------------------
;;; Method
;;; ----------------------------------------------------------------------

(defclass method () ())

(defclass standard-method (method)
  #.(remove-accessors +standard-method-slots+))


(defun function-keywords (method)
  (multiple-value-bind (reqs opts rest-var key-flag keywords)
      (si::process-lambda-list (slot-value method 'lambda-list) 'function)
    (declare (ignore reqs opts rest-var))
    (when key-flag
      (do* ((output '())
	    (l (cdr keywords) (cddddr l)))
	   ((endp l)
	    output)
	(push (first l) output)))))

(defclass standard-accessor-method (standard-method)
  ((slot-definition :initarg :slot-definition
		    :initform nil 
		    :reader accessor-method-slot-definition)))

(defclass standard-reader-method (standard-accessor-method) ())

(defclass standard-writer-method (standard-accessor-method) ())

(defmethod shared-initialize ((method standard-method) slot-names &rest initargs)
  (declare (ignore initargs method slot-names))
  (add-method-keywords (call-next-method)))
