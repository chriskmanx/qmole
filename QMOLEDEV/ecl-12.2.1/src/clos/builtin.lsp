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
;;; Methods

;;; ======================================================================
;;; Built-in classes
;;; ----------------------------------------------------------------------
;;;
;;; IMPORTANT!
;;; This class did not exist until now. This was no problem, because it is
;;; not used anywhere in ECL. However, we have to define and we have to
;;; ensure that "T" becomes an instance of BUILT-IN-CLASS.

;;; We have to build the class manually, because
;;;	(ENSURE-CLASS-USING-CLASS NIL ...)
;;; does not work yet, since the class NULL does not exist.
;;;
(setf (find-class 'built-in-class)
      (make-instance (find-class 'standard-class)
		     :name 'built-in-class
		     :direct-superclasses (list (find-class 'class))
		     :direct-slots nil))

(si:instance-class-set (find-class 't) (find-class 'built-in-class))

(defmethod make-instance ((class built-in-class) &rest initargs)
  (declare (ignore initargs))
  (error "The built-in class (~A) cannot be instantiated" class))

(eval-when (:compile-toplevel :execute)
  (defconstant +builtin-classes-list+
	 '(;(t object)
	    (sequence)
	      (list sequence)
	        (cons list)
	    (array)
	      (vector array sequence)
	        (string vector)
                #+unicode
	        (base-string string vector)
	        (bit-vector vector)
	    (stream)
	      (ext:ansi-stream stream)
		(file-stream ext:ansi-stream)
		(echo-stream ext:ansi-stream)
		(string-stream ext:ansi-stream)
		(two-way-stream ext:ansi-stream)
		(synonym-stream ext:ansi-stream)
		(broadcast-stream ext:ansi-stream)
		(concatenated-stream ext:ansi-stream)
		(ext:sequence-stream ext:ansi-stream)
	    (character)
	    (number)
	      (real number)
	        (rational real)
		  (integer rational)
		  (ratio rational)
	        (float real)
	      (complex number)
	    (symbol)
	      (null symbol list)
	      (keyword symbol)
	    (method-combination)
	    (package)
	    (function)
	    (pathname)
	      (logical-pathname pathname)
	    (hash-table)
	    (random-state)
	    (readtable)
            (si::code-block)
	    (si::foreign-data)
	    (si::frame)
	    (si::weak-pointer)
	    #+threads (mp::process)
	    #+threads (mp::lock)
	    #+threads (mp::rwlock)
	    #+threads (mp::condition-variable)
	    #+semaphores (mp::semaphore)
	    #+sse2 (ext::sse-pack))))

(loop for (name . rest) in '#.+builtin-classes-list+
   with index = 1
   with built-in-class = (find-class 'built-in-class)
   with array = (make-array #.(1+ (length +builtin-classes-list+))
			    :initial-element (find-class 't))
   do (let* ((direct-superclasses (mapcar #'find-class (or rest '(t))))
	     (class (make-instance built-in-class :name name
				   :direct-superclasses direct-superclasses
				   :direct-slots nil)))
	(setf (find-class name) class
	      (aref array index) class
	      index (1+ index)))
   finally (si::*make-constant '+builtin-classes+ array))

(defmethod ensure-class-using-class ((class null) name &rest rest)
  (declare (ignore class))
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
    (declare (ignore direct-superclasses))
    (apply #'make-instance metaclass :name name options)))

(defmethod change-class ((instance t) (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class))
  class)

(defmethod make-instance ((class-name symbol) &rest initargs)
  (apply #'make-instance (find-class class-name) initargs))

(defmethod slot-makunbound-using-class ((class built-in-class) self slotd)
  (declare (ignore class self slotd))
  (error "SLOT-MAKUNBOUND-USING-CLASS cannot be applied on built-in objects"))

(defmethod slot-boundp-using-class ((class built-in-class) self slotd)
  (declare (ignore class self slotd))
  (error "SLOT-BOUNDP-USING-CLASS cannot be applied on built-in objects"))

(defmethod slot-value-using-class ((class built-in-class) self slotd)
  (declare (ignore class self slotd))
  (error "SLOT-VALUE-USING-CLASS cannot be applied on built-in objects"))

(defmethod (setf slot-value-using-class) (val (class built-in-class) self slotd)
  (declare (ignore class self slotd val))
  (error "SLOT-VALUE-USING-CLASS cannot be applied on built-in objects"))

(defmethod slot-exists-p-using-class ((class built-in-class) self slotd)
  (declare (ignore class self slotd))
  nil)

;;; ======================================================================
;;; STRUCTURES
;;;

(defclass structure-class (class)
  (slot-descriptions
   initial-offset
   defstruct-form
   constructors
   documentation
   copier
   predicate
   print-function))

;;; structure-classes cannot be instantiated
(defmethod make-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (error "The structure-class (~A) cannot be instantiated" class))

(defmethod finalize-inheritance ((class structure-class))
  (call-next-method)
  (dolist (slot (class-slots class))
    (unless (eq :INSTANCE (slot-definition-allocation slot))
      (error "The structure class ~S can't have shared slots" (class-name class)))))

;;; ----------------------------------------------------------------------
;;; Structure-object
;;;
;;; Structure-object has no slots and inherits only from t:

(defclass structure-object (t) ()
  (:metaclass structure-class))

(defmethod make-load-form ((object structure-object) &optional environment)
  (make-load-form-saving-slots object :key environment))

(defmethod print-object ((obj structure-object) stream)
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class)))
    (declare (:read-only class))
    (when (and slotds
               ;; *p-readably* effectively disables *p-level*
	       (not *print-readably*)
	       *print-level*
	       (zerop *print-level*))
      (write-string "#" stream)
      (return-from print-object obj))
    (write-string "#S(" stream)
    (prin1 (class-name class) stream)
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i))
	 (limit (or *print-length* most-positive-fixnum))
	 (sv))
	((null scan))
      (declare (fixnum i))
      (when (>= i limit)
	(write-string " ..." stream)
	(return))
      (setq sv (si:instance-ref obj i))
      (write-string " :" stream)
      (prin1 (slot-definition-name (car scan)) stream)
      (write-string " " stream)
      (prin1 sv stream))
    (write-string ")" stream)
    obj))
