;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  CMPOPT-BITS  -- Optimize operations acting on bits
;;;;

(in-package "COMPILER")

;;;
;;; LDB
;;; Look for inline expansion of LDB1 in sysfun.lsp
;;;

(define-compiler-macro ldb (&whole whole &rest args)
  (let ((arg1 (first args))
	(len (integer-length most-positive-fixnum))
	size pos)
    (if (and (consp arg1)
	     (eq 'BYTE (car arg1))
	     (integerp (setq size (second arg1)))
	     (integerp (setq pos (third arg1)))
	     (<= (+ size pos) len)
	     (subtypep (result-type (second args)) 'FIXNUM))
	`(truly-the fixnum (ldb1 ,size ,pos ,(second args)))
	whole)))

;;;
;;; ASH
;;; Bit fiddling. It is a bit tricky because C does not allow
;;; shifts in << or >> which exceed the integer size. In those
;;; cases the compiler may do whatever it wants (and gcc does!)
;;;

(define-compiler-macro ash (&whole whole argument shift)
  (if (and (policy-assume-right-type)
           (integerp shift))
      (if (zerop shift)
          argument
          `(shift ,argument ,shift))
      whole))

(define-c-inliner shift (return-type argument orig-shift)
  (let* ((arg-type (inlined-arg-type argument))
         (arg-c-type (lisp-type->rep-type arg-type))
	 (return-c-type (lisp-type->rep-type return-type))
         (shift (loc-immediate-value (inlined-arg-loc orig-shift))))
    (if (or (not (c-integer-rep-type-p arg-c-type))
            (not (c-integer-rep-type-p return-c-type)))
        (produce-inline-loc (list argument orig-shift) '(:object :fixnum) '(:object)
                            "ecl_ash(#0,#1)" nil t)
        (let* ((arg-bits (c-integer-rep-type-bits arg-c-type))
	       (return-bits (c-integer-rep-type-bits return-c-type))
	       (max-type (if (and (plusp shift)
				  (< arg-bits return-bits))
			   return-c-type
			   arg-c-type)))
	  (produce-inline-loc (list argument) (list max-type) (list return-type)
			      (format nil
				      (if (minusp shift)
					  "((#0) >> (~D))"
					  "((#0) << (~D))")
				      (abs shift))
			      nil t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPE PROPAGATION
;;;

(def-type-propagator logand (a b)
  (let ((output (dolist (int-type '((UNSIGNED-BYTE 8) FIXNUM) 'INTEGER)
		  (when (or (subtypep a int-type)
			    (subtypep b int-type))
		    (return int-type)))))
    (values (list a b) output)))
