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

;;;; CMPWT  Output routines.

(in-package "COMPILER")

;;; ======================================================================
;;;
;;; DATA FILES
;;;
;;; Each lisp compiled file consists on code and a data section. Whenever an
;;; #'in-package toplevel form is found, a read-time evaluated expression is
;;; inserted in the data section which changes the current package for the
;;; rest of it. This way it is possible to save some space by writing the
;;; symbol's package only when it does not belong to the current package.


(defun data-permanent-storage-size ()
  (length *permanent-objects*))

(defun data-temporary-storage-size ()
  (length *temporary-objects*))

(defun data-size ()
  (+ (data-permanent-storage-size)
     (data-temporary-storage-size)))

(defun data-init (&optional filename)
  (if (and filename (probe-file filename))
      (with-open-file (s filename :direction :input)
	(setf *permanent-objects* (read s)
	      *temporary-objects* (read s)))
      (setf *permanent-objects* (make-array 128 :adjustable t :fill-pointer 0)
	    *temporary-objects* (make-array 128 :adjustable t :fill-pointer 0))))

(defun data-get-all-objects ()
  ;; We collect all objects that are to be externalized, but filter out
  ;; those which will be created by a lisp form.
  (loop for array in (list *permanent-objects* *temporary-objects*)
     nconc (loop for (object vv-record . rest) across array
              collect (cond ((gethash object *load-objects*)
                             0)
                            ((vv-used-p vv-record)
                             object)
                            (t
                             ;; Value optimized away or not used
                             0))))
  #+(or)
  (loop for i in (nconc (map 'list #'first *permanent-objects*)
			(map 'list #'first *temporary-objects*))
	collect (if (gethash i *load-objects*)
		    0
		    i)))

(defun data-dump-array ()
  (cond (*compiler-constants*
         (setf si::*compiler-constants* (concatenate 'vector (data-get-all-objects)))
         "")
        #+externalizable
        ((plusp (data-size))
         (let* ((data-vector (concatenate 'vector (data-get-all-objects))))
           (si::serialize data-vector)))
        #-externalizable
        ((plusp (data-size))
         (let* ((*wt-string-size* 0)
                (*wt-data-column* 80)
                (data (data-get-all-objects))
                (data-string (si::with-ecl-io-syntax
                                 (prin1-to-string data)))
                (l (length data-string)))
           (subseq data-string 1 (1- l))))
	(t
	 "")))

(defun data-c-dump (filename)
  (with-open-file (stream filename :direction :output :if-does-not-exist :create
                          :if-exists :supersede :external-format :default)
    (let ((string (data-dump-array)))
      (if (and *compile-in-constants* (plusp (length string)))
	  (let ((*wt-string-size* 0)
		(*wt-data-column* 80))
	    (princ "static const char compiler_data_text[] = " stream)
	    (wt-filtered-data string stream)
	    (princ #\; stream)
	    (format stream "~%#define compiler_data_text_size ~D~%"
		    *wt-string-size*))
          (princ "#define compiler_data_text NULL
#define compiler_data_text_size 0" stream)))))

(defun data-binary-dump (filename &optional string)
  (unless *compile-in-constants*
    (si::add-cdata filename (or string (data-dump-array)))))

(defun wt-data-begin (stream)
  nil)

(defun wt-data-end (stream)
  (princ #\; stream)
  (format stream "~%#define compiler_data_text_size ~D~%" *wt-string-size*)
  (setf *wt-string-size* 0))

(defun data-empty-loc ()
  (add-object 0 :duplicate t :permanent t))

(defun add-load-form (object location)
  (when (clos::need-to-make-load-form-p object *cmp-env*)
    (if (not (eq *compiler-phase* 't1))
	(cmperr "Unable to internalize complex object ~A in ~a phase" object *compiler-phase*)
	(multiple-value-bind (make-form init-form) (make-load-form object)
	  (setf (gethash object *load-objects*) location)
	  (when make-form
	    (push (make-c1form* 'MAKE-FORM :args location (c1expr make-form)) *make-forms*))
	  (when init-form
	    (push (make-c1form* 'INIT-FORM :args location (c1expr init-form)) *make-forms*))))))

(defun add-object (object &key (duplicate nil)
		   (permanent (or (symbolp object) *permanent-data*)))
  ;; FIXME! Currently we have two data vectors and, when compiling
  ;; files, it may happen that a constant is duplicated and stored
  ;; both in VV and VVtemp. This would not be a problem if the
  ;; constant were readable, but due to using MAKE-LOAD-FORM we may
  ;; end up having two non-EQ objects created for the same value.
  (let* ((test (if *compiler-constants* 'eq 'equal))
	 (array (if permanent *permanent-objects* *temporary-objects*))
	 (x (or (and (not permanent)
		     (find object *permanent-objects* :test test
			   :key #'first))
		(find object array :test test :key #'first)))
	 (next-ndx (length array))
         (forced duplicate)
	 found)
    (cond ((add-static-constant object))
          ((and x duplicate)
	   (setq x (make-vv :location next-ndx :used-p forced
                            :permanent-p permanent
                            :value object))
	   (vector-push-extend (list object x next-ndx) array)
	   x)
	  (x
	   (second x))
	  ((and (not duplicate)
		(symbolp object)
		(multiple-value-setq (found x) (si::mangle-name object)))
	   x)
	  (t
	   (setq x (make-vv :location next-ndx :used-p forced
                            :permanent-p permanent
                            :value object))
	   (vector-push-extend (list object x next-ndx) array)
	   (unless *compiler-constants*
	     (add-load-form object x))
	   x))))

(defun add-symbol (symbol)
  (add-object symbol :duplicate nil :permanent t))

(defun add-keywords (keywords)
  ;; We have to build, in the vector VV[], a sequence with all
  ;; the keywords that this function uses. It does not matter
  ;; whether each keyword has appeared separately before, because
  ;; cl_parse_key() needs the whole list. However, we can reuse
  ;; keywords lists from other functions when they coincide with ours.
  ;; We search for keyword lists that are similar. However, the list
  ;; *OBJECTS* contains elements in decreasing order!!!
  (let ((x (search keywords *permanent-objects*
		   :test #'(lambda (k record) (eq k (first record))))))
    (if x
        (second (elt *permanent-objects* x))
	(prog1
	    (add-object (pop keywords) :duplicate t :permanent t)
	  (dolist (k keywords)
	    (add-object k :duplicate t :permanent t))))))

;;; ======================================================================
;;;
;;; STATIC CONSTANTS
;;;

(defun static-base-string-builder (name value stream)
  (format stream "ecl_def_ct_base_string(~A," name)
  (wt-filtered-data value stream :one-liner t)
  (format stream ",~D,static,const);" (length value)))

(defun static-single-float-builder (name value stream)
  (let* ((*read-default-float-format* 'single-float)
	 (*print-readably* t))
    (format stream "ecl_def_ct_single_float(~A,~S,static,const);"
	    name value stream)))

(defun static-double-float-builder (name value stream)
  (let* ((*read-default-float-format* 'double-float)
	 (*print-readably* t))
    (format stream "ecl_def_ct_double_float(~A,~S,static,const);"
	    name value stream)))

#+long-float
(defun static-long-float-builder (name value stream)
  (let* ((*read-default-float-format* 'long-float)
	 (*print-readably* t))
    (format stream "ecl_def_ct_long_float(~A,~SL,static,const);"
	    name value stream)))

(defun static-rational-builder (name value stream)
  (let* ((*read-default-float-format* 'double-float)
	 (*print-readably* t))
    (format stream
	    "ecl_def_ct_ratio(~A,MAKE_FIXNUM(~D),MAKE_FIXNUM(~D),static,const);"
	    name (numerator value) (denominator value))))

(defun static-constant-delegate (name value stream)
  (funcall (static-constant-expression value)
	   name value stream))

(defun static-complex-builder (name value stream)
  (let* ((*read-default-float-format* 'double-float)
	 (*print-readably* t)
	 (name-real (concatenate 'string name "_real"))
	 (name-imag (concatenate 'string name "_imag")))
    (static-constant-delegate name-real (realpart value) stream)
    (terpri stream)
    (static-constant-delegate name-imag (imagpart value) stream)
    (terpri stream)
    (format stream
	    "ecl_def_ct_complex(~A,&~A_data,&~A_data,static,const);"
	    name name-real name-imag)))

#+sse2
(defun static-sse-pack-builder (name value stream)
  (let* ((bytes (ext:sse-pack-to-vector value '(unsigned-byte 8)))
	 (type-code (nth-value 1 (ext:sse-pack-element-type value))))
    (format stream
	    "ecl_def_ct_sse_pack(~A,~A~{,~A~});"
	    name type-code (coerce bytes 'list))))

(defun static-constant-builder (format value)
  (lambda (name stream)
    (format stream format name value)))

(defun static-constant-expression (object)
  (typecase object
    (base-string #'static-base-string-builder)
    (ratio (and (static-constant-expression (numerator object))
		(static-constant-expression (denominator object))
		#'static-rational-builder))
    (single-float (and (not (ext:float-nan-p object))
		       (not (ext:float-infinity-p object))
		       #'static-single-float-builder))
    (double-float (and (not (ext:float-nan-p object))
		       (not (ext:float-infinity-p object))
		       #'static-double-float-builder))
    #+long-float
    (long-float (and (not (ext:float-nan-p object))
		     (not (ext:float-infinity-p object))
		     #'static-long-float-builder))
    (complex (and (static-constant-expression (realpart object))
		  (static-constant-expression (imagpart object))
		  #'static-complex-builder))
    #+sse2
    (ext:sse-pack #'static-sse-pack-builder)
    (t nil)))

(defun add-static-constant (object)
  #+msvc
  nil
  #-msvc
  ;; FIXME! The Microsoft compiler does not allow static initialization of bit fields.
  ;; SSE uses always unboxed static constants. No reference
  ;; is kept to them -- it is thus safe to use them even on code
  ;; that might be unloaded.
  (unless (or *compiler-constants*
              (and (not *use-static-constants-p*)
                   #+sse2
                   (not (typep object 'ext:sse-pack)))
              (not (listp *static-constants*)))
    (let ((record (find object *static-constants* :key #'first :test #'equal)))
      (if record
          (second record)
          (let ((builder (static-constant-expression object)))
            (when builder
              (let* ((c-name (format nil "_ecl_static_~D" (length *static-constants*))))
                (push (list object c-name builder) *static-constants*)
                (make-vv :location c-name :value object))))))))

(defun wt-vv-index (index permanent-p)
  (cond ((not (numberp index))
         (wt index))
        (permanent-p
         (wt "VV[" index "]"))
        (t
         (wt "VVtemp[" index "]"))))

(defun set-vv-index (loc index permanent-p)
  (wt-nl) (wt-vv-index index permanent-p) (wt "= ")
  (wt-coerce-loc :object loc)
  (wt ";"))

(defun wt-vv (vv-loc)
  (setf (vv-used-p vv-loc) t)
  (wt-vv-index (vv-location vv-loc) (vv-permanent-p vv-loc)))

(defun set-vv (loc vv-loc)
  (setf (vv-used-p vv-loc) t)
  (set-vv-index loc (vv-location vv-loc) (vv-permanent-p vv-loc)))

(defun vv-type (loc)
  (let ((value (vv-value loc)))
    (if (and value (not (ext:fixnump value)))
        (type-of value)
        t)))
