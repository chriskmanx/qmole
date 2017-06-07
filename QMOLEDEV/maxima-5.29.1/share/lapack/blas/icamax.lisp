;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.215 2009/04/07 22:05:21 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.200 2009/01/19 02:38:17 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.112 2009/01/08 12:57:19 rtoy Exp $")

;;; Using Lisp CMU Common Lisp 19f (19F)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :blas)


(defun icamax (n cx incx)
  (declare (type (array f2cl-lib:complex8 (*)) cx)
           (type (f2cl-lib:integer4) incx n))
  (f2cl-lib:with-multi-array-data
      ((cx f2cl-lib:complex8 cx-%data% cx-%offset%))
    (labels ((cabs1 (zdum)
               (+ (abs (f2cl-lib:freal zdum)) (abs (f2cl-lib:aimag zdum)))))
      (declare (ftype (function (f2cl-lib:complex8)
                       (values single-float &rest t))
                      cabs1))
      (prog ((zdum #C(0.0f0 0.0f0)) (i 0) (ix 0) (smax 0.0f0) (icamax 0))
        (declare (type (single-float) smax)
                 (type (f2cl-lib:integer4) icamax ix i)
                 (type (f2cl-lib:complex8) zdum))
        (setf icamax 0)
        (if (or (< n 1) (<= incx 0)) (go end_label))
        (setf icamax 1)
        (if (= n 1) (go end_label))
        (if (= incx 1) (go label20))
        (setf ix 1)
        (setf smax (cabs1 (f2cl-lib:fref cx-%data% (1) ((1 *)) cx-%offset%)))
        (setf ix (f2cl-lib:int-add ix incx))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (if
             (<= (cabs1 (f2cl-lib:fref cx-%data% (ix) ((1 *)) cx-%offset%))
                 smax)
             (go label5))
            (setf icamax i)
            (setf smax
                    (cabs1 (f2cl-lib:fref cx-%data% (ix) ((1 *)) cx-%offset%)))
           label5
            (setf ix (f2cl-lib:int-add ix incx))
           label10))
        (go end_label)
       label20
        (setf smax (cabs1 (f2cl-lib:fref cx-%data% (1) ((1 *)) cx-%offset%)))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (if
             (<= (cabs1 (f2cl-lib:fref cx-%data% (i) ((1 *)) cx-%offset%))
                 smax)
             (go label30))
            (setf icamax i)
            (setf smax
                    (cabs1 (f2cl-lib:fref cx-%data% (i) ((1 *)) cx-%offset%)))
           label30))
        (go end_label)
       end_label
        (return (values icamax nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::icamax
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex8 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil)
           :calls 'nil)))

