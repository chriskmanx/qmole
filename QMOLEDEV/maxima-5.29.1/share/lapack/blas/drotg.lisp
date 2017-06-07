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


(defun drotg (da db c s)
  (declare (type (double-float) s c db da))
  (prog ((roe 0.0) (scale 0.0) (r 0.0) (z 0.0))
    (declare (type (double-float) z r scale roe))
    (setf roe db)
    (if (> (f2cl-lib:dabs da) (f2cl-lib:dabs db)) (setf roe da))
    (setf scale (+ (f2cl-lib:dabs da) (f2cl-lib:dabs db)))
    (if (/= scale 0.0) (go label10))
    (setf c 1.0)
    (setf s 0.0)
    (setf r 0.0)
    (setf z 0.0)
    (go label20)
   label10
    (setf r
            (* scale
               (f2cl-lib:dsqrt
                (+ (expt (/ da scale) 2) (expt (/ db scale) 2)))))
    (setf r (* (f2cl-lib:dsign 1.0 roe) r))
    (setf c (/ da r))
    (setf s (/ db r))
    (setf z 1.0)
    (if (> (f2cl-lib:dabs da) (f2cl-lib:dabs db)) (setf z s))
    (if (and (>= (f2cl-lib:dabs db) (f2cl-lib:dabs da)) (/= c 0.0))
        (setf z (/ 1.0 c)))
   label20
    (setf da r)
    (setf db z)
    (go end_label)
   end_label
    (return (values da db c s))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::drotg fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float))
           :return-values '(fortran-to-lisp::da fortran-to-lisp::db
                            fortran-to-lisp::c fortran-to-lisp::s)
           :calls 'nil)))

