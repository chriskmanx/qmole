;; A replacement function for gcl.
;;
;; This is the same as make-proclaims in Gcl (2.6.8), but we don't try
;; to coalesce all functions that have the same argument types into
;; one proclamation.  Just sort them and output them one function at a
;; time.  This should make sys-proclaim.lisp change less often.
(in-package "COMPILER")
(defun make-proclaims ( &optional (st *standard-output*)
				  *print-length* *print-level*)
  (let ((procs '()))
    (princ ";; This file is generated via 'make sys-proclaim'." st)
    (print `(in-package ,(package-name *package*)) st)
    ;; Leave GCL-specific SLOOP as it stands due to non-standard IN-TABLE keyword.
    (sloop::sloop with ret with at
		  for (ke val) in-table *call-table* 
		  do
		  (cond ((or (eql 1 (fn-no-emit val))
			     (not (eq (fn-def val) 'defun))))
			(t (setq ret (get-value-type ke))
			   (setq at (fn-arg-types val))
			   (push (list ke at ret) procs))))
    ;; Sort by function name, using package and symbol
    (setf procs (sort procs #'(lambda (a b)
				(string-lessp (write-to-string a)
					      (write-to-string b)))
		      :key #'first))
    (dolist (p procs)
      (destructuring-bind (fn at ret)
	  p
	(print `(proclaim '(ftype (function ,at ,ret) ,fn))
	       st)))))
