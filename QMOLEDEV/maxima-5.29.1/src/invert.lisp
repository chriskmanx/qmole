;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

(in-package :maxima)

(defmfun $adjoint (mat)
  (let* ((n ($length mat))
	 (adj (simplify ($ident n))))
    (unless (like n 1)
      (do ((i 1 (1+ i)))
	  ((> i n))
	(do ((j 1 (1+ j)))
	    ((> j n))
	  (maset (mul* (power -1 (+ i j))
		       (simplify ($determinant (simplify ($minor mat j i)))))
		 adj i j))))
    adj))

(defmfun $invert (mat)
  (let* ((adj (simplify ($adjoint mat)))
	 (ans (let (($scalarmatrixp t))
		(div adj
		     (ncmul2 (simplify ($row mat 1))
			     (simplify ($col adj 1)))))))
    (if (and (like (trd-msymeval $scalarmatrixp '$scalarmatrixp) t)
	     (eql ($length mat) 1))
	(maref ans 1 1)
	ans)))

(add2lnc '$adjoint $props)
(add2lnc '$invert $props)
