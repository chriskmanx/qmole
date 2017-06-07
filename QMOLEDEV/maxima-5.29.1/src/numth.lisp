;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;;   *****************************************************************
;;;   ***** NUMTH ******* VARIOUS NUMBER THEORY FUNCTIONS *************
;;;   *****************************************************************

(macsyma-module numth)

(declare-top (special $intfaclim))

(load-macsyma-macros rzmac)

;;; Sum of divisors and Totient functions

(defmfun $divsum (n &optional (k 1))
  (let (($intfaclim nil))
    (if (and (integerp k) (integerp n))
	(let ((n (abs n)))
	  (cond ((= n 1) 1)
		((zerop n) 1)
		((zerop k)
		 (do ((l (cfactorw n) (cddr l))
		      (a 1 (* a (1+ (cadr l)))))
		     ((null l) a)))
		((minusp k)
		 `((rat) ,(divsum (cfactorw n) (- k)) ,(expt n (- k))))
		(t
		 (divsum (cfactorw n) k))))
	(list '($divsum) n k))))

(defun divsum (l k)
  (do ((l l (cddr l))
       (ans 1))
      ((null l) ans)
    (unless (eql (car l) 1)
      (let ((temp (expt (car l) k)))
	(setq ans (* ans
		     (truncate (1- (expt temp (1+ (cadr l))))
			       (1- temp))))))))

(defmfun $totient (n)
  (cond ((integerp n)
	 (setq n (abs n))
	 (cond ((< n 1) 0)
	       ((equal n 1) 1)
	       (t (do ((factors (let ($intfaclim) (cfactorw n))
				(cddr factors))
		       (total 1 (* total (1- (car factors))
				   (expt (car factors) (1- (cadr factors))))))
		      ((null factors) total)))))
	(t (list '($totient) n))))


;;; JACOBI symbol and Gaussian factoring

(declare-top (special modulus $intfaclim))

(defvar *incl* (let ((l (list 2 4))) (nconc l l)))

(defun imodp (p)
  (cond ((not (= (rem p 4) 1)) nil)
	((= (rem p 8) 5) (imodp1 2 p))
	((= (rem p 24) 17) (imodp1 3 p)) ;p=2(mod 3)
	(t (do ((i 5 (+ i (car j)))	;p=1(mod 24)
		(j *incl* (cdr j)))
	       ((= (jacobi i p) -1) (imodp1 i p))))))

(defun imodp1 (i modulus)
  (abs (cexpt i (ash (1- modulus) -2) )))

(defun psumsq (p)
  (let ((x (imodp p)))
    (cond ((equal p 2) (list 1 1))
	  ((null x) nil)
	  (t (psumsq1 p x)))))

(defun psumsq1 (p x)
  (do ((sp ($isqrt p))
       (r1 p r2)
       (r2 x (rem r1 r2)))
      ((not (> r1 sp)) (list r1 r2))))

(defun gctimes (a b c d)
  (list (- (* a c) (* b d))
	(+ (* a d) (* b c))))

(defmfun $gcfactor (n)
  (let ((n (cdr ($totaldisrep ($bothcoef ($rat n '$%i) '$%i)))))
    (if (not (and (integerp (car n)) (integerp (cadr n))))
	(gcdisp (nreverse n))
	(do ((factors (gcfactor (cadr n) (car n)) (cddr factors))
	     (res nil))
	    ((null factors)
	     (cond ((null res) 1)
		   ((null (cdr res)) (car res))
		   (t (cons '(mtimes simp) (nreverse res)))))
	  (let ((term (car factors))
		(exp (cadr factors)))
	    (push (if (= exp 1)
		      (gcdisp term)
		      (pow (gcdisp term) exp))
		  res))))))

(defun gcdisp (term)
  (cond ((atom term) term)
	((let ((rp (car term))
	       (ip (cadr term)))
	   (setq ip (if (equal ip 1) '$%i (list '(mtimes) ip '$%i)))
	   (if (equal rp 0)
	       ip
	       (list '(mplus) rp ip))))))

(defun gcfactor (a b &aux tem)
  (prog (gl cd dc econt p e1 e2 ans plis nl $intfaclim )
     (setq e1 0
	   e2 0
	   econt 0
	   gl (gcd a b)
	   a (quotient a gl)
	   b (quotient b gl)
	   nl (cfactorw (+ (* a a) (* b b)))
	   gl (cfactorw gl))
     (and (equal 1 (car gl)) (setq gl nil))
     (and (equal 1 (car nl)) (setq nl nil))
     loop
     (cond ((null gl)
	    (cond ((null nl) (go ret))
		  ((setq p (car nl)))))
	   ((null nl) (setq p (car gl)))
	   (t (setq p (max (car gl) (car nl)))))
     (setq cd (psumsq p))
     (cond ((null cd)
	    (setq plis (cons p (cons (cadr gl) plis)))
	    (setq gl (cddr gl)) (go loop))
	   ((equal p (car nl))
	    (cond ((zerop (rem (setq tem (+ (* a (car cd)) ;gcremainder
					    (* b (cadr cd))))
			       p))		;remainder(real((a+bi)cd~),p)
					;z~ is complex conjugate
		   (setq e1 (cadr nl)) (setq dc cd))
		  (t (setq e2 (cadr nl))
		     (setq dc (reverse cd))))
	    (setq dc (gcexpt dc (cadr nl)) ;
		  dc (gctimes a b (car dc) (- (cadr dc)))
		  a (quotient (car dc) p)
		  b (quotient (cadr dc) p)
		  nl (cddr nl))))
     (cond ((equal p (car gl))
	    (setq econt (+ econt (cadr gl)))
	    (cond ((equal p 2)
		   (setq e1 (+ e1 (* 2 (cadr gl)))))
		  (t (setq e1 (+ e1 (cadr gl))
			   e2 (+ e2 (cadr gl)))))
	    (setq gl (cddr gl))))
     (and (not (zerop e1))
	  (setq ans (cons cd (cons e1 ans)))
	  (setq e1 0))
     (and (not (zerop e2))
	  (setq ans (cons (reverse cd) (cons e2 ans)))
	  (setq e2 0))
     (go loop)
     ret    (setq cd (gcexpt (list 0 -1)
			     (rem econt 4)))
     (setq a (gctimes a b (car cd) (cadr cd)))
     ;;a hasn't been divided by p yet..
     (setq a (mapcar 'signum a))
     #+cl (assert (or (zerop (car a))(zerop (second a))))
     (cond ((or (equal (car a) -1) (equal (cadr a) -1))
	    (setq plis (cons -1 (cons 1 plis)))))
     (cond ((equal (car a) 0)
	    (setq ans (cons '(0 1) (cons 1 ans)))))
     (setq ans (nconc plis ans))
     (return ans)))

(defun multiply-gcfactors (lis)
  (loop for (term exp) on (cddr lis) by #'cddr
	 with answ = (cond ((numberp (car lis))(list (pexpt (car lis) (second lis)) 0))
			   (t(gcexpt (car lis) (second lis))))
	 when (numberp term)
	 do (setq answ (list (* (first answ) term) (* (second answ) term)))
	 (show answ)
	 else
	 do (setq answ (apply 'gctimes (append answ (gcexpt term exp))))
	 finally (return answ)))

(defun gcexpt (a n)
  (cond ((zerop n) '(1 0))
        ((equal n 1) a)
        ((evenp n) (gcexpt (gctime1 a a) (truncate n 2)))
        (t (gctime1 a (gcexpt (gctime1 a a) (truncate n 2))))))

(defun gctime1 (a b)
  (gctimes (car a) (cadr a) (car b) (cadr b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Maxima functions in (Z/nZ)*
;; 
;; zn_order, zn_primroot_p, zn_primroot, zn_log, zn_mult_table, zn_power_table, 
;; chinese
;;
;; 2012, Volker van Nek  
;;

;; Maxima option variables:
(defmvar $zn_primroot_limit 1000 "Upper bound for `zn_primroot'." fixnum)
(defmvar $zn_primroot_verbose nil "Print message when `zn_primroot_limit' is reached." boolean)
(defmvar $zn_primroot_pretest nil "`zn_primroot' performs pretest if (Z/nZ)* is cyclic." boolean)


;; compute the order of x in (Z/nZ)*
;;
;; optional argument: ifactors of totient(n) as returned in Maxima by 
;;    block([factors_only:false], ifactors(totient(n)))
;;    e.g. [[2, 3], [3, 1], ... ]
;;
(defmfun $zn_order (x n &optional fs-phi) 
  (unless (and (integerp x) (integerp n))
    (return-from $zn_order 
      (if fs-phi 
        (list '($zn_order) x n fs-phi)
        (list '($zn_order) x n) )))
  (when (minusp x) (setq x (mod x n)))
  (cond 
    ((= 0 x) nil)
    ((= 1 x) (if (= n 1) nil 1))
    ((/= 1 (gcd x n)) nil)
    (t 
      (if fs-phi
         (if (and ($listp fs-phi) ($listp (cadr fs-phi)))
           (progn 
             (setq fs-phi (mapcar #'cdr (cdr fs-phi))) ; Lispify fs-phi
             (setq fs-phi (cons (totient-from-factors fs-phi) fs-phi)) )
           (merror (intl:gettext 
             "Third argument to `zn_order' must be of the form [[p1, e1], ..., [pk, ek]].")) )
         (setq fs-phi (totient-with-factors n)) )
      (zn_order x 
                n
                (car fs-phi) ;; phi
                (cdr fs-phi)) ))) ;; factors of phi with multiplicity
;;
(defun zn_order (x n phi fs-phi)
  (let ((s phi) p e)
    (dolist (f fs-phi s)
      (setq p (car f) e (cadr f))
      (setq s (truncate s (expt p e)))
      (do ((z (power-mod x s n)))
          ((= z 1))
        (setq z (power-mod z p n))
        (setq s (* s p)) )) ))


;; compute totient (euler-phi) of n and its factors in one function
;;
;; returns a list of the form (phi ((p1 e1) ... (pk ek)))
;;
(defun totient-with-factors (n)
  (let (($intfaclim) (phi 1) fs-n (fs) p e (fs-phi) g)
    (setq fs-n (get-factor-list n))
    (dolist (f fs-n fs)
      (setq p (car f) e (cadr f))
      (setq phi (* phi (1- p) (expt p (1- e))))
      (when (> e 1) (setq fs (cons `(,p ,(1- e)) fs)))
      (setq fs (append (get-factor-list (1- p)) fs)) )
    (setq fs (copy-tree fs)) ;; this deep copy is a workaround to avoid references 
                             ;; to the list returned by ifactor.lisp/get-factor-list.
                             ;; see bug 3510983
    (setq fs (sort fs #'(lambda (a b) (< (car a) (car b)))))
    (setq g (car fs))
    (dolist (f (cdr fs) (cons phi (reverse (cons g fs-phi))))
      (if (= (car f) (car g)) 
        (incf (cadr g) (cadr f)) ;; assignment
        (progn 
          (setq fs-phi (cons g fs-phi))
          (setq g f) ))) ))

;; recompute totient from given factors
;;
;; fs-phi: factors of totient with multiplicity: ((p1 e1) ... (pk ek))
;;
(defun totient-from-factors (fs-phi) 
  (let ((phi 1) p e)
    (dolist (f fs-phi phi)
      (setq p (car f) e (cadr f))
      (setq phi (* phi (expt p e))) )))


;; for n > 2 is x a primitive root modulo n 
;;   when n does not divide x
;;   and for all prime factors p of phi = totient(n)
;;   x^(phi/p) mod n # 1
;;
;; optional argument: ifactors of totient(n)
;;
(defmfun $zn_primroot_p (x n &optional fs-phi)
  (unless (and (integerp x) (integerp n))
    (return-from $zn_primroot_p 
      (if fs-phi 
        (list '($zn_primroot_p) x n fs-phi)
        (list '($zn_primroot_p) x n) )))
  (when (minusp x) (setq x (mod x n)))
  (cond 
    ((= 0 x) nil)
    ((= 1 x) (if (= n 2) 1 nil))
    ((<= n 2) nil)
    ((= 0 (mod x n)) nil)
    (t 
      (if fs-phi
         (if (and ($listp fs-phi) ($listp (cadr fs-phi)))
           (progn 
             (setq fs-phi (mapcar #'cdr (cdr fs-phi))) ; Lispify fs-phi
             (setq fs-phi (cons (totient-from-factors fs-phi) fs-phi)) )
           (merror (intl:gettext 
             "Third argument to `zn_primroot_p' must be of the form [[p1, e1], ..., [pk, ek]].")) )
         (setq fs-phi (totient-with-factors n)) )
      (zn-primroot-p x 
                     n
                     (car fs-phi) ;; phi
                     (mapcar #'car (cdr fs-phi))) ))) ;; factors only (omitting multiplicity)
;;
(defun zn-primroot-p (x n phi fs-phi)
  (unless (= 1 (gcd x n))
    (return-from zn-primroot-p nil) )  
  (dolist (p fs-phi t)
    (when (= 1 (power-mod x (truncate phi p) n))
      (return-from zn-primroot-p nil) )))

;;
;; find the smallest primitive root modulo n
;;
;; optional argument: ifactors of totient(n)
;;
(defmfun $zn_primroot (n &optional fs-phi)
  (unless (integerp n)
    (return-from $zn_primroot 
      (if fs-phi 
        (list '($zn_primroot) n fs-phi)
        (list '($zn_primroot) n) )))
  (cond 
    ((<= n 1) nil)
    ((= n 2) 1)
    (t 
      (when $zn_primroot_pretest
        (unless (cyclic-p n)
          (return-from $zn_primroot nil) ))
      (if fs-phi
         (if (and ($listp fs-phi) ($listp (cadr fs-phi)))
           (progn 
             (setq fs-phi (mapcar #'cdr (cdr fs-phi))) ; Lispify fs-phi
             (setq fs-phi (cons (totient-from-factors fs-phi) fs-phi)) )
           (merror (intl:gettext
             "Second argument to `zn_primroot' must be of the form [[p1, e1], ..., [pk, ek]].")) )
         (setq fs-phi (totient-with-factors n)) )
      (zn-primroot n 
                   (car fs-phi) ;; phi
                   (mapcar #'car (cdr fs-phi))) ))) ;; factors only (omitting multiplicity)
;;
;; (Z/nZ)* is cyclic if n = 2, 4, p^k or 2*p^k where p prime > 2
(defun cyclic-p (n) 
  (cond
    ((< n 2) nil)
    ((< n 8) t)
    (t 
      (when (evenp n) 
        (setq n (ash n -1))
        (when (evenp n) (return-from cyclic-p nil)) )
      (let (($intfaclim) (fs (get-small-factors n)) (len 0) p q)
        (setq n (car fs))
        (when (cadr fs) (setq len (length (cadr fs))))
        (if (= 1 n) 
          (return-from cyclic-p (= 1 len))
          (when (> len 0) (return-from cyclic-p nil)) )
        (when (primep n) (return-from cyclic-p t))
        (setq q (setq p (get-one-factor n)))
        (do () (())
          (setq n (truncate n q))
          (when (primep n) (return (= n p)))
          (setq q (get-one-factor n))
          (when (/= p q) (return nil)) )))))
;;
(defun zn-primroot (n phi fs-phi) 
  (do ((i 2 (1+ i)))
       ((= i n) nil)
    (when (zn-primroot-p i n phi fs-phi)
      (return i) )
    (when (= i $zn_primroot_limit)
      (when $zn_primroot_verbose
        (format t "`zn_primroot' stopped at zn_primroot_limit = ~A~%" $zn_primroot_limit) )
      (return nil) )))

;;
;; Chinese Remainder Theorem
;;
(defmfun $chinese (rems mods) 
  (cond 
    ((not (and ($listp rems) ($listp mods)))
      (list '($chinese) rems mods) )
    ((or (= 0 ($length rems)) (= 0 ($length mods)))
      (merror (intl:gettext
        "At least one argument to `chinese' was an empty list." )))
    ((notevery #'integerp (setq rems (cdr rems)))
      (list '($chinese) (cons '(mlist simp) rems) mods) )
    ((notevery #'integerp (setq mods (cdr mods)))
      (list '($chinese) (cons '(mlist simp) rems) (cons '(mlist simp) mods)) )
    (t
      (car (chinese rems mods)) )))
;;
(defun chinese (rems mods)
  (if (onep (length mods)) 
    (list (car rems) (car mods))
    (let* ((rp (car rems))
           (p  (car mods))
           (rq-q (chinese (cdr rems) (cdr mods)))
           (rq (car rq-q))
           (q (cadr rq-q))
           (q-inv (inv-mod q p))
           (h (mod (* (- rp rq) q-inv) p))
           (x (+ (* h q) rq)) )
      (list x (* p q)) )))

;;
;; discrete logarithm:
;; solve g^x = a mod n, where g is a generator of (Z/nZ)* 
;;
;; see: lecture notes 'Grundbegriffe der Kryptographie' - Eike Best
;; http://theoretica.informatik.uni-oldenburg.de/~best/publications/kry-Mai2005.pdf
;;
;; optional argument: ifactors of totient(n)
;;
(defmfun $zn_log (a g n &optional fs-phi)
  (unless (and (integerp a) (integerp g) (integerp n))
    (return-from $zn_log 
      (if fs-phi 
        (list '($zn_log) a g n fs-phi)
        (list '($zn_log) a g n) )))
  (when (minusp a) (setq a (mod a n)))
  (cond 
    ((or (= 0 a) (>= a n)) nil)
    ((= 1 a) 0)
    ((= g a) 1)
    ((> (gcd a n) 1) nil)
    (t 
      (if fs-phi
        (if (and ($listp fs-phi) ($listp (cadr fs-phi)))
          (progn 
            (setq fs-phi (mapcar #'cdr (cdr fs-phi))) ; Lispify fs-phi
            (setq fs-phi (cons (totient-from-factors fs-phi) fs-phi)) )
          (merror (intl:gettext
             "Fourth argument to `zn_log' must be of the form [[p1, e1], ..., [pk, ek]].")) )
        (setq fs-phi (totient-with-factors n)) )
      (unless (zn-primroot-p g n (car fs-phi) (mapcar #'car (cdr fs-phi)))
        (merror (intl:gettext "Second argument to `zn_log' must be a generator of (Z/~MZ)*.") n) )
      (when (= 0 (mod (- a (* g g)) n)) 
        (return-from $zn_log 2) )
      (when (= 1 (mod (* a g) n))
        (return-from $zn_log (mod -1 (car fs-phi))) )
      (zn-dlog a 
               g 
               n 
               (car fs-phi) ;; phi
               (cdr fs-phi)) ))) ;; factors with multiplicity
;;
;; Pohlig and Hellman reduction:
(defun zn-dlog (a g n phi fs-phi)
  (let (p e phip gp x dlog (dlogs nil))
    (dolist (f fs-phi)
      (setq p (car f) e (cadr f))
      (setq phip (truncate phi p))
      (setq gp (power-mod g phip n))
      (if (= 1 e) 
        (setq x (dlog-rho (power-mod a phip n) gp p n))
        (progn 
          (setq x 0)
          (do ((agx a) (k 1) (pk 1)) (())
            (setq dlog (dlog-rho (power-mod agx (truncate phip pk) n) gp p n))
            (setq x (+ x (* dlog pk)))
            (if (= k e) 
              (return)
              (setq k (1+ k) pk (* pk p)) )
            (setq agx (mod (* a ($power_mod g (- x) n)) n)) )))
      (setq dlogs (cons x dlogs)) )
    (car (chinese (nreverse dlogs) (mapcar #'(lambda (z) (apply #'expt z)) fs-phi))) ))
;;
;; brute-force:
(defun dlog-naive (a g q n)
  (decf q)
  (do ((i 0 (1+ i)) (gi 1 (mod (* gi g) n)))
      ((= gi a) i) ))
;;
;; Pollard rho for dlog computation (Brents variant of collision detection)
(defun dlog-rho (a g q n)  
  (cond
    ((= 1 a) 0)
    ((= g a) 1)
    ((= 0 (mod (- a (* g g)) n)) 2)
    ((= 1 (mod (* a g) n)) (1- q))
    ((< q 512) (dlog-naive a g q n))
    (t
      (prog ((b 1) (y 0) (z 0) (bb 1) (yy 0) (zz 0) rnd dy dz)
        rho
        (do ((i 0)(j 1)) (()) (declare (fixnum i j))
          (multiple-value-setq (b y z) (dlog-f b y z a g q n))
          (when (equal b bb) (return)) 
          (incf i)
          (when (= i j)
            (setq j (1+ (ash j 1)))
            (setq bb b yy y zz z) ))
        (setq dy (mod (- y yy) q) dz (mod (- zz z) q))
        (when (= 1 (gcd dz q))
          (return (mod (* dy (inv-mod dz q)) q)) )
        (setq rnd (1+ (random (1- q))))
        (multiple-value-setq (b y z) 
          (values (mod (* a (power-mod g rnd n)) n) rnd 1) )
        (multiple-value-setq (bb yy zz) (values 1 0 0))
        (go rho) ))))
;;
;; iteration for Pollard rho:
(defun dlog-f (b y z a g q n)
  (let ((s (mod b 3)))
    (cond 
      ((= 0 s)
        (values (mod (* b b) n) (mod (ash y 1) q) (mod (ash z 1) q)) )
      ((= 1 s)
        (values (mod (* a b) n) y                 (mod (+ z 1) q)) )
      (t
        (values (mod (* g b) n) (mod (+ y 1) q)   z) ))))


;; for educational puposes: tables of small rings

(defmfun $zn_mult_table (n &optional (all? t))
  (declare (fixnum n))
  (unless (and (fixnump n) (< 1 n))
    (merror (intl:gettext 
      "Argument to `zn_mult_table' must be a small fixnum greater 1." )) )
  (do ((i 0 (1+ i)) res)
      ((= i n) (cons '($matrix) (nreverse res)))
      (declare (fixnum i))
    (when (or all? (= 1 (gcd i n))) 
      (push 
        (mfuncall '$cons 0
          (mfuncall '$makelist `(mod (* ,i $j) ,n) '$j 1 (1- n)) )
        res ) )))

(defmfun $zn_power_table (n &optional (all? t))
  (declare (fixnum n))
  (unless (and (fixnump n) (< 1 n))
    (merror (intl:gettext 
      "Argument to `zn_power_table' must be a small fixnum greater 1." )) )
  (do ((i 0 (1+ i)) (tn1 (1+ (mfuncall '$totient n))) res)
      ((= i n) (cons '($matrix) (nreverse res)))
      (declare (fixnum i))
    (when (or all? (= 1 (gcd i n))) 
      (push (mfuncall '$makelist `(power-mod ,i $j ,n) '$j 1 tn1) res) )))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ----------------------------------------------------------------------------- 
;; *** GALOIS FIELDS *** 
                                                                        
;; The following is a revision and improvement of the first part of share/
;; contrib/gf/gf.mac by Alasdair McAndrew, Fabrizio Caruso and Jacopo D'Aurizio 
;; released under terms of the GPLv2 in 2007.          

;; I would like to thank the original authors for their contribution to Maxima 
;; which allowed me to study, improve and extend the source code.

;; I would also like to thank Camm Maguire who helped me coding compiler macros
;; for GCL.                                                      

;; 2012, Volker van Nek

;; Maxima functions: 

;; gf_set, gf_unset, gf_minimal_set, gf_info, 
;; gf_characteristic, gf_primitive, gf_reduction, gf_order, 
;; gf_make_arrays, gf_mult_table, gf_powers, 
;; gf_add, gf_sub, gf_mult, gf_inv, gf_div, gf_exp, gf_index, gf_log, 
;; gf_p2n, gf_n2p, gf_p2l, gf_l2p, gf_l2n, gf_n2l, 
;; gf_irreducible_p, gf_primitive_p, gf_next_primitive,  
;; gf_eval, gf_random, gf_factor, gf_gcd, gf_gcdex, gf_degree, gf_minimal_poly, 
;; gf_normal_p, gf_normal, gf_random_normal, gf_normal_basis, gf_normal_basis_rep, 
;; gf_matadd, gf_matmult, gf_matinv

(declare-top (special $gf_powers $gf_logs $gf_rat)) 

(declare-top (special 
  *gf-var* *gf-rat-sym* *gf-rat-header*
  *gf-char* *fixnump-2gf-char* *gf-exp* *gf-ord* *gf-card* *gf-prim* *gf-red* 
  *gf-fs-ord* *gf-fs* *gf-fs-base-p* *gf-x^p-powers* 
  *gf-set?* *gf-minset?* *gf-tables?* ))

(declaim (fixnum *gf-exp*)) ;; this doesn't seem to be a real practical limitation 

(defmvar $gf_rat nil "functions in gf return rational expressions?" boolean)
(defmvar *gf-set?* nil "gf data are set?" boolean)
(defmvar *gf-minset?* nil "characteristic and reduction polynomial are set?" boolean)
(defmvar *gf-tables?* nil "log and power tables are computed?" boolean)


;; basic coefficient arithmetic ------------------------------------------------
;;

;; optimize the fixnum case

(defmacro maybe-fixnum-let (binds &body body)
  `(if (typep *gf-char* 'fixnum)                                              
    (let ,binds
         (declare (fixnum ,@(mapcar #'(lambda (x) (car x)) binds)))
         ,@body)
    (let ,binds ,@body) ))

;; basic functions and compiler macros

(declaim (inline gf-cmod gf-ctimes gf-cplus-b gf-cminus-b ))

(defun gf-cmod (a) 
  (maybe-fixnum-let ((a a))
    (mod a *gf-char*) ))

(defun gf-ctimes (a b)
  (maybe-fixnum-let ((a a)(b b))
    (mod (* a b) *gf-char*) ))

(defun gf-cplus-b (a b) ;; assumes that both 0 <= a,b < *gf-char* 
  (maybe-fixnum-let ((a a)(b b))
    (let ((s (+ a b)))
      (if (< s *gf-char*) s (- s *gf-char*)) ))) 

(defun gf-cminus-b (a) ;; assumes that 0 <= a < *gf-char* 
  (maybe-fixnum-let ((a a))
    (- *gf-char* a) )) 
     
(defun gf-cinv (a)
  (maybe-fixnum-let ((a a))
    (if (= 1 a) 1 (inv-mod a *gf-char*)) ))


#-gcl (eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (define-compiler-macro gf-cmod (a)
      `(if (and (typep *gf-char* 'fixnum) (typep ,a 'fixnum))  
        (let ((x ,a) (z *gf-char*)) (declare (fixnum x z))
          (the fixnum (mod x z)) )
        (mod ,a *gf-char*) ))

    (define-compiler-macro gf-ctimes (a b) 
      `(if (typep *gf-char* 'fixnum)                                               
        (let ((x ,a) (y ,b) (z *gf-char*)) (declare (fixnum x y z))                               
          (the fixnum (mod (* x y) z)) )                                               
        (mod (* ,a ,b) *gf-char*) ))

    (define-compiler-macro gf-cplus-b (a b) ;; assumes that both 0 <= a,b < *gf-char* 
      `(if *fixnump-2gf-char*                                               
        (let ((x ,a) (y ,b) (z *gf-char*)) (declare (fixnum x y z))                               
          (setq x (the fixnum (+ x y)))
          (if (< x z) x (the fixnum (- x z))) )                                               
        (let ((x (+ ,a ,b)))                                     
          (if (< x *gf-char*) x (- x *gf-char*)) )))  
 
    (define-compiler-macro gf-cminus-b (a) ;; assumes that 0 <= a < *gf-char* 
      `(if (typep *gf-char* 'fixnum)
        (let ((x ,a) (z *gf-char*)) (declare (fixnum x z))
          (the fixnum (- z x)) )
        (- *gf-char* ,a) ))
))

#+gcl (eval-when (compile load eval) 
  (progn
    (push '((fixnum fixnum) fixnum #.(compiler::flags compiler::rfa)
            "(fixnum)(((long long)(#0))%((long long)(#1)))" ) 
          (get 'i% 'compiler::inline-always) )
    (push '((fixnum fixnum fixnum) fixnum #.(compiler::flags compiler::rfa)
            "(fixnum)((((long long)(#0))*((long long)(#1)))%((long long)(#2)))" ) 
          (get '*% 'compiler::inline-always) )
    (push '((fixnum fixnum fixnum) fixnum #.(compiler::flags compiler::rfa compiler::set)
            "@02;({long long _t=((long long)(#0))+((long long)(#1)),_w=((long long)(#2));_t<_w ? (fixnum)_t : (fixnum)(_t - _w);})" )          
          (get '+%b 'inline-always) )
    (push '((fixnum fixnum) fixnum #.(compiler::flags compiler::rfa)
            "(fixnum)(((long long)(#1))-((long long)(#0)))" ) 
          (get 'neg%b 'compiler::inline-always) )
    
    (setf (get 'i% 'compiler::return-type) t)
    (setf (get '*% 'compiler::return-type) t)
    (setf (get '+%b 'compiler::return-type) t)
    (setf (get 'neg%b 'compiler::return-type) t) 

    (si::define-compiler-macro gf-cmod (a) ;; assumes that 0 <= a
      `(if (and (typep *gf-char* 'fixnum) (typep ,a 'fixnum)) ;; maybe a > *gf-char* 
        (let ((x ,a) (z *gf-char*)) (declare (fixnum x z))
          (i% x z) )
        (mod ,a *gf-char*) ))
 
    (si::define-compiler-macro gf-ctimes (a b) ;; assumes that 0 <= a,b
      `(if (typep *gf-char* 
             ',(if (< (integer-length most-positive-fixnum) 32) `fixnum `(signed-byte 32)) )                                               
        (let ((x ,a) (y ,b) (z *gf-char*)) (declare (fixnum x y z))                               
          (*% x y z) )                                               
        (mod (* ,a ,b) *gf-char*) ))

    (si::define-compiler-macro gf-cplus-b (a b) ;; assumes that both 0 <= a,b < *gf-char*
      `(if (typep *gf-char* 
             ',(if (< (integer-length most-positive-fixnum) 63) `fixnum `(signed-byte 63)) )                                               
        (let ((x ,a) (y ,b) (z *gf-char*)) (declare (fixnum x y z))                               
          (+%b x y z) )                                               
        (let ((x (+ ,a ,b)))                                     
          (if (< x *gf-char*) x (- x *gf-char*)) )))  
 
    (si::define-compiler-macro gf-cminus-b (a) ;; assumes that 0 <= a < *gf-char*
      `(if (typep *gf-char* 'fixnum)
        (let ((x ,a) (z *gf-char*)) (declare (fixnum x z))
          (neg%b x z) )
        (- *gf-char* ,a) ))
))
;;
;; -----------------------------------------------------------------------------


;; setting the finite field and retrieving basic informations ------------------
;;
(defmfun $gf_set (p &optional a1 a2 a3) ;; opt: *gf-exp*, *gf-red*, *gf-fs-ord*
  (unless (and (integerp p) (primep p))
    (merror (intl:gettext "Field characteristic must be a prime number." )) )
  ($gf_unset)
  (setq *gf-char* p)
  #-gcl (setq *fixnump-2gf-char* (< (* 2 p) most-positive-fixnum))

  (let ((modulus))
    (when a1 ;; exponent or reduction poly
      (cond 
        ((integerp a1) (setq *gf-exp* a1))
        (($listp a1)
          (merror (intl:gettext 
            "Unsuitable second argument to `gf_set': ~m") a1 ))
        (t
          (setq a1 (gf-set-red a1))
          (unless (fixnump (setq *gf-exp* (car a1)))
            (merror (intl:gettext "The exponent must be a fixnum." )) )
          (unless (gf-irr-p a1 *gf-char* *gf-exp*)
            (setq *gf-prim* '$false) ) )))
    (when a2 ;; reduction poly or factors of ord
      (cond 
        ((integerp a2) 
          (merror (intl:gettext 
            "Third argument to `gf_set' must be the reduction polynomial or a list of factors.")) )
        (($listp a2)
          (unless ($listp (cadr a2))
            (merror (intl:gettext 
              "The list of factors must be of the form [[p1, e1], ..., [pk, ek]].")) )
          (setq *gf-fs-ord* (mapcar #'cdr (cdr a2))) )
        (t
          (setq a2 (gf-set-red a2))
          (unless (gf-irr-p a2 *gf-char* *gf-exp*)
            (setq *gf-prim* '$false) )
          (unless (= *gf-exp* (car *gf-red*))
            (merror (intl:gettext 
              "The reduction polynomial must be of degree ~m." ) *gf-exp* )) )))
    (when a3 ;; factors of ord
      (unless (and ($listp a3) ($listp (cadr a3)))
        (merror (intl:gettext 
          "Unsuitable fourth argument to `gf_set': ~m") a3 ))
      (setq *gf-fs-ord* (mapcar #'cdr (cdr a3))) ) 

    (when (= 0 *gf-exp*)
      (setq *gf-exp* 1) )
    
    (unless *gf-red*
      (let ((cre (mfuncall '$rat '$x)))
        (setq *gf-var* '$x 
              *gf-rat-header* (car cre)
              *gf-rat-sym* (caadr cre)
              *gf-red* (if (= 1 *gf-exp*) (list 1 1) (gf-irr p *gf-exp*)) )))

    (setq *gf-ord*
      (cond 
        ((= 1 *gf-exp*) (1- p))
        ((equal *gf-prim* '$false) (gf-ord-ring))
        (t (1- (expt p *gf-exp*))) ))

    (setq *gf-card* (expt p *gf-exp*))

    (unless *gf-fs-ord*
      (let* (($intfaclim)
             (fs (nreverse (get-factor-list *gf-ord*))) ) 
        (setq *gf-fs-ord* (sort fs #'(lambda (a b) (< (car a) (car b))))) ))  ;; .. [pi, ei] .. 

    (unless *gf-prim*
      (setq *gf-prim*
        (cond 
          ((= 1 *gf-exp*)
            (if (= 2 *gf-char*) (list 0 1)
              (list 0 (zn-primroot p *gf-ord* (mapcar #'car *gf-fs-ord*))) )) ;; .. pi ..  (factors_only:true)
          (t
            (gf-precomp)
            (gf-next-prim 1) ))))

    (when (equal *gf-prim* '$false) (setq *gf-prim* nil))
    
    (setq *gf-minset?* t
          *gf-set?* t )

    `((mlist simp) 
      ,(when *gf-prim* (gf-x2p *gf-prim*)) 
      ,(gf-x2p *gf-red*) ) ))

;; part of $gf_set:

(defun gf-set-red (p-orig)
  (let ((p ($rat p-orig)))
    (unless (listp (cadr p))
      (merror (intl:gettext "Argument not suitable for the reduction polynomial: ~m" ) p-orig) )
    (let ((vars (caddar p)))
      (when (> (length vars) 1)
        (merror (intl:gettext "Argument not suitable for the reduction polynomial: ~m" ) p-orig) )
      (setq *gf-var* (car vars) 
            *gf-rat-header* (car p)
            *gf-rat-sym* (caadr p)
            *gf-red* (gf-mod (cdadr p)) )
      (when (/= 1 (cadr *gf-red*))
        (merror (intl:gettext "A monic reduction polynomial is assumed." )) )
      *gf-red* )))


(defmfun $gf_unset ()
  (setq $gf_powers '$gf_powers
        $gf_logs '$gf_logs
        $gf_rat nil
        *gf-var* nil *gf-rat-sym* nil *gf-rat-header* nil
        *gf-char* 0 *gf-exp* 0 *gf-ord* 0 
        *gf-prim* nil *gf-red* nil 
        *gf-fs-ord* nil *gf-fs-base-p* nil *gf-x^p-powers* nil 
        *gf-tables?* nil *gf-set?* nil )
  t )


(defun gf-minset? ()
  (unless *gf-minset?*
    (merror (intl:gettext "gf data not set." )) ))

(defun gf-set? ()
  (unless *gf-set?*
    (merror (intl:gettext "gf data not or not fully set." )) ))  

(defun field? ()
  (unless *gf-prim*
    (merror (intl:gettext "Not a field." )) ))

(defmfun $gf_characteristic () 
  (gf-minset?) *gf-char* )

(defmfun $gf_primitive () 
  (gf-set?) (gf-x2p *gf-prim*) )

(defmfun $gf_reduction () 
  (gf-minset?) (gf-x2p *gf-red*) )


(defmfun $gf_info (&optional (print? t))
  (gf-set?)
  (cond
    (print?
      (mfuncall '$print "char:" *gf-char*)
      (mfuncall '$print "exp:" *gf-exp*)
      (mfuncall '$print "ord:" *gf-ord*)
      (cond 
        (*gf-prim*
          (mfuncall '$print "prim:" (gf-x2p *gf-prim*)) )
        (t
          (mfuncall '$print "card:" *gf-card*)
          (mfuncall '$print "prim:" nil) ))
      (mfuncall '$print "red:" (gf-x2p *gf-red*))
      nil )
    (t
      `((mlist simp) 
        ,*gf-char* ,*gf-exp* ,*gf-ord* ,@(unless *gf-prim* `(,*gf-card*))
        ,(when *gf-prim* (gf-x2p *gf-prim*)) 
        ,(gf-x2p *gf-red*) )) ))

;; Minimal set
;; Just set characteristic and reduction poly to allow basic arithmetics on the fly.
(defmfun $gf_minimal_set (p red)
  (unless (and (integerp p) (primep p))
    (merror (intl:gettext "First argument to `gf_minimal_set' must be a prime number." )) )
  ($gf_unset)
  (setq *gf-char* p)
  #-gcl (setq *fixnump-2gf-char* (< (* 2 p) most-positive-fixnum))
  (setq red (gf-set-red red))
  (unless (fixnump (setq *gf-exp* (car red)))
    (merror (intl:gettext "The exponent must be a fixnum." )) )
  (when (= 0 *gf-exp*) (setq *gf-exp* 1))
  (setq *gf-minset?* t) )
;;
;; -----------------------------------------------------------------------------


;; lookup tables ---------------------------------------------------------------
;;
(defmfun $gf_make_arrays () 
  (gf-set?)
  (field?)
  (unless (fixnump *gf-ord*)
    (merror (intl:gettext "`gf_make_arrays': Field order must be a fixnum." )) )
  (when *gf-prim* (gf-make-tables)) )

(defun gf-make-tables () 
  (let ((x (list 0 1)) (ord *gf-ord*) (primx *gf-prim*)) 
       (declare (fixnum ord))
;;
;; power table of the field, where the i-th element is the numerical
;; equivalent of the field element e^i, where e is a primitive element 
;;
    (setq $gf_powers (make-array (1+ ord) :element-type 'integer))
    (setf (svref $gf_powers 0) 1)
    (do ((i 1 (1+ i)))
        ((> i ord))
        (declare (fixnum i))
      (setq x (gf-xtimes x primx))
      (setf (svref $gf_powers i) (gf-x2n x)) )
;;
;; log table: the inverse lookup of the power table 
;;
    (setq $gf_logs (make-array (1+ ord) :initial-element nil))
    (do ((i 0 (1+ i)))
        ((= i ord))
        (declare (fixnum i))
      (setf (svref $gf_logs (svref $gf_powers i)) i) )
    (setq *gf-tables?* t)
    `((mlist simp) ,$gf_powers ,$gf_logs) ))

(defun gf-clear-tables () 
  (setq $gf_powers '$gf_powers
        $gf_logs '$gf_logs
        *gf-tables?* nil ))
;;
;; -----------------------------------------------------------------------------


;; converting to/from internal representation ----------------------------------
;;
;; user level      <---> internal
;; 0                     nil
;; integer # 0           (0 integer') where integer' = mod(integer, *gf-char*) 
;; x                     (1 1)
;; x^4 + 3*x^2 + 4       (4 1 2 3 0 4) 
;;
;; This representation uses the term part of the internal CRE representation.
;; The coeffcients are exclusively positive: 1, 2, ..., (*gf-char* -1)
;; Header informations are stored in *gf-var* *gf-rat-sym* *gf-rat-header*.
;;
;; gf_set(5, 4)$
;; :lisp `(,*gf-char* ,*gf-exp*)
;; (5 4)
;; p : x^4 + 3*x^2 - 1$
;; :lisp (mfuncall '$rat $p)
;; ((MRAT SIMP ($X) (X33303)) (X33303 4 1 2 3 0 -1) . 1)
;; :lisp (gf-p2x $p)
;; (4 1 2 3 0 4)
;; :lisp `(,*gf-rat-header* ,*gf-var* ,*gf-rat-sym*)
;; ((MRAT SIMP ($X) (X33303)) $X X33303)
;;
;; Remark: I compared the timing results of the arithmetic functions using this 
;; data structure to arithmetics using an array implementation and in case of 
;; modulus 2 to an implementation using bit-arithmetics over integers. 
;; It turns out that in all cases the timing advantages of other data structures 
;; were consumed by conversions from/to the top-level.
;; So for sparse polynomials the CRE representation seems to fit best.
;; 
(defun gf-p2x (p) 
  (let ((modulus *gf-char*))
    (setq p (cadr (mfuncall '$rat p)))
    (if (integerp p)
      (if (= 0 p) nil (list 0 (mod p *gf-char*))) 
      (gf-mod (cdr p)) )))

(defun gf-x2p (x)
  (cond
    ((null x) (setq x 0))
    ((= 0 (car x)) (setq x (cadr x))) )
  (if (eql $gf_rat t)
    (gf-x2cre x)
    (gf-disrep x) ))
;;
;; depending on $gf_rat gf-x2p returns a CRE or a ratdisrepped expression
;;
(defun gf-x2cre (x)
  (if (integerp x) 
    `(,*gf-rat-header* ,x . 1)
    `(,*gf-rat-header* ,(cons *gf-rat-sym* x) . 1) ))

(defun gf-disrep (x &optional (var *gf-var*)) 
  (if (integerp x) x
    (maybe-fixnum-let ((c 0))
      (do ((not-plus? (null (cddr x))) p (e 0)) 
          ((null x) (if not-plus? (car p) (cons '(mplus simp) p)))
          (declare (fixnum e))
        (setq e (the fixnum (car x)) c (cadr x) x (cddr x) 
              p (cond 
                  ((= 0 e) 
                    (cons c p) )
                  ((= 1 e) 
                    (if (= 1 c) 
                      (cons var p)
                      (cons `((mtimes simp) ,c ,var) p) ))
                  ((= 1 c)
                    (cons `((mexpt simp) ,var ,e) p) )
                  (t
                    (cons `((mtimes simp) ,c ((mexpt simp) ,var ,e)) p) )))))))


;; tables of small fields ------------------------------------------------------
;;
(defmfun $gf_mult_table ()
  (gf-set?)
  (mfuncall '$genmatrix  
              #'(lambda (i j) (gf-x2n (gf-xtimes (gf-n2x (1- i)) (gf-n2x (1- j))))) 
              *gf-card* 
              *gf-card* ))

(defmfun $gf_powers ()
  (gf-set?)
  (mfuncall '$genmatrix  
              #'(lambda (i j) (gf-x2n (gf-pow (gf-n2x (1- i)) j)))
              *gf-card* 
              (1+ *gf-ord*) ))
;;
;; -----------------------------------------------------------------------------


;; evaluation and adjustment ---------------------------------------------------
;;

;; an arbitrary polynomial is evaluated in a given field

(defmfun $gf_eval (a) 
  (gf-minset?) 
  (let ((modulus *gf-char*))
    (setq a (mfuncall '$remainder ($rat a) ($gf_reduction)))
    (if (integerp (cadr a))
      (rplaca (cdr a) (mod (cadr a) *gf-char*))
      (let ((b (gf-mod (cdadr a))))
        (if (null b) 
          (rplaca (cdr a) 0)
          (rplacd (cadr a) b) )))
    (if (eql t $gf_rat) ($rat a)
      (mfuncall '$ratdisrep ($rat a)) )))

;; gf-mod adjusts arbitrary integer coefficients (pos, neg or unbounded)

(defun gf-mod (x) 
  (if (null x) x
    (maybe-fixnum-let ((m *gf-char*) (c 0))
      (do ((r x (cddr r)) res) ((null r) (nreverse res))
        (cond 
          ((integerp (cadr r))
            (setq c (mod (cadr r) m))
            (when (> c 0) (setq res (cons c (cons (car r) res)))) )
          (t
            (let ((y (cadr r)))
              (setq y (cons (car y) (gf-mod (cdr y)))
                    res (cons y (cons (car r) res))) ))) ))))

;; gf-nmod is an adjustment after using rat3a functions
;; it assumes coefficients in between -modulus/2 ... +modulus/2
 
(defun gf-nmod (x) ;; destructive function, modifies x
  (if (null x) x
    (maybe-fixnum-let ((m *gf-char*))
      (do ((r (cdr x) (cddr r))) (())
        (when (< (car r) 0) (incf (car r) m)) 
        (when (null (cdr r)) (return x)) ))))
;;
;; -----------------------------------------------------------------------------


;; arithmetic in Galois Fields - Maxima level functions ------------------------
;;
(defmfun $gf_add (&rest args) 
  (gf-minset?)
  (setq args (mapcar #'gf-p2x args))
  (gf-x2p (reduce #'gf-xplus args)) )

(defmfun $gf_sub (&rest args) 
  (gf-minset?)
  (setq args (mapcar #'gf-p2x args))
  (gf-x2p (gf-xplus (car args) (gf-xminus (reduce #'gf-xplus (cdr args))))) )

(defmfun $gf_mult (&rest args) 
  (gf-minset?)
  (setq args (mapcar #'gf-p2x args))
  (gf-x2p (reduce #'gf-xtimes args)) ) 

(defmfun $gf_inv (a) 
  (gf-minset?) 
  (setq a (gf-inv (gf-p2x a)))
  (when a (gf-x2p a)) ) ;; a is nil in case the inverse does not exist

(defmfun $gf_div (&rest args) 
  (gf-minset?)
  (setq args (mapcar #'gf-p2x args)
        args (cons (car args) (mapcar #'gf-inv (cdr args))) )
  (cond
    ((null (car args)) 0)
    ((some #'null (cdr args)) nil)
    (t (gf-x2p (reduce #'gf-xtimes args))) )) 

(defmfun $gf_exp (&optional a n)
  (gf-minset?) 
  (cond 
    ((not a) *gf-exp*)
    ((not n) 
      (merror (intl:gettext "`gf_exp' needs zero or two arguments." )) )
    ((not (integerp n))
      (merror (intl:gettext "Second argument to `gf_exp' must be an integer." )) )
    ((< n 0)
      (setq a (gf-inv (gf-p2x a)))
      (when a ($gf_exp (gf-x2p a) (neg n))) ) ;; a is nil in case the inverse does not exist
    (t 
      (gf-x2p (gf-pow (gf-p2x a) (mod n *gf-ord*))) )))
;;
;; -----------------------------------------------------------------------------


;; arithmetic in Galois Fields - Lisp level functions --------------------------
;;

;; Remark: A prefixed character 'n' indicates a destructive function.

;; c * x

(defun gf-xctimes (c x)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (inline gf-ctimes))
  (maybe-fixnum-let ((c c))
    (if (or (= 0 c) (null x)) nil
      (do* ((res (list (the fixnum (car x)) (gf-ctimes c (cadr x))))
            (r (cdr res) (cddr r)) 
            (rx (cddr x) (cddr rx)) )
           ((null rx) res)
        (rplacd r (list (the fixnum (car rx)) (gf-ctimes c (cadr rx)))) ))))

(defun gf-nxctimes (c x) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (inline gf-ctimes))
  (maybe-fixnum-let ((c c))
    (if (or (= 0 c) (null x)) nil
    (do ((r (cdr x) (cddr r)))
        ((null r) x)
      (rplaca r (gf-ctimes c (car r))) ))))

;; c*v^e * x

(defun gf-xcetimes (x e c)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum e) (inline gf-ctimes))
  (maybe-fixnum-let ((c c))
    (if (or (= 0 c) (null x)) nil
      (do* ((res (list (the fixnum (+ e (the fixnum (car x)))) (gf-ctimes c (cadr x))))
            (r (cdr res) (cddr r)) 
            (rx (cddr x) (cddr rx)) )
           ((null rx) res)
        (rplacd r (list (the fixnum (+ e (the fixnum (car rx)))) (gf-ctimes c (cadr rx)))) ))))

;; - x

(defun gf-xminus (x) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (inline gf-cminus-b))
  (if (or (null x) (= 2 *gf-char*)) x
    (do* ((res (list (the fixnum (car x)) (gf-cminus-b (cadr x))))
          (r (cdr res) (cddr r)) 
          (rx (cddr x) (cddr rx)) )
         ((null rx) res)
      (rplacd r (list (the fixnum (car rx)) (gf-cminus-b (cadr rx)))) )))

(defun gf-nxminus (x) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (inline gf-cminus-b))
  (if (or (null x) (= 2 *gf-char*)) x
    (do ((r (cdr x) (cddr r))) (())
      (rplaca r (gf-cminus-b (car r)))
      (when (null (cdr r)) (return x)) )))

;; x + c, 0 < c < *gf-char*

(defun gf-nxcplus (x c) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (inline gf-cplus-b))
  (maybe-fixnum-let ((c c))
    (cond 
      ((null x) (list 0 c))
      (t (setq x (nreverse x))
         (cond
           ((= 0 (the fixnum (cadr x))) ;; e
             (setq c (gf-cplus-b c (car x)))
             (if (= 0 c)
               (setq x (cddr x))
               (rplaca x c) ))
           (t (setq x (cons c (cons 0 x)))) )
         (nreverse x) ))))

;; x + y

(defun gf-xplus (x y) 
  (cond 
    ((null x) y)
    ((null y) x)
    (t (gf-nxplus (copy-list x) y)) )) 

;; merge y into x

(defun gf-nxplus (x y) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (inline gf-cplus-b))
  (cond 
    ((null x) y)
    ((null y) x)
    (t (maybe-fixnum-let ((cy 0)(c 0))
        (prog (r (ex 0)(ey 0)) (declare (fixnum ex ey))
          a1
          (setq ex (the fixnum (car x)) ey (the fixnum (car y)) cy (cadr y))
          (cond 
            ((> ey ex)
              (setq x (cons ey (cons cy x)) y (cddr y)) ) 
            ((= ey ex)
              (setq c (gf-cplus-b (cadr x) cy) y (cddr y))
              (cond  
                ((= 0 c)
                  (when (null (setq x (cddr x))) (return y)) 
                  (when (null y) (return x))
                  (go a1) )
                (t (rplaca (cdr x) c)) ))
            (t (setq r (cdr x)) (go b)) )
          (setq r (cdr x))
          a
          (when (null y) (return x))
          (setq ey (the fixnum (car y)) cy (cadr y))
          b
          (while (and (cdr r) (> (the fixnum (cadr r)) ey))
            (setq r (cddr r)) )
          (cond 
            ((null (cdr r)) (rplacd r y) (return x))
            ((> ey (the fixnum (cadr r)))
              (rplacd r (cons ey (cons cy (cdr r))))
              (setq r (cddr r) y (cddr y)) )
            (t
              (setq c (gf-cplus-b (caddr r) cy) y (cddr y))
              (if (= 0 c)
                (rplacd r (cdddr r))
                (rplaca (setq r (cddr r)) c) )) ) 
          (go a) )))))

;; x + c*v^e*y

(defun gf-xyceplus (x y e c) 
  (cond 
    ((null y) x)
    ((null x) (gf-xcetimes y e c))
    (t (gf-nxyceplus (copy-list x) y e c) )))

;; merge c*v^e*y into x

(defun gf-nxyceplus (x y e c) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum e)(inline gf-ctimes gf-cplus-b))
  (cond 
    ((null y) x)
    ((null x) (gf-xcetimes y e c))
    (t (maybe-fixnum-let ((cy 0)(cc 0))
        (prog (r (ex 0)(ey 0)) (declare (fixnum ex ey))
          a1
          (setq ey (the fixnum (+ (the fixnum (car y)) e)) 
                cy (gf-ctimes c (cadr y)) 
                ex (the fixnum (car x)) )
          (cond 
            ((> ey ex)
              (setq x (cons ey (cons cy x)) y (cddr y)) ) 
            ((= ey ex)
              (setq cc (gf-cplus-b (cadr x) cy) y (cddr y)) 
              (cond  
                ((= 0 cc)
                  (when (null (setq x (cddr x))) (return (gf-xcetimes y e c))) 
                  (when (null y) (return x))
                  (go a1) )
                (t (rplaca (cdr x) cc)) ))
            (t (setq r (cdr x)) (go b)) )
          (setq r (cdr x))
          a
          (when (null y) (return x))
          (setq ey (the fixnum (+ (the fixnum (car y)) e)) 
                cy (gf-ctimes c (cadr y)) )
          b
          (when (null (cdr r)) (go d))
          (setq ex (the fixnum (cadr r)))
          (cond 
            ((> ey ex)
              (rplacd r (cons ey (cons cy (cdr r))))
              (setq r (cddr r) y (cddr y))
              (go a) )
            ((= ey ex)
              (setq cc (gf-cplus-b (caddr r) cy)) 
              (if (= 0 cc)
                (rplacd r (cdddr r))
                (rplaca (setq r (cddr r)) cc) )
              (setq y (cddr y))
              (go a) ) 
            (t (setq r (cddr r)) (go b)) ) 
          d
          (do () ((null y))
            (setq x (nconc x (list (the fixnum (+ (the fixnum (car y)) e)) (gf-ctimes c (cadr y))))
                  y (cddr y) ))
          (return x) ) ))))

;; x * y 
;;
;; It turns out that in case of sparse polynomials (in Galois Fields)
;; simple school multiplication is faster than Karatsuba.
;; 
;; x * y = (x1 + x2 + ... + xk) * (y1 + y2 + ... + yn)
;;       =  x1 * (y1 + y2 + ... + yn) + x2 * (y1 + y2 + ... + yn) + ...
;;
;;         where e.g. xi = ci*v^ei
;;
(defun gf-xtimes (x y)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (inline gf-ctimes gf-cplus-b))
  (if (or (null x) (null y)) nil
    (maybe-fixnum-let ((c 0)(cx 0))
      (do* ((res (gf-xcetimes y (car x) (cadr x))) ;; x1 * (y1 + y2 + ... + yn). summands in res are sorted. res is a new list.
            (r1 (cdr res))                         ;; r1 marks the place of xi*y1 in res. x[i+1]*y1 will be smaller.
            ry                                     ;; ry iterates over y
            (x (cddr x) (cddr x))                  ;; each loop: res += xi * (y1 + y2 + ... + yn)
            (e 0)(ex 0))
           ((or (null x)(null y)) (gf-nred res))
           (declare (fixnum e ex))
        (setq ry y                                        ;; start with y1 again
              ex (the fixnum (car x)) cx (cadr x)         ;; xi = ci*v^ei
              e (the fixnum (+ ex (the fixnum (car ry)))) ;; c*v^e = xi*y1
              c (gf-ctimes (cadr ry) cx) )                ;; zero divisor free mult in Fp^n

        (while (and (cdr r1) (< e (the fixnum (cadr r1))))
          (setq r1 (cddr r1)) )                    ;; mark the position of xi*y1

        (do ((r r1)) (())                          ;; merge xi*y1 into res and then xi*y2, etc...
          (cond  
            ((or (null (cdr r)) (> e (the fixnum (cadr r))))
              (rplacd r (cons e (cons c (cdr r)))) 
              (setq r (cddr r)) )
            ((= 0 (setq c (gf-cplus-b (caddr r) c)))
              (rplacd r (cdddr r)) ) 
            (t (rplaca (setq r (cddr r)) c)) )
          
          (when (null (setq ry (cddr ry))) (return))
          (setq e (the fixnum (+ ex (the fixnum (car ry)))) 
                c (gf-ctimes (cadr ry) cx) )
        
          (while (and (cdr r) (< e (the fixnum (cadr r)))) 
            (setq r (cddr r)) ) )) )))

;; x^2
;;
;; x * x = (x1 + x2 + ... + xk) * (x1 + x2 + ... + xk)
;;
;;       = x1^2 + 2*x1*x2 + 2*x1*x3 + ... + x2^2 + 2*x2*x3 + 2*x2*x4 + ...
;;
;;       = xk^2 + x[k-1]^2 + 2*x[k-1]*xk + x[k-2]^2 + 2*x[k-2]*x[k-1] + 2*x[k-2]*xk + ...
;;
;; The reverse needs some additional consing but is slightly faster.
;;
(defun gf-sq (x) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (inline gf-ctimes gf-cplus-b))
  (cond 
    ((null x) x) 
    ((eql *gf-char* 2)       ;; the mod 2 case degrades to x1^2 + x2^2 + ... + xk^2
      (do (res)((null x) (gf-nred (nreverse res)))
        (setq res (cons 1 (cons (ash (the fixnum (car x)) 1) res))
              x (cddr x) ) ))
    (t 
      (maybe-fixnum-let ((ci 0)(*2ci 0)(c 0))
        (setq x (reverse x)) ;; start with xk
        (prog (res           ;; result
               r             ;; insertion marker in res
               acc           ;; acc accumulates previous xi
               r1            ;; r1 iterates in each loop over acc
               (ei 0)(e 0) )
              (declare (fixnum ei e))
          a1
          (setq ci (car x) ei (the fixnum (cadr x))                            ;; xi = ci*v^ei
                *2ci (gf-cplus-b ci ci)                                        ;; 2*ci (2*ci # 0 when *gf-char* # 2) 
                res (cons (the fixnum (+ ei ei)) (cons (gf-ctimes ci ci) res)) ;; res += xi^2 (ci^2 # 0, no zero divisors) 
                r (cdr res)                                                    ;; place insertion marker behind xi^2
                r1 acc ) 
          a
          (when (null r1) 
            (when (null (setq x (cddr x))) (return (gf-nred res))) 
            (setq acc (cons ei (cons ci acc)))           ;; cons previous xi to acc ..
            (go a1) )                                    ;; .. and start next loop

          (setq e (the fixnum (+ ei (the fixnum (car r1)))) 
                c (gf-ctimes *2ci (cadr r1))
                r1 (cddr r1) )

          (while (< e (the fixnum (cadr r)))
            (setq r (cddr r)) )
          (cond 
            ((> e (the fixnum (cadr r)))
              (rplacd r (cons e (cons c (cdr r))))
              (setq r (cddr r)) )
            (t
              (setq c (gf-cplus-b c (caddr r)))
              (if (= 0 c)
                (rplacd r (cdddr r))
                (rplaca (setq r (cddr r)) c) ) )) 
          (go a) ))) ))

;; x^n mod y

(defun gf-pow (x n) ;; assume 0 <= n <= order
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (cond 
    ((= 0 n) (list 0 1))
    (*gf-tables?* (gf-pow-by-table x n) )
    (t (do (res)(())
        (when (oddp n)
          (setq res (if res (gf-xtimes x res) x)) 
          (when (= 1 n)
            (return-from gf-pow res) ))
        (setq n (ash n -1) 
              x (gf-sq x)) ))))

(defun gf-pow-by-table (x n) 
  (let ((index (svref $gf_logs (gf-x2n x))))
    (gf-n2x (svref $gf_powers (mod (* index n) *gf-ord*))) ))
  
;; remainder:
;; x - quotient(x, y) * y 

(defun gf-rem (x y)
  (when (null y) (errrjf "Quotient by zero"))
  (if (null x) x 
    (gf-nrem (copy-list x) y) ))

(defun gf-nrem (x y) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (inline gf-ctimes gf-cminus-b))
  (when (null y) (errrjf "Quotient by zero"))
  (if (null x) x 
    (maybe-fixnum-let ((c 0)(lcx 0)(lcy-inv (gf-cminus-b (gf-cinv (cadr y))))) 
      (do ((e 0)(ley (car y))(y (cddr y)))
          ((null x) x)   
          (declare (fixnum e ley))
        (setq e (the fixnum (- (the fixnum (car x)) ley)))
        (when (< e 0) (return x)) 
        (setq lcx (cadr x) 
              c (gf-ctimes lcx lcy-inv)
              x (gf-nxyceplus (cddr x) y e c))  ))))

;; reduce x by y
;;
;; assume lc(y) = 1, reduction poly is monic

(defun gf-nred (x) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (inline gf-cminus-b))
  (if (null x) x
    (let* ((y *gf-red*) (e 0) (ley (car y)))
          (declare (fixnum e ley))
      (setq y (cddr y))
      (do () ((null x) x)
        (setq e (the fixnum (- (the fixnum (car x)) ley)))
        (when (< e 0) (return x))
        (setq x (gf-nxyceplus (cddr x) y e (gf-cminus-b (cadr x)))) ))))

;; gcd

(defun gf-gcd (x y) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (cond 
    ((null x) y)
    ((null y) x)
    (t (let ((r nil)) 
         (do ()((null y) 
                 (if (= 0 (the fixnum (car x))) (list 0 1) 
                   (let ((inv (gf-cinv (cadr x)))) (gf-xctimes inv x)) )) 
           (setq r (gf-rem x y)) 
           (psetf x y y r) )))))

;; extended gcd

(defun gf-gcdex (x y) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let ((x1 (list 0 1)) x2 y1 (y2 (list 0 1)) q r) 
    (do ()((null y) 
            (let ((inv (gf-cinv (cadr x)))) 
              (mapcar #'(lambda (a) (gf-xctimes inv a)) (list x1 x2 x)) )) 
      (multiple-value-setq (q r) (gf-divide x y))
      (psetf x y y r)
      (psetf 
        y1 (gf-nxplus (gf-nxminus (gf-xtimes q y1)) x1) 
        x1 y1 ) 
      (psetf 
        y2 (gf-nxplus (gf-nxminus (gf-xtimes q y2)) x2) 
        x2 y2 ) )))


;; inversion: y^-1
;;
;; returns nil in case the inverse does not exist (when reduction poly isn't irreducible)

(defun gf-inv (y) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (when (null y) (errrjf "Quotient by zero"))
  (let ((x *gf-red*) 
        (y1 (list 0 1)) x1 q r) 
    (setq y (copy-list y))
    (do ()((null y) 
            (when (= 0 (car x)) ;; gcd = 1 (const)
              (gf-nxctimes (gf-cinv (cadr x)) x1) )) 
      (multiple-value-setq (q r) (gf-divide x y)) 
      (psetf x y y r)
      (psetf 
        x1 y1
        y1 (gf-nxplus (gf-nxminus (gf-xtimes q y1)) x1) )) )) 

(defun gf-divide (x y)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (inline gf-cminus-b))
  (cond 
    ((null y) (errrjf "Quotient by zero"))
    ((null x) (values nil nil))
    (t (maybe-fixnum-let ((c 0)(lcx 0)(lcyi (gf-cinv (cadr y))))
         (setq x (copy-list x)) 
         (do (q (e 0) (ley (car y)) (y (cddr y)))
             ((null x) (values (nreverse q) x))
             (declare (fixnum e ley))
           (setq e (the fixnum (- (the fixnum (car x)) ley)))
           (when (< e 0) 
             (return (values (nreverse q) x)) )
           (setq lcx (cadr x) 
                 x (cddr x)
                 c (gf-ctimes lcx lcyi) )
           (unless (null y) (setq x (gf-nxyceplus x y e (gf-cminus-b c)))) 
           (setq q (cons c (cons e q))) )))))


;; polynomial/number/list - conversions ----------------------------------------
;;
(defmfun $gf_p2n (p) 
  (gf-minset?) (gf-x2n (gf-p2x p)) )

(defun gf-x2n (x)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if (null x) 0
    (maybe-fixnum-let ((m *gf-char*))
      (do ((n 0))(())
        (incf n (cadr x))
        (if (null (cddr x)) 
          (return (* n (expt m (the fixnum (car x)))))
          (setq n (* n (expt m (- (the fixnum (car x)) (the fixnum (caddr x)))))) )
        (setq x (cddr x)) ))))

(defmfun $gf_n2p (n) 
  (gf-minset?)
  (unless (integerp n)
    (merror (intl:gettext "`gf_n2p': Argument must be an integer.")) )
  (gf-x2p (gf-n2x n)) )

(defun gf-n2x (n)
  (maybe-fixnum-let ((r 0)(m *gf-char*))
    (do ((e 0 (1+ e)) x) 
        ((= 0 n) x)
      (declare (fixnum e))
      (multiple-value-setq (n r) (truncate n m))
      (unless (= 0 r)
        (setq x (cons e (cons r x))) ))))


(defmfun $gf_p2l (p &optional (len 0)) ;; in case of len = 0 the list isn't padded or truncated
  (declare (fixnum len))
  (gf-minset?) 
  (let ((x (gf-p2x p)))
    (cons '(mlist simp) (gf-x2l x len)) ))

(defun gf-x2l (x len)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum len))
  (do* ((e (the fixnum (car x))) (k (if (= 0 len) e (1- len)) (1- k)) l) 
       ((< k 0) (nreverse l))
       (declare (fixnum e k))
    (cond
      ((or (null x) (> k e))
        (push 0 l) )
      ((= k e) 
        (push (cadr x) l)
        (setq x (cddr x))
        (unless (null x) (setq e (the fixnum (car x)))) ))))

(defmfun $gf_l2p (l)
  (gf-minset?)
  (unless ($listp l)
    (merror (intl:gettext "`gf_l2p': Argument must be a list of integers.")) )
  (gf-x2p (gf-l2x (cdr l))) )

(defun gf-l2x (l)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (setq l (reverse l))
  (maybe-fixnum-let ((c 0))
    (do ((e 0) x)
        ((null l) x)
        (declare (fixnum e))
      (unless (= 0 (setq c (car l)))
        (setq x (cons e (cons c x))) )
      (setq l (cdr l))
      (incf e) )))


(defmfun $gf_l2n (l) 
  (gf-minset?)
  (unless ($listp l)
    (merror (intl:gettext "Argument to `gf_l2n' must be a list of integers.")) )
  (gf-l2n (cdr l)) )

(defun gf-l2n (l) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (maybe-fixnum-let ((m *gf-char*)(c1 (car l))(c 0))
    (setq l (reverse (cdr l)))
    (do ((n 0)(b 1)) 
        ((null l) (+ (* c1 b) n))
      (unless (= 0 (setq c (car l))) (incf n (* c b))) 
      (setq b (* b m) l (cdr l)) )))

(defmfun $gf_n2l (n &optional (len 0)) ;; in case of len = 0 the list isn't padded or truncated
  (declare (fixnum len))
  (gf-minset?)
  (unless (integerp n)
    (merror (intl:gettext "First argument to `gf_n2l' must be an integer.")) )
  (cons '(mlist simp) (if (= 0 len) (gf-n2l n) (gf-n2l-twoargs n len))) )

(defun gf-n2l (n) ;; this version is frequently called by gf-precomp, we want to keep it simple
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (maybe-fixnum-let ((d *gf-char*)(r 0))
    (do (l) ((= 0 n) l)
      (multiple-value-setq (n r) (truncate n d))
      (setq l (cons r l)) )))

(defun gf-n2l-twoargs (n len)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum len) )
  (maybe-fixnum-let ((d *gf-char*)(r 0))
    (do (l) ((= 0 len) l) 
      (multiple-value-setq (n r) (truncate n d))
      (setq l (cons r l))
      (decf len) )))


;; leading coefficient retrieved from number representation 

(defun gf-n2lc (n)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (maybe-fixnum-let ((d *gf-char*)(r 0))
    (do () ((= 0 n) r)
      (multiple-value-setq (n r) (truncate n d)) )))
;;
;; -----------------------------------------------------------------------------


;; irreducibility (Ben-Or algorithm) -------------------------------------------
;;
(defmfun $gf_irreducible_p (a &optional p) 
  (cond
    (p (unless (and (integerp p) (primep p))
        (merror (intl:gettext "`gf_irreducible_p': Second argument must be a prime number." )) ))
    (t (gf-minset?) (setq p *gf-char*)) )
  (let* ((*gf-char* p)                ;; gf_irreducible_p is independent of the given environment
         (x (gf-p2x a)) n) 
    (cond
      ((null x) nil)
      ((= 0 (setq n (car x))) nil)
      ((= 1 n) t)
      (t (gf-irr-p x p (car x))) )))

;; is y irreducible of degree n over Fp[x] ?

(defun gf-irr-p (y p n) ;; p,n > 1 !
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let ((*gf-char* p)                  ;; gf_irreducible_p is independent of the field set
        #-gcl (*fixnump-2gf-char* (< (* 2 p) most-positive-fixnum)) ;; see above
        (*gf-red* y)
        (x (list 1 1)) (mx (list 1 (1- p)))) 
    (do ((i 1 (1+ i)) (xp x) (n2 (ash n -1))) 
        ((> i n2) t)
        (declare (fixnum i n2))
      (setq xp (gf-pow xp p))
      (unless (equal '(0 1) (gf-gcd y (gf-xplus xp mx)))
        (return) ) )))

;; find the smallest irreducible element

(defun gf-irr (gf-char gf-exp) 
  #+ (or ccl ecl gcl)  (declare (optimize (speed 3) (safety 0)))
  (when (= 1 gf-exp)
    (return-from gf-irr (list 1 1)) )
  (setq *gf-char* gf-char) 
  (do ((n 1 (1+ n)) inc x) (()) (declare (fixnum n))
    (setq inc (gf-n2x n) 
          x (cons gf-exp (cons 1 inc)) )
    (when (gf-irr-p x gf-char gf-exp) (return x)) ))
;;
;; -----------------------------------------------------------------------------


;; Primitive elements ----------------------------------------------------------
;;

;; Tests if an element is primitive in the field 

(defmfun $gf_primitive_p (a) 
  (gf-set?)
  (let ((n (gf-x2n (gf-p2x a))))
    (when (< n *gf-card*) (gf-prim-p (gf-n2x n))) ))

(defun gf-prim-p (x) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let ((fbp *gf-fs-base-p*) (mp *gf-x^p-powers*) tmp prod)
    (do ((i 0 (1+ i)) (j 0 0) (lf (array-dimension *gf-fs-base-p* 0)))
        ((= i lf) t)
        (declare (fixnum i j lf))
      (setq prod (list 0 1) )
      (dolist (kj (svref fbp i))
        (setq tmp (gf-compose (svref mp j) x) 
              tmp (gf-pow tmp kj) 
              prod (gf-xtimes prod tmp) ) 
        (setq j (1+ j)) )
      (when (equal prod (list 0 1)) (return nil)) )))

;; Find the next primitive element

(defmfun $gf_next_primitive (a) 
  (gf-set?)
  (field?)
  (let* ((n (gf-x2n (gf-p2x a)))
         (x (if (= 1 *gf-exp*) (gf-next-prim1 n) (gf-next-prim n))) )
    (when x (gf-x2p x)) ))

(defun gf-next-prim1 (n)
  (maybe-fixnum-let ((m *gf-char*))
    (do ((i (1+ n) (1+ i)) (ord *gf-ord*) (fs-ord (mapcar #'car *gf-fs-ord*)))
         ((>= i m) nil)
        (declare (fixnum ord))
      (when (zn-primroot-p i m ord fs-ord)
        (return (list 0 i)) ) )))


;; Testing primitivity in Fp^n:

;; We check f(x)^qi # 1 (qi = *gf-ord*/pi) for all prime factors pi of *gf-ord*.
;; 
;; With qi = sum(aij*p^j, j,0,m) in base p and using f(x)^p = f(x^p) we get
;; 
;; f(x)^qi = f(x)^sum(aij*p^j, j,0,m) = prod(f(x^p^j)^aij, j,0,m).


;; Special case: f(x) = x+c and pi|*gf-ord* and pi|p-1.
;; 
;; Then qi = (p^n-1)/(p-1) * (p-1)/pi = sum(p^j, j,0,n-1) * (p-1)/pi.
;; 
;; With ai = (p-1)/pi and using *gf-red*(z) = prod(z - x^p^j, j,0,n-1) we get
;; 
;; f(x)^qi = f(x)^sum(ai*p^j, j,0,n-1) = (prod(f(x)^p^j, j,0,n-1))^ai 
;; 
;;         = (prod(x^p^j + c, j,0,n-1))^ai = ((-1)^n * prod(-c - x^p^j, j,0,n-1))^ai
;; 
;;         = ((-1)^n * *gf-red*(-c))^ai


;; precomputation for gf-next-prim (called by gf_set):
;;
(defun gf-precomp () 
  (let ((p-1 (1- *gf-char*))
        (fs-ord *gf-fs-ord*)
        fs-p-1 fs-list
        ($intfaclim) )
 
    (setq fs-p-1 (nreverse (get-factor-list p-1))
          fs-p-1 (sort fs-p-1 #'(lambda (a b) (< (car a) (car b)))) )            ;; .. [pi, ei] ..
    
    (dolist (fj fs-p-1) 
      (setq fs-ord (delete-if #'(lambda (sj) (= (car fj) (car sj))) fs-ord :count 1)))

    (setq fs-p-1 
      (mapcar #'(lambda (pe) (list (car pe) t (truncate p-1 (car pe)))) fs-p-1)) ;; .. [pi, true, (p-1)/pi] ..
    (setq fs-ord 
      (mapcar #'(lambda (pe) (list (car pe) nil)) fs-ord))                       ;; .. [pi, false] ..

    (setq fs-list (merge 'list fs-p-1 fs-ord #'(lambda (a b) (< (car a) (car b))))
          *gf-fs* (apply #'vector fs-list) )

    (setq *gf-fs-base-p* 
      (apply #'vector 
        (mapcar #'(lambda (pe) (nreverse (gf-n2l (truncate *gf-ord* (car pe))))) ;; qi = *gf-ord*/pi = sum(aij*p^j, j,0,m)
                fs-list) ))  

    (setq *gf-x^p-powers* (gf-x^p-powers *gf-exp*)) ))                           ;; x^p^j


(defun gf-next-prim (prev-n) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let* ((p *gf-char*) 
         (two-p (* 2 p))
         (even-exp (evenp *gf-exp*)) 
         (fs *gf-fs*)
         (fs-base-p *gf-fs-base-p*)
         (x^p-powers *gf-x^p-powers*)
         (red *gf-red*)
         k x y prod )

    (do ((cand (max (1+ prev-n) p) (1+ cand)) 
         found linear )
        (found (gf-n2x (1- cand)))    
      (when (> cand *gf-ord*) (return))
      (when (or (setq linear (< cand two-p)) (= 1 (gf-n2lc cand)))
        (setq x (gf-n2x cand))
;; testing primitivity:
        (do ((i 0 (1+ i)) (j 0) (lf (length fs)))
            ((= i lf) (setq found t))
            (declare (fixnum i j lf))
          (cond 
            ((and linear (cadr (svref fs i)))                       ;; linear and pi|*gf-ord* and pi|p-1
              (setq k (mod (- (gf-at red (- p cand))) p))           ;;           *gf-red*(-c)
              (when even-exp (setq k (- k)))                        ;;  (-1)^n * *gf-red*(-c)
              (setq k (power-mod k (caddr (svref fs i)) p))         ;; ((-1)^n * *gf-red*(-c))^ai
              (when (= k 1) (return nil)) )
            (t
              (setq prod (list 0 1) 
                    j 0 )
              (dolist (aij (svref fs-base-p i))
                (setq y (gf-compose (svref x^p-powers j) x))        ;;      f(x^p^j)
                (setq y (gf-pow y aij))                             ;;      f(x^p^j)^aij
                (setq prod (gf-xtimes prod y)) 
                (setq j (1+ j)) )
              (when (equal prod (list 0 1)) (return nil)) )) ))) )) ;; prod(f(x^p^j)^aij, j,0,m)

;; returns an array of polynomials x^p^j, j = 0, 1, .. , (n-1), where n = *gf-exp*

(defun gf-x^p-powers (n) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let ((p *gf-char*)(a (make-array n :element-type 'list :initial-element nil)) )
    (setf (svref a 0) (list 1 1)) ;; x
    (do ((j 1 (1+ j)))
        ((= j n) a)
        (declare (fixnum j))
      (setf (svref a j) (gf-pow (svref a (1- j)) p)) )))

;; modular composition (uses Horner and square and multiply)
;; y(x) mod *gf-red*

(defun gf-compose (x y) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if (integerp x) (gf-at y x)
    (do (res) (())
      (setq res (gf-nxcplus res (cadr y))) 
      (when (null (cddr y)) 
        (unless (= 0 (car y))
          (setq res (gf-xtimes res (gf-pow x (car y)))) ) 
        (return res) )
      (setq res (gf-xtimes res (gf-pow x (- (car y) (caddr y))))
            y (cddr y) ) )))

;; x(a)

(defun gf-at (x a) ;; Horner and square and multiply
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if (integerp x) x
    (maybe-fixnum-let ((a a))
      (do ((n 0)) (())
        (incf n (cadr x))
        (if (null (cddr x)) 
          (return (* n (expt a (the fixnum (car x)))))
          (setq n (* n (expt a (- (the fixnum (car x)) (the fixnum (caddr x)))))) )
        (setq x (cddr x)) ))))
;;
;; -----------------------------------------------------------------------------


;; random elements -------------------------------------------------------------
;;

;; Produces a random element within the given environment 

(defmfun $gf_random () 
  (gf-set?) (gf-x2p (gf-rand)) )

(defun gf-rand ()
  (let ((r (random *gf-card*)))
    (gf-n2x r) ))
;;
;; -----------------------------------------------------------------------------


;; factoring and gcd computation -----------------------------------------------
;;

(defmfun $gf_factor (a &optional p) ;; set p to switch to another modulus
  (cond
    (p (unless (and (integerp p) (primep p))
        (merror (intl:gettext "`gf_factor': Second argument must be a prime number." )) ))
    (t (gf-minset?) 
       (setq p *gf-char*) ))
  (let* ((*gf-char* p) (modulus p) 
         (a (mfuncall '$rat a))
         (vars (caddar a))(*gf-var* (car vars)) )
    (when (> (length vars) 1) 
      (merror (intl:gettext "`gf_factor': Polynomial must be univariate." )) )
    (setq a 
      (gf-disrep-factors 
        (gf-cmod-factors (pfactor (cadr a))) ))  
    (and (consp a) (consp (car a)) (equal (caar a) 'mtimes)
      (setq a (simplifya (cons '(mtimes) (cdr a)) nil)) )
    a ))

(defun gf-cmod-factors (fs) 
  (do ((r fs (cddr r)))
      ((null r) fs)
    (if (numberp (car r))
      (when (< (car r) 0) (incf (car r) *gf-char*))
      (rplaca r (gf-nmod (cdar r))) )))

(defun gf-disrep-factors (fs) 
  (cond 
    ((integerp fs) fs)
    (t 
      (setq fs (nreverse fs))
      (do ((e 0) fac p)
          ((null fs) (cons '(mtimes simp factored) p))
          (declare (fixnum e))
        (setq e (the fixnum (car fs)) 
              fac (cadr fs) 
              fs (cddr fs)
              p (cond 
                  ((integerp fac) (cons fac p))
                  ((= 1 e) (cons (gf-disrep fac) p))
                  (t (cons `((mexpt simp) ,(gf-disrep fac) ,e) p)) ))))))

;; gcd and gcdex

(defmfun $gf_gcd (a b) 
  (gf-minset?)
  (setq a (gf-p2x a) b (gf-p2x b))
  (gf-x2p (gf-gcd a b)) )

(defmfun $gf_gcdex (a b) 
  (gf-minset?)
  (setq a (gf-p2x a) b (gf-p2x b))
  (cons '(mlist simp) (mapcar #'gf-x2p (gf-gcdex a b))) )
;;
;; -----------------------------------------------------------------------------
       

;; order, degree and minimal polynomial ----------------------------------------
;;

;; Finds the lowest value k for which x^k = 1  (Otto Forsters Version)

(defmfun $gf_order (&optional a) 
  (gf-set?) 
  (cond 
    (a (setq a (gf-p2x a))
      (unless (and (null *gf-prim*) (/= 0 (car (gf-gcd a *gf-red*)))) ;; gcd # 1 
        (gf-ord a) )) 
    (t *gf-ord*) ))

(defun gf-ord (x) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if *gf-tables?*
    (gf-ord-by-table x)
    (let ((k *gf-ord*) p (e 0)) (declare (fixnum e))
      (dolist (pe *gf-fs-ord* k)
        (setq p (car pe) e (the fixnum (cadr pe))
              k (truncate k (expt p e)) )
        (do ((z (gf-pow x k)))
            ((equal z (list 0 1)))
          (setq z (gf-pow z p) 
                k (* k p) ) )) )))

(defun gf-ord-by-table (x) 
  (let ((index (svref $gf_logs (gf-x2n x))))
    (truncate *gf-ord* (gcd *gf-ord* index)) ))


;; If Fp^n = F[x]/(f) is no field then f splits into factors
;;
;;   f = f1^e1 * ... * fk^ek where fi are irreducible of degree ni.
;;
;; We compute ord(F[x]/(fi^ei)) by 
;;
;;   ((p^ni)^ei - (p^ni)^(ei-1)) = ((p^ni) - 1) * (p^ni)^(ei-1)
;;
;; and ord(Fp^n) with help of the Chinese Remainder Theorem.
;;
(defun gf-ord-ring () 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (maybe-fixnum-let ((modulus *gf-char*) (p *gf-char*))
    (prog (e-list p^n (e 0) (ord 1)) (declare (fixnum e))
      (do ((x (pfactor (cons *gf-rat-sym* *gf-red*)))) ;; a monic *gf-red* is assumed
          ((null x))
        (push (list (cadar x) (cadr x)) e-list)
        (setq x (cddr x)) )
      (dolist (a e-list)
        (setq p^n (expt p (the fixnum (car a))) 
              e   (the fixnum (cadr a))
              ord (* ord (1- p^n) (expt p^n (the fixnum (1- e)))) ))
      (return ord) )))


;; Finds the lowest value d for which x^(p^d) = x

(defmfun $gf_degree (a) 
  (gf-set?) 
  (field?)
  (gf-deg (gf-p2x a)) )

(defun gf-deg (x)
  (let ((p *gf-char*) (y x))
    (do ((d 1 (1+ d))) (()) (declare (fixnum d))
      (setq y (gf-pow y p))
      (when (equal x y) (return d)) )))

;; produce the minimal polynomial 

(defmfun $gf_minimal_poly (a)
  (gf-set?)
  (field?)
  (let ((z-cre (mfuncall '$rat (if (equal *gf-var* '$z) '$x '$z)))) 
    (setq a (gf-minpoly (gf-p2x a)))
    (if (eql $gf_rat t)
      `(,(car z-cre) ,(cons (caadr z-cre) a) . 1)
      (gf-disrep a '$z) ) ))
;;
;;                                  2             (d-1)
;;                        p        p             p
;;   f(z) = (z - x) (z - x ) (z - x  ) ... (z - x  )   , where d = degree(x)
;;
(defun gf-minpoly (x)
  (let ((p *gf-char*) (d (gf-deg x)) (powers (list (gf-x2p (gf-xminus x)))) prod) 
    (dotimes (i (1- d))
      (push (gf-x2p (gf-xminus (setq x (gf-pow x p)))) powers) )
    (dolist (pow powers)
      (push `((mplus simp) ,pow $z) prod) )
    (setq prod (cons '(mtimes simp) prod))
    (let ((modulus *gf-char*))
      (setq prod (mfuncall '$remainder prod (gf-x2p *gf-red*))) )
    (setq x (gf-p2x prod))
    (gf-nmod x) ))
;;
;; -----------------------------------------------------------------------------


;; normal elements and normal basis --------------------------------------------
;;

;; Tests if an element is normal 

(defmfun $gf_normal_p (a) 
  (gf-set?)
  (field?)
  (gf-normal-p (gf-p2x a)) )

(defun gf-normal-p (x) 
  (unless (null x) 
    (let ((modulus *gf-char*) (mat (gf-maybe-normal-basis x)))
      (equal (mfuncall '$rank mat) *gf-exp*) )))

;; Finds a normal element e in the field; that is, 
;; an element for which the list [e, e^p, e^(p^2), ... , e^(p^(n-1))] is a basis 

(defmfun $gf_normal (&optional start-with-ord?) 
  (gf-set?)
  (field?)
  (gf-x2p (gf-normal (eql start-with-ord? t))) )
    
(defun gf-normal (start-with-ord?) 
  (if start-with-ord?
    (do* ((n *gf-ord* (1- n)) 
          (x (gf-n2x n) (gf-n2x n)) ) 
         ((gf-normal-p x) x) )
    (do* ((n 1 (1+ n)) 
          (x (gf-n2x n) (gf-n2x n)) ) 
         ((gf-normal-p x) x) )))


;; Finds a normal element in the field by producing random elements and checking 
;; if each one is normal 

(defmfun $gf_random_normal ()
  (gf-set?)
  (field?)
  (gf-x2p (gf-random-normal)) )
  
(defun gf-random-normal ()
  (do ((x (gf-rand) (gf-rand))) 
      ((gf-normal-p x) x) ))

;; Produces a normal basis as a matrix; 
;; the rows are the coefficients of the powers e^(p^i) of the normal element 

(defmfun $gf_normal_basis (&optional a)
  (gf-set?)
  (field?)
  (let (a?)
    (setq a (cond (a (setq a? t) (gf-p2x a)) (t (gf-random-normal))))
    (let ((modulus *gf-char*) (mat (gf-maybe-normal-basis a)))
      (and a? (not (equal (mfuncall '$rank mat) *gf-exp*))
        (merror (intl:gettext "Argument to `gf_normal_basis' must be a normal element." )) )
      mat )))

(defun gf-maybe-normal-basis (x)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let ((powers (make-array *gf-exp* :initial-element nil)) 
        (gf-char *gf-char*) (gf-exp *gf-exp*) (e (1- *gf-exp*)) )
       (declare (fixnum gf-exp e))
    (setf (svref powers 0) x)
    (do ((k 1 (1+ k)))
        ((= gf-exp k))
        (declare (fixnum k))
      (setq x (gf-pow x gf-char))
      (setf (svref powers k) x) )
    (mfuncall 
      '$genmatrix 
        #'(lambda (i j) (svref (gf-coeffs-array (svref powers (1- i)) e) (1- j)))
        gf-exp 
        gf-exp )))

;; coeffs returns all coefficients of a polynomial, as a list of designated length.
;; The elements of the list are values in the range 0, 1, 2, ..., gf_characteristic - 1. 

(defun gf-coeffs-array (x n) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum n))
  (let ((cs (make-array (1+ n) :initial-element 0)))
    (do ((k n)) ((null x) cs) (declare (fixnum k))
      (cond 
        ((> k (the fixnum (car x))) 
          (decf k) )
        ((= k (the fixnum (car x))) 
          (setf (svref cs (- n k)) (cadr x)) 
          (setq x (cddr x)) 
          (decf k) )
        (t 
          (setq x (cddr x)) ) ))))

(defun gf-coeffs-list (x k) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum k))
  (do () ((or (null x) (>= k (the fixnum (car x)))))
    (setq x (cddr x)) )
  (when (null x) 
    (let ((cs nil))
      (dotimes (i k cs) (cons 0 cs)) ))

  (do ((k k (1- k)) (e (the fixnum (car x))) cs) 
      ((< k 0) (nreverse cs))
      (declare (fixnum k e))
    (cond
      ((or (null x) (> k e))
        (push 0 cs) )
      ((= k e) 
        (push (cadr x) cs)
        (setq x (cddr x))
        (unless (null x) (setq e (the fixnum (car x)))) ))))
    
;; Produces the normal representation of an element as a list of coefficients 

(defmfun $gf_normal_basis_rep (a m)
  (gf-set?)
  (field?)
  (let* ((inv ($gf_matinv m))
         (cs (cons '(mlist simp) (gf-coeffs-list (gf-p2x a) (1- *gf-exp*)))) 
         ($matrix_element_mult '*) ($matrix_element_add '+)
         (nbrep (meval `((mnctimes) ,cs ,inv))) res)
    (dolist (c (cdadr nbrep)) (push (gf-cmod c) res)) ;; 0 <= c
    (cons '(mlist simp) res) ))
;;
;; -----------------------------------------------------------------------------


;; functions for matrices ------------------------------------------------------
;;

;; matrix addition (convenience: mat, list or poly possible as argument)

(defmfun $gf_matadd (&rest args) 
  (reduce #'gf-matadd args) )

(defun gf-matadd (m1 m2) 
  (when ($listp m1) (setq m1 ($transpose m1)))
  (when ($listp m2) (setq m2 ($transpose m2)))
  (cond 
    ((and ($matrixp m1) ($matrixp m2))
      (gf-matadd2 m1 m2) )
    (($matrixp m1)
      (gf-matadd1 m1 m2) ) ;; assumed without checking: m2 is poly  
    (($matrixp m2)
      (gf-matadd1 m2 m1) )
    (t 
      ($gf_add m1 m2) ) ))

(defmfun gf-matadd1 (m poly) 
  (do ((r (cdr m) (cdr r)) new)
      ((null r) (cons '($matrix simp) (nreverse new)))
    (push (cons '(mlist simp) 
                (mapcar #'(lambda (p) ($gf_add p poly)) (cdar r)) ) new) ))

(defmfun gf-matadd2 (m1 m2) 
  (setq m1 (cdr m1) m2 (cdr m2))
  (unless (= (length (car m1)) (length (car m2)))
    (merror (intl:gettext "Arguments to `gf_matadd' must have same formal structure.")) )
  (do ((r1 m1 (cdr r1)) (r2 m2 (cdr r2)) new)
      ((or (null r1) (null r2)) 
        (unless (and (null r1) (null r2))
          (merror (intl:gettext "Arguments to `gf_matadd' must have same formal structure.")) )
        (cons '($matrix simp) (nreverse new)))
    (push (cons '(mlist simp) (mapcar #'$gf_add (cdar r1) (cdar r2))) new) ))


;; matrix multiplication (convenience: mat, list or poly possible as argument)

(defmfun $gf_matmult (&rest args) 
  (mfuncall '$rreduce #'gf-matmult (cons '(mlist simp) args)) )

(defun gf-matmult (m1 m2) 
  (when ($listp m1) (setq m1 (list '($matrix simp) m1)))
  (when ($listp m2) (setq m2 (mfuncall '$transpose m2)))
  (cond 
    ((and ($matrixp m1) ($matrixp m2))
      (gf-matmult2 m1 m2) )
    (($matrixp m1)
      (gf-matmult1 m1 m2) ) ;; assumed without checking: m2 is poly 
    (($matrixp m2)
      (gf-matmult1 m2 m1) )
    (t 
      ($gf_mult m1 m2) ) ))

(defmfun gf-matmult1 (m poly) 
  (do ((r (cdr m) (cdr r)) new)
      ((null r) (cons '($matrix simp) (nreverse new)))
    (push (cons '(mlist simp) 
                (mapcar #'(lambda (p) ($gf_mult p poly)) (cdar r)) ) new) ))

(defmfun gf-matmult2 (m1 m2) 
  (setq m1 (cdr m1) m2 (cdr ($transpose m2)))
  (unless (= (length (car m1)) (length (car m2)))
    (merror (intl:gettext "`gf_matmult': attempt to multiply nonconformable matrices.")) )
  (do ((r1 m1 (cdr r1)) new-mat)
      ((null r1) 
        (if (and (not (eq nil $scalarmatrixp))
                 (= 1 (length new-mat)) (= 1 (length (cdar new-mat))) )
          (cadar new-mat)
          (cons '($matrix simp) (nreverse new-mat)) )) 
    (do ((r2 m2 (cdr r2)) new-row)
        ((null r2) 
          (push (cons '(mlist simp) (nreverse new-row)) new-mat) ) 
      (push (gf-x2p 
             (reduce #'gf-nxplus 
              (mapcar #'(lambda (a b) (gf-xtimes (gf-p2x a) (gf-p2x b))) 
                (cdar r1) (cdar r2) ))) new-row) )))


;; inversion

(defmfun $gf_matinv (m) 
  (gf-set?)
  (field?)
  (let* (($matrix_element_mult '$gf_mult) ($matrix_element_add '$gf_add)
         (dm (mfuncall '$determinant m))
         (am (mfuncall '$adjoint m)) )
    (mfuncall '$matrixmap #'(lambda (p) ($gf_div p dm)) am) ))
;;
;; -----------------------------------------------------------------------------


;; discrete logarithm ----------------------------------------------------------
;;

;; solve g^x = a in Fp^n, where g is a generator 

(defmfun $gf_index (a) 
  (gf-set?) 
  (field?)
  (if (= 1 *gf-exp*)
    (mfuncall '$zn_log a (gf-x2n *gf-prim*) *gf-char*)
    (gf-dlog (gf-p2x a)) ))

(defmfun $gf_log (a &optional b) 
  (gf-set?)
  (field?)
  (cond 
    ((= 1 *gf-exp*)
      (mfuncall '$zn_log a (if b b (gf-x2n *gf-prim*)) *gf-char*) )
    (t
      (setq a (gf-p2x a))
      (if b
        (gf-dlogb a (gf-p2x b))
        (gf-dlog a) ))))


(defun gf-dlogb (a b) 
  (let* ((a-ind (gf-dlog a)) (b-ind (gf-dlog b))
         (d (gcd (gcd a-ind b-ind) *gf-ord*))
         (m (truncate *gf-ord* d)) )
    (mod (* (inv-mod (truncate b-ind d) m) (truncate a-ind d)) m) ))

;; Pohlig and Hellman reduction

(defun gf-dlog (a)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if *gf-tables?*
    (svref $gf_logs (gf-x2n a))
    (let (p (e 0) odivp (g *gf-prim*) gg x dlog dlogs tmp) 
         (declare (fixnum e))
      (dolist (f *gf-fs-ord*)
        (setq p (car f) e (cadr f)
              odivp (truncate *gf-ord* p)
              gg (gf-pow g odivp) )
        (cond 
          ((= 1 e) 
            (setq x (gf-dlog-rho-brent (gf-pow a odivp) gg p)) )
          (t
            (setq x 0)
            (do ((aa a) (k 1) (pk 1)) (()) (declare (fixnum k))
              (setq tmp (gf-pow aa (truncate odivp pk)) 
                    dlog (gf-dlog-rho-brent tmp gg p) 
                    x (+ x (* dlog pk)) )
              (if (= k e) 
                (return)
                (setq k (1+ k) pk (* pk p)) )
              (setq tmp (gf-inv (gf-pow g x))) 
              (setq aa (gf-xtimes a tmp)) ))) 
        (setq dlogs (cons x dlogs)) )
      (car (chinese (nreverse dlogs) 
                    (mapcar #'(lambda (z) (apply #'expt z)) *gf-fs-ord*) )) )))

;; iteration for Pollard rho:  b = g^y * a^z in each step

(declaim (inline gf-dlog-f))
(defun gf-dlog-f (b y z a g q)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let ((c (mod (cadr b) 3))) (declare (fixnum c))
    (cond 
      ((= 0 c) 
        (values (gf-xtimes a b) y                 (mod (+ z 1) q)  ) )
      ((= 1 c)
        (values (gf-xtimes g b) (mod (+ y 1) q)   z                ) )
      (t
        (values (gf-sq b)       (mod (ash y 1) q) (mod (ash z 1) q)) ) )))

;; Pollard rho for dlog computation (Brents variant of collision detection)

(defun gf-dlog-rho-brent (a g q)  
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (inline gf-dlog-f))
  (cond
    ((equal '(0 1) a) 0)
    ((equal g a) 1)
    ((equal a (gf-sq g)) 2) 
    ((equal '(0 1) (gf-xtimes a g)) (1- q)) 
    (t
      (prog ((b (list 0 1)) (y 0) (z 0)    ;; b = g^y * a^z
             (bb (list 0 1)) (yy 0) (zz 0) ;; bb = g^yy * a^zz
             dy dz rnd )
        rho
        (do ((i 0)(j 1)) (()) (declare (fixnum i j)) 
          (multiple-value-setq (b y z) (gf-dlog-f b y z a g q))
          (when (equal b bb) (return))     ;; g^y * a^z = g^yy * a^zz
          (incf i)
          (when (= i j)
            (setq j (1+ (ash j 1)))
            (setq bb b yy y zz z) ))
        (setq dy (mod (- yy y) q) dz (mod (- z zz) q)) ;; g^dy = a^dz = g^(x*dz)
        (when (= 1 (gcd dz q))
          (return (mod (* dy (inv-mod dz q)) q)) )     ;; x = dy/dz mod q (since g is generator of order q)
        (setq rnd (1+ (random (1- q))))
        (multiple-value-setq (b y z) 
          (values (gf-xtimes a (gf-pow g rnd)) rnd 1) )
        (multiple-value-setq (bb yy zz) (values (list 0 1) 0 0))
        (go rho) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
