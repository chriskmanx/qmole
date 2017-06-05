;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;

(in-package 'system)

(eval-when (compile) (proclaim '(optimize (safety 2) (space 3))))
(eval-when (eval compile) (defun sys:clear-compiler-properties (symbol)))
(eval-when (eval compile) (setq sys:*inhibit-macro-special* nil))

(in-package 'compiler)

(defun d (x) (disassemble x))
(defun foo (x) (labels ((g () (funcall x))) (g)))
(defun foo (x) (labels ((g () (funcall g x))) (g)))
(defun foo (x) (labels ((g (y) (funcall y x))) (g 0)))

#|
#+:CCL (rplaca (symbol-function 'foo) 'lambda-block)

(defun d () (disassemble 'foo))
(defmacro defn (&rest body)
  #+:CCL `(rplacd (symbol-function 'foo) ',body)
  #+KCL `(defun ,@body))
(defun test ()
  (compile-file "test.lsp" :c-file t :h-file t :data-file t :output-file nil))
|#

(defun fact (n) (if (= 0 n) 1 (* n (fact (1- n)))))
(defun t1 (n) (flet ((fl1 (x) (h (fl1 (g x))))) (fl1 (t1 n))))
(defun t2 (n) (labels ((fl1 (x) (h (fl1 (g x))))) (fl1 (t1 n))))
(defun t3 (n) (macrolet ((m (x) `(h (g ,x)))) (m (t1 n))))

(defun t4 (n) (multiple-value-call #'(lambda (x y &optional z) (foo x y z))
		(bar n) (gee n)))
(defun t5 (n) (multiple-value-call #'foo (bar n) (gee n)))
(defun t6 (n) ((lambda (x y) (foo x y))
		(bar n) (gee n)))
(defun t7 (line)
  (do ((i 0 line)
       (space-p #'(lambda (c) (or (eql c #\space) (eql c #\tab)))))
      ((>= i 0)) (funcall space-p)))


(defun t8 (n) (bar (bar n n) (bar n n)))
(defun t9 (n) (bar (bar n (bar n n))))
(defun t10 (n &optional acc) (bar n acc))
(proclaim '(special z))
(defun t11 (z) (bar z z))
(defun t12 (n &optional z) (bar n z))
(defconstant kappa 0)
;(defun t13 (kappa) (bar kappa))

(defun t14 (n) (let ((i 0)) (declare (fixnum i)) (bar (1+ i) n)))
(defun t15 (n) (let ((i (gee)) (j (gee)))
                     (declare (fixnum i)) (setq i j) (bar i j n)))
(defun t16 (n) (let ((sp 1))
                     (setq sp 3) (bar sp n)))
(defun t17 (n) (let ((sp 1))
                     (declare (special sp))
                     (setq sp 3) (bar sp n)))
(defun t18 (n) (let ((sp (gee)) (j sp))
                     (declare (special sp))
                     (setq j 3) (bar j n)))
(defun t19 (n) (let* ((sp n) (j sp))
                     (declare (special sp))
                     (setq j 3) (bar j n)))
(defun t20 (n) (let* ((sp (gee)) (j sp))
                     (declare (special sp))
                     (setq j 3) (bar j n)))
;; test cmpmulti.lsp
(defun t21 (n) (multiple-value-call 'bar (bar n n) (bar n n)))
(defun t22 (n) (multiple-value-prog1 (bar n n) (bar n n)))
(defun t22.1 (n) (multiple-value-prog1 (values 1 n) (bar n))))
(defun t23 (n) (bar (multiple-value-prog1 (bar n n) (bar n n))))
(defun t24 (n) (bar (multiple-value-prog1 (list n n) (bar n n))))
(defun t25 (n) (bar (multiple-value-prog1 (* n n n) (bar n n))))
(defun t25.1 (n) (bar (multiple-value-prog1
			 (multiple-value-prog1 (* n n n) (bar n n))
		       (foo n))))
(defun t26 (n) (values))
(defun t27 (n) (values n))
(defun t28 (n) (values n (bar n n)))

(defun t29 (n m) (multiple-value-setq () (bar n n)))
(defun t30 (n m) (multiple-value-setq (n m) (bar n n)))
(defun t31 (n m) (multiple-value-setq (n m) (values n n)))
(defun t32 (n m) (multiple-value-setq (n m) (progn (bar n) (values n n))))

(defun t33 (n)
          (multiple-value-bind (x y) (bar n n) (bar x y)))
(defun t34 (n)
          (multiple-value-bind (x y) (bar n n)
            (declare (special x)) (bar x y)))

;;; cmpflet.lsp
(defun t35 (n)
          (flet ((bar (x) (list x n)))
            (bar n)))
(defun t36 (n) (let ((y (gee)))
                     (flet ((bar (x) (list x y n)))
                       (bar n))))
(defun t37 (n) (let ((y n))
                     (flet ((bar (x) (list x y n)))
                       (bar n))))
(defun t38 (n &optional y)
          (flet ((bar (x) (list x y n)))
            (bar n)))
(defun t39 (n &optional y)
          (flet ((bar (x) 
                      (flet ((gee () (list x y n)))
                        (gee))))
            (bar n)))
(defun t40 (n)
  (flet ((bar (x) 
	   (flet ((gee () (list n n)))
	     (gee))))
    (bar n)))
(defun t41 (n)
  (print n)
  (flet ((bar () n)) #'bar))
(defun t42 (n)
  (flet ((bar (x) 
	   (flet ((gee () (list n n)))
	     #'gee)))
    (bar n)))
(defun t43 (n)
          (let ((y n))
            (flet ((bar (x) (list x y n)))
              (bar n)))
          (let ((y n))
            (flet ((bar (x) (list x y n)))
              (bar n))))
(defun t44 (x y z)
          (flet ((gen (v) (h y z #'(lambda () (p x v)))))
            (list x (gen y) (gen z))))
(defun t45 ()
          (flet ((gen (z) (bar z))) #'gen))
(defun t46 ()
          (flet ((gen (z) (bar z))) (list #'gen)))
(defun t47 ()
          (flet ((gen (z) (bar z))) #'(lambda () #'gen)))
(defun t48 (x y)
          (flet ((gen (z) (h x y z))) #'gen))
(defun t49 (x y)
          (flet ((gen (z) (h x y z)))
            #'(lambda () (gen 0))))
(defun t50 (x)
  (flet ((gen (z) (h x z)))
    (flet ((bar (y) (gen y)))
      (bar x))))
(defun t51 (x)
  (flet ((gen (z)
              (flet ((bar () (h x z)))
                (bar x z))))
    #'gen))
(defun t52 (x)
  (flet ((gen (z)
              (flet ((bar (y) y)) (bar x z))))
    (gen x)))

(defun t53 (x)
  (labels ((bar (y) (gen y))
           (gen (z) (h x z)))
    (bar x)))
(defun t54 (x)
  (labels ((bar () #'gen)
           (gen (z) (h x z)))
    (bar)))
(defun t55 (x)
  (labels ((gen (z) (h x z #'gen))
           (bar () #'gen))
    (bar)))
(defun t56 (x)
  (labels ((gen () #'bar)
           (bar () #'gen))
    (bar)))
(defun t57 (x)
  (let ((z (list x)))
    ;; the closure should have no lex:
    (labels ((gen () (bar x #'(lambda (y) (list z y)))))
      (gen))))
(defun t58 (x)
  (let ((z (list x)))
    (labels ((foo () x)
	     (gen () (bar z #'gen)))
      (gen))))
(defun t59 (s)
  ;; no closures:
  (labels ((vc (x) (list s x))
	   (hor (y) (vc y)))
    (mapcar #'hor s)))
;; g closure:
(defun foo (x) (flet ((g () (setq x 0)) (h () x)) (k #'h (g))))
;;; cmpcall.lsp
(defun t58 (x)
  ((lambda (&key y z) (foo y z)) :y x))
(defun t59 (x &rest r)
  (apply #'(lambda (&key a b c) (foo c b a)) x r))

;;; cmpvar.lsp
(defun t60 (n m)
          (psetq n (setq m 0) m (bar m)))
;; contravariance
(defun t61 (n) (let ((k 0))
                     (declare (fixnum k)) (setq k (* (length n) n))))
(defun t62 (n) (let ((k 0))
                     (declare (special k))
                     (setq k (* (length n) n))))
(defun t63 (n) (declare (vector n))
          (aref n (length n)))

(defun t64 (x y) (rplaca (bar x) y))
(defun t65 (x) ((lambda (y &optional z) (cons y z)) (1+ x) 3))

;;; cmptag.lsp
(defun t66 () (tagbody (flet ((bar () (go a))) (bar)) a (boh)))
(defun t67 () (tagbody (flet ((bar ()
				 (flet ((gee () (go a))) (gee))))
			  (bar)) a (boh)))
(defun foo () (tagbody l1 (a) (flet ((b () (go l1))) (b)) (go l1)))
(defun foo () (tagbody l1 (a) (unwind-protect (go l1) (b))))

;;; cmpblock.lsp
(defun foo () (flet ((bar () (return-from foo))) (bar)))
(defun foo () (flet ((bar () (return-from foo))) #'bar))
(defun foo () (block b (bar #'(lambda (x) (return-from b)))))
;; non buono il trattamento di catch:
(defun t71 () (catch 'ME (bar)))
(defun t71 () (catch 'ME (unwind-protect (throw 'ME (bar)) (gee))))

;;; cmpcatch.lsp
(defun foo () (unwind-protect (return-from foo 0) 1))

;;; cmpflet.lsp + cmplam.lsp
;;; bug: CLV0 is estracted in both lambda's
(defun foo (clv0)
  (labels ((bar (clv2)
	     (gee clv0)
	     #'(lambda () (bar clv2 #'bar))))
    #'(lambda (x) (bar x #'bar))))

(defun foo (hmm)			; hmm is closure variable
  (flet ((bar () hmm))
    #'(lambda (y) (bar y))))

;;; g lex, f cfun:
(defun foo ()
  (labels ((f (x) x)
	   (g () #'f))
    (g)))

;;; f, g, h closures:
(defun foo (x)
  (labels ((f () (list (h) (g)))
	   (g () #'f)
	   (h () x))
    (g)))

;;; f, g, h closures:
(defun foo (x)
  (flet ((h () x))
    (labels ((f () (list (h) (g)))
	     (g () #'f))
      (g))))

;;; b closure, c lex:
(defun foo () (tagbody l1 (a)
		 (labels ((b () (go l1)) (c () #'b)) (c))
		 (go l1)))

;;; cmpfun.lsp
(defun t80 (x) (apply #'(lambda (y &rest z) (list y z)) x))

;;; cmptop.lsp
(let ((z))
  (foo #'(lambda () (setq z nil)))
  (defun gee (hmm r)
    (flet ((do-state ()
	     (bar #'(lambda (x) x) hmm)))
      (do-state))
    #'(lambda ()
	(bar #'(lambda (x) x) hmm))))

;;; various test of closures:

(defun foo (x)
  (when x (block ccb
	    #'(lambda (x) (return-from ccb y))))
  (bar #'(lambda () (car x))))

(defun foo (x)
  (when x (let (ccb)
	    (bar #'(lambda (x) (list ccb y)))))
  (bar #'(lambda () (car x))))

(defun foo ()
  (let (x)
    (when x (let (ccb)
	      (bar #'(lambda (x) (list ccb y)))))
    (bar #'(lambda () (car x)))))

(defun foo (x)
  (let (ccb)
    (bar #'(lambda () (list ccb x)))))

(defun foo (x)
  (let (ccb *print-pretty*)
    (bar #'(lambda () (list ccb x)))))

(defun foo (x)
  (if x (let (ccb)
	  (bar #'(lambda (x) (list ccb y))))
      (bar #'(lambda () (car x)))))

(defun foo (gram)
  (let* ((new (let ((x use-nt)) (bar x))))
    #'(lambda () (merge-in-nt new gram))))

;; lex0 and env0:
(defun foo (gf em)
  (labels ((bar (c) (list gf em #'(lambda () gf) (bar c))))
    (bar gf)))

;; kintern should not use lex0:
(defun foo (x)
  (flet ((kintern (s) s)
	 (bar () x))
    (list (bar) #'(lambda (z) (kintern z)))))

;; tail recursion for labels:
(defun foo (gf em)
  (labels ((bar (c) (bar #'(lambda () gf))))
    (bar gf)))

;; proper use of temporary in inline-args:
(defun foo (x v) (declare (fixnum x) (simple-vector v))
       (the fixnum (+ (the fixnum (aref v 0)) (the fixnum (bar)))))
(defun foo (x v) (declare (fixnum x) (simple-vector v))
       (bar (the fixnum (aref v 0)) (bar)))
(defun foo (x v) (declare (fixnum x) (type (vector fixnum) v))
       (the fixnum (+ (the fixnum (aref v 0)) (the fixnum (bar)))))
(defun foo (z)
  (declare (double-float z))
  (* (+ z 0.5d0) (log z)))
(defun foo () (bar (plusp 3) (bar)))

;; keyword initialization
(defun foo (&rest r &key (arg (PROGN (X-ERROR) *DUMMY*)) b) arg)

;; volatile
(defun foo (l)
  (dolist (var l) (when var (return-from foo var))))
(defun foo (l &aux (y (car l)))
  (flet ((bar (x) (return-from foo x)))
    (bar l) y))
(defun foo (l &aux y)
 (setq y (car l)) (list #'(lambda (x) (return-from foo x)) y))

;; lex0 only:
(defun foo (arg)
  (flet ((bar (x) (cons x arg)))
    (flet ((gee (x) (bar x)))
      (gee arg))))
;; lex0 only:
(defun foo (arg)
  (flet ((bar (x) (cons x arg)))
    (let ((y arg))
      (flet ((gee (x) (cons y (bar x))))
	(gee arg)))))
;; lex0, lex1:
(defun foo (x)
  (labels ((gen (z)
	     (flet ((bar (w) (list x w z)))
	       (bar (gen (cdr z))))))
    (gen x)))
;; lex0, lex1:
(defun foo (x)
  (flet ((gen (z)
	     (flet ((bar (w) (list x w z)))
	       (bar (cdr z)))))
    (gen x)))
;; lex0, lex1:
(defun foo (x)
  (flet ((gen (y)
	   (let ((z y))
	     (flet ((bar (w) (list x w z)))
	       (bar (cdr z))))))
    (gen x)))
;; link should not be a closure:
(defun foo ()
  (labels ((link (y) (list #'(lambda (x) (eq x y)))))
    (link 3)))
;; lex required for gee:
(defun foo (x)
  (labels ((bar (y) y)
	   (gee () (list #'bar)))
    (gee)))
;; Still to fix. No closure for bar:
(defun foo ()
  (labels ((bar () (bar))) #'bar))

;; inlininig:
(defun foo (a) (declare (type (array bit) a)) (setf (aref a 3) 0))
(defun foo (map byte bit)
  (declare (fixnum bit byte) (type (simple-array bit (*)) map))
  (setq byte (the fixnum (logior (the fixnum (ash byte 1))
				 (aref map bit)))))
;; Still to fix. Should use just env0:
(defun foo (x)
  (TAGBODY
     (CONS
      #'(LAMBDA () (GO tag))
      #'(LAMBDA () x))
   tag)
  NIL)
;; bar itself should be in its own env:
(defun foo (x)
  (labels ((bar () (cons x (gee #'bar))))
    (bar)))
;; Segmentation violation
(defun c2compiler-let (symbols values body)
  (progv symbols values (c2expr body)))

;; Bug signalled by donc@ISI.EDU (Don Cohen)
(defun foo ()
  (labels ((outer ()
	     (labels ((inner () nil))
	       (list
		 #'(lambda () (outer))
  	         #'(lambda () (inner))))))
    (outer)))
;; variant:
(defun foo ()
  (labels ((outer ()
	     (list
	      #'(lambda () (outer))
	      (labels ((inner () nil))
		#'(lambda () (inner))))))
    (outer)))

;; let*: extra bds_unwind1
;;        if((V3=CDR(V3))==Cnil){
;;        bds_bind(VV[0],T0);                       /*  *VAL*           */
;;        bds_unwind1;
;;        goto L1;}
(defun foo (x)
  (let* ((*val* (mapcar #'list *val*)))
    (declare (special *val*))
    x))
;; compiler error:
(defun foo (x)
  (flet ((bar () x))
    #'(lambda () #'bar)))
;;
(defun foo (x)
  (flet ((bar (z) x))
    #'(lambda (args) (mapcar #'bar args)))) ; must be closure
;; Nested closures. Compiler bug:
(defun foo (x) #'(lambda () (list #'(lambda () x))))
;;
(defun foo () (macrolet ((g () nil)) (g)))
;; extra var V2:
(defun foo (htp) (flet ((b () htp)) (b)))
;; declaration of lex0 in .h must be lex0[1]
;; (use :tr (wt-function-epilogue :print (*lex*)) for disassemble):
(defun foo (x)
  (let (x) (flet ((bar () x)) (bar)))
  nil)
;; cc: undeclared variable i:
(defun foo (x)
  (MULTIPLE-VALUE-CALL #'(LAMBDA (&REST TRYVALUE) TRYVALUE) x))
;; cc: undeclared variable args:
(defun foo (x) (flet ((g (&key a))) (g :a x)))
;; lambda's should NOT be shared:
(defun foo () (cons #'(lambda (x y) x) #'(lambda (x y) y)))
(defun foo () (cons #'(lambda (x y) y) #'(lambda (x &rest y) y)))
(defun foo () (cons #'(lambda () (setq x nil)) #'(lambda () (setq y nil))))
;; lambda's should be shared:
(defun foo () (cons #'(lambda (x y) x) #'(lambda (y x) y)))
(defun foo () (cons #'(lambda (x) (let (z) (cons z x)))
		    #'(lambda (z) (let (x) (cons x z)))))
;; result should not be fixnum
(defun foo () (/ 3 2))
