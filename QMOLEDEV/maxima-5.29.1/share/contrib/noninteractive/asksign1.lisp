(defvar $e% nil)

(defmvar $no_questions t)

;; ADAPTED FROM $CATCH IN SRC/SUPRV1.LISP
;; CLEARSIGN CALL COPIED FROM MEVAL* IN SRC/SUPRV1.LISP
(defun meval* ($e%)
  (declare (special $e%))
  (let
    ((mcatch (cons bindlist loclist)))
    (prog1
      (catch 'mcatch (mfuncall '|$meval1|))
      ;; Clear the facts from asksign and friends.
      (clearsign)
      (errlfun1 mcatch))))

;; ADAPTED FROM MEVALN IN SRC/MLISP.LISP
;; CALL MEVAL* INSTEAD OF MEVAL
(defmfun meval*n (l) ;; called in a few places externally.
  (do ((body l (cdr body))
       ($%% '$%%))
      ((null (cdr body)) (meval* (car body)))
    (setq $%% (meval (car body)))))

;; ADAPTED FROM $ERRCATCH IN SRC/SUPRV1.LISP
;; CALL MEVAL*N INSTEAD OF MEVALN
;; ALSO SPECIAL CASE FOR MERROR
(defmspec $errcatch (form)
  (let ((errcatch (cons bindlist loclist)) ret)
    (if (null (setq ret (let (*mdebug*)
			  (errset (meval*n (cdr form)) lisperrprint))))
	(errlfun1 errcatch))
    (if (and (consp (car ret)) (eq (caar (car ret)) 'merror))
      (progn
        (apply 'mtell (cdr (car ret)))
        (fresh-line)
        '((mlist)))
      (cons '(mlist) ret))))

(defun asksign1 ($askexp)
  (let ($radexpand) (sign1 $askexp))
  (cond ((member sign '($pos $neg $zero) :test #'eq) sign)
	((null odds)
	 (setq $askexp (lmul evens)
	       sign (cdr (assol $askexp locals)))
	 (do ()
	     (nil)
	   (cond ((member sign '($zero |$Z| |$z| 0 0.0) :test #'equal)
		  (tdzero $askexp) (setq sign '$zero) (return t))
		 ((member sign '($pn $nonzero |$N| |$n| $nz $nonz $non0) :test #'eq)
		  (tdpn $askexp) (setq sign '$pos) (return t))
		 ((member sign '($pos |$P| |$p| $positive) :test #'eq)
		  (tdpos $askexp) (setq sign '$pos) (return t))
		 ((member sign '($neg |$N| |$n| $negative) :test #'eq)
		  (tdneg $askexp) (setq sign '$pos) (return t)))
	   (if $no_questions (meval `(($throw) '(($asksign) ,$askexp $znz))) (setq sign (ask "Is  " $askexp "  zero or nonzero?"))))

	 (if minus (flip sign) sign))
	(t (if minus (setq sign (flip sign)))
	   (setq $askexp (lmul (nconc odds (mapcar #'(lambda (l) (pow l 2)) evens))))
	   (do ((dom (cond ((eq '$pz sign) "  positive or zero?")
			   ((eq '$nz sign) "  negative or zero?")
			   ((eq '$pn sign) "  positive or negative?")
			   (t "  positive, negative, or zero?")))
		(ans (cdr (assol $askexp locals))))
	       (nil)
	     (cond ((and (member ans '($pos |$P| |$p| $positive) :test #'eq)
			 (member sign '($pz $pn $pnz) :test #'eq))
		    (tdpos $askexp) (setq sign '$pos) (return t))
		   ((and (member ans '($neg |$N| |$n| $negative) :test #'eq)
			 (member sign '($nz $pn $pnz) :test #'eq))
		    (tdneg $askexp) (setq sign '$neg) (return t))
		   ((and (member ans '($zero |$Z| |$z| 0 0.0) :test #'equal)
			 (member sign '($pz $nz $pnz) :test #'eq))
		    (tdzero $askexp) (setq sign '$zero) (return t)))
	     (if $no_questions (meval `(($throw) '(($asksign) ,$askexp ,sign))) (setq ans (ask "Is  " $askexp dom))))
	   (if minus (flip sign) sign))))

;;; Asks the user a question about the property of an object.
;;; Returns only $yes, $no or $unknown.
(defun ask-prop (object property fun-or-number)
  (if $no_questions (meval `(($throw) '(($askprop) ,object ,property))))

  (if fun-or-number (setq fun-or-number (list '| | fun-or-number)))
  (do ((end-flag) (answer))
      (end-flag (cond ((member answer '($yes |$Y| |$y|) :test #'eq) '$yes)
		      ((member answer '($no |$N| |$n|) :test #'eq) '$no)
		      ((member answer '($unknown $uk) :test #'eq) '$unknown)))
    (setq answer (retrieve
		  `((mtext) "Is " ,object 
		    ,(if (member (char (symbol-name property) 0)
				 '(#\a #\e #\i #\o #\u) :test #'char-equal)
			 " an "
			 " a ")
		    ,property ,@fun-or-number "?")
		  nil))
    (cond ((member answer '($yes |$Y| |$y| |$N| |$n| $no $unknown $uk) :test #'eq)
	   (setq end-flag t))
	  (t (mtell "~%Acceptable answers are Yes, Y, No, N, Unknown, Uk~%")))))
