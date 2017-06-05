;;; calist.el --- Condition functions

;; Copyright (C) 1998 Free Software Foundation, Inc.
;; Copyright (C) 1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: condition, alist, tree

;; This file is part of APEL (A Portable Emacs Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(eval-when-compile (require 'cl))

(require 'alist)

(defvar calist-package-alist nil)
(defvar calist-field-match-method-obarray nil)

(defun find-calist-package (name)
  "Return a calist-package by NAME."
  (cdr (assq name calist-package-alist)))

(defun define-calist-field-match-method (field-type function)
  "Set field-match-method for FIELD-TYPE to FUNCTION."
  (fset (intern (symbol-name field-type) calist-field-match-method-obarray)
	function))

(defun use-calist-package (name)
  "Make the symbols of package NAME accessible in the current package."
  (mapatoms (lambda (sym)
	      (if (intern-soft (symbol-name sym)
			       calist-field-match-method-obarray)
		  (signal 'conflict-of-calist-symbol
			  (list (format "Conflict of symbol %s" sym)))
		(if (fboundp sym)
		    (define-calist-field-match-method
		      sym (symbol-function sym))
		  )))
	    (find-calist-package name)))

(defun make-calist-package (name &optional use)
  "Create a new calist-package."
  (let ((calist-field-match-method-obarray (make-vector 7 0)))
    (set-alist 'calist-package-alist name
	       calist-field-match-method-obarray)
    (use-calist-package (or use 'standard))
    calist-field-match-method-obarray))

(defun in-calist-package (name)
  "Set the current calist-package to a new or existing calist-package."
  (setq calist-field-match-method-obarray
	(or (find-calist-package name)
	    (make-calist-package name))))

(in-calist-package 'standard)

(defun calist-default-field-match-method (calist field-type field-value)
  (let ((s-field (assoc field-type calist)))
    (cond ((null s-field)
	   (cons (cons field-type field-value) calist)
	   )
	  ((eq field-value t)
	   calist)
	  ((equal (cdr s-field) field-value)
	   calist))))

(define-calist-field-match-method t (function calist-default-field-match-method))

(defsubst calist-field-match-method (field-type)
  (symbol-function
   (or (intern-soft (if (symbolp field-type)
			(symbol-name field-type)
		      field-type)
		    calist-field-match-method-obarray)
       (intern-soft "t" calist-field-match-method-obarray))))

(defsubst calist-field-match (calist field-type field-value)
  (funcall (calist-field-match-method field-type)
	   calist field-type field-value))

(defun ctree-match-calist (rule-tree alist)
  "Return matched condition-alist if ALIST matches RULE-TREE."
  (if (null rule-tree)
      alist
    (let ((type (car rule-tree))
	  (choices (cdr rule-tree))
	  default)
      (catch 'tag
	(while choices
	  (let* ((choice (car choices))
		 (choice-value (car choice)))
	    (if (eq choice-value t)
		(setq default choice)
	      (let ((ret-alist (calist-field-match alist type (car choice))))
		(if ret-alist
		    (throw 'tag
			   (if (cdr choice)
			       (ctree-match-calist (cdr choice) ret-alist)
			     ret-alist))
		  ))))
	  (setq choices (cdr choices)))
	(if default
	    (let ((ret-alist (calist-field-match alist type t)))
	      (if ret-alist
		  (if (cdr default)
		      (ctree-match-calist (cdr default) ret-alist)
		    ret-alist))))
	))))

(defun ctree-match-calist-partially (rule-tree alist)
  "Return matched condition-alist if ALIST matches RULE-TREE."
  (if (null rule-tree)
      alist
    (let ((type (car rule-tree))
	  (choices (cdr rule-tree))
	  default)
      (catch 'tag
	(while choices
	  (let* ((choice (car choices))
		 (choice-value (car choice)))
	    (if (eq choice-value t)
		(setq default choice)
	      (let ((ret-alist (calist-field-match alist type (car choice))))
		(if ret-alist
		    (throw 'tag
			   (if (cdr choice)
			       (ctree-match-calist-partially
				(cdr choice) ret-alist)
			     ret-alist))
		  ))))
	  (setq choices (cdr choices)))
	(if default
	    (let ((ret-alist (calist-field-match alist type t)))
	      (if ret-alist
		  (if (cdr default)
		      (ctree-match-calist-partially (cdr default) ret-alist)
		    ret-alist)))
	  (calist-field-match alist type t))
	))))

(defun ctree-find-calist (rule-tree alist &optional all)
  "Return list of condition-alist which matches ALIST in RULE-TREE.
If optional argument ALL is specified, default rules are not ignored
even if other rules are matched for ALIST."
  (if (null rule-tree)
      (list alist)
    (let ((type (car rule-tree))
	  (choices (cdr rule-tree))
	  default dest)
      (while choices
	(let* ((choice (car choices))
	       (choice-value (car choice)))
	  (if (eq choice-value t)
	      (setq default choice)
	    (let ((ret-alist (calist-field-match alist type (car choice))))
	      (if ret-alist
		  (if (cdr choice)
		      (let ((ret (ctree-find-calist
				  (cdr choice) ret-alist all)))
			(while ret
			  (let ((elt (car ret)))
			    (or (member elt dest)
				(setq dest (cons elt dest))
				))
			  (setq ret (cdr ret))
			  ))
		    (or (member ret-alist dest)
			(setq dest (cons ret-alist dest)))
		    )))))
	(setq choices (cdr choices)))
      (or (and (not all) dest)
	  (if default
	      (let ((ret-alist (calist-field-match alist type t)))
		(if ret-alist
		    (if (cdr default)
			(let ((ret (ctree-find-calist
				    (cdr default) ret-alist all)))
			  (while ret
			    (let ((elt (car ret)))
			      (or (member elt dest)
				  (setq dest (cons elt dest))
				  ))
			    (setq ret (cdr ret))
			    ))
		      (or (member ret-alist dest)
			  (setq dest (cons ret-alist dest)))
		      ))))
	  )
      dest)))

(defun calist-to-ctree (calist)
  "Convert condition-alist CALIST to condition-tree."
  (if calist
      (let* ((cell (car calist)))
	(cons (car cell)
	      (list (cons (cdr cell)
			  (calist-to-ctree (cdr calist))
			  ))))))

(defun ctree-add-calist-strictly (ctree calist)
  "Add condition CALIST to condition-tree CTREE without default clause."
  (cond ((null calist) ctree)
	((null ctree)
	 (calist-to-ctree calist)
	 )
	(t
	 (let* ((type (car ctree))
		(values (cdr ctree))
		(ret (assoc type calist)))
	   (if ret
	       (catch 'tag
		 (while values
		   (let ((cell (car values)))
		     (if (equal (car cell)(cdr ret))
			 (throw 'tag
				(setcdr cell
					(ctree-add-calist-strictly
					 (cdr cell)
					 (delete ret (copy-alist calist)))
					))))
		   (setq values (cdr values)))
		 (setcdr ctree (cons (cons (cdr ret)
					   (calist-to-ctree
					    (delete ret (copy-alist calist))))
				     (cdr ctree)))
		 )
	     (catch 'tag
	       (while values
		 (let ((cell (car values)))
		   (setcdr cell
			   (ctree-add-calist-strictly (cdr cell) calist))
		   )
		 (setq values (cdr values))))
	     )
	   ctree))))

(defun ctree-add-calist-with-default (ctree calist)
  "Add condition CALIST to condition-tree CTREE with default clause."
  (cond ((null calist) ctree)
	((null ctree)
	 (let* ((cell (car calist))
		(type (car cell))
		(value (cdr cell)))
	   (cons type
		 (list (list t)
		       (cons value (calist-to-ctree (cdr calist)))))
	   ))
	(t
	 (let* ((type (car ctree))
		(values (cdr ctree))
		(ret (assoc type calist)))
	   (if ret
	       (catch 'tag
		 (while values
		   (let ((cell (car values)))
		     (if (equal (car cell)(cdr ret))
			 (throw 'tag
				(setcdr cell
					(ctree-add-calist-with-default
					 (cdr cell)
					 (delete ret (copy-alist calist)))
					))))
		   (setq values (cdr values)))
		 (if (assq t (cdr ctree))
		     (setcdr ctree
			     (cons (cons (cdr ret)
					 (calist-to-ctree
					  (delete ret (copy-alist calist))))
				   (cdr ctree)))
		   (setcdr ctree
			   (list* (list t)
				  (cons (cdr ret)
					(calist-to-ctree
					 (delete ret (copy-alist calist))))
				  (cdr ctree)))
		   ))
	     (catch 'tag
	       (while values
		 (let ((cell (car values)))
		   (setcdr cell
			   (ctree-add-calist-with-default (cdr cell) calist))
		   )
		 (setq values (cdr values)))
	       (let ((cell (assq t (cdr ctree))))
		 (if cell
		     (setcdr cell
			     (ctree-add-calist-with-default (cdr cell)
							    calist))
		   (let ((elt (cons t (calist-to-ctree calist))))
		     (or (member elt (cdr ctree))
			 (setcdr ctree (cons elt (cdr ctree)))
			 ))
		   )))
	     )
	   ctree))))

(defun ctree-set-calist-strictly (ctree-var calist)
  "Set condition CALIST in CTREE-VAR without default clause."
  (set ctree-var
       (ctree-add-calist-strictly (symbol-value ctree-var) calist)))

(defun ctree-set-calist-with-default (ctree-var calist)
  "Set condition CALIST to CTREE-VAR with default clause."
  (set ctree-var
       (ctree-add-calist-with-default (symbol-value ctree-var) calist)))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'calist) (require 'apel-ver))

;;; calist.el ends here
