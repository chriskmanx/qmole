;;; atype.el --- atype functions

;; Copyright (C) 1994,1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: atype.el,v 6.6 1997/03/10 14:11:23 morioka Exp $
;; Keywords: atype

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

(require 'emu)				; for backward compatibility.
(require 'poe)				; delete.
(require 'alist)


;;; @ field unifier
;;;

(defun field-unifier-for-default (a b)
  (let ((ret
	 (cond ((equal a b)    a)
	       ((null (cdr b)) a)
	       ((null (cdr a)) b)
	       )))
    (if ret
	(list nil ret nil)
      )))

(defun field-unify (a b)
  (let ((f
	 (let ((type (car a)))
	   (and (symbolp type)
		(intern (concat "field-unifier-for-" (symbol-name type)))
		))))
    (or (fboundp f)
	(setq f (function field-unifier-for-default))
	)
    (funcall f a b)
    ))


;;; @ type unifier
;;;

(defun assoc-unify (class instance)
  (catch 'tag
    (let ((cla (copy-alist class))
	  (ins (copy-alist instance))
	  (r class)
	  cell aret ret prev rest)
      (while r
	(setq cell (car r))
	(setq aret (assoc (car cell) ins))
	(if aret
	    (if (setq ret (field-unify cell aret))
		(progn
		  (if (car ret)
		      (setq prev (put-alist (car (car ret))
					    (cdr (car ret))
					    prev))
		    )
		  (if (nth 2 ret)
		      (setq rest (put-alist (car (nth 2 ret))
					    (cdr (nth 2 ret))
					    rest))
		    )
		  (setq cla (put-alist (car cell)(cdr (nth 1 ret)) cla))
		  (setq ins (del-alist (car cell) ins))
		  )
	      (throw 'tag nil)
	      ))
	(setq r (cdr r))
	)
      (setq r (copy-alist ins))
      (while r
	(setq cell (car r))
	(setq aret (assoc (car cell) cla))
	(if aret
	    (if (setq ret (field-unify cell aret))
		(progn
		  (if (car ret)
		      (setq prev (put-alist (car (car ret))
					    (cdr (car ret))
					    prev))
		    )
		  (if (nth 2 ret)
		      (setq rest (put-alist (car (nth 2 ret))
					    (cdr (nth 2 ret))
					    rest))
		    )
		  (setq cla (del-alist (car cell) cla))
		  (setq ins (put-alist (car cell)(cdr (nth 1 ret)) ins))
		  )
	      (throw 'tag nil)
	      ))
	(setq r (cdr r))
	)
      (list prev (append cla ins) rest)
      )))

(defun get-unified-alist (db al)
  (let ((r db) ret)
    (catch 'tag
      (while r
	(if (setq ret (nth 1 (assoc-unify (car r) al)))
	    (throw 'tag ret)
	  )
	(setq r (cdr r))
	))))


;;; @ utilities
;;;

(defun delete-atype (atl al)
  (let* ((r atl) ret oal)
    (setq oal
	  (catch 'tag
	    (while r
	      (if (setq ret (nth 1 (assoc-unify (car r) al)))
		  (throw 'tag (car r))
		)
	      (setq r (cdr r))
	      )))
    (delete oal atl)
    ))

(defun remove-atype (sym al)
  (and (boundp sym)
       (set sym (delete-atype (eval sym) al))
       ))

(defun replace-atype (atl old-al new-al)
  (let* ((r atl) ret oal)
    (if (catch 'tag
	  (while r
	    (if (setq ret (nth 1 (assoc-unify (car r) old-al)))
		(throw 'tag (rplaca r new-al))
	      )
	    (setq r (cdr r))
	    ))
	atl)))

(defun set-atype (sym al &rest options)
  (if (null (boundp sym))
      (set sym al)
    (let* ((replacement (memq 'replacement options))
	   (ignore-fields (car (cdr (memq 'ignore options))))
	   (remove (or (car (cdr (memq 'remove options)))
		       (let ((ral (copy-alist al)))
			 (mapcar (function
				  (lambda (type)
				    (setq ral (del-alist type ral))
				    ))
				 ignore-fields)
			 ral)))
	   )
      (set sym
	   (or (if replacement
		   (replace-atype (eval sym) remove al)
		 )
	       (cons al
		     (delete-atype (eval sym) remove)
		     )
	       )))))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'atype) (require 'apel-ver))

;;; atype.el ends here
