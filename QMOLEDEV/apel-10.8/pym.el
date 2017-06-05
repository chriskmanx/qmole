;;; pym.el --- Macros for Your Poe.

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;	Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: byte-compile, evaluation, edebug, internal

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

;;; Commentary:

;; This module provides `def*-maybe' macros for conditional definition.
;;
;; Many APEL modules use these macros to provide the emulating version
;; of the Emacs builtins (both C primitives and lisp subroutines) for
;; backward compatibility.  While compilation time, if `def*-maybe'
;; find that functions/variables being defined is already provided by
;; Emacs used for compilation, it does not leave the definitions in
;; compiled code and resulting .elc files will be highly specialized
;; for your environment.  Lisp programmers should be aware that these
;; macros will never provide functions or variables at run-time if they
;; are defined for some reason (or by accident) at compilation time.

;; For `find-function' lovers, the following definitions may work with
;; `def*-maybe'.
;;
;; (setq find-function-regexp
;;       "^\\s-*(def[^cgvW]\\(\\w\\|-\\)+\\*?\\s-+'?%s\\(\\s-\\|$\\)")
;; (setq find-variable-regexp
;;       "^\\s-*(def[^umaW]\\(\\w\\|-\\)+\\*?\\s-+%s\\(\\s-\\|$\\)")
;;
;; I'm too lazy to write better regexps, sorry. -- shuhei

;;; Code:

;; for `load-history'.
(or (boundp 'current-load-list) (setq current-load-list nil))

(require 'static)


;;; Conditional define.

(put 'defun-maybe 'lisp-indent-function 'defun)
(defmacro defun-maybe (name &rest everything-else)
  "Define NAME as a function if NAME is not defined.
See also the function `defun'."
  (or (and (fboundp name)
	   (not (get name 'defun-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (defun (, name) (,@ everything-else))
	       ;; This `defun' will be compiled to `fset',
	       ;; which does not update `load-history'.
	       ;; We must update `current-load-list' explicitly.
	       (setq current-load-list
		     (cons (quote (, name)) current-load-list))
	       (put (quote (, name)) 'defun-maybe t))))))

(put 'defmacro-maybe 'lisp-indent-function 'defun)
(defmacro defmacro-maybe (name &rest everything-else)
  "Define NAME as a macro if NAME is not defined.
See also the function `defmacro'."
  (or (and (fboundp name)
	   (not (get name 'defmacro-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (defmacro (, name) (,@ everything-else))
	       ;; This `defmacro' will be compiled to `fset',
	       ;; which does not update `load-history'.
	       ;; We must update `current-load-list' explicitly.
	       (setq current-load-list
		     (cons (quote (, name)) current-load-list))
	       (put (quote (, name)) 'defmacro-maybe t))))))

(put 'defsubst-maybe 'lisp-indent-function 'defun)
(defmacro defsubst-maybe (name &rest everything-else)
  "Define NAME as an inline function if NAME is not defined.
See also the macro `defsubst'."
  (or (and (fboundp name)
	   (not (get name 'defsubst-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (defsubst (, name) (,@ everything-else))
	       ;; This `defsubst' will be compiled to `fset',
	       ;; which does not update `load-history'.
	       ;; We must update `current-load-list' explicitly.
	       (setq current-load-list
		     (cons (quote (, name)) current-load-list))
	       (put (quote (, name)) 'defsubst-maybe t))))))

(defmacro defalias-maybe (symbol definition)
  "Define SYMBOL as an alias for DEFINITION if SYMBOL is not defined.
See also the function `defalias'."
  (setq symbol (eval symbol))
  (or (and (fboundp symbol)
	   (not (get symbol 'defalias-maybe)))
      (` (or (fboundp (quote (, symbol)))
	     (prog1
		 (defalias (quote (, symbol)) (, definition))
	       ;; `defalias' updates `load-history' internally.
	       (put (quote (, symbol)) 'defalias-maybe t))))))

(defmacro defvar-maybe (name &rest everything-else)
  "Define NAME as a variable if NAME is not defined.
See also the function `defvar'."
  (or (and (boundp name)
	   (not (get name 'defvar-maybe)))
      (` (or (boundp (quote (, name)))
	     (prog1
		 (defvar (, name) (,@ everything-else))
	       ;; byte-compiler will generate code to update
	       ;; `load-history'.
	       (put (quote (, name)) 'defvar-maybe t))))))

(defmacro defconst-maybe (name &rest everything-else)
  "Define NAME as a constant variable if NAME is not defined.
See also the function `defconst'."
  (or (and (boundp name)
	   (not (get name 'defconst-maybe)))
      (` (or (boundp (quote (, name)))
	     (prog1
		 (defconst (, name) (,@ everything-else))
	       ;; byte-compiler will generate code to update
	       ;; `load-history'.
	       (put (quote (, name)) 'defconst-maybe t))))))

(defmacro defun-maybe-cond (name args &optional doc &rest clauses)
  "Define NAME as a function if NAME is not defined.
CLAUSES are like those of `cond' expression, but each condition is evaluated
at compile-time and, if the value is non-nil, the body of the clause is used
for function definition of NAME.
See also the function `defun'."
  (or (stringp doc)
      (setq clauses (cons doc clauses)
	    doc nil))
  (or (and (fboundp name)
	   (not (get name 'defun-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (static-cond
		  (,@ (mapcar
		       (function
			(lambda (case)
			  (list (car case)
				(if doc
				    (` (defun (, name) (, args)
					 (, doc)
					 (,@ (cdr case))))
				  (` (defun (, name) (, args)
				       (,@ (cdr case))))))))
		       clauses)))
	       ;; This `defun' will be compiled to `fset',
	       ;; which does not update `load-history'.
	       ;; We must update `current-load-list' explicitly.
	       (setq current-load-list
		     (cons (quote (, name)) current-load-list))
	       (put (quote (, name)) 'defun-maybe t))))))

(defmacro defmacro-maybe-cond (name args &optional doc &rest clauses)
  "Define NAME as a macro if NAME is not defined.
CLAUSES are like those of `cond' expression, but each condition is evaluated
at compile-time and, if the value is non-nil, the body of the clause is used
for macro definition of NAME.
See also the function `defmacro'."
  (or (stringp doc)
      (setq clauses (cons doc clauses)
	    doc nil))
  (or (and (fboundp name)
	   (not (get name 'defmacro-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (static-cond
		  (,@ (mapcar
		       (function
			(lambda (case)
			  (list (car case)
				(if doc
				    (` (defmacro (, name) (, args)
					 (, doc)
					 (,@ (cdr case))))
				  (` (defmacro (, name) (, args)
				       (,@ (cdr case))))))))
		       clauses)))
	       ;; This `defmacro' will be compiled to `fset',
	       ;; which does not update `load-history'.
	       ;; We must update `current-load-list' explicitly.
	       (setq current-load-list
		     (cons (quote (, name)) current-load-list))
	       (put (quote (, name)) 'defmacro-maybe t))))))

(defmacro defsubst-maybe-cond (name args &optional doc &rest clauses)
  "Define NAME as an inline function if NAME is not defined.
CLAUSES are like those of `cond' expression, but each condition is evaluated
at compile-time and, if the value is non-nil, the body of the clause is used
for function definition of NAME.
See also the macro `defsubst'."
  (or (stringp doc)
      (setq clauses (cons doc clauses)
	    doc nil))
  (or (and (fboundp name)
	   (not (get name 'defsubst-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (static-cond
		  (,@ (mapcar
		       (function
			(lambda (case)
			  (list (car case)
				(if doc
				    (` (defsubst (, name) (, args)
					 (, doc)
					 (,@ (cdr case))))
				  (` (defsubst (, name) (, args)
				       (,@ (cdr case))))))))
		       clauses)))
	       ;; This `defsubst' will be compiled to `fset',
	       ;; which does not update `load-history'.
	       ;; We must update `current-load-list' explicitly.
	       (setq current-load-list
		     (cons (quote (, name)) current-load-list))
	       (put (quote (, name)) 'defsubst-maybe t))))))


;;; Edebug spec.

;; `def-edebug-spec' is an autoloaded macro in v19 and later.
;; (Note that recent XEmacs provides "edebug" as a separate package.)
(defmacro-maybe def-edebug-spec (symbol spec)
  "Set the edebug-form-spec property of SYMBOL according to SPEC.
Both SYMBOL and SPEC are unevaluated. The SPEC can be 0, t, a symbol
\(naming a function\), or a list."
  (` (put (quote (, symbol)) 'edebug-form-spec (quote (, spec)))))

;; edebug-spec for `def*-maybe' macros.
(def-edebug-spec defun-maybe defun)
(def-edebug-spec defmacro-maybe defmacro)
(def-edebug-spec defsubst-maybe defun)
(def-edebug-spec defun-maybe-cond
  (&define name lambda-list
	   [&optional stringp]
	   [&rest ([&not eval] [&rest sexp])]
	   [&optional (eval [&optional ("interactive" interactive)] def-body)]
	   &rest (&rest sexp)))
(def-edebug-spec defmacro-maybe-cond
  (&define name lambda-list
	   [&rest ([&not eval] [&rest sexp])]
	   [&optional (eval def-body)]
	   &rest (&rest sexp)))
(def-edebug-spec defsubst-maybe-cond
  (&define name lambda-list
	   [&optional stringp]
	   [&rest ([&not eval] [&rest sexp])]
	   [&optional (eval [&optional ("interactive" interactive)] def-body)]
	   &rest (&rest sexp)))

;; edebug-spec for `static-*' macros are also defined here.
(def-edebug-spec static-if t) 
(def-edebug-spec static-when when)
(def-edebug-spec static-unless unless)
(def-edebug-spec static-condition-case condition-case)
(def-edebug-spec static-defconst defconst)
(def-edebug-spec static-cond cond)


;;; for backward compatibility.

(defun subr-fboundp (symbol)
  "Return t if SYMBOL's function definition is a built-in function."
  (and (fboundp symbol)
       (subrp (symbol-function symbol))))
;; (make-obsolete 'subr-fboundp "don't use it.")


;;; End.

(require 'product)
(product-provide (provide 'pym) (require 'apel-ver))

;;; pym.el ends here
