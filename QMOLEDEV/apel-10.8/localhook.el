;;; localhook.el --- local hook variable support in emacs-lisp.

;; Copyright (C) 1985,86,92,94,95,1999 Free Software Foundation, Inc.

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: compatibility

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file (re)defines the following functions.
;; These functions support local hook feature in emacs-lisp level.
;;
;;	add-hook, remove-hook, make-local-hook,
;;	run-hooks, run-hook-with-args,
;;	run-hook-with-args-until-success, and
;;	run-hook-with-args-until-failure.

;; The following functions which do not exist in 19.28 are used in the
;; original definitions of add-hook, remove-hook, and make-local-hook.
;;
;;	local-variable-p, and local-variable-if-set-p.
;;
;; In this file, these functions are replaced with mock versions.

;; In addition, the following functions which do not exist in v18 are used.
;;
;;	default-boundp, byte-code-function-p, functionp, member, and delete.
;;
;; These functions are provided by poe-18.el.

;; For historians:
;;
;;	`add-hook' and `remove-hook' were introduced in v19.
;;
;;	Local hook feature and `make-local-hook' were introduced in 19.29.
;;
;;	`run-hooks' exists in v17.
;;	`run-hook-with-args' was introduced in 19.23 as a lisp function.
;;	Two variants of `run-hook-with-args' were introduced in 19.29 as
;;	lisp functions.  `run-hook' family became C primitives in 19.30.

;;; Code:

;; beware of circular dependency.
(require 'product)
(product-provide (provide 'localhook) (require 'apel-ver))

(require 'poe)				; this file is loaded from poe.el.

;; These two functions are not complete, but work enough for our purpose.
;;
;; (defun local-variable-p (variable &optional buffer)
;;   "Non-nil if VARIABLE has a local binding in buffer BUFFER.
;; BUFFER defaults to the current buffer."
;;   (and (or (assq variable (buffer-local-variables buffer)) ; local and bound.
;; 	   (memq variable (buffer-local-variables buffer))); local but void.
;;        ;; docstring is ambiguous; 20.3 returns bool value.
;;        t))
;;
;; (defun local-variable-if-set-p (variable &optional buffer)
;;   "Non-nil if VARIABLE will be local in buffer BUFFER if it is set there.
;; BUFFER defaults to the current buffer."
;;   (and (or (assq variable (buffer-local-variables buffer)) ; local and bound.
;; 	   (memq variable (buffer-local-variables buffer))); local but void.
;;        ;; docstring is ambiguous; 20.3 returns bool value.
;;        t))

;;; Hook manipulation functions.

;; The following three functions are imported from emacs-20.3/lisp/subr.el.
;; (local-variable-p, and local-variable-if-set-p are expanded.)
(defun make-local-hook (hook)
  "Make the hook HOOK local to the current buffer.
The return value is HOOK.

When a hook is local, its local and global values
work in concert: running the hook actually runs all the hook
functions listed in *either* the local value *or* the global value
of the hook variable.

This function works by making `t' a member of the buffer-local value,
which acts as a flag to run the hook functions in the default value as
well.  This works for all normal hooks, but does not work for most
non-normal hooks yet.  We will be changing the callers of non-normal
hooks so that they can handle localness; this has to be done one by
one.

This function does nothing if HOOK is already local in the current
buffer.

Do not use `make-local-variable' to make a hook variable buffer-local."
  (if ;; (local-variable-p hook)
      (or (assq hook (buffer-local-variables)) ; local and bound.
	  (memq hook (buffer-local-variables))); local but void.
      nil
    (or (boundp hook) (set hook nil))
    (make-local-variable hook)
    (set hook (list t)))
  hook)

(defun add-hook (hook function &optional append local)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes no difference if the hook is not buffer-local.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(set hook (list old))))
  (if (or local
	  ;; Detect the case where make-local-variable was used on a hook
	  ;; and do what we used to do.
	  (and ;; (local-variable-if-set-p hook)
	   (or (assq hook (buffer-local-variables)) ; local and bound.
	       (memq hook (buffer-local-variables))); local but void.
	   (not (memq t (symbol-value hook)))))
      ;; Alter the local value only.
      (or (if (or (consp function) (byte-code-function-p function))
	      (member function (symbol-value hook))
	    (memq function (symbol-value hook)))
	  (set hook
	       (if append
		   (append (symbol-value hook) (list function))
		 (cons function (symbol-value hook)))))
    ;; Alter the global value (which is also the only value,
    ;; if the hook doesn't have a local value).
    (or (if (or (consp function) (byte-code-function-p function))
	    (member function (default-value hook))
	  (memq function (default-value hook)))
	(set-default hook
		     (if append
			 (append (default-value hook) (list function))
		       (cons function (default-value hook)))))))

(defun remove-hook (hook function &optional local)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'.

The optional third argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes no difference if the hook is not buffer-local.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'."
  (if (or (not (boundp hook))		;unbound symbol, or
	  (not (default-boundp hook))
	  (null (symbol-value hook))	;value is nil, or
	  (null function))		;function is nil, then
      nil				;Do nothing.
    (if (or local
	    ;; Detect the case where make-local-variable was used on a hook
	    ;; and do what we used to do.
	    (and ;; (local-variable-p hook)
	     (or (assq hook (buffer-local-variables)) ; local and bound.
		 (memq hook (buffer-local-variables))); local but void.
	     (consp (symbol-value hook))
	     (not (memq t (symbol-value hook)))))
	(let ((hook-value (symbol-value hook)))
	  (if (consp hook-value)
	      (if (member function hook-value)
		  (setq hook-value (delete function (copy-sequence hook-value))))
	    (if (equal hook-value function)
		(setq hook-value nil)))
	  (set hook hook-value))
      (let ((hook-value (default-value hook)))
	(if (and (consp hook-value) (not (functionp hook-value)))
	    (if (member function hook-value)
		(setq hook-value (delete function (copy-sequence hook-value))))
	  (if (equal hook-value function)
	      (setq hook-value nil)))
	(set-default hook hook-value)))))

;;; Hook execution functions.

(defun run-hook-with-args-internal (hook args cond)
  "Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  Its value should be a list of
functions.  We call those functions, one by one, passing arguments ARGS
to each of them, until specified COND is satisfied.  If COND is nil, we
call those functions until one of them returns a non-nil value, and then
we return that value.  If COND is t, we call those functions until one
of them returns nil, and then we return nil.  If COND is not nil and not
t, we call all the functions."
  (if (not (boundp hook))
      ;; hook is void.
      (not cond)
    (let* ((functions (symbol-value hook))
	   (ret (eq cond t))
	   (all (and cond (not ret)))
	   function)
      (if (functionp functions)
	  ;; hook is just a function.
	  (apply functions args)
	;; hook is nil or a list of functions.
	(while (and functions
		    (or all		; to-completion
			(if cond
			    ret		; until-failure
			  (null ret))))	; until-success
	  (setq function (car functions)
		functions(cdr functions))
	  (if (eq function t)
	      ;; this hook has a local binding.
	      ;; we must run the global binding too.
	      (let ((globals (default-value hook))
		    global)
		(if (functionp globals)
		    (setq ret (apply globals args))
		  (while (and globals
			      (or all
				  (if cond
				      ret
				    (null ret))))
		    (setq global (car globals)
			  globals(cdr globals))
		    (or (eq global t)	; t should not occur.
			(setq ret (apply global args))))))
	    (setq ret (apply function args))))
	ret))))

;; The following four functions are direct translation of their
;; C definitions in emacs-20.3/src/eval.c.
(defun run-hooks (&rest hooks)
  "Run each hook in HOOKS.  Major mode functions use this.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments.

To make a hook variable buffer-local, use `make-local-hook',
not `make-local-variable'."
  (while hooks
    (run-hook-with-args-internal (car hooks) nil 'to-completion)
    (setq hooks (cdr hooks))))

(defun run-hook-with-args (hook &rest args)
  "Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
value, that value may be a function or a list of functions to be
called to run the hook.  If the value is a function, it is called with
the given arguments and its return value is returned.  If it is a list
of functions, those functions are called, in order,
with the given arguments ARGS.
It is best not to depend on the value return by `run-hook-with-args',
as that may change.

To make a hook variable buffer-local, use `make-local-hook',
not `make-local-variable'."
  (run-hook-with-args-internal hook args 'to-completion))

(defun run-hook-with-args-until-success (hook &rest args)
  "Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  Its value should
be a list of functions.  We call those functions, one by one,
passing arguments ARGS to each of them, until one of them
returns a non-nil value.  Then we return that value.
If all the functions return nil, we return nil.

To make a hook variable buffer-local, use `make-local-hook',
not `make-local-variable'."
  (run-hook-with-args-internal hook args nil))

(defun run-hook-with-args-until-failure (hook &rest args)
  "Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  Its value should
be a list of functions.  We call those functions, one by one,
passing arguments ARGS to each of them, until one of them
returns nil.  Then we return nil.
If all the functions return non-nil, we return non-nil.

To make a hook variable buffer-local, use `make-local-hook',
not `make-local-variable'."
  (run-hook-with-args-internal hook args t))

;;; localhook.el ends here
