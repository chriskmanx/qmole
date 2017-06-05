;; tinycustom.el -- a tiny custom.el for emulating purpose.

;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima  <minakaji@osaka.email.ne.jp>
;;         Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: emulating, custom

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

;; Purpose of this program is emulating for who does not have "custom".
;; (custom.el bundled with v19 is old; does not have following macros.)
;;
;; DEFCUSTOM below has the same effect as the original DEFVAR has.
;; DEFFACE below interprets almost all arguments.
;; DEFGROUP and DEFINE-WIDGET below are just nop macro.

;;; Code:

(require 'poe)

(defmacro-maybe defgroup (symbol members doc &rest args)
  "Declare SYMBOL as a customization group containing MEMBERS.
SYMBOL does not need to be quoted.
Third arg DOC is the group documentation.

This is a nop defgroup only for emulating purpose."
  nil)

(defmacro-maybe defcustom (symbol value doc &rest args)
  "Declare SYMBOL as a customizable variable that defaults to VALUE.
DOC is the variable documentation.

This is a defcustom only for emulating purpose.
Its effect is just as same as that of defvar."
  (` (defvar (, symbol) (, value) (, doc))))

(defvar-maybe frame-background-mode nil
  "*The brightness of the background.
Set this to the symbol dark if your background color is dark, light if
your background is light, or nil (default) if you want Emacs to
examine the brightness for you.  However, the old Emacsen might not
examine the brightness, so you should set this value definitely.")

(defun-maybe-cond custom-declare-face (face spec doc &rest args)
  "Like `defface', but FACE is evaluated as a normal argument.
Note that this function does not have the full specification; DOC or
ARGS are ignored and some keywords are ignored in SPEC except for
`:foreground', `:background', `:bold', `:italic' and `:underline'.
It does nothing if FACE has been defined."
  ((fboundp 'make-face)
   (or (find-face face)
       (let ((colorp (and window-system (x-display-color-p)))
	     display atts req item match done)
	 (make-face face)
	 (while (and spec (not done))
	   (setq display (car (car spec))
		 atts (car (cdr (car spec)))
		 spec (cdr spec))
	   (cond ((consp display)
		  (setq match t)
		  (while (and display match)
		    (setq req (car (car display))
			  item (car (cdr (car display)))
			  display (cdr display))
		    (cond ((eq 'type req)
			   (setq match (or (eq window-system item)
					   (and (not window-system)
						(eq 'tty item)))))
			  ((eq 'class req)
			   (setq match (or (and colorp
						(eq 'color item))
					   (and (not colorp)
						(memq item
						      '(grayscale mono))))))
			  ((eq 'background req)
			   (setq match (eq (or frame-background-mode 'light)
					   item)))))
		  (setq done match))
		 ((eq t display)
		  (setq done t))))
	 (if done
	     (let ((alist
		    '((:foreground . set-face-foreground)
		      (:background . set-face-background)
		      (:bold . set-face-bold-p)
		      (:italic . set-face-italic-p)
		      (:underline . set-face-underline-p)))
		   function)
	       (while atts
		 (if (setq function (cdr (assq (car atts) alist)))
		     (funcall function face (car (cdr atts))))
		 (setq atts (cdr (cdr atts))))))
	 face)))
  (t
   nil))

(defmacro-maybe defface (face spec doc &rest args)
  "Declare FACE as a customizable face that defaults to SPEC.
FACE does not need to be quoted.

Third argument DOC is the face documentation.

If FACE has been set with `custom-set-face', set the face attributes
as specified by that function, otherwise set the face attributes
according to SPEC.

The remaining arguments should have the form

   [KEYWORD VALUE]...

The following KEYWORDs are defined:

:group  VALUE should be a customization group.
        Add FACE to that group.

SPEC should be an alist of the form ((DISPLAY ATTS)...).

ATTS is a list of face attributes and their values.  The possible
attributes are defined in the variable `custom-face-attributes'.

The ATTS of the first entry in SPEC where the DISPLAY matches the
frame should take effect in that frame.  DISPLAY can either be the
symbol t, which will match all frames, or an alist of the form
\((REQ ITEM...)...)

For the DISPLAY to match a FRAME, the REQ property of the frame must
match one of the ITEM.  The following REQ are defined:

`type' (the value of `window-system')
  Should be one of `x' or `tty'.

`class' (the frame's color support)
  Should be one of `color', `grayscale', or `mono'.

`background' (what color is used for the background text)
  Should be one of `light' or `dark'.

Read the section about customization in the Emacs Lisp manual for more
information."
  (nconc (list 'custom-declare-face (list 'quote face) spec doc)
	 ;; Quote colon keywords.
	 (let (rest)
	   (while args
	     (setq rest (cons (list 'quote (car args)) rest)
		   args (cdr args)
		   rest (cons (car args) rest)
		   args (cdr args)))
	   (nreverse rest))))

(defmacro-maybe define-widget (name class doc &rest args)
  "Define a new widget type named NAME from CLASS.
The third argument DOC is a documentation string for the widget.

This is a nop define-widget only for emulating purpose."
  nil)

(provide 'custom)

(require 'product)
(product-provide (provide 'tinycustom) (require 'apel-ver))

;;; tinycustom.el ends here
