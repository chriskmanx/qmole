;;; mcs-xmu.el --- Functions to unify ISO646 characters for XEmacs-mule

;; Copyright (C) 1997,1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, Mule

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

;;    This module will be loaded from mcs-xm automatically.
;;    There is no guarantee that it will work alone.

;;; Code:

(defcustom mime-iso646-character-unification-alist
  (eval-when-compile
    (let (dest
	  (i 33))
      (while (< i 92)
	(setq dest
	      (cons (cons (char-to-string (make-char 'latin-jisx0201 i))
			  (format "%c" i))
		    dest))
	(setq i (1+ i)))
      (setq i 93)
      (while (< i 126)
	(setq dest
	      (cons (cons (char-to-string (make-char 'latin-jisx0201 i))
			  (format "%c" i))
		    dest))
	(setq i (1+ i)))
      (nreverse dest)))
  "Alist unified string vs. canonical string."
  :group 'i18n
  :type '(repeat (cons string string)))

(defcustom mime-unified-character-face nil
  "Face of unified character."
  :group 'i18n
  :type 'face)

(defcustom mime-character-unification-limit-size 2048
  "Limit size to unify characters.  It is referred by the function
`decode-mime-charset-region-with-iso646-unification'.  If the length of
the specified region (start end) is larger than its value, the function
works for only decoding MIME-CHARSET.  If it is nil, size is unlimited."
  :group 'i18n
  :type '(radio (integer :tag "Max size")
		(const :tag "Unlimited" nil)))

(defun decode-mime-charset-region-with-iso646-unification (start end charset
								 lbt)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (if (prog1
	      (or (null mime-character-unification-limit-size)
		  (<= (- end start) mime-character-unification-limit-size))
	    (decode-mime-charset-region-default start end charset lbt))
	  (let ((rest mime-iso646-character-unification-alist))
	    (while rest
	      (let ((pair (car rest))
		    case-fold-search)
		(goto-char (point-min))
		(while (search-forward (car pair) nil t)
		  (let ((str (cdr pair)))
		    (if mime-unified-character-face
			(put-text-property
			 0 (length str)
			 'face mime-unified-character-face str))
		    (replace-match str 'fixed-case 'literal)
		    )
		  ))
	      (setq rest (cdr rest)))))
      )))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'mcs-xmu) (require 'apel-ver))

;;; mcs-xmu.el ends here
