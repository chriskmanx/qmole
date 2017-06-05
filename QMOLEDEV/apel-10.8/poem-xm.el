;;; poem-xm.el --- poem module for XEmacs-mule; -*-byte-compile-dynamic: t;-*-

;; Copyright (C) 1998,1999,2002,2003,2005 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
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

;;; Code:

(eval-when-compile
  (require 'poe))


;;; @ buffer representation
;;;

(defsubst-maybe set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
The buffer contents remain unchanged as a sequence of bytes
but the contents viewed as characters do change.
\[Emacs 20.3 emulating function]"
  flag)


;;; @ character
;;;

;; avoid bug of XEmacs
(or (integerp (car (cdr (split-char ?a))))
    (defun split-char (char)
      "Return list of charset and one or two position-codes of CHAR."
      (let ((charset (char-charset char)))
	(if (eq charset 'ascii)
	    (list charset (char-int char))
	  (let ((i 0)
		(len (charset-dimension charset))
		(code (if (integerp char)
			  char
			(char-int char)))
		dest)
	    (while (< i len)
	      (setq dest (cons (logand code 127) dest)
		    code (lsh code -7)
		    i (1+ i)))
	    (cons charset dest)))))
    )

(defmacro char-next-index (char index)
  "Return index of character succeeding CHAR whose index is INDEX."
  `(1+ ,index))

(if (not (fboundp 'char-length))
    (defalias 'char-length
      (lambda (char)
	"Return number of bytes a CHARACTER occupies in a string or buffer.
It always returns 1 in XEmacs.  It is for compatibility with MULE 2.3."
	1)))

(defalias-maybe 'char-valid-p 'characterp)


;;; @ string
;;;

(defun-maybe string-to-int-list (str)
  (mapcar #'char-int str))

(defun-maybe string-to-char-list (str)
  (mapcar #'identity str))

(defalias 'looking-at-as-unibyte 'looking-at)


;;; @ end
;;;

(require 'product)
(product-provide (provide 'poem-xm) (require 'apel-ver))

;;; poem-xm.el ends here
