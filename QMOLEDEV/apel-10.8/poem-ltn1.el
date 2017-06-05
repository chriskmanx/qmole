;;; poem-ltn1.el --- poem implementation for Emacs 19 and XEmacs without MULE

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

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

;;; @ buffer representation
;;;

(eval-when-compile
  (require 'poe))

(defun-maybe set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
The buffer contents remain unchanged as a sequence of bytes
but the contents viewed as characters do change.
\[Emacs 20.3 emulating macro]"
  )


;;; @ character set
;;;

(put 'ascii 'charset-description "Character set of ASCII")
(put 'ascii 'charset-registry "ASCII")

(put 'latin-iso8859-1 'charset-description "Character set of ISO-8859-1")
(put 'latin-iso8859-1 'charset-registry "ISO8859-1")

(defun charset-description (charset)
  "Return description of CHARSET."
  (get charset 'charset-description))

(defun charset-registry (charset)
  "Return registry name of CHARSET."
  (get charset 'charset-registry))

(defun charset-width (charset)
  "Return number of columns a CHARSET occupies when displayed."
  1)

(defun charset-direction (charset)
  "Return the direction of a character of CHARSET by
  0 (left-to-right) or 1 (right-to-left)."
  0)

(defun find-charset-string (str)
  "Return a list of charsets in the string."
  (if (string-match "[\200-\377]" str)
      '(latin-iso8859-1)
    ))

(defalias 'find-non-ascii-charset-string 'find-charset-string)

(defun find-charset-region (start end)
  "Return a list of charsets in the region between START and END."
  (if (save-excursion
	(goto-char start)
	(re-search-forward "[\200-\377]" end t))
      '(latin-iso8859-1)
    ))

(defalias 'find-non-ascii-charset-region 'find-charset-region)


;;; @ character
;;;

(defun char-charset (char)
  "Return the character set of char CHAR."
  (if (< char 128)
      'ascii
    'latin-iso8859-1))

(defun char-bytes (char)
  "Return number of bytes a character in CHAR occupies in a buffer."
  1)

(defun char-width (char)
  "Return number of columns a CHAR occupies when displayed."
  1)

(defun split-char (character)
  "Return list of charset and one or two position-codes of CHARACTER."
  (cons (char-charset character) character))

(defalias 'char-length 'char-bytes)

(defmacro char-next-index (char index)
  "Return index of character succeeding CHAR whose index is INDEX."
  (` (1+ (, index))))


;;; @ string
;;;

(defalias 'string-width 'length)

(defun string-to-char-list (str)
  (mapcar (function identity) str))

(defalias 'string-to-int-list 'string-to-char-list)

(defalias 'sref 'aref)

(defun truncate-string (str width &optional start-column)
  "Truncate STR to fit in WIDTH columns.
Optional non-nil arg START-COLUMN specifies the starting column.
\[emu-latin1.el; MULE 2.3 emulating function]"
  (or start-column
      (setq start-column 0))
  (if (> (length str) width)
      (substring str start-column width)
    str))

(defalias 'looking-at-as-unibyte 'looking-at)

;;; @@ obsoleted aliases
;;;
;;; You should not use them.

(defalias 'string-columns 'length)
(make-obsolete 'string-columns 'string-width)


;;; @ end
;;;

(require 'product)
(product-provide (provide 'poem-ltn1) (require 'apel-ver))

;;; poem-ltn1.el ends here
