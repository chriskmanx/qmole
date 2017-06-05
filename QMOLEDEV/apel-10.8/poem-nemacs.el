;;; poem-nemacs.el --- poem implementation for Nemacs

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

;;; @ character set
;;;

(put 'ascii
     'charset-description "Character set of ASCII")
(put 'ascii
     'charset-registry "ASCII")

(put 'japanese-jisx0208
     'charset-description "Character set of JIS X0208-1983")
(put 'japanese-jisx0208
     'charset-registry "JISX0208.1983")

(defun charset-description (charset)
  "Return description of CHARSET. [emu-nemacs.el]"
  (get charset 'charset-description))

(defun charset-registry (charset)
  "Return registry name of CHARSET. [emu-nemacs.el]"
  (get charset 'charset-registry))

(defun charset-width (charset)
  "Return number of columns a CHARSET occupies when displayed.
\[emu-nemacs.el]"
  (if (eq charset 'ascii)
      1
    2))

(defun charset-direction (charset)
  "Return the direction of a character of CHARSET by
  0 (left-to-right) or 1 (right-to-left). [emu-nemacs.el]"
  0)

(defun find-charset-string (str)
  "Return a list of charsets in the string.
\[emu-nemacs.el; Mule emulating function]"
  (if (string-match "[\200-\377]" str)
      '(japanese-jisx0208)
    ))

(defalias 'find-non-ascii-charset-string 'find-charset-string)

(defun find-charset-region (start end)
  "Return a list of charsets in the region between START and END.
\[emu-nemacs.el; Mule emulating function]"
  (if (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (re-search-forward "[\200-\377]" nil t)))
      '(japanese-jisx0208)
    ))

(defalias 'find-non-ascii-charset-region 'find-charset-region)

(defun check-ASCII-string (str)
  (let ((i 0)
	len)
    (setq len (length str))
    (catch 'label
      (while (< i len)
	(if (>= (elt str i) 128)
	    (throw 'label nil))
	(setq i (+ i 1)))
      str)))

;;; @@ for old MULE emulation
;;;

;;(defconst lc-ascii 0)
;;(defconst lc-jp  146)


;;; @ buffer representation
;;;

(defsubst-maybe set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
The buffer contents remain unchanged as a sequence of bytes
but the contents viewed as characters do change.
\[Emacs 20.3 emulating function]"
  (setq kanji-flag flag)
  )


;;; @ character
;;;

(defun char-charset (chr)
  "Return the character set of char CHR.
\[emu-nemacs.el; MULE emulating function]"
  (if (< chr 128)
      'ascii
    'japanese-jisx0208))

(defun char-bytes (chr)
  "Return number of bytes CHAR will occupy in a buffer.
\[emu-nemacs.el; Mule emulating function]"
  (if (< chr 128)
      1
    2))

(defun char-width (char)
  "Return number of columns a CHAR occupies when displayed.
\[emu-nemacs.el]"
  (if (< char 128)
      1
    2))

(defalias 'char-length 'char-bytes)

(defmacro char-next-index (char index)
  "Return index of character succeeding CHAR whose index is INDEX.
\[emu-nemacs.el]"
  (` (+ (, index) (char-bytes (, char)))))


;;; @ string
;;;

(defalias 'string-width 'length)

(defun sref (str idx)
  "Return the character in STR at index IDX.
\[emu-nemacs.el; Mule emulating function]"
  (let ((chr (aref str idx)))
    (if (< chr 128)
	chr
      (logior (lsh (aref str (1+ idx)) 8) chr))))

(defun string-to-char-list (str)
  (let ((i 0)(len (length str)) dest chr)
    (while (< i len)
      (setq chr (aref str i))
      (if (>= chr 128)
	  (setq i (1+ i)
		chr (+ (lsh chr 8) (aref str i)))
	)
      (setq dest (cons chr dest))
      (setq i (1+ i)))
    (reverse dest)))

(fset 'string-to-int-list (symbol-function 'string-to-char-list))

;;; Imported from Mule-2.3
(defun truncate-string (str width &optional start-column)
  "Truncate STR to fit in WIDTH columns.
Optional non-nil arg START-COLUMN specifies the starting column.
\[emu-mule.el; Mule 2.3 emulating function]"
  (or start-column
      (setq start-column 0))
  (let ((max-width (string-width str))
	(len (length str))
	(from 0)
	(column 0)
	to-prev to ch)
    (if (>= width max-width)
	(setq width max-width))
    (if (>= start-column width)
	""
      (while (< column start-column)
	(setq ch (aref str from)
	      column (+ column (char-width ch))
	      from (+ from (char-bytes ch))))
      (if (< width max-width)
	  (progn
	    (setq to from)
	    (while (<= column width)
	      (setq ch (aref str to)
		    column (+ column (char-width ch))
		    to-prev to
		    to (+ to (char-bytes ch))))
	    (setq to to-prev)))
      (substring str from to))))

(defalias 'looking-at-as-unibyte 'looking-at)

;;; @@ obsoleted aliases
;;;
;;; You should not use them.

(defalias 'string-columns 'length)


;;; @ end
;;;

(require 'product)
(product-provide (provide 'poem-nemacs) (require 'apel-ver))

;;; poem-nemacs.el ends here
