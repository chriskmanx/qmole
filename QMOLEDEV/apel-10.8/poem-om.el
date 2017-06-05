;;; poem-om.el --- poem implementation for Mule 1.* and Mule 2.*

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;         Katsumi Yamaoka  <yamaoka@jpl.org>
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

(require 'poe)


;;; @ version specific features
;;;

(if (= emacs-major-version 19)
    ;; Suggested by SASAKI Osamu <osamu@shuugr.bekkoame.or.jp>
    ;; (cf. [os2-emacs-ja:78])
    (defun fontset-pixel-size (fontset)
      (let* ((font (get-font-info
		    (aref (cdr (get-fontset-info fontset)) 0)))
	     (open (aref font 4)))
	(if (= open 1)
	    (aref font 5)
	  (if (= open 0)
	      (let ((pat (aref font 1)))
		(if (string-match "-[0-9]+-" pat)
		    (string-to-number
		     (substring
		      pat (1+ (match-beginning 0)) (1- (match-end 0))))
		  0))
	    ))))
  )


;;; @ character set
;;;

(defalias 'make-char 'make-character)

(defalias 'find-non-ascii-charset-string 'find-charset-string)
(defalias 'find-non-ascii-charset-region 'find-charset-region)

(defalias 'charset-bytes	'char-bytes)
(defalias 'charset-description	'char-description)
(defalias 'charset-registry	'char-registry)
(defalias 'charset-columns	'char-width)
(defalias 'charset-direction	'char-direction)

(defun charset-chars (charset)
  "Return the number of characters per dimension of CHARSET."
  (if (= (logand (nth 2 (character-set charset)) 1) 1)
      96
    94))


;;; @ buffer representation
;;;

(defsubst-maybe set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
The buffer contents remain unchanged as a sequence of bytes
but the contents viewed as characters do change.
\[Emacs 20.3 emulating function]"
  (setq mc-flag flag)
  )


;;; @ character
;;;

(defalias 'char-charset 'char-leading-char)

(defun split-char (character)
  "Return list of charset and one or two position-codes of CHARACTER."
  (let ((p (1- (char-bytes character)))
	dest)
    (while (>= p 1)
      (setq dest (cons (- (char-component character p) 128) dest)
	    p (1- p)))
    (cons (char-charset character) dest)))

(defmacro char-next-index (char index)
  "Return index of character succeeding CHAR whose index is INDEX."
  (` (+ (, index) (char-bytes (, char)))))


;;; @@ obsoleted aliases
;;;
;;; You should not use them.

(defalias 'char-length 'char-bytes)
;;(defalias 'char-columns 'char-width)


;;; @ string
;;;

(defalias 'string-columns 'string-width)

(defalias 'string-to-int-list 'string-to-char-list)

;; Imported from Mule-2.3
(defun-maybe truncate-string (str width &optional start-column)
  "\
Truncate STR to fit in WIDTH columns.
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


;;; @ end
;;;

(require 'product)
(product-provide (provide 'poem-om) (require 'apel-ver))

;;; poem-om.el ends here
