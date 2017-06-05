;;; poem-e20.el --- poem submodule for Emacs 20; -*-byte-compile-dynamic: t;-*-

;; Copyright (C) 1998 Free Software Foundation, Inc.

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

;;; Code:

(defun fontset-pixel-size (fontset)
  (let* ((info (fontset-info fontset))
	 (height (aref info 1))
	 )
    (cond ((> height 0) height)
	  ((string-match "-\\([0-9]+\\)-" fontset)
	   (string-to-number
	    (substring fontset (match-beginning 1)(match-end 1))))
	  (t 0))))


;;; @ character set
;;;

;; (defalias 'charset-columns 'charset-width)

(defun find-non-ascii-charset-string (string)
  "Return a list of charsets in the STRING except ascii."
  (delq 'ascii (find-charset-string string)))

(defun find-non-ascii-charset-region (start end)
  "Return a list of charsets except ascii
in the region between START and END."
  (delq 'ascii (find-charset-string (buffer-substring start end))))


;;; @ end
;;;

(if (and (fboundp 'set-buffer-multibyte)
	 (subrp (symbol-function 'set-buffer-multibyte)))
    (require 'poem-e20_3) ; for Emacs 20.3
  (require 'poem-e20_2) ; for Emacs 20.1 and 20.2
  )

(require 'product)
(product-provide (provide 'poem-e20) (require 'apel-ver))

;;; poem-e20.el ends here
