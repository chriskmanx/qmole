;;; inv-23.el --- invisible feature implementation for Emacs 23 or later

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2001, 2010
;;   Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: invisible, text-property, region, Emacs 23

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

(defun enable-invisible ())
(defun disable-invisible ())
(defalias 'end-of-invisible 'disable-invisible)
(make-obsolete 'end-of-invisible 'disable-invisible)

(defun invisible-region (start end)
  (if (save-excursion
	(goto-char (1- end))
	(eq (following-char) ?\n))
      (setq end (1- end)))
  (put-text-property start end 'invisible t))

(defun visible-region (start end)
  (put-text-property start end 'invisible nil))

(defun next-visible-point (pos)
  (if (setq pos (next-single-property-change pos 'invisible))
      (if (eq ?\n (char-after pos))
	  (1+ pos)
	pos)
    (point-max)))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'inv-23) (require 'apel-ver))

;;; inv-23.el ends here
