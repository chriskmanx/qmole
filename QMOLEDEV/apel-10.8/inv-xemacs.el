;;; inv-xemacs.el --- invisible feature implementation for XEmacs

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995,1996,1997 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: invisible, text-property, region, XEmacs

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Code:

(require 'poe)

(defun enable-invisible ())
(defun disable-invisible ())
(defalias 'end-of-invisible 'disable-invisible)
(make-obsolete 'end-of-invisible 'disable-invisible)

(defun invisible-region (start end)
  (if (save-excursion
	(goto-char start)
	(eq (following-char) ?\n))
      (setq start (1+ start)))
  (put-text-property start end 'invisible t))

(defun visible-region (start end)
  (put-text-property start end 'invisible nil))

(defun invisible-p (pos)
  (if (save-excursion
	(goto-char pos)
	(eq (following-char) ?\n))
      (setq pos (1+ pos)))
  (get-text-property pos 'invisible))

(defun next-visible-point (pos)
  (save-excursion
    (if (save-excursion
	  (goto-char pos)
	  (eq (following-char) ?\n))
	(setq pos (1+ pos)))
    (or (next-single-property-change pos 'invisible)
	(point-max))))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'inv-xemacs) (require 'apel-ver))

;;; inv-xemacs.el ends here
