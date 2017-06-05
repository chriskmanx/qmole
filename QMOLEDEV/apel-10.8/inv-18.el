;;; inv-18.el --- invisible feature implementation for Emacs 18

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: invisible, text-property, region, Emacs 18

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

(defun enable-invisible ()
  (make-local-variable 'original-selective-display)
  (setq original-selective-display selective-display)
  (setq selective-display t))

(defun disable-invisible ()
  (setq selective-display
	(and (boundp 'original-selective-display)
	     original-selective-display)))
(defalias 'end-of-invisible 'disable-invisible)
(make-obsolete 'end-of-invisible 'disable-invisible)

(defun invisible-region (start end)
  (let ((buffer-read-only nil)
	(modp (buffer-modified-p)))
    (if (save-excursion
	  (goto-char (1- end))
	  (eq (following-char) ?\n))
	(setq end (1- end)))
    (unwind-protect
        (subst-char-in-region start end ?\n ?\r t)
      (set-buffer-modified-p modp))))

(defun visible-region (start end)
  (let ((buffer-read-only nil)
	(modp (buffer-modified-p)))
    (unwind-protect
        (subst-char-in-region start end ?\r ?\n t)
      (set-buffer-modified-p modp))))

(defun invisible-p (pos)
  (save-excursion
    (goto-char pos)
    (eq (following-char) ?\r)))

(defun next-visible-point (pos)
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (if (eq (following-char) ?\n)
	(forward-char))
    (point)))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'inv-18) (require 'apel-ver))

;;; inv-18.el ends here
