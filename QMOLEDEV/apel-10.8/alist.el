;;; alist.el --- utility functions for association list

;; Copyright (C) 1993,1994,1995,1996,1998,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: alist

;; This file is part of GNU Emacs.

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

;;;###autoload
(defun put-alist (key value alist)
  "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST."
  (let ((elm (assoc key alist)))
    (if elm
	(progn
	  (setcdr elm value)
	  alist)
      (cons (cons key value) alist))))

;;;###autoload
(defun del-alist (key alist)
  "Delete an element whose car equals KEY from ALIST.
Return the modified ALIST."
  (let ((pair (assoc key alist)))
    (if pair
	(delq pair alist)
      alist)))

;;;###autoload
(defun set-alist (symbol key value)
  "Set cdr of an element (KEY . ...) in the alist bound to SYMBOL to VALUE."
  (or (boundp symbol)
      (set symbol nil))
  (set symbol (put-alist key value (symbol-value symbol))))

;;;###autoload
(defun remove-alist (symbol key)
  "Delete an element whose car equals KEY from the alist bound to SYMBOL."
  (and (boundp symbol)
       (set symbol (del-alist key (symbol-value symbol)))))

;;;###autoload
(defun modify-alist (modifier default)
  "Store elements in the alist MODIFIER in the alist DEFAULT.
Return the modified alist."
  (mapcar (function
	   (lambda (as)
	     (setq default (put-alist (car as)(cdr as) default))))
	  modifier)
  default)

;;;###autoload
(defun set-modified-alist (symbol modifier)
  "Store elements in the alist MODIFIER in an alist bound to SYMBOL.
If SYMBOL is not bound, set it to nil at first."
  (if (not (boundp symbol))
      (set symbol nil))
  (set symbol (modify-alist modifier (eval symbol))))


;;; @ association-vector-list
;;;

;;;###autoload
(defun vassoc (key avlist)
  "Search AVLIST for an element whose first element equals KEY.
AVLIST is a list of vectors.
See also `assoc'."
  (while (and avlist
	      (not (equal key (aref (car avlist) 0))))
    (setq avlist (cdr avlist)))
  (and avlist
       (car avlist)))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'alist) (require 'apel-ver))

;;; alist.el ends here
