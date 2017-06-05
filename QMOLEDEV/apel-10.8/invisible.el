;;; invisible.el --- hide region

;; Copyright (C) 1995,1996,1997,1998,1999,2010 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: invisible, text-property, region

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

(cond
 ((featurep 'xemacs)
  (require 'inv-xemacs))
 ((>= emacs-major-version 23)
  (require 'inv-23))
 ((>= emacs-major-version 19)
  (require 'inv-19))
 (t
  (require 'inv-18)))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'invisible) (require 'apel-ver))

;;; invisible.el ends here
