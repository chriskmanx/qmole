;;; pces-xfc.el --- pces module for XEmacs with file coding

;; Copyright (C) 1998,1999 Free Software Foundation, Inc.

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

;; Redefine if -{dos|mac|unix} is not found.
(or (find-coding-system 'raw-text-dos)
    (copy-coding-system 'no-conversion-dos 'raw-text-dos))
(or (find-coding-system 'raw-text-mac)
    (copy-coding-system 'no-conversion-mac 'raw-text-mac))
(or (find-coding-system 'raw-text-unix)
    (copy-coding-system 'no-conversion-unix 'raw-text-unix))

(if (featurep 'mule)
    (require 'pces-xm)
  )

(require 'pces-20)


;;; @ end
;;;

(require 'product)
(product-provide (provide 'pces-xfc) (require 'apel-ver))

;;; pces-xfc.el ends here
