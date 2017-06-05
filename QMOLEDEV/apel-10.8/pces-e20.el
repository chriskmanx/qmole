;;; pces-e20.el --- pces submodule for Emacs 20

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

(require 'pces-20)

(unless (and (fboundp 'set-buffer-multibyte)
	     (subrp (symbol-function 'set-buffer-multibyte)))
  (require 'pces-e20_2) ; for Emacs 20.1 and 20.2
  )

(defsubst-maybe find-coding-system (obj)
  "Return OBJ if it is a coding-system."
  (if (coding-system-p obj)
      obj))

(defalias 'set-process-input-coding-system 'set-process-coding-system)


;;; @ end
;;;

(require 'product)
(product-provide (provide 'pces-e20) (require 'apel-ver))

;;; pces-e20.el ends here
