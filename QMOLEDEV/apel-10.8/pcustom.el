;;; pcustom.el -- a portable custom.el.

;; Copyright (C) 1999 Free Software Foundation, Inc.
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;;	Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: emulating, custom

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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'poe)
(eval-when-compile (require 'static))

(static-if (condition-case nil
	       ;; compile-time check.
	       (if (and (require 'custom)
			(fboundp 'custom-declare-variable))
		   ;; you have "new custom".
		   t
		 ;; you have custom, but it is "old".
		 (message "\
  ** \"old custom\" is loaded.  See README if you want to use \"new custom\".")
		 (sleep-for 1)
		 nil)
	     ;; you don't have custom.
	     (error nil))
    ;; you have "new custom". no load-time check.
    (require 'custom)
  ;; your custom is "old custom",
  ;; or you don't have custom library at compile-time.
  (or (condition-case nil
	  ;; load-time check.
	  ;; load "custom" if exists.
	  (and (require 'custom)
	       (fboundp 'custom-declare-variable))
	(error nil))
      ;; your custom is "old custom",
      ;; or you don't have custom library.
      ;; load emulation version of "new custom".
      (require 'tinycustom)))

(require 'product)
(product-provide (provide 'pcustom) (require 'apel-ver))

;;; pcustom.el ends here
