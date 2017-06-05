;;; sha1-dl.el --- SHA1 Secure Hash Algorithm using DL module.

;; Copyright (C) 1999, 2001, 2004  Free Software Foundation, Inc.

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: SHA1, FIPS 180-1

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(provide 'sha1-dl)			; beware of circular dependency.
(eval-when-compile (require 'sha1))	; sha1-dl-module.

;;; This file is loaded (from "sha1.el") only when sha1-dl-module exists.
(defvar sha1-dl-handle (dynamic-link sha1-dl-module))

;;; sha1-dl-module provides `sha1-string' and `sha1-binary'.
(dynamic-call "emacs_sha1_init" sha1-dl-handle)

(defun sha1-region (beg end &optional binary)
  (if binary
      (sha1-binary (buffer-substring-no-properties beg end))
    (sha1-string (buffer-substring-no-properties beg end))))

(defun sha1 (object &optional beg end binary)
  "Return the SHA1 (Secure Hash Algorithm) of an object.
OBJECT is either a string or a buffer.
Optional arguments BEG and END denote buffer positions for computing the
hash of a portion of OBJECT.
If BINARY is non-nil, return a string in binary form."
  (if (stringp object)
      (if binary
	  (sha1-binary object)
	(sha1-string object))
    (save-excursion
      (set-buffer object)
      (sha1-region (or beg (point-min)) (or end (point-max)) binary))))

;;; sha1-dl.el ends here
