;;; mule-caesar.el --- ROT 13-47 Caesar rotation utility

;; Copyright (C) 1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: ROT 13-47, caesar, mail, news, text/x-rot13-47

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

(require 'emu)				; for backward compatibility.
(require 'poe)				; char-after.
(require 'poem)				; charset-chars, char-charset,
					; and split-char.

(defun mule-caesar-region (start end &optional stride-ascii)
  "Caesar rotation of current region.
Optional argument STRIDE-ASCII is rotation-size for Latin alphabet
\(A-Z and a-z).  For non-ASCII text, ROT-N/2 will be performed in any
case (N=charset-chars; 94 for 94 or 94x94 graphic character set; 96
for 96 or 96x96 graphic character set)."
  (interactive "r\nP")
  (setq stride-ascii (if stride-ascii
			 (mod stride-ascii 26)
		       13))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (< (point)(point-max))
	(let* ((chr (char-after (point))))
	  (cond ((and (<= ?A chr) (<= chr ?Z))
		 (setq chr (+ chr stride-ascii))
		 (if (> chr ?Z)
		     (setq chr (- chr 26))
		   )
		 (delete-char 1)
		 (insert chr)
		 )
		((and (<= ?a chr) (<= chr ?z))
		 (setq chr (+ chr stride-ascii))
		 (if (> chr ?z)
		     (setq chr (- chr 26))
		   )
		 (delete-char 1)
		 (insert chr)
		 )
		((<= chr ?\x9f)
		 (forward-char)
		 )
		(t
		 (let* ((stride (lsh (charset-chars (char-charset chr)) -1))
			(ret (mapcar (function
				      (lambda (octet)
					(if (< octet 80)
					    (+ octet stride)
					  (- octet stride)
					  )))
				     (cdr (split-char chr)))))
		   (delete-char 1)
		   (insert (make-char (char-charset chr)
				      (car ret)(car (cdr ret))))
		   )))
	  )))))


(require 'product)
(product-provide (provide 'mule-caesar) (require 'apel-ver))

;;; mule-caesar.el ends here
