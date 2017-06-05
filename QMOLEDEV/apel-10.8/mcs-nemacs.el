;;; mcs-nemacs.el --- MIME charset implementation for Nemacs

;; Copyright (C) 1995,1996,1997,1998,2000 Free Software Foundation, Inc.

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

(defvar charsets-mime-charset-alist
  '(((ascii) . us-ascii)))

(defvar default-mime-charset 'iso-2022-jp)

(defvar mime-charset-coding-system-alist
  '((iso-2022-jp     . 2)
    (shift_jis       . 1)
    ))

(defsubst lbt-to-string (lbt)
  (cdr (assq lbt '((nil . nil)
		   (CRLF . "\r\n")
		   (CR . "\r")
		   (dos . "\r\n")
		   (mac . "\r"))))
  )

(defun mime-charset-to-coding-system (charset &optional lbt)
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (cdr (assq charset mime-charset-coding-system-alist)))

(fset 'mime-charset-p 'mime-charset-to-coding-system)

(defun detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END.
\[emu-nemacs.el]"
  (if (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (re-search-forward "[\200-\377]" nil t)))
      default-mime-charset
    'us-ascii))

(defun encode-mime-charset-region (start end charset &optional lbt)
  "Encode the text between START and END as MIME CHARSET.
\[emu-nemacs.el]"
  (let ((cs (mime-charset-to-coding-system charset))
	(nl (lbt-to-string lbt)))
    (and (numberp cs)
	 (or (= cs 3)
	     (save-excursion
	       (save-restriction
		 (narrow-to-region start end)
		 (convert-region-kanji-code start end 3 cs)
		 (if nl
		     (progn
		       (goto-char (point-min))
		       (while (search-forward "\n" nil t)
			 (replace-match nl)))
		   )))
	     ))))

(defun decode-mime-charset-region (start end charset &optional lbt)
  "Decode the text between START and END as MIME CHARSET.
\[emu-nemacs.el]"
  (let ((cs (mime-charset-to-coding-system charset))
	(nl (lbt-to-string lbt)))
    (and (numberp cs)
	 (or (= cs 3)
	     (save-excursion
	       (save-restriction
		 (narrow-to-region start end)
		 (convert-region-kanji-code start end cs 3)
		 (if nl
		     (progn
		       (goto-char (point-min))
		       (while (search-forward nl nil t)
			 (replace-match "\n")))
		   )))
	     ))))

(defun encode-mime-charset-string (string charset &optional lbt)
  "Encode the STRING as MIME CHARSET. [emu-nemacs.el]"
  (with-temp-buffer
    (insert string)
    (encode-mime-charset-region (point-min)(point-max) charset lbt)
    (buffer-string)))

(defun decode-mime-charset-string (string charset &optional lbt)
  "Decode the STRING as MIME CHARSET. [emu-nemacs.el]"
  (with-temp-buffer
    (insert string)
    (decode-mime-charset-region (point-min)(point-max) charset lbt)
    (buffer-string)))

(defun write-region-as-mime-charset (charset start end filename)
  "Like `write-region', q.v., but code-convert by MIME CHARSET.
\[emu-nemacs.el]"
  (let ((kanji-fileio-code
	 (or (mime-charset-to-coding-system charset) 0)))
    (write-region start end filename)))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'mcs-nemacs) (require 'apel-ver))

;;; mcs-nemacs.el ends here
