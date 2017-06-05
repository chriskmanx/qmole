;;; pces-nemacs.el --- pces implementation for Nemacs

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

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

;;; @ coding system
;;;

(defvar coding-system-kanji-code-alist
  '((binary	 . 0)
    (raw-text	 . 0)
    (shift_jis	 . 1)
    (iso-2022-jp . 2)
    (ctext	 . 2)
    (euc-jp	 . 3)
    ))

(defun decode-coding-string (string coding-system)
  "Decode the STRING which is encoded in CODING-SYSTEM.
\[emu-nemacs.el; EMACS 20 emulating function]"
  (let ((code (if (integerp coding-system)
		  coding-system
		(cdr (assq coding-system coding-system-kanji-code-alist)))))
    (if (eq code 3)
	string
      (convert-string-kanji-code string code 3)
      )))

(defun encode-coding-string (string coding-system)
  "Encode the STRING to CODING-SYSTEM.
\[emu-nemacs.el; EMACS 20 emulating function]"
  (let ((code (if (integerp coding-system)
		  coding-system
		(cdr (assq coding-system coding-system-kanji-code-alist)))))
    (if (eq code 3)
	string
      (convert-string-kanji-code string 3 code)
      )))

(defun decode-coding-region (start end coding-system)
  "Decode the text between START and END which is encoded in CODING-SYSTEM.
\[emu-nemacs.el; EMACS 20 emulating function]"
  (let ((code (if (integerp coding-system)
		  coding-system
		(cdr (assq coding-system coding-system-kanji-code-alist)))))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(convert-region-kanji-code start end code 3)
	))))

(defun encode-coding-region (start end coding-system)
  "Encode the text between START and END to CODING-SYSTEM.
\[emu-nemacs.el; EMACS 20 emulating function]"
  (let ((code (if (integerp coding-system)
		  coding-system
		(cdr (assq coding-system coding-system-kanji-code-alist)))))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(convert-region-kanji-code start end 3 code)
	))))

(defun detect-coding-region (start end)
  "Detect coding-system of the text in the region between START and END.
\[emu-nemacs.el; Emacs 20 emulating function]"
  (if (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (re-search-forward "[\200-\377]" nil t)))
      'euc-jp
    ))

(defalias 'set-buffer-file-coding-system 'set-kanji-fileio-code)


;;; @ without code-conversion
;;;

(defmacro as-binary-process (&rest body)
  (` (let (selective-display	; Disable ^M to nl translation.
	   ;; Nemacs
	   kanji-flag
	   (default-kanji-process-code 0)
	   program-kanji-code-alist)
       (,@ body))))

(defmacro as-binary-input-file (&rest body)
  (` (let (kanji-flag default-kanji-flag)
       (,@ body))))

(defmacro as-binary-output-file (&rest body)
  (` (let (kanji-flag)
       (,@ body))))

(defun write-region-as-binary (start end filename
				     &optional append visit lockname)
  "Like `write-region', q.v., but don't code conversion. [emu-nemacs.el]"
  (as-binary-output-file
   (write-region start end filename append visit)))

(defun insert-file-contents-as-binary (filename
				       &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't character code conversion.
\[emu-nemacs.el]"
  (as-binary-input-file
   ;; Returns list absolute file name and length of data inserted.
   (insert-file-contents filename visit)))

(defun insert-file-contents-as-raw-text (filename
					 &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't character code conversion.
It converts line-break code from CRLF to LF. [emu-nemacs.el]"
  (save-restriction
    (narrow-to-region (point) (point))
    (let ((return (as-binary-input-file
		   (insert-file-contents filename visit))))
      (while (search-forward "\r\n" nil t)
	(replace-match "\n"))
      (goto-char (point-min))
      ;; Returns list absolute file name and length of data inserted.
      (list (car return) (- (point-max) (point-min))))))

(defalias 'insert-file-contents-as-raw-text-CRLF
  'insert-file-contents-as-raw-text)

(defun write-region-as-raw-text-CRLF (start end filename
					    &optional append visit lockname)
  "Like `write-region', q.v., but don't code conversion. [emu-nemacs.el]"
  (let ((the-buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring the-buf start end)
      (goto-char (point-min))
      (while (re-search-forward "\\(\\=\\|[^\r]\\)\n" nil t)
	(replace-match "\\1\r\n"))
      (write-region-as-binary (point-min)(point-max)
			      filename append visit))))

(defun find-file-noselect-as-binary (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but don't code conversion.
\[emu-nemacs.el]"
  (as-binary-input-file (find-file-noselect filename nowarn)))

(defun find-file-noselect-as-raw-text (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but it does not code conversion
except for line-break code. [emu-nemacs.el]"
  (let ((buf (get-file-buffer filename))
	cur)
    (if buf
	(prog1
	    buf
	  (or nowarn
	      (verify-visited-file-modtime buf)
	      (cond ((not (file-exists-p filename))
		     (error "File %s no longer exists!" filename))
		    ((yes-or-no-p
		      (if (buffer-modified-p buf)
    "File has changed since last visited or saved.  Flush your changes? "
  "File has changed since last visited or saved.  Read from disk? "))
		     (setq cur (current-buffer))
		     (set-buffer buf)
		     (revert-buffer t t)
		     (save-excursion
		       (goto-char (point-min))
		       (while (search-forward "\r\n" nil t)
			 (replace-match "\n")))
		     (set-buffer-modified-p nil)
		     (set-buffer cur)))))
      (save-excursion
	(prog1
	    (set-buffer
	     (find-file-noselect-as-binary filename nowarn rawfile))
	  (while (search-forward "\r\n" nil t)
	    (replace-match "\n"))
	  (goto-char (point-min))
	  (set-buffer-modified-p nil))))))

(defalias 'find-file-noselect-as-raw-text-CRLF
  'find-file-noselect-as-raw-text)

(defun open-network-stream-as-binary (name buffer host service)
  "Like `open-network-stream', q.v., but don't code conversion.
\[emu-nemacs.el]"
  (let ((process (open-network-stream name buffer host service)))
    (set-process-kanji-code process 0)
    process))

(defun save-buffer-as-binary (&optional args)
  "Like `save-buffer', q.v., but don't encode. [emu-nemacs.el]"
  (as-binary-output-file
   (save-buffer args)))

(defun save-buffer-as-raw-text-CRLF (&optional args)
  "Like `save-buffer', q.v., but save as network representation.
\[emu-nemacs.el]"
  (if (buffer-modified-p)
      (save-restriction
	(widen)
	(let ((the-buf (current-buffer))
	      (filename (buffer-file-name)))
	  (if filename
	      (prog1
		  (with-temp-buffer
		    (insert-buffer the-buf)
		    (goto-char (point-min))
		    (while (re-search-forward "\\(\\=\\|[^\r]\\)\n" nil t)
		      (replace-match "\\1\r\n"))
		    (setq buffer-file-name filename)
		    (save-buffer-as-binary args))
		(set-buffer-modified-p nil)
		(clear-visited-file-modtime)))))))


;;; @ with code-conversion
;;;

(defun insert-file-contents-as-coding-system
  (coding-system filename &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but CODING-SYSTEM the first arg will
be applied to `kanji-fileio-code'. [emu-nemacs.el]"
  (let ((kanji-fileio-code coding-system)
	kanji-expected-code)
    (insert-file-contents filename visit)))

(defun write-region-as-coding-system
  (coding-system start end filename &optional append visit lockname)
  "Like `write-region', q.v., but CODING-SYSTEM the first arg will be
applied to `kanji-fileio-code'. [emu-nemacs.el]"
  (let ((kanji-fileio-code coding-system)
	jka-compr-compression-info-list jam-zcat-filename-list)
    (write-region start end filename append visit)))

(defun find-file-noselect-as-coding-system
  (coding-system filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but CODING-SYSTEM the first arg will
be applied to `kanji-fileio-code'. [emu-nemacs.el]"
  (let ((default-kanji-fileio-code coding-system)
	kanji-fileio-code kanji-expected-code)
    (find-file-noselect filename nowarn)))

(defun save-buffer-as-coding-system (coding-system &optional args)
  "Like `save-buffer', q.v., but CODING-SYSTEM the first arg will be
applied to `kanji-fileio-code'. [emu-nemacs.el]"
  (let ((kanji-fileio-code coding-system))
    (save-buffer args)))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'pces-nemacs) (require 'apel-ver))

;;; pces-nemacs.el ends here
