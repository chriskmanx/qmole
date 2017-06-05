;;; pces-raw.el --- pces submodule for emacsen without coding-system features

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

;;; @ coding-system
;;;

(defun decode-coding-string (string coding-system)
  "Decode the STRING which is encoded in CODING-SYSTEM."
  (copy-sequence string))

(defun encode-coding-string (string coding-system)
  "Encode the STRING as CODING-SYSTEM."
  (copy-sequence string))

(defun decode-coding-region (start end coding-system)
  "Decode the text between START and END which is encoded in CODING-SYSTEM."
  0)

(defun encode-coding-region (start end coding-system)
  "Encode the text between START and END to CODING-SYSTEM."
  0)

(defun detect-coding-region (start end)
  "Detect coding-system of the text in the region between START and END."
  )

(defun set-buffer-file-coding-system (coding-system &optional force)
  "Set buffer-file-coding-system of the current buffer to CODING-SYSTEM."
  )


;;; @ without code-conversion
;;;

(defmacro as-binary-process (&rest body)
  (` (let (selective-display)	; Disable ^M to nl translation.
       (,@ body))))

(defmacro as-binary-input-file (&rest body)
  (` (let ((emx-binary-mode t)) ; Stop CRLF to LF conversion in OS/2
       (,@ body))))

(defmacro as-binary-output-file (&rest body)
  (` (let ((emx-binary-mode t)) ; Stop CRLF to LF conversion in OS/2
       (,@ body))))

(defun write-region-as-binary (start end filename
				     &optional append visit lockname)
  "Like `write-region', q.v., but don't code conversion."
  (let ((emx-binary-mode t))
    (write-region start end filename append visit lockname)))

(defun insert-file-contents-as-binary (filename
				       &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't code and format conversion.
Like `insert-file-contents-literary', but it allows find-file-hooks,
automatic uncompression, etc.

Namely this function ensures that only format decoding and character
code conversion will not take place."
  (let ((emx-binary-mode t))
    ;; Returns list of absolute file name and length of data inserted.
    (insert-file-contents filename visit beg end replace)))

(defun write-region-as-raw-text-CRLF (start end filename
					    &optional append visit lockname)
  "Like `write-region', q.v., but write as network representation."
  (let ((the-buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring the-buf start end)
      (goto-char (point-min))
      (while (re-search-forward "\\(\\=\\|[^\r]\\)\n" nil t)
	(replace-match "\\1\r\n"))
      (write-region (point-min)(point-max) filename append visit lockname))))

(defalias 'insert-file-contents-as-raw-text 'insert-file-contents)

(defalias 'insert-file-contents-as-raw-text-CRLF 'insert-file-contents)

(defun find-file-noselect-as-binary (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but don't code and format conversion."
  (let ((emx-binary-mode t))
    (find-file-noselect filename nowarn rawfile)))

(defalias 'find-file-noselect-as-raw-text 'find-file-noselect)

(defalias 'find-file-noselect-as-raw-text-CRLF 'find-file-noselect)

(defun save-buffer-as-binary (&optional args)
  "Like `save-buffer', q.v., but don't encode."
  (let ((emx-binary-mode t))
    (save-buffer args)))

(defun save-buffer-as-raw-text-CRLF (&optional args)
  "Like `save-buffer', q.v., but save as network representation."
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
		    (save-buffer args))
		(set-buffer-modified-p nil)
		(clear-visited-file-modtime)))))))

(defun open-network-stream-as-binary (name buffer host service)
  "Like `open-network-stream', q.v., but don't code conversion."
  (let ((emx-binary-mode t))
    (open-network-stream name buffer host service)))


;;; @ with code-conversion (but actually it might be not done)
;;;

(defun insert-file-contents-as-coding-system
  (coding-system filename &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but CODING-SYSTEM is used to decode."
  (insert-file-contents filename visit beg end replace))

(defun write-region-as-coding-system
  (coding-system start end filename &optional append visit lockname)
  "Like `write-region', q.v., but CODING-SYSTEM is used to encode."
  (let (jka-compr-compression-info-list jam-zcat-filename-list)
    (write-region start end filename append visit lockname)))

(defun find-file-noselect-as-coding-system
  (coding-system filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., CODING-SYSTEM is used to decode."
  (find-file-noselect filename nowarn rawfile))

(defun save-buffer-as-coding-system (coding-system &optional args)
  "Like `save-buffer', q.v., CODING-SYSTEM is used to encode."
  (save-buffer args))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'pces-raw) (require 'apel-ver))

;;; pces-raw.el ends here
