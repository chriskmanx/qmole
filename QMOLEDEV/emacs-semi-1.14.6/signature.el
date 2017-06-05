;;; signature.el --- a signature utility for GNU Emacs

;; Copyright (C) 1994,1995,1996,1997,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;         OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;         Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Maintainer: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Created: 1994/7/11
;; Keywords: mail, news, signature

;; This file is part of SEMI (SEMI is Emacs MIME Interfaces).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'std11)


;;; @ valiables
;;;

(defvar signature-insert-at-eof nil
  "*If non-nil, insert signature at the end of file.")

(defvar signature-delete-blank-lines-at-eof nil
  "*If non-nil, signature-insert-at-eof deletes blank lines at the end
of file.")

(defvar signature-load-hook nil
  "*List of functions called after signature.el is loaded.")

(defvar signature-separator "-- \n"
  "*String to separate contents and signature.
It is inserted when signature is inserted at end of file.")

(defvar signature-file-name "~/.signature"
  "*Name of file containing the user's signature.")

(defvar signature-file-alist nil
  "*Alist of the form:
    (((FIELD . PATTERN) . FILENAME)
     ...)
PATTERN is a string or list of string. If PATTERN matches the contents of
FIELD, the contents of FILENAME is inserted.")

(defvar signature-file-prefix nil
  "*String containing optional prefix for the signature file names")

(defvar signature-insert-hook nil
  "*List of functions called before inserting a signature.")

(defvar signature-use-bbdb nil
  "*If non-nil, Register sigtype to BBDB.")

(autoload 'signature/get-sigtype-from-bbdb "mime-bbdb")

(defun signature/get-sigtype-interactively (&optional default)
  (read-file-name "Insert your signature: "
                  (or default (concat signature-file-name "-"))
                  (or default signature-file-name)
                  nil))

(defun signature/get-signature-file-name ()
  (save-excursion
    (save-restriction
      (narrow-to-region
       (goto-char (point-min))
       (if (re-search-forward
            (concat "^" (regexp-quote mail-header-separator) "$")
            nil t)
           (match-beginning 0)
         (point-max)
         ))
      (catch 'found
        (let ((alist signature-file-alist) cell field value)
          (while alist
            (setq cell  (car alist)
                  field (std11-field-body (car (car cell)))
                  value (cdr (car cell)))
            (cond ((functionp value)
		   (let ((name (apply value field (cdr cell))))
		     (if name
			 (throw 'found
				(concat signature-file-prefix name))
		       )))
		  ((stringp field)
		   (cond ((consp value)
			  (while value
			    (if (string-match (car value) field)
				(throw 'found
				       (concat
					signature-file-prefix (cdr cell)))
			      (setq value (cdr value))
			      )))
			 ((stringp value)
			  (if (string-match value field)
			      (throw 'found
				     (concat
				      signature-file-prefix (cdr cell)))
			    )))))
            (setq alist (cdr alist))
            ))
        signature-file-name))))

(defun insert-signature (&optional arg)
  "Insert the file named by signature-file-name.
It is inserted at the end of file if signature-insert-at-eof is non-nil,
and otherwise at the current point.  A prefix argument enables user to
specify a file named <signature-file-name>-DISTRIBUTION interactively."
  (interactive "P")
  (let ((signature-file-name
         (expand-file-name
          (or (and signature-use-bbdb
                   (signature/get-sigtype-from-bbdb arg))
              (and arg
                   (signature/get-sigtype-interactively))
              (signature/get-signature-file-name))
          )))
    (or (file-readable-p signature-file-name)
        (error "Cannot open signature file: %s" signature-file-name))
    (if signature-insert-at-eof
        (progn
          (goto-char (point-max))
          (or (bolp) (insert "\n"))
          (if signature-delete-blank-lines-at-eof (delete-blank-lines))
          ))
    (run-hooks 'signature-insert-hook)
    (if (= (point)(point-max))
	(insert signature-separator)
      )
    (insert-file-contents signature-file-name)
    (force-mode-line-update)
    signature-file-name))


;;; @ end
;;;

(provide 'signature)

(run-hooks 'signature-load-hook)

;;; signature.el ends here
