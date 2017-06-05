;;; mime-mc.el --- Mailcrypt interface for SEMI

;; Copyright (C) 1996,1997,1998 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: PGP, security, MIME, multimedia, mail, news

;; This file is part of SEMI (Secure Emacs MIME Interface).

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'mailcrypt)
(eval-and-compile (load "mc-pgp"))

(defun mime-mc-pgp-generic-parser (result)
  (let ((ret (mc-pgp-generic-parser result)))
    (if (consp ret)
	(vector (car ret)(cdr ret))
      )))

(defun mime-mc-process-region
  (beg end passwd program args parser &optional buffer boundary)
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	mybuf result rgn proc)
    (unwind-protect
	(progn
	  (setq mybuf (or buffer (generate-new-buffer " *mailcrypt temp")))
	  (set-buffer mybuf)
	  (erase-buffer)
	  (set-buffer obuf)
	  (buffer-disable-undo mybuf)
	  (setq proc
		(apply 'start-process "*PGP*" mybuf program args))
	  (if passwd
	      (progn
		(process-send-string proc (concat passwd "\n"))
		(or mc-passwd-timeout (mc-deactivate-passwd t))))
	  (process-send-region proc beg end)
	  (process-send-eof proc)
	  (while (eq 'run (process-status proc))
	    (accept-process-output proc 5))
	  (setq result (process-exit-status proc))
	  ;; Hack to force a status_notify() in Emacs 19.29
	  (delete-process proc)
	  (set-buffer mybuf)
	  (goto-char (point-max))
	  (if (re-search-backward "\nProcess \\*PGP.*\n\\'" nil t)
	      (delete-region (match-beginning 0) (match-end 0)))
	  (goto-char (point-min))
	  ;; CRNL -> NL
	  (while (search-forward "\r\n" nil t)
	    (replace-match "\n"))
	  ;; Hurm.  FIXME; must get better result codes.
	  (if (stringp result)
	      (error "%s exited abnormally: '%s'" program result)
	    (setq rgn (funcall parser result))
	    ;; If the parser found something, migrate it
	    (if (consp rgn)
		(progn
		  (set-buffer obuf)
		  (if boundary
		      (save-restriction
			(narrow-to-region beg end)
			(goto-char beg)
			(insert (format "--%s\n" boundary))
			(goto-char (point-max))
			(insert (format "\n--%s
Content-Type: application/pgp-signature
Content-Transfer-Encoding: 7bit

" boundary))
			(insert-buffer-substring mybuf (car rgn) (cdr rgn))
			(goto-char (point-max))
			(insert (format "\n--%s--\n" boundary))
			)
		    (delete-region beg end)
		    (goto-char beg)
		    (insert-buffer-substring mybuf (car rgn) (cdr rgn))
		    )
		  (set-buffer mybuf)
		  (delete-region (car rgn) (cdr rgn)))))
	  ;; Return nil on failure and exit code on success
	  (if rgn result))
      ;; Cleanup even on nonlocal exit
      (if (and proc (eq 'run (process-status proc)))
	  (interrupt-process proc))
      (set-buffer obuf)
      (or buffer (null mybuf) (kill-buffer mybuf)))))

(defun mime-mc-pgp-sign-region (start end &optional id unclear boundary)
  ;; (if (not (boundp 'mc-pgp-user-id))
  ;;     (load "mc-pgp")
  ;;   )
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	passwd args key
	(parser (function mc-pgp-generic-parser))
	(pgp-path mc-pgp-path)
	)
    (setq key (mc-pgp-lookup-key (or id mc-pgp-user-id)))
    (setq passwd
	  (mc-activate-passwd
	   (cdr key)
	   (format "PGP passphrase for %s (%s): " (car key) (cdr key))))
    (setenv "PGPPASSFD" "0")
    (setq args
	  (cons
	   (if boundary
	       "-fbast"
	     "-fast")
	   (list "+verbose=1" "+language=en"
		 (format "+clearsig=%s" (if unclear "off" "on"))
		 "+batchmode" "-u" (cdr key))))
    (if mc-pgp-comment
	(setq args (cons (format "+comment=%s" mc-pgp-comment) args))
      )
    (message "Signing as %s..." (car key))
    (if (mime-mc-process-region
	 start end passwd pgp-path args parser buffer boundary)
	(progn
	  (if boundary
	      (progn
		(goto-char (point-min))
		(insert
		 (format "\
--[[multipart/signed; protocol=\"application/pgp-signature\";
 boundary=\"%s\"; micalg=pgp-md5][7bit]]\n" boundary))
		))
	  (message "Signing as %s...done" (car key))
	  t)
      nil)))

(defun mime-mc-pgp-encrypt-region (recipients start end &optional id sign)
  (let ((mc-pgp-always-sign (if (eq sign 'maybe)
				mc-pgp-always-sign
			      'never)))
    (mc-pgp-encrypt-region
     (mc-split "\\([ \t\n]*,[ \t\n]*\\)+" recipients)
     start end id nil)
    ))

		
;;; @ end
;;;

(provide 'mime-mc)

;;; mime-mc.el ends here
