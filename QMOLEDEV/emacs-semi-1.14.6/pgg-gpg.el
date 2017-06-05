;;; pgg-gpg.el --- GnuPG support for PGG.

;; Copyright (C) 1999,2000 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1999/10/28
;; Keywords: PGP, OpenPGP, GnuPG

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

(require 'mel) ; binary-to-text-funcall
(eval-when-compile (require 'pgg))

(defgroup pgg-gpg ()
  "GnuPG interface"
  :group 'pgg)

(defcustom pgg-gpg-program "gpg" 
  "The GnuPG executable."
  :group 'pgg-gpg
  :type 'string)

(defcustom pgg-gpg-extra-args nil
  "Extra arguments for every GnuPG invocation."
  :group 'pgg-gpg
  :type 'string)

(eval-and-compile
  (luna-define-class pgg-scheme-gpg (pgg-scheme)))

(defvar pgg-gpg-user-id nil
  "GnuPG ID of your default identity.")

(defvar pgg-gpg-messages-coding-system pgg-messages-coding-system
  "Coding system used when reading from a GnuPG external process.")

(defvar pgg-gpg-messages-locale pgg-messages-locale
  "Locale set before running a GnuPG external process.")

(defvar pgg-scheme-gpg-instance nil)

;;;###autoload
(defun pgg-make-scheme-gpg ()
  (or pgg-scheme-gpg-instance
      (setq pgg-scheme-gpg-instance
	    (luna-make-entity 'pgg-scheme-gpg))))

(defun pgg-gpg-process-region (start end passphrase program args)
  (let* ((output-file-name (make-temp-file
			    (expand-file-name "pgg-output"
					      temporary-file-directory)))
	 (args
	  `("--status-fd" "2"
	    ,@(if passphrase '("--passphrase-fd" "0"))
	    "--yes" ; overwrite
	    "--output" ,output-file-name
	    ,@pgg-gpg-extra-args ,@args))
	 (output-buffer pgg-output-buffer)
	 (errors-buffer pgg-errors-buffer)
	 (process-connection-type nil)
	 (process-environment process-environment)
	 process status exit-status)
    (when pgg-gpg-messages-locale
      (setq process-environment (copy-sequence process-environment))
      (setenv "LC_ALL" pgg-gpg-messages-locale)
      (setenv "LANGUAGE" pgg-gpg-messages-locale))
    (with-current-buffer (get-buffer-create errors-buffer)
      (buffer-disable-undo)
      (erase-buffer))
    (unwind-protect
	(progn
	  (setq process
		(apply #'binary-to-text-funcall
		       pgg-gpg-messages-coding-system
		       #'start-process "*GnuPG*" errors-buffer
		       program args))
	  (set-process-sentinel process #'ignore)
	  (when passphrase
	    (process-send-string process (concat passphrase "\n")))
	  (process-send-region process start end)
	  (process-send-eof process)
	  (while (eq 'run (process-status process))
	    (accept-process-output process 5))
	  (setq status (process-status process)
		exit-status (process-exit-status process))
	  (delete-process process)
	  (with-current-buffer (get-buffer-create output-buffer)
	    (buffer-disable-undo)
	    (erase-buffer)
	    (if (file-exists-p output-file-name)
		(let ((coding-system-for-read 'raw-text-dos))
		  (insert-file-contents output-file-name)))
	    (set-buffer errors-buffer)
	    (if (memq status '(stop signal))
		(error "%s exited abnormally: '%s'" program exit-status))
	    (if (= 127 exit-status)
		(error "%s could not be found" program))))
      (if (and process (eq 'run (process-status process)))
	  (interrupt-process process))
      (if (file-exists-p output-file-name)
	  (delete-file output-file-name)))))

(defun pgg-gpg-possibly-cache-passphrase (passphrase)
  (if (and pgg-cache-passphrase
	   (progn
	     (goto-char (point-min))
	     (re-search-forward "^\\[GNUPG:] GOOD_PASSPHRASE\\>" nil t)))
      (pgg-add-passphrase-cache
       (progn
	 (goto-char (point-min))
	 (if (re-search-forward
	      "^\\[GNUPG:] NEED_PASSPHRASE \\w+ ?\\w*" nil t)
	     (substring (match-string 0) -8)))
       passphrase)))

(luna-define-method pgg-scheme-lookup-key ((scheme pgg-scheme-gpg)
					   string &optional type)
  (let ((args (list "--with-colons" "--no-greeting" "--batch"
		    (if type "--list-secret-keys" "--list-keys")
		    string)))
    (with-temp-buffer
      (apply #'call-process pgg-gpg-program nil t nil args)
      (goto-char (point-min))
      (if (re-search-forward "^\\(sec\\|pub\\):"  nil t)
	  (substring
	   (nth 3 (split-string
		   (buffer-substring (match-end 0)
				     (progn (end-of-line)(point)))
		   ":")) 8)))))

(luna-define-method pgg-scheme-encrypt-region ((scheme pgg-scheme-gpg)
					       start end recipients)
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (args
	  `("--batch" "--armor" "--always-trust" "--encrypt"
	    ,@(if recipients
		  (apply #'nconc
			 (mapcar (lambda (rcpt)
				   (list "--remote-user" rcpt))
				 (append recipients
					 (if pgg-encrypt-for-me
					     (list pgg-gpg-user-id)))))))))
    (pgg-as-lbt start end 'CRLF
      (pgg-gpg-process-region start end nil pgg-gpg-program args))
    (pgg-process-when-success)))

(luna-define-method pgg-scheme-decrypt-region ((scheme pgg-scheme-gpg)
					       start end)
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (passphrase
	  (pgg-read-passphrase
	   (format "GnuPG passphrase for %s: " pgg-gpg-user-id)
	   (pgg-scheme-lookup-key scheme pgg-gpg-user-id 'encrypt)))
	 (args '("--batch" "--decrypt")))
    (pgg-gpg-process-region start end passphrase pgg-gpg-program args)
    (with-current-buffer pgg-errors-buffer
      (pgg-gpg-possibly-cache-passphrase passphrase)
      (goto-char (point-min))
      (re-search-forward "^\\[GNUPG:] DECRYPTION_OKAY\\>" nil t))))

(luna-define-method pgg-scheme-sign-region ((scheme pgg-scheme-gpg)
					    start end &optional cleartext)
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (passphrase
	  (pgg-read-passphrase
	   (format "GnuPG passphrase for %s: " pgg-gpg-user-id)
	   (pgg-scheme-lookup-key scheme pgg-gpg-user-id 'sign)))
	 (args
	  (list (if cleartext "--clearsign" "--detach-sign")
		"--armor" "--batch" "--verbose"
		"--local-user" pgg-gpg-user-id))
	 (inhibit-read-only t)
	 buffer-read-only)
    (pgg-as-lbt start end 'CRLF
      (pgg-gpg-process-region start end passphrase pgg-gpg-program args))
    (with-current-buffer pgg-errors-buffer
      (pgg-gpg-possibly-cache-passphrase passphrase))
    (pgg-process-when-success)))

(luna-define-method pgg-scheme-verify-region ((scheme pgg-scheme-gpg)
					      start end &optional signature)
  (let ((args '("--batch" "--verify")))
    (when (stringp signature)
      (setq args (append args (list signature))))
    (setq args (append args '("-")))
    (pgg-gpg-process-region start end nil pgg-gpg-program args)
    (with-current-buffer pgg-errors-buffer
      (goto-char (point-min))
      (while (re-search-forward "^gpg: " nil t)
	(replace-match ""))
      (goto-char (point-min))
      (prog1 (re-search-forward "^\\[GNUPG:] GOODSIG\\>" nil t)
	(goto-char (point-min))
	(delete-matching-lines "^\\[GNUPG:] ")
	;; XXX: copy contents of pgg-errors-buffer into
	;; pgg-output-buffer for backward compatibility.
	(with-current-buffer pgg-output-buffer
	  (set-buffer-multibyte t)
	  (insert-buffer-substring pgg-errors-buffer))))))

(luna-define-method pgg-scheme-insert-key ((scheme pgg-scheme-gpg))
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (args (list "--batch" "--export" "--armor"
		     pgg-gpg-user-id)))
    (pgg-gpg-process-region (point)(point) nil pgg-gpg-program args)
    (insert-buffer-substring pgg-output-buffer)))

(luna-define-method pgg-scheme-snarf-keys-region ((scheme pgg-scheme-gpg)
						  start end)
  (let ((args '("--import" "--batch" "-")) status)
    (pgg-gpg-process-region start end nil pgg-gpg-program args)
    (set-buffer pgg-errors-buffer)
    (goto-char (point-min))
    (when (re-search-forward "^\\[GNUPG:] IMPORT_RES\\>" nil t)
      (setq status (buffer-substring (match-end 0)
				     (progn (end-of-line)(point)))
	    status (vconcat (mapcar #'string-to-int (split-string status))))
      (erase-buffer)
      (insert (format "Imported %d key(s).
\tArmor contains %d key(s) [%d bad, %d old].\n"
		      (+ (aref status 2)
			 (aref status 10))
		      (aref status 0)
		      (aref status 1)
		      (+ (aref status 4)
			 (aref status 11)))
	      (if (zerop (aref status 9))
		  ""
		"\tSecret keys are imported.\n"))
      ;; XXX: copy contents of pgg-errors-buffer into
      ;; pgg-output-buffer for backward compatibility.
      (with-current-buffer pgg-output-buffer
	(set-buffer-multibyte t)
	(insert-buffer-substring pgg-errors-buffer))
      t)))

(provide 'pgg-gpg)

;;; pgg-gpg.el ends here
