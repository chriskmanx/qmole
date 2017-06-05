;;; smtpmail.el --- SMTP interface for mail-mode

;; Copyright (C) 1995, 1996, 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Tomoji Kagatani <kagatani@rbc.ncl.omron.co.jp>
;; Keywords: mail

;; This file is part of FLIM (Faithful Library about Internet Message).

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Send Mail to smtp host from smtpmail temp buffer.

;; Please add these lines in your .emacs(_emacs).
;;
;;(setq send-mail-function 'smtpmail-send-it)
;;(setq smtp-default-server "YOUR SMTP HOST")
;;(setq smtp-service "smtp")
;;(setq smtp-local-domain "YOUR DOMAIN NAME")
;;(setq smtp-debug-info t)
;;(autoload 'smtpmail-send-it "smtpmail")
;;(setq user-full-name "YOUR NAME HERE")

;; To queue mail, set smtpmail-queue-mail to t and use 
;; smtpmail-send-queued-mail to send.


;;; Code:

(require 'custom)
(require 'smtp)
(require 'sendmail)
(require 'time-stamp)
(require 'mel) ; binary-write-decoded-region, binary-find-file-noselect

(eval-when-compile (require 'static))

(static-when (featurep 'xemacs)
  (define-obsolete-variable-alias 'smtpmail-default-smtp-server
    'smtp-default-server)
  (define-obsolete-variable-alias 'smtpmail-smtp-server 'smtp-server)
  (define-obsolete-variable-alias 'smtpmail-smtp-service 'smtp-service)
  (define-obsolete-variable-alias 'smtpmail-local-domain 'smtp-local-domain)
  (define-obsolete-variable-alias 'smtpmail-debug-info 'smtp-debug-info)
  )

;;;

(defcustom smtpmail-queue-mail nil 
  "Specify if mail is queued (if t) or sent immediately (if nil).
If queued, it is stored in the directory `smtpmail-queue-dir'
and sent with `smtpmail-send-queued-mail'."
  :type 'boolean
  :group 'smtp)

(defcustom smtpmail-queue-dir "~/Mail/queued-mail/"
  "Directory where `smtpmail.el' stores queued mail."
  :type 'directory
  :group 'smtp)

(defvar smtpmail-queue-index-file "index"
  "File name of queued mail index,
This is relative to `smtpmail-queue-dir'.")

(defvar smtpmail-queue-index
  (concat (file-name-as-directory smtpmail-queue-dir)
	  smtpmail-queue-index-file))

(defvar smtpmail-recipient-address-list nil)


;;;
;;;
;;;

;;;###autoload
(defun smtpmail-send-it ()
  (require 'mail-utils)
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " smtpmail errors")
		  0))
	(tembuf (generate-new-buffer " smtpmail temp"))
	(case-fold-search nil)
	resend-to-addresses
	delimline
	(mailbuf (current-buffer)))
    (unwind-protect
	(save-excursion
	  (set-buffer tembuf)
	  (erase-buffer)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what sendmail expects.
	  (goto-char (point-min))
	  (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "\n"))
	  (replace-match "\n")
	  (backward-char 1)
	  (setq delimline (point-marker))
;;	  (sendmail-synch-aliases)
	  (if (and mail-aliases (fboundp 'expand-mail-aliases)) ; XEmacs
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t))
	    (goto-char (point-min))
	    (goto-char (point-min))
	    (while (re-search-forward "^Resent-to:" delimline t)
	      (setq resend-to-addresses
		    (save-restriction
		      (narrow-to-region (point)
					(save-excursion
					  (forward-line 1)
					  (while (looking-at "^[ \t]")
					    (forward-line 1))
					  (point)))
		      (append (mail-parse-comma-list)
			      resend-to-addresses))))
;;; Apparently this causes a duplicate Sender.
;;;	    ;; If the From is different than current user, insert Sender.
;;;	    (goto-char (point-min))
;;;	    (and (re-search-forward "^From:"  delimline t)
;;;		 (progn
;;;		   (require 'mail-utils)
;;;		   (not (string-equal
;;;			 (mail-strip-quoted-names
;;;			  (save-restriction
;;;			    (narrow-to-region (point-min) delimline)
;;;			    (mail-fetch-field "From")))
;;;			 (user-login-name))))
;;;		 (progn
;;;		   (forward-line 1)
;;;		   (insert "Sender: " (user-login-name) "\n")))
	    ;; Don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:\\([ \t]*\n\\)+\\b" delimline t)
		(replace-match "")
	      ;; This one matches a Subject just before the header delimiter.
	      (if (and (re-search-forward "^Subject:\\([ \t]*\n\\)+" delimline t)
		       (= (match-end 0) delimline))
		  (replace-match "")))
	    ;; Put the "From:" field in unless for some odd reason
	    ;; they put one in themselves.
	    (goto-char (point-min))
	    (if (not (re-search-forward "^From:" delimline t))
		(let* ((login user-mail-address)
		       (fullname (user-full-name)))
		  (cond ((eq mail-from-style 'angles)
			 (insert "From: " fullname)
			 (let ((fullname-start (+ (point-min) 6))
			       (fullname-end (point-marker)))
			   (goto-char fullname-start)
			   ;; Look for a character that cannot appear unquoted
			   ;; according to RFC 822.
			   (if (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
						  fullname-end 1)
			       (progn
				 ;; Quote fullname, escaping specials.
				 (goto-char fullname-start)
				 (insert "\"")
				 (while (re-search-forward "[\"\\]"
							   fullname-end 1)
				   (replace-match "\\\\\\&" t))
				 (insert "\""))))
			 (insert " <" login ">\n"))
			((eq mail-from-style 'parens)
			 (insert "From: " login " (")
			 (let ((fullname-start (point)))
			   (insert fullname)
			   (let ((fullname-end (point-marker)))
			     (goto-char fullname-start)
			     ;; RFC 822 says \ and nonmatching parentheses
			     ;; must be escaped in comments.
			     ;; Escape every instance of ()\ ...
			     (while (re-search-forward "[()\\]" fullname-end 1)
			       (replace-match "\\\\\\&" t))
			     ;; ... then undo escaping of matching parentheses,
			     ;; including matching nested parentheses.
			     (goto-char fullname-start)
			     (while (re-search-forward 
				     "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
				     fullname-end 1)
			       (replace-match "\\1(\\3)" t)
			       (goto-char fullname-start))))
			 (insert ")\n"))
			((null mail-from-style)
			 (insert "From: " login "\n")))))
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
	    (if (eval mail-mailer-swallows-blank-line)
		(newline))
	    ;; Find and handle any FCC fields.
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		(mail-do-fcc delimline))
	    (if mail-interactive
		(save-excursion
		  (set-buffer errbuf)
		  (erase-buffer))))
	  ;;
	  ;;
	  ;;
	  (setq smtpmail-recipient-address-list
		(or resend-to-addresses
		    (smtp-deduce-address-list tembuf (point-min) delimline)))

	  (smtpmail-do-bcc delimline)
	  ; Send or queue
	  (if (not smtpmail-queue-mail)
	      (if smtpmail-recipient-address-list
		  (smtp-send-buffer user-mail-address
				    smtpmail-recipient-address-list
				    tembuf)
		(error "Sending failed; no recipients"))
	    (let* ((file-data (convert-standard-filename
			       (concat
				(file-name-as-directory smtpmail-queue-dir)
				(time-stamp-yyyy-mm-dd)
				"_" (time-stamp-hh:mm:ss))))
		   (file-elisp (concat file-data ".el"))
		   (buffer-data (create-file-buffer file-data))
		   (buffer-elisp (create-file-buffer file-elisp))
		   (buffer-scratch "*queue-mail*"))
	      (save-excursion
		(set-buffer buffer-data)
		(erase-buffer)
		(insert-buffer tembuf)
		(or (file-directory-p smtpmail-queue-dir)
		    (make-directory smtpmail-queue-dir t))
		(binary-write-decoded-region (point-min) (point-max) file-data)
		(set-buffer buffer-elisp)
		(erase-buffer)
		(insert (concat
			 "(setq smtpmail-recipient-address-list '"
			 (prin1-to-string smtpmail-recipient-address-list)
			 ")\n"))	    	    
		(write-file file-elisp)
		(set-buffer (generate-new-buffer buffer-scratch))
		(insert (concat file-data "\n"))
		(append-to-file (point-min) 
				(point-max) 
				smtpmail-queue-index)
		)
	      (kill-buffer buffer-scratch)
	      (kill-buffer buffer-data)
	      (kill-buffer buffer-elisp))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (kill-buffer errbuf)))))

(defun smtpmail-send-queued-mail ()
  "Send mail that was queued as a result of setting `smtpmail-queue-mail'."
  (interactive)
  ;;; Get index, get first mail, send it, get second mail, etc...
  (let ((buffer-index (find-file-noselect smtpmail-queue-index))
	(file-msg "")
	(tembuf nil))
    (save-excursion
      (set-buffer buffer-index)
      (beginning-of-buffer)
      (while (not (eobp))
	(setq file-msg (buffer-substring (point) (save-excursion
						   (end-of-line)
						   (point))))
	(load file-msg)
	(setq tembuf (binary-find-file-noselect file-msg))
	(if smtpmail-recipient-address-list
	    (smtp-send-buffer user-mail-address
			      smtpmail-recipient-address-list tembuf)
	  (error "Sending failed; no recipients"))  
	(delete-file file-msg)
	(delete-file (concat file-msg ".el"))
	(kill-buffer tembuf)
	(kill-line 1))      
      (set-buffer buffer-index)
      (save-buffer smtpmail-queue-index)
      (kill-buffer buffer-index)
      )))


(defun smtpmail-do-bcc (header-end)
  "Delete BCC: and their continuation lines from the header area.
There may be multiple BCC: lines, and each may have arbitrarily
many continuation lines."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      ;; iterate over all BCC: lines
      (while (re-search-forward "^BCC:" header-end t)
	(delete-region (match-beginning 0) (progn (forward-line 1) (point)))
	;; get rid of any continuation lines
	(while (and (looking-at "^[ \t].*\n") (< (point) header-end))
	  (replace-match ""))
	)
      ) ;; save-excursion
    ) ;; let
  )


;;;

(provide 'smtpmail)

;;; smtpmail.el ends here
