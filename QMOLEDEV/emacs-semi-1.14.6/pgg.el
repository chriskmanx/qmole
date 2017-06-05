;;; pgg.el --- glue for the various PGP implementations.

;; Copyright (C) 1999,2000 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1999/10/28
;; Keywords: PGP

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


;;; Commentary:
;; 

;;; Code:

(require 'calist)

(eval-and-compile (require 'luna))

(require 'pgg-def)
(require 'pgg-parse)

(eval-when-compile
  (ignore-errors
    (require 'w3)
    (require 'url)))

(in-calist-package 'pgg)

(defun pgg-field-match-method-with-containment
  (calist field-type field-value)
  (let ((s-field (assq field-type calist)))
    (cond ((null s-field)
	   (cons (cons field-type field-value) calist))
	  ((memq (cdr s-field) field-value)
	   calist))))

(define-calist-field-match-method 'signature-version
  #'pgg-field-match-method-with-containment)

(define-calist-field-match-method 'symmetric-key-algorithm
  #'pgg-field-match-method-with-containment)

(define-calist-field-match-method 'public-key-algorithm
  #'pgg-field-match-method-with-containment)

(define-calist-field-match-method 'hash-algorithm
  #'pgg-field-match-method-with-containment)

(defvar pgg-verify-condition nil
  "Condition-tree about which PGP implementation is used for verifying.")

(defvar pgg-decrypt-condition nil
  "Condition-tree about which PGP implementation is used for decrypting.")

(ctree-set-calist-strictly
 'pgg-verify-condition
 '((signature-version 3)(public-key-algorithm RSA)(hash-algorithm MD5)
   (scheme . pgp)))

(ctree-set-calist-strictly
 'pgg-decrypt-condition
 '((public-key-algorithm RSA)(symmetric-key-algorithm IDEA)
   (scheme . pgp)))

(ctree-set-calist-strictly
 'pgg-verify-condition
 '((signature-version 3 4)
   (public-key-algorithm RSA ELG DSA)
   (hash-algorithm MD5 SHA1 RIPEMD160)
   (scheme . pgp5)))

(ctree-set-calist-strictly
 'pgg-decrypt-condition
 '((public-key-algorithm RSA ELG DSA)
   (symmetric-key-algorithm 3DES CAST5 IDEA)
   (scheme . pgp5)))

(ctree-set-calist-strictly
 'pgg-verify-condition
 '((signature-version 3 4)
   (public-key-algorithm ELG-E DSA ELG)
   (hash-algorithm MD5 SHA1 RIPEMD160)
   (scheme . gpg)))

(ctree-set-calist-strictly
 'pgg-decrypt-condition
 '((public-key-algorithm ELG-E DSA ELG)
   (symmetric-key-algorithm 3DES CAST5 BLOWFISH TWOFISH)
   (scheme . gpg)))

;;; @ definition of the implementation scheme
;;;

(eval-and-compile
  (luna-define-class pgg-scheme ())

  (luna-define-internal-accessors 'pgg-scheme))

(luna-define-generic pgg-scheme-lookup-key (scheme string &optional type)
  "Search keys associated with STRING.")

(luna-define-generic pgg-scheme-encrypt-region (scheme start end recipients)
  "Encrypt the current region between START and END.")

(luna-define-generic pgg-scheme-decrypt-region (scheme start end)
  "Decrypt the current region between START and END.")

(luna-define-generic pgg-scheme-sign-region
  (scheme start end &optional cleartext)
  "Make detached signature from text between START and END.")

(luna-define-generic pgg-scheme-verify-region
  (scheme start end &optional signature)
  "Verify region between START and END as the detached signature SIGNATURE.")

(luna-define-generic pgg-scheme-insert-key (scheme)
  "Insert public key at point.")

(luna-define-generic pgg-scheme-snarf-keys-region (scheme start end)
  "Add all public keys in region between START and END to the keyring.")

;;; @ utility functions
;;;

(defvar pgg-fetch-key-function (function pgg-fetch-key-with-w3))

(defmacro pgg-make-scheme (scheme)
  `(progn
     (require (intern (format "pgg-%s" ,scheme)))
     (funcall (intern (format "pgg-make-scheme-%s"
			      ,scheme)))))

(put 'pgg-save-coding-system 'lisp-indent-function 2)

(defmacro pgg-save-coding-system (start end &rest body)
  `(if (interactive-p)
       (let ((buffer (current-buffer)))
	 (with-temp-buffer
	   (let (buffer-undo-list)
	     (insert-buffer-substring buffer ,start ,end)
	     (encode-coding-region (point-min)(point-max)
				   buffer-file-coding-system)
	     (prog1 (save-excursion ,@body)
	       (push nil buffer-undo-list)
	       (ignore-errors (undo))))))
     (save-restriction
       (narrow-to-region ,start ,end)
       ,@body)))

(defun pgg-temp-buffer-show-function (buffer)
  (let ((window (split-window-vertically)))
    (set-window-buffer window buffer)
    (shrink-window-if-larger-than-buffer window)))

(defun pgg-display-output-buffer (start end status)
  (if status
      (progn
	(delete-region start end)
	(insert-buffer-substring pgg-output-buffer)
	(decode-coding-region start (point) buffer-file-coding-system))
    (let ((temp-buffer-show-function
	   (function pgg-temp-buffer-show-function)))
      (with-output-to-temp-buffer pgg-echo-buffer
	(set-buffer standard-output)
	(insert-buffer-substring pgg-errors-buffer)))))

(defvar pgg-passphrase-cache-expiry 16)
(defvar pgg-passphrase-cache (make-vector 7 0))

(defvar pgg-read-passphrase nil)
(defun pgg-read-passphrase (prompt &optional key)
  (if (not pgg-read-passphrase)
      (if (functionp 'read-passwd)
	  (setq pgg-read-passphrase 'read-passwd)
	(if (load "passwd" t)
	    (setq pgg-read-passphrase 'read-passwd)
	  (autoload 'ange-ftp-read-passwd "ange-ftp")
	  (setq pgg-read-passphrase 'ange-ftp-read-passwd))))
  (or (and pgg-cache-passphrase
	   key (setq key (pgg-truncate-key-identifier key))
	   (symbol-value (intern-soft key pgg-passphrase-cache)))
      (funcall pgg-read-passphrase prompt)))

(defun pgg-add-passphrase-cache (key passphrase)
  (setq key (pgg-truncate-key-identifier key))
  (set (intern key pgg-passphrase-cache)
       passphrase)
  (run-at-time pgg-passphrase-cache-expiry nil
	       #'pgg-remove-passphrase-cache
	       key))

(defun pgg-remove-passphrase-cache (key)
  (let ((passphrase (symbol-value (intern-soft key pgg-passphrase-cache))))
    (when passphrase
      (fillarray passphrase ?_)
      (unintern key pgg-passphrase-cache))))

(defmacro pgg-convert-lbt-region (start end lbt)
  `(let ((pgg-conversion-end (set-marker (make-marker) ,end)))
     (goto-char ,start)
     (case ,lbt
       (CRLF
	(while (progn
		 (end-of-line)
		 (> (marker-position pgg-conversion-end) (point)))
	  (insert "\r")
	  (forward-line 1)))
       (LF
	(while (re-search-forward "\r$" pgg-conversion-end t)
	  (replace-match ""))))))

(put 'pgg-as-lbt 'lisp-indent-function 3)

(defmacro pgg-as-lbt (start end lbt &rest body)
  `(let ((inhibit-read-only t)
	 buffer-read-only
	 buffer-undo-list)
     (pgg-convert-lbt-region ,start ,end ,lbt)
     (let ((,end (point)))
       ,@body)
     (push nil buffer-undo-list)
     (ignore-errors (undo))))

(put 'pgg-process-when-success 'lisp-indent-function 0)

(defmacro pgg-process-when-success (&rest body)
  `(with-current-buffer pgg-output-buffer
     (if (zerop (buffer-size)) nil ,@body t)))


;;; @ interface functions
;;;

;;;###autoload
(defun pgg-encrypt-region (start end rcpts)
  "Encrypt the current region between START and END for RCPTS."
  (interactive
   (list (region-beginning)(region-end)
	 (split-string (read-string "Recipients: ") "[ \t,]+")))
  (let* ((entity (pgg-make-scheme pgg-default-scheme))
	 (status
	  (pgg-save-coding-system start end
	    (pgg-scheme-encrypt-region entity (point-min)(point-max) rcpts))))
    (when (interactive-p)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-decrypt-region (start end)
  "Decrypt the current region between START and END."
  (interactive "r")
  (let* ((packet (cdr (assq 1 (pgg-parse-armor-region start end))))
	 (scheme
	  (or pgg-scheme
	      (cdr (assq 'scheme
			 (progn
			   (in-calist-package 'pgg)
			   (ctree-match-calist pgg-decrypt-condition
					       packet))))
	      pgg-default-scheme))
	 (entity (pgg-make-scheme scheme))
	 (status
	  (pgg-save-coding-system start end
	    (pgg-scheme-decrypt-region entity (point-min)(point-max)))))
    (when (interactive-p)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-sign-region (start end &optional cleartext)
  "Make the signature from text between START and END.
If the optional 3rd argument CLEARTEXT is non-nil, it does not create
a detached signature."
  (interactive "r")
  (let* ((entity (pgg-make-scheme pgg-default-scheme))
	 (status (pgg-save-coding-system start end
		   (pgg-scheme-sign-region entity (point-min)(point-max)
					   (or (interactive-p) cleartext)))))
    (when (interactive-p)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-verify-region (start end &optional signature fetch)
  "Verify the current region between START and END.
If the optional 3rd argument SIGNATURE is non-nil, it is treated as
the detached signature of the current region.

If the optional 4th argument FETCH is non-nil, we attempt to fetch the
signer's public key from `pgg-default-keyserver-address'."
  (interactive "r")
  (let* ((packet
	  (if (null signature) nil
	    (with-temp-buffer
	      (buffer-disable-undo)
	      (set-buffer-multibyte nil)
	      (insert-file-contents signature)
	      (cdr (assq 2 (pgg-decode-armor-region
			    (point-min)(point-max)))))))
	 (scheme
	  (or pgg-scheme
	      (cdr (assq 'scheme
			 (progn
			   (in-calist-package 'pgg)
			   (ctree-match-calist pgg-verify-condition
					       packet))))
	      pgg-default-scheme))
	 (entity (pgg-make-scheme scheme))
	 (key (cdr (assq 'key-identifier packet)))
	 status keyserver)
    (and (stringp key)
	 (setq key (concat "0x" (pgg-truncate-key-identifier key)))
	 (null (let ((pgg-scheme scheme))
		 (pgg-lookup-key key)))
	 (or fetch (interactive-p))
	 (y-or-n-p (format "Key %s not found; attempt to fetch? " key))
	 (setq keyserver
	       (or (cdr (assq 'preferred-key-server packet))
		   pgg-default-keyserver-address))
	 (pgg-fetch-key keyserver key))
    (setq status (pgg-save-coding-system start end
		   (pgg-scheme-verify-region entity (point-min)(point-max)
					     signature)))
    (when (interactive-p)
      (let ((temp-buffer-show-function
	     (function pgg-temp-buffer-show-function)))
	(with-output-to-temp-buffer pgg-echo-buffer
	  (set-buffer standard-output)
	  (insert-buffer-substring (if status pgg-output-buffer
				     pgg-errors-buffer)))))
    status))

;;;###autoload
(defun pgg-insert-key ()
  "Insert the ASCII armored public key."
  (interactive)
  (let ((entity (pgg-make-scheme (or pgg-scheme pgg-default-scheme))))
    (pgg-scheme-insert-key entity)))

;;;###autoload
(defun pgg-snarf-keys-region (start end)
  "Import public keys in the current region between START and END."
  (interactive "r")
  (let ((entity (pgg-make-scheme (or pgg-scheme pgg-default-scheme))))
    (pgg-save-coding-system start end
      (pgg-scheme-snarf-keys-region entity start end))))

(defun pgg-lookup-key (string &optional type)
  (let ((entity (pgg-make-scheme (or pgg-scheme pgg-default-scheme))))
    (pgg-scheme-lookup-key entity string type)))

(defvar pgg-insert-url-function  (function pgg-insert-url-with-w3))

(defun pgg-insert-url-with-w3 (url)
  (require 'w3)
  (require 'url)
  (let (buffer-file-name)
    (url-insert-file-contents url)))

(defvar pgg-insert-url-extra-arguments nil)
(defvar pgg-insert-url-program nil)

(defun pgg-insert-url-with-program (url)
  (let ((args (copy-sequence pgg-insert-url-extra-arguments))
	process)
    (insert
     (with-temp-buffer
       (setq process
	     (apply #'start-process " *PGG url*" (current-buffer)
		    pgg-insert-url-program (nconc args (list url))))
       (set-process-sentinel process #'ignore)
       (while (eq 'run (process-status process))
	 (accept-process-output process 5))
       (delete-process process)
       (if (and process (eq 'run (process-status process)))
	   (interrupt-process process))
       (buffer-string)))))

(defun pgg-fetch-key (keyserver key)
  "Attempt to fetch a KEY from KEYSERVER for addition to PGP or GnuPG keyring."
  (with-current-buffer (get-buffer-create pgg-output-buffer)
    (buffer-disable-undo)
    (erase-buffer)
    (let ((proto (if (string-match "^[a-zA-Z\\+\\.\\\\-]+:" keyserver)
		     (substring keyserver 0 (1- (match-end 0))))))
      (save-excursion
	(funcall pgg-insert-url-function
		 (if proto keyserver
		   (format "http://%s:11371/pks/lookup?op=get&search=%s"
			   keyserver key))))
      (when (re-search-forward "^-+BEGIN" nil 'last)
	(delete-region (point-min) (match-beginning 0))
	(when (re-search-forward "^-+END" nil t)
	  (delete-region (progn (end-of-line) (point))
			 (point-max)))
	(insert "\n")
	(with-temp-buffer
	  (insert-buffer-substring pgg-output-buffer)
	  (pgg-snarf-keys-region (point-min)(point-max)))))))


(provide 'pgg)

;;; pgg.el ends here
