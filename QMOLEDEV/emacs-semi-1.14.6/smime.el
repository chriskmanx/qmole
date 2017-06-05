;;; smime.el --- S/MIME interface.

;; Copyright (C) 1999 Daiki Ueno

;; Author: Daiki Ueno <ueno@ueda.info.waseda.ac.jp>
;; Created: 1999/12/08
;; Keywords: S/MIME, OpenSSL

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

;;    This module is based on

;;      [SMIMEV3] RFC 2633: "S/MIME Version 3 Message Specification"
;;          by Crocker, D., Flanigan, B., Hoffman, P., Housley, R.,
;;          Pawling, J. and Schaad, J. (1999/06)

;;      [SMIMEV2] RFC 2311: "S/MIME Version 2 Message Specification"
;;          by Dusse, S., Hoffman, P., Ramsdell, B., Lundblade, L.
;;          and L. Repka. (1998/03)

;;; Code:

(require 'path-util)
(require 'mel)
;; binary-funcall, binary-write-decoded-region, binary-insert-encoded-file
(eval-when-compile (require 'static))

(defgroup smime ()
  "S/MIME interface"
  :group 'mime)

(defcustom smime-program "smime" 
  "The S/MIME executable."
  :group 'smime
  :type 'string)

(defcustom smime-shell-file-name "/bin/sh"
  "File name to load inferior shells from.  Bourne shell or its equivalent
\(not tcsh) is needed for \"2>\"."
  :group 'smime
  :type 'string)

(defcustom smime-shell-command-switch "-c"
  "Switch used to have the shell execute its command line argument."
  :group 'smime
  :type 'string)

(defcustom smime-x509-program
  (let ((file (exec-installed-p "openssl")))
    (and file (list file "x509" "-noout")))
  "External program for x509 parser."
  :group 'smime
  :type 'string)

(defcustom smime-cache-passphrase t
  "Cache passphrase."
  :group 'smime
  :type 'boolean)

(defcustom smime-certificate-directory "~/.w3/certs"
  "Certificate directory."
  :group 'smime
  :type 'directory)

(defcustom smime-public-key-file nil
  "Public key file."
  :group 'smime
  :type 'boolean)

(defcustom smime-private-key-file nil
  "Private key file."
  :group 'smime
  :type 'boolean)

(defvar smime-errors-buffer " *S/MIME errors*")
(defvar smime-output-buffer " *S/MIME output*")

;;; @ utility functions
;;;
(put 'smime-process-when-success 'lisp-indent-function 0)

(defmacro smime-process-when-success (&rest body)
  `(with-current-buffer smime-output-buffer
     (if (zerop (buffer-size)) nil ,@body t)))

(defvar smime-passphrase-cache-expiry 16)
(defvar smime-passphrase-cache (make-vector 7 0))

(defvar smime-read-passphrase nil)
(defun smime-read-passphrase (prompt &optional key)
  (if (not smime-read-passphrase)
      (if (functionp 'read-passwd)
	  (setq smime-read-passphrase 'read-passwd)
	(if (load "passwd" t)
	    (setq smime-read-passphrase 'read-passwd)
	  (autoload 'ange-ftp-read-passwd "ange-ftp")
	  (setq smime-read-passphrase 'ange-ftp-read-passwd))))
  (or (and smime-cache-passphrase
	   (symbol-value (intern-soft key smime-passphrase-cache)))
      (funcall smime-read-passphrase prompt)))

(defun smime-add-passphrase-cache (key passphrase)
  (set (intern key smime-passphrase-cache)
       passphrase)
  (run-at-time smime-passphrase-cache-expiry nil
	       #'smime-remove-passphrase-cache
	       key))

(defun smime-remove-passphrase-cache (key)
  (let ((passphrase (symbol-value (intern-soft key smime-passphrase-cache))))
    (when passphrase
      (fillarray passphrase ?_)
      (unintern key smime-passphrase-cache))))

(defsubst smime-parse-attribute (string)
  (delq nil (mapcar 
	     (lambda (attr)
	       (if (string-match "=" attr)
		   (cons (intern (substring attr 0 (match-beginning 0)))
			 (substring attr (match-end 0)))
		 nil))
	     (split-string string "/"))))

(defsubst smime-query-signer (start end)
  (smime-process-region start end smime-program (list "-qs"))
  (with-current-buffer smime-output-buffer
    (if (zerop (buffer-size)) nil
      (goto-char (point-min))
      (when (re-search-forward "^/" nil t)
	(smime-parse-attribute 
	 (buffer-substring (point) (progn (end-of-line)(point)))))
      )))

(defsubst smime-x509-hash (cert-file)
  (with-current-buffer (get-buffer-create smime-output-buffer)
    (buffer-disable-undo)
    (erase-buffer)
    (apply #'call-process (car smime-x509-program) nil t nil 
	   (append (cdr smime-x509-program) 
		   (list "-hash" "-in" cert-file)))
    (if (zerop (buffer-size)) nil
      (buffer-substring (point-min) (1- (point-max))))))

(defsubst smime-x509-subject (cert-file)
  (with-current-buffer (get-buffer-create smime-output-buffer)
    (buffer-disable-undo)
    (erase-buffer)
    (apply #'call-process (car smime-x509-program) nil t nil 
	   (append (cdr smime-x509-program)
		   (list "-subject" "-in" cert-file)))
    (if (zerop (buffer-size)) nil
      (goto-char (point-min))
      (when (re-search-forward "^subject=" nil t)
	(smime-parse-attribute
	 (buffer-substring (point)(progn (end-of-line)(point))))))))

(defsubst smime-find-certificate (attr)
  (let ((files
	 (and (file-directory-p smime-certificate-directory)
	      (delq nil (mapcar (lambda (file) 
				  (if (file-directory-p file) nil
				    file))
				(directory-files 
				 smime-certificate-directory
				 'full))))))
    (catch 'found
      (while files
	(if (or (string-equal 
		 (cdr (assq 'CN (smime-x509-subject (car files))))
		 (cdr (assq 'CN attr)))
		(string-equal
		 (cdr (assq 'Email (smime-x509-subject (car files))))
		 (cdr (assq 'Email attr))))
	    (throw 'found (car files)))
	(pop files)))))

(defun smime-process-region (start end program args)
  (let* ((errors-file-name (make-temp-file "smime-errors"))
	 (args (append args (list (concat "2>" errors-file-name))))
	 (shell-file-name smime-shell-file-name)
	 (shell-command-switch smime-shell-command-switch)
	 (process-connection-type nil)
	 process status exit-status)
    (with-current-buffer (get-buffer-create smime-output-buffer)
      (buffer-disable-undo)
      (erase-buffer))
    (setq process
	  (apply #'binary-funcall #'start-process-shell-command
		 "*S/MIME*" smime-output-buffer
		 program args))
    (set-process-sentinel process 'ignore)
    (process-send-region process start end)
    (process-send-eof process)
    (while (eq 'run (process-status process))
      (accept-process-output process 5))
    (setq status (process-status process)
	  exit-status (process-exit-status process))
    (delete-process process)
    (with-current-buffer smime-output-buffer
      (goto-char (point-min))
      (while (re-search-forward "\r$" (point-max) t)
	(replace-match ""))

      (if (memq status '(stop signal))
	  (error "%s exited abnormally: '%s'" program exit-status))
      (if (= 127 exit-status)
	  (error "%s could not be found" program))

      (set-buffer (get-buffer-create smime-errors-buffer))
      (buffer-disable-undo)
      (erase-buffer)
      (insert-file-contents errors-file-name)
      (delete-file errors-file-name)
      
      (if (and process (eq 'run (process-status process)))
	  (interrupt-process process))
      )
    ))

;;; @ interface functions
;;;

;;;###autoload
(defun smime-encrypt-region (start end)
  "Encrypt the current region between START and END."
  (let* ((key-file
	  (or smime-private-key-file
	      (expand-file-name (read-file-name "Public key file: "))))
	 (args (list "-e" key-file)))
    (smime-process-region start end smime-program args)
    (smime-process-when-success 
      (goto-char (point-min))
      (delete-region (point-min) (progn
				   (re-search-forward "^$" nil t)
				   (1+ (point)))))))

;;;###autoload
(defun smime-decrypt-region (start end)
  "Decrypt the current region between START and END."
  (let* ((key-file
	  (or smime-private-key-file
	      (expand-file-name (read-file-name "Private key file: "))))
	 (hash (smime-x509-hash key-file))
	 (passphrase (smime-read-passphrase 
		      (format "S/MIME passphrase for %s: " hash)
		      hash))
	 (args (list "-d" key-file passphrase)))
    (smime-process-region start end smime-program args)
    (smime-process-when-success 
      (when smime-cache-passphrase
	(smime-add-passphrase-cache hash passphrase)))))
	 
;;;###autoload
(defun smime-sign-region (start end &optional cleartext)
  "Make the signature from text between START and END.
If the optional 3rd argument CLEARTEXT is non-nil, it does not create
a detached signature."
  (let* ((key-file
	  (or smime-private-key-file
	      (expand-file-name (read-file-name "Private key file: "))))
	 (hash (smime-x509-hash key-file))
	 (passphrase (smime-read-passphrase 
		      (format "S/MIME passphrase for %s: " hash)
		      hash))
	 (args (list "-ds" key-file passphrase)))
    (smime-process-region start end smime-program args)
    (smime-process-when-success 
      (goto-char (point-min))
      (delete-region (point-min) (progn
				   (re-search-forward "^$" nil t)
				   (1+ (point))))
      (when smime-cache-passphrase
	(smime-add-passphrase-cache hash passphrase)))))

;;;###autoload
(defun smime-verify-region (start end signature)
  "Verify the current region between START and END.
If the optional 3rd argument SIGNATURE is non-nil, it is treated as
the detached signature of the current region."
  (let* ((orig-file (make-temp-file "smime"))
	 (orig-mode (default-file-modes)))
    (unwind-protect
	(progn
	  (set-default-file-modes 448)
	  (binary-write-decoded-region start end orig-file))
      (set-default-file-modes orig-mode))
    (with-temp-buffer
      (binary-insert-encoded-file signature)
      (goto-char (point-max))
      (binary-insert-encoded-file
       (or (smime-find-certificate 
	    (smime-query-signer (point-min)(point-max)))
	   (expand-file-name 
	    (read-file-name "Certificate file: "))))
      (smime-process-region (point-min)(point-max) smime-program 
			    (list "-dv" orig-file)))
    (smime-process-when-success nil)))

(provide 'smime)

;;; smime.el ends here
