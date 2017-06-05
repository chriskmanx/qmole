;;; mime-pgp.el --- mime-view internal methods for PGP.

;; Copyright (C) 1995,1996,1997,1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;	Daiki Ueno <ueno@ueda.info.waseda.ac.jp>
;; Created: 1995/12/7
;;	Renamed: 1997/2/27 from tm-pgp.el
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

;;; Commentary:

;;    This module is based on

;;	[security-multipart] RFC 1847: "Security Multiparts for MIME:
;;	    Multipart/Signed and Multipart/Encrypted" by
;;          Jim Galvin <galvin@tis.com>, Sandy Murphy <sandy@tis.com>,
;;	    Steve Crocker <crocker@cybercash.com> and
;;          Ned Freed <ned@innosoft.com> (1995/10)

;;	[PGP/MIME] RFC 2015: "MIME Security with Pretty Good Privacy
;;	    (PGP)" by Michael Elkins <elkins@aero.org> (1996/6)

;;	[PGP-kazu] draft-kazu-pgp-mime-00.txt: "PGP MIME Integration"
;;	    by Kazuhiko Yamamoto <kazu@is.aist-nara.ac.jp> (1995/10;
;;	    expired)

;;	[OpenPGP/MIME] draft-yamamoto-openpgp-mime-00.txt: "MIME
;;	    Security with OpenPGP (OpenPGP/MIME)" by Kazuhiko YAMAMOTO
;;	    <kazu@iijlab.net> (1998/1)

;;; Code:

(require 'mime-play)
(require 'pgg-def)

(autoload 'pgg-decrypt-region "pgg"
  "PGP decryption of current region." t)
(autoload 'pgg-verify-region "pgg"
  "PGP verification of current region." t)
(autoload 'pgg-snarf-keys-region "pgg"
  "Snarf PGP public keys in current region." t)
(autoload 'smime-decrypt-region "smime"
  "S/MIME decryption of current region.")
(autoload 'smime-verify-region "smime"
  "S/MIME verification of current region.")
(defvar smime-output-buffer)
(defvar smime-errors-buffer)


;;; @ Internal method for multipart/signed
;;;
;;; It is based on RFC 1847 (security-multipart).

(defun mime-verify-multipart/signed (entity situation)
  "Internal method to verify multipart/signed."
  (mime-play-entity
   (nth 1 (mime-entity-children entity)) ; entity-info of signature
   (list (assq 'mode situation)) ; play-mode
   ))


;;; @ internal method for application/pgp
;;;
;;; It is based on draft-kazu-pgp-mime-00.txt (PGP-kazu).

(defun mime-view-application/pgp (entity situation)
  (let* ((p-win (or (get-buffer-window (current-buffer))
		    (get-largest-window)))
	 (new-name
	  (format "%s-%s" (buffer-name) (mime-entity-number entity)))
	 (mother (current-buffer))
	 (preview-buffer (concat "*Preview-" (buffer-name) "*"))
	 representation-type message-buf)
    (set-buffer (setq message-buf (get-buffer-create new-name)))
    (erase-buffer)
    (mime-insert-entity entity)
    (cond ((progn
	     (goto-char (point-min))
	     (re-search-forward "^-+BEGIN PGP SIGNED MESSAGE-+$" nil t))
	   (pgg-verify-region (match-beginning 0)(point-max) nil 'fetch)
	   (goto-char (point-min))
	   (delete-region
	    (point-min)
	    (and
	     (re-search-forward "^-+BEGIN PGP SIGNED MESSAGE-+\n\n")
	     (match-end 0)))
	   (delete-region
	    (and (re-search-forward "^-+BEGIN PGP SIGNATURE-+")
		 (match-beginning 0))
	    (point-max))
	   (goto-char (point-min))
	   (while (re-search-forward "^- -" nil t)
	     (replace-match "-"))
	   (setq representation-type (if (mime-entity-cooked-p entity)
					 'cooked)))
	  ((progn
	     (goto-char (point-min))
	     (re-search-forward "^-+BEGIN PGP MESSAGE-+$" nil t))
	   (pgg-decrypt-region (point-min)(point-max))
	   (delete-region (point-min)(point-max))
	   (insert-buffer pgg-output-buffer)
	   (setq representation-type 'binary)))
    (setq major-mode 'mime-show-message-mode)
    (save-window-excursion
      (mime-view-buffer nil preview-buffer mother
			nil representation-type)
      (make-local-variable 'mime-view-temp-message-buffer)
      (setq mime-view-temp-message-buffer message-buf))
    (set-window-buffer p-win preview-buffer)))


;;; @ Internal method for application/pgp-signature
;;;
;;; It is based on RFC 2015 (PGP/MIME) and
;;; draft-yamamoto-openpgp-mime-00.txt (OpenPGP/MIME).

(defun mime-verify-application/pgp-signature (entity situation)
  "Internal method to check PGP/MIME signature."
  (let* ((entity-node-id (mime-entity-node-id entity))
	 (mother (mime-entity-parent entity))
	 (knum (car entity-node-id))
	 (onum (if (> knum 0)
		   (1- knum)
		 (1+ knum)))
	 (orig-entity (nth onum (mime-entity-children mother)))
	 (sig-file (make-temp-file "tm" nil ".asc")))
    (save-excursion 
      (mime-show-echo-buffer)
      (set-buffer mime-echo-buffer-name)
      (set-window-start 
       (get-buffer-window mime-echo-buffer-name)
       (point-max)))
    (mime-write-entity-content entity sig-file)
    (unwind-protect
	(with-temp-buffer
	  (mime-insert-entity orig-entity)
	  (goto-char (point-min))
	  (while (progn (end-of-line) (not (eobp)))
	    (insert "\r")
	    (forward-line 1))
	  (pgg-verify-region (point-min)(point-max) 
			     sig-file 'fetch)
	  (save-excursion 
	    (set-buffer mime-echo-buffer-name)
	    (insert-buffer-substring pgg-errors-buffer)))
      (delete-file sig-file))))


;;; @ Internal method for application/pgp-encrypted
;;;
;;; It is based on RFC 2015 (PGP/MIME) and
;;; draft-yamamoto-openpgp-mime-00.txt (OpenPGP/MIME).

(defun mime-decrypt-application/pgp-encrypted (entity situation)
  (let* ((entity-node-id (mime-entity-node-id entity))
	 (mother (mime-entity-parent entity))
	 (knum (car entity-node-id))
	 (onum (if (> knum 0)
		   (1- knum)
		 (1+ knum)))
	 (orig-entity (nth onum (mime-entity-children mother))))
    (mime-view-application/pgp orig-entity situation)))


;;; @ Internal method for application/pgp-keys
;;;
;;; It is based on RFC 2015 (PGP/MIME) and
;;; draft-yamamoto-openpgp-mime-00.txt (OpenPGP/MIME).

(defun mime-add-application/pgp-keys (entity situation)
  (save-excursion 
    (mime-show-echo-buffer)
    (set-buffer mime-echo-buffer-name)
    (set-window-start 
     (get-buffer-window mime-echo-buffer-name)
     (point-max)))
  (with-temp-buffer
    (mime-insert-entity-content entity)
    (mime-decode-region (point-min) (point-max)
                        (cdr (assq 'encoding situation)))
    (pgg-snarf-keys-region (point-min)(point-max))
    (save-excursion 
      (set-buffer mime-echo-buffer-name)
      (insert-buffer-substring pgg-errors-buffer))))


;;; @ Internal method for application/pkcs7-signature
;;;
;;; It is based on RFC 2633 (S/MIME version 3).

(defun mime-verify-application/pkcs7-signature (entity situation)
  "Internal method to check S/MIME signature."
  (let* ((entity-node-id (mime-entity-node-id entity))
	 (mother (mime-entity-parent entity))
	 (knum (car entity-node-id))
	 (onum (if (> knum 0)
		   (1- knum)
		 (1+ knum)))
	 (orig-entity (nth onum (mime-entity-children mother)))
	 (sig-file (make-temp-file "tm" nil ".asc"))
	 status)
    (save-excursion 
      (mime-show-echo-buffer)
      (set-buffer mime-echo-buffer-name)
      (set-window-start 
       (get-buffer-window mime-echo-buffer-name)
       (point-max)))
    (mime-write-entity entity sig-file)
    (unwind-protect
	(with-temp-buffer
	  (mime-insert-entity orig-entity)
	  (goto-char (point-min))
	  (while (progn (end-of-line) (not (eobp)))
	    (insert "\r")
	    (forward-line 1))
	  (setq status (smime-verify-region (point-min)(point-max) 
					    sig-file))
	  (save-excursion 
	    (set-buffer mime-echo-buffer-name)
	    (insert-buffer-substring (if status smime-output-buffer
				       smime-errors-buffer))))
      (delete-file sig-file))))


;;; @ Internal method for application/pkcs7-mime
;;;
;;; It is based on RFC 2633 (S/MIME version 3).

(defun mime-view-application/pkcs7-mime (entity situation)
  (let* ((p-win (or (get-buffer-window (current-buffer))
		    (get-largest-window)))
	 (new-name
	  (format "%s-%s" (buffer-name) (mime-entity-number entity)))
	 (mother (current-buffer))
	 (preview-buffer (concat "*Preview-" (buffer-name) "*"))
	 message-buf)
    (when (memq (or (cdr (assq 'smime-type situation)) 'enveloped-data)
		'(enveloped-data signed-data))
      (set-buffer (setq message-buf (get-buffer-create new-name)))
      (let ((inhibit-read-only t)
	    buffer-read-only)
	(erase-buffer)
	(mime-insert-entity entity)
	(smime-decrypt-region (point-min)(point-max))
	(delete-region (point-min)(point-max))
	(insert-buffer smime-output-buffer))
      (setq major-mode 'mime-show-message-mode)
      (save-window-excursion
	(mime-view-buffer nil preview-buffer mother
			  nil 'binary)
	(make-local-variable 'mime-view-temp-message-buffer)
	(setq mime-view-temp-message-buffer message-buf))
      (set-window-buffer p-win preview-buffer))))


;;; @ end
;;;

(provide 'mime-pgp)

(run-hooks 'mime-pgp-load-hook)

;;; mime-pgp.el ends here
