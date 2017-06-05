;;; mime-bbdb.el --- SEMI shared module for BBDB

;; Copyright (C) 1995,1996,1997 Shuhei KOBAYASHI
;; Copyright (C) 1997,1998 MORIOKA Tomohiko

;; Author: Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;; Maintainer: Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;; Keywords: BBDB, MIME, multimedia, multilingual, mail, news

;; This file is part of SEMI (Suite of Emacs MIME Interfaces).

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

(require 'path-util)
(require 'std11)
(require 'mime-view)

(if (module-installed-p 'bbdb-com)
    (require 'bbdb-com)
  (eval-when-compile
    ;; imported from bbdb-1.51
    (defmacro bbdb-pop-up-elided-display ()
      '(if (boundp 'bbdb-pop-up-elided-display)
	   bbdb-pop-up-elided-display
	 bbdb-elided-display))
    (defmacro bbdb-user-mail-names ()
      "Returns a regexp matching the address of the logged-in user"
      '(or bbdb-user-mail-names
	   (setq bbdb-user-mail-names
		 (concat "\\b" (regexp-quote (user-login-name)) "\\b"))))
    ))


;;; @ User Variables
;;;

(defvar mime-bbdb/use-mail-extr t
  "*If non-nil, `mail-extract-address-components' is used.
Otherwise `mime-bbdb/extract-address-components' overrides it.")

(defvar mime-bbdb/auto-create-p nil
  "*If t, create new BBDB records automatically.
If function, then it is called with no arguments to decide whether an
entry should be automatically creaded.

mime-bbdb uses this variable instead of `bbdb/mail-auto-create-p' or
`bbdb/news-auto-create-p' unless other tm-MUA overrides it.")

(defvar mime-bbdb/delete-empty-window nil
  "*If non-nil, delete empty BBDB window.
All bbdb-MUAs but bbdb-gnus display BBDB window even if it is empty.
If you prefer behavior of bbdb-gnus, set this variable to t.

For framepop users: If empty, `framepop-banish' is used instead.")

;;; @ mail-extr
;;;

(defun mime-bbdb/extract-address-components (str)
  (let* ((ret     (std11-extract-address-components str))
         (phrase  (car ret))
         (address (car (cdr ret)))
         (methods mime-bbdb/canonicalize-full-name-methods))
    (while (and phrase methods)
      (setq phrase  (funcall (car methods) phrase)
            methods (cdr methods)))
    (if (string= address "") (setq address nil))
    (if (string= phrase "") (setq phrase nil))
    (list phrase address)
    ))

(or mime-bbdb/use-mail-extr
    (progn
      (require 'mail-extr) ; for `what-domain'
      (or (fboundp 'tm:mail-extract-address-components)
          (fset 'tm:mail-extract-address-components
                (symbol-function 'mail-extract-address-components)))
      (fset 'mail-extract-address-components
	    (symbol-function 'mime-bbdb/extract-address-components))
      ))


;;; @ bbdb-extract-field-value
;;;

(or (fboundp 'tm:bbdb-extract-field-value)
    (progn
      ;; (require 'bbdb-hooks) ; not provided.
      ;; (or (fboundp 'bbdb-extract-field-value) ; defined as autoload

      ;; almost BBDB functions are autoloaded.
      ;; (or (fboundp 'bbdb-header-start)
      (or (and (fboundp 'bbdb-extract-field-value)
	       (not (eq 'autoload (car-safe (symbol-function
					     'bbdb-extract-field-value)))))
	  (load "bbdb-hooks"))
      (fset 'tm:bbdb-extract-field-value
	    (symbol-function 'bbdb-extract-field-value))
      (defun bbdb-extract-field-value (field)
        (let ((value (tm:bbdb-extract-field-value field)))
          (and value
               (eword-decode-string value))))
      ))


;;; @ full-name canonicalization methods
;;;

(defun mime-bbdb/canonicalize-spaces (str)
  (let (dest)
    (while (string-match "\\s +" str)
      (setq dest (cons (substring str 0 (match-beginning 0)) dest))
      (setq str (substring str (match-end 0)))
      )
    (or (string= str "")
        (setq dest (cons str dest)))
    (setq dest (nreverse dest))
    (mapconcat 'identity dest " ")
    ))

(defun mime-bbdb/canonicalize-dots (str)
  (let (dest)
    (while (string-match "\\." str)
      (setq dest (cons (substring str 0 (match-end 0)) dest))
      (setq str (substring str (match-end 0)))
      )
    (or (string= str "")
        (setq dest (cons str dest)))
    (setq dest (nreverse dest))
    (mapconcat 'identity dest " ")
    ))

(defvar mime-bbdb/canonicalize-full-name-methods
  '(eword-decode-string
    mime-bbdb/canonicalize-dots
    mime-bbdb/canonicalize-spaces))


;;; @ BBDB functions for mime-view-mode
;;;

(defun mime-bbdb/update-record (&optional offer-to-create)
  "Return the record corresponding to the current MIME previewing message.
Creating or modifying it as necessary. A record will be created if
mime-bbdb/auto-create-p is non-nil, or if OFFER-TO-CREATE is non-nil and
the user confirms the creation."
  (save-excursion
    (if (and mime-preview-buffer
             (get-buffer mime-preview-buffer))
        (set-buffer mime-preview-buffer))
    (if bbdb-use-pop-up
        (mime-bbdb/pop-up-bbdb-buffer offer-to-create)
      (let* ((message (get-text-property (point-min) 'mime-view-entity))
	     (from (mime-entity-fetch-field message 'From))
	     addr)
	(if (or (null from)
                (null (setq addr (car (mime-entity-read-field message 'From))))
                (string-match (bbdb-user-mail-names)
			      (std11-address-string addr)))
            (setq from (or (mime-entity-fetch-field message 'To)
			   from))
	  )
        (if from
            (bbdb-annotate-message-sender
             (mime-decode-field-body from 'From) t
             (or (bbdb-invoke-hook-for-value mime-bbdb/auto-create-p)
                 offer-to-create)
             offer-to-create))
        ))))

(defun mime-bbdb/annotate-sender (string)
  "Add a line to the end of the Notes field of the BBDB record
corresponding to the sender of this message."
  (interactive
   (list (if bbdb-readonly-p
             (error "The Insidious Big Brother Database is read-only.")
           (read-string "Comments: "))))
  (bbdb-annotate-notes (mime-bbdb/update-record t) string))

(defun mime-bbdb/edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (let ((record (or (mime-bbdb/update-record t)
                    (error ""))))
    (bbdb-display-records (list record))
    (if arg
	(bbdb-record-edit-property record nil t)
      (bbdb-record-edit-notes record t))))

(defun mime-bbdb/show-sender ()
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings."
  (interactive)
  (let ((record (mime-bbdb/update-record t)))
    (if record
	(bbdb-display-records (list record))
	(error "unperson"))))

(defun mime-bbdb/pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the MIME preview window(s),
displaying the record corresponding to the sender of the current message."
  (let ((framepop (eq temp-buffer-show-function 'framepop-display-buffer)))
    (or framepop
        (bbdb-pop-up-bbdb-buffer
         (function
          (lambda (w)
            (let ((b (current-buffer)))
              (set-buffer (window-buffer w))
              (prog1 (eq major-mode 'mime-view-mode)
                (set-buffer b)))))))
    (let ((bbdb-gag-messages t)
          (bbdb-use-pop-up nil)
          (bbdb-electric-p nil))
      (let ((record (mime-bbdb/update-record offer-to-create))
            (bbdb-elided-display (bbdb-pop-up-elided-display))
            (b (current-buffer)))
        (if framepop
            (if record
                (bbdb-display-records (list record))
              (framepop-banish))
          (bbdb-display-records (if record (list record) nil))
          (if (and (null record)
                   mime-bbdb/delete-empty-window)
              (delete-windows-on (get-buffer "*BBDB*"))))
        (set-buffer b)
        record))))

(defun mime-bbdb/define-keys ()
  (let ((mime-view-mode-map (current-local-map)))
    (define-key mime-view-mode-map ";" 'mime-bbdb/edit-notes)
    (define-key mime-view-mode-map ":" 'mime-bbdb/show-sender)
    ))

(add-hook 'mime-view-define-keymap-hook 'mime-bbdb/define-keys)


;;; @ for signature.el
;;;

(defun signature/get-bbdb-sigtype (addr)
  "Extract sigtype information from BBDB."
  (let ((record (bbdb-search-simple nil addr)))
    (and record
         (bbdb-record-getprop record 'sigtype))
    ))

(defun signature/set-bbdb-sigtype (sigtype addr)
  "Add sigtype information to BBDB."
  (let* ((bbdb-notice-hook nil)
         (record (bbdb-annotate-message-sender
                  addr t
                  (bbdb-invoke-hook-for-value
                   bbdb/mail-auto-create-p)
                  t)))
    (if record
        (progn
          (bbdb-record-putprop record 'sigtype sigtype)
          (bbdb-change-record record nil))
      )))

(defun signature/get-sigtype-from-bbdb (&optional verbose)
  (let* ((to (std11-field-body "To"))
         (addr (and to
                    (car (cdr (mail-extract-address-components to)))))
         (sigtype (signature/get-bbdb-sigtype addr))
         return
         )
    (if addr
        (if verbose
            (progn
              (setq return (signature/get-sigtype-interactively sigtype))
              (if (and (not (string-equal return sigtype))
                       (y-or-n-p
                        (format "Register \"%s\" for <%s>? " return addr))
                       )
                  (signature/set-bbdb-sigtype return addr)
                )
              return)
          (or sigtype
              (signature/get-signature-file-name))
          ))
    ))


;;; @ end
;;;

(provide 'mime-bbdb)

(run-hooks 'mime-bbdb-load-hook)

;;; end of mime-bbdb.el
