;;; mime-play.el --- Playback processing module for mime-view.el

;; Copyright (C) 1994,95,96,97,98,99,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Created: 1995/9/26 (separated from tm-view.el)
;;	Renamed: 1997/2/21 from tm-play.el
;; Keywords: MIME, multimedia, mail, news

;; This file is part of SEMI (Secretariat of Emacs MIME Interfaces).

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

(require 'mime-view)
(require 'alist)
(require 'filename)

(eval-when-compile
  (condition-case nil
      (require 'bbdb)
    (error (defvar bbdb-buffer-name nil)))
  )

(defcustom mime-save-directory "~/"
  "*Name of the directory where MIME entity will be saved in.
If t, it means current directory."
  :group 'mime-view
  :type '(choice (const :tag "Current directory" t)
		 (directory)))

(defcustom mime-play-delete-file-immediately t
  "If non-nil, delete played file immediately."
  :group 'mime-view
  :type 'boolean)

(defvar mime-play-find-every-situations t
  "*Find every available situations if non-nil.")

(defvar mime-play-messages-coding-system nil
  "Coding system to be used for external MIME playback method.")


;;; @ content decoder
;;;

;;;###autoload
(defun mime-preview-play-current-entity (&optional ignore-examples mode)
  "Play current entity.
It decodes current entity to call internal or external method.  The
method is selected from variable `mime-acting-condition'.
If IGNORE-EXAMPLES (C-u prefix) is specified, this function ignores
`mime-acting-situation-example-list'.
If MODE is specified, play as it.  Default MODE is \"play\"."
  (interactive "P")
  (let ((entity (get-text-property (point) 'mime-view-entity)))
    (if entity
	(let ((situation
	       (get-text-property (point) 'mime-view-situation)))
	  (or mode
	      (setq mode "play"))
	  (setq situation 
		(if (assq 'mode situation)
		    (put-alist 'mode mode (copy-alist situation))
		  (cons (cons 'mode mode)
			situation)))
	  (if ignore-examples
	      (setq situation
		    (cons (cons 'ignore-examples ignore-examples)
			  situation)))
	  (mime-play-entity entity situation)
	  ))))

;;;###autoload
(defun mime-play-entity (entity &optional situation ignored-method)
  "Play entity specified by ENTITY.
It decodes the entity to call internal or external method.  The method
is selected from variable `mime-acting-condition'.  If MODE is
specified, play as it.  Default MODE is \"play\"."
  (let ((ret
	 (mime-unify-situations (mime-entity-situation entity situation)
				mime-acting-condition
				mime-acting-situation-example-list
				'method ignored-method
				mime-play-find-every-situations))
	method)
    (setq mime-acting-situation-example-list (cdr ret)
	  ret (car ret))
    (cond ((cdr ret)
	   (setq ret (mime-select-menu-alist
		      "Methods"
		      (mapcar (function
			       (lambda (situation)
				 (cons
				  (format "%s"
					  (cdr (assq 'method situation)))
				  situation)))
			      ret)))
	   (setq ret (mime-sort-situation ret))
	   (add-to-list 'mime-acting-situation-example-list (cons ret 0))
	   )
	  (t
	   (setq ret (car ret))
	   ))
    (setq method (cdr (assq 'method ret)))
    (cond ((and (symbolp method)
		(fboundp method))
	   (funcall method entity ret)
	   )
	  ((stringp method)
	   (mime-activate-mailcap-method entity ret)
	   )
          ;; ((and (listp method)(stringp (car method)))
          ;;  (mime-activate-external-method entity ret)
          ;;  )
	  (t
	   (mime-show-echo-buffer "No method are specified for %s\n"
				  (mime-type/subtype-string
				   (cdr (assq 'type situation))
				   (cdr (assq 'subtype situation))))
	   (if (y-or-n-p "Do you want to save current entity to disk?")
	       (mime-save-content entity situation))
	   ))
    ))


;;; @ external decoder
;;;

(defvar mime-mailcap-method-filename-alist nil)

(defun mime-activate-mailcap-method (entity situation)
  (let ((method (cdr (assoc 'method situation)))
	(name (mime-entity-safe-filename entity)))
    (setq name (expand-file-name (if (and name (not (string= name "")))
				     name
				   (make-temp-name "EMI"))
				 (make-temp-file "EMI" 'directory)))
    (mime-write-entity-content entity name)
    (message "External method is starting...")
    (let ((process
	   (let ((command
		  (mime-format-mailcap-command
		   method
		   (cons (cons 'filename name) situation)))
		 (coding-system-for-read mime-play-messages-coding-system))
	     (start-process command mime-echo-buffer-name
	      shell-file-name shell-command-switch command))))
      (set-alist 'mime-mailcap-method-filename-alist process name)
      (set-process-sentinel process 'mime-mailcap-method-sentinel))))

(defun mime-mailcap-method-sentinel (process event)
  (when mime-play-delete-file-immediately
    (let ((file (cdr (assq process mime-mailcap-method-filename-alist))))
      (when (file-exists-p file)
	(ignore-errors
	  (delete-file file)
	  (delete-directory (file-name-directory file)))))
    (remove-alist 'mime-mailcap-method-filename-alist process))
  (message "%s %s" process event))

(defun mime-mailcap-delete-played-files ()
  (dolist (elem mime-mailcap-method-filename-alist)
    (when (file-exists-p (cdr elem))
      (ignore-errors
	(delete-file (cdr elem))
	(delete-directory (file-name-directory (cdr elem)))))))

(add-hook 'kill-emacs-hook 'mime-mailcap-delete-played-files)

(defvar mime-echo-window-is-shared-with-bbdb
  (module-installed-p 'bbdb)
  "*If non-nil, mime-echo window is shared with BBDB window.")

(defvar mime-echo-window-height
  (function
   (lambda ()
     (/ (window-height) 5)
     ))
  "*Size of mime-echo window.
It allows function or integer.  If it is function,
`mime-show-echo-buffer' calls it to get height of mime-echo window.
Otherwise `mime-show-echo-buffer' uses it as height of mime-echo
window.")

(defun mime-show-echo-buffer (&rest forms)
  "Show mime-echo buffer to display MIME-playing information."
  (get-buffer-create mime-echo-buffer-name)
  (let ((the-win (selected-window))
	(win (get-buffer-window mime-echo-buffer-name)))
    (unless win
      (unless (and mime-echo-window-is-shared-with-bbdb
		   (condition-case nil
		       (setq win (get-buffer-window bbdb-buffer-name))
		     (error nil)))
	(select-window (get-buffer-window (or mime-preview-buffer
					      (current-buffer))))
	(setq win (split-window-vertically
		   (- (window-height)
		      (if (functionp mime-echo-window-height)
			  (funcall mime-echo-window-height)
			mime-echo-window-height)
		      )))
	)
      (set-window-buffer win mime-echo-buffer-name)
      )
    (select-window win)
    (goto-char (point-max))
    (if forms
	(let ((buffer-read-only nil))
	  (insert (apply (function format) forms))
	  ))
    (select-window the-win)
    ))


;;; @ file name
;;;

(defvar mime-view-file-name-char-regexp "[A-Za-z0-9+_-]")

(defvar mime-view-file-name-regexp-1
  (concat mime-view-file-name-char-regexp "+\\."
	  mime-view-file-name-char-regexp "+"))

(defvar mime-view-file-name-regexp-2
  (concat (regexp-* mime-view-file-name-char-regexp)
	  "\\(\\." mime-view-file-name-char-regexp "+\\)*"))

(defun mime-entity-safe-filename (entity)
  (let ((filename
	 (or (mime-entity-filename entity)
	     (let ((subj
		    (or (mime-entity-read-field entity 'Content-Description)
			(mime-entity-read-field entity 'Subject))))
	       (if (and subj
			(or (string-match mime-view-file-name-regexp-1 subj)
			    (string-match mime-view-file-name-regexp-2 subj)))
		   (substring subj (match-beginning 0)(match-end 0))
		 )))))
    (if filename
	(replace-as-filename filename)
      )))


;;; @ file extraction
;;;

(defun mime-save-content (entity situation)
  (let ((name (or (mime-entity-safe-filename entity)
		  (format "%s" (mime-entity-media-type entity))))
	(dir (if (eq t mime-save-directory)
		 default-directory
	       mime-save-directory))
	filename)
    (setq filename (read-file-name
		    (concat "File name: (default "
			    (file-name-nondirectory name) ") ")
		    dir
		    (concat (file-name-as-directory dir)
			    (file-name-nondirectory name))))
    (if (file-directory-p filename)
	(setq filename (concat (file-name-as-directory filename)
			       (file-name-nondirectory name))))
    (if (file-exists-p filename)
	(or (yes-or-no-p (format "File %s exists. Save anyway? " filename))
	    (error "")))
    (mime-write-entity-content entity (expand-file-name filename))
    ))


;;; @ file detection
;;;

(defvar mime-magic-type-alist
  '(("^\377\330\377[\340\356]..JFIF"	image jpeg)
    ("^\211PNG"				image png)
    ("^GIF8[79]"			image gif)
    ("^II\\*\000"			image tiff)
    ("^MM\000\\*"			image tiff)
    ("^MThd"				audio midi)
    ("^\000\000\001\263"		video mpeg)
    )
  "*Alist of regexp about magic-number vs. corresponding media-types.
Each element looks like (REGEXP TYPE SUBTYPE).
REGEXP is a regular expression to match against the beginning of the
content of entity.
TYPE is symbol to indicate primary type of media-type.
SUBTYPE is symbol to indicate subtype of media-type.")

(defun mime-detect-content (entity situation)
  (let (type subtype)
    (let ((mdata (mime-entity-content entity))
	  (rest mime-magic-type-alist))
      (while (not (let ((cell (car rest)))
		    (if cell
			(if (string-match (car cell) mdata)
			    (setq type (nth 1 cell)
				  subtype (nth 2 cell))
			  )
		      t)))
	(setq rest (cdr rest))))
    (setq situation (del-alist 'method (copy-alist situation)))
    (mime-play-entity entity
		      (if type
			  (put-alist 'type type
				     (put-alist 'subtype subtype
						situation))
			situation)
		      'mime-detect-content)))


;;; @ mail/news message
;;;

(defun mime-preview-quitting-method-for-mime-show-message-mode ()
  "Quitting method for mime-view.
It is registered to variable `mime-preview-quitting-method-alist'."
  (let ((mother mime-mother-buffer)
	(win-conf mime-preview-original-window-configuration))
    (if (and (boundp 'mime-view-temp-message-buffer)
	     (buffer-live-p mime-view-temp-message-buffer))
	(kill-buffer mime-view-temp-message-buffer))
    (mime-preview-kill-buffer)
    (set-window-configuration win-conf)
    (pop-to-buffer mother)))

(defun mime-view-message/rfc822 (entity situation)
  (let* ((new-name
	  (format "%s-%s" (buffer-name) (mime-entity-number entity)))
	 (mother (current-buffer))
	 (children (car (mime-entity-children entity)))
	 (preview-buffer
	  (mime-display-message
	   children new-name mother nil
	   (cdr (assq 'major-mode
		      (get-text-property (point) 'mime-view-situation))))))
    (or (get-buffer-window preview-buffer)
	(let ((m-win (get-buffer-window mother)))
	  (if m-win
	      (set-window-buffer m-win preview-buffer)
	    (switch-to-buffer preview-buffer)
	    )))))


;;; @ message/partial
;;;

(defun mime-require-safe-directory (dir)
  "Create a directory DIR safely.
The permission of the created directory becomes `700' (for the owner only).
If the directory already exists and is writable by other users, an error
occurs."
  (let ((attr (file-attributes dir))
	(orig-modes (default-file-modes)))
    (if (and attr (eq (car attr) t)) ; directory already exists.
	(unless (or (memq system-type '(windows-nt ms-dos OS/2 emx))
		    (and (eq (nth 2 attr) (user-real-uid))
			 (eq (file-modes dir) 448)))
	  (error "Invalid owner or permission for %s" dir))
      (unwind-protect
	  (progn
	    (set-default-file-modes 448)
	    (make-directory dir))
	(set-default-file-modes orig-modes)))))

(defun mime-store-message/partial-piece (entity cal)
  (let* ((root-dir
	  (expand-file-name
	   (concat "m-prts-" (user-login-name)) temporary-file-directory))
	 (id (cdr (assoc "id" cal)))
	 (number (cdr (assoc "number" cal)))
	 (total (cdr (assoc "total" cal)))
	 file
	 (mother (current-buffer))
	 (orig-modes (default-file-modes)))
    (mime-require-safe-directory root-dir)
    (or (file-exists-p root-dir)
	(unwind-protect
	    (progn
	      (set-default-file-modes 448)
	      (make-directory root-dir))
	  (set-default-file-modes orig-modes)))
    (setq id (replace-as-filename id))
    (setq root-dir (concat root-dir "/" id))

    (or (file-exists-p root-dir)
	(unwind-protect
	    (progn
	      (set-default-file-modes 448)
	      (make-directory root-dir))
	  (set-default-file-modes orig-modes)))

    (setq file (concat root-dir "/FULL"))
    (if (file-exists-p file)
	(let ((full-buf (get-buffer-create "FULL"))
	      (pwin (or (get-buffer-window mother)
			(get-largest-window)))
	      pbuf)
	  (save-window-excursion
	    (set-buffer full-buf)
	    (erase-buffer)
	    (binary-insert-encoded-file file)
	    (setq major-mode 'mime-show-message-mode)
	    (mime-view-buffer (current-buffer) nil mother)
	    (setq pbuf (current-buffer))
	    (make-local-variable 'mime-view-temp-message-buffer)
	    (setq mime-view-temp-message-buffer full-buf))
	  (set-window-buffer pwin pbuf)
	  (select-window pwin))
      (setq file (concat root-dir "/" number))
      (mime-write-entity-body entity file)
      (let ((total-file (concat root-dir "/CT")))
	(setq total
	      (if total
		  (progn
		    (or (file-exists-p total-file)
			(save-excursion
			  (set-buffer
			   (get-buffer-create mime-temp-buffer-name))
			  (erase-buffer)
			  (insert total)
			  (write-region (point-min)(point-max) total-file)
			  (kill-buffer (current-buffer))
			  ))
		    (string-to-number total)
		    )
		(and (file-exists-p total-file)
		     (save-excursion
		       (set-buffer (find-file-noselect total-file))
		       (prog1
			   (and (re-search-forward "[0-9]+" nil t)
				(string-to-number
				 (buffer-substring (match-beginning 0)
						   (match-end 0)))
				)
			 (kill-buffer (current-buffer))
			 )))
		)))
      (if (and total (> total 0)
	       (>= (length (directory-files root-dir nil "^[0-9]+$" t))
		   total))
	  (catch 'tag
	    (save-excursion
	      (set-buffer (get-buffer-create mime-temp-buffer-name))
	      (let ((full-buf (current-buffer)))
		(erase-buffer)
		(let ((i 1))
		  (while (<= i total)
		    (setq file (concat root-dir "/" (int-to-string i)))
		    (or (file-exists-p file)
			(throw 'tag nil)
			)
		    (binary-insert-encoded-file file)
		    (goto-char (point-max))
		    (setq i (1+ i))))
		(binary-write-decoded-region
		 (point-min)(point-max)
		 (expand-file-name "FULL" root-dir))
		(let ((i 1))
		  (while (<= i total)
		    (let ((file (format "%s/%d" root-dir i)))
		      (and (file-exists-p file)
			   (delete-file file)))
		    (setq i (1+ i))))
		(let ((file (expand-file-name "CT" root-dir)))
		  (and (file-exists-p file)
		       (delete-file file)))
		(let ((buf (current-buffer))
		      (pwin (or (get-buffer-window mother)
				(get-largest-window)))
		      (pbuf (mime-display-message
			     (mime-open-entity 'buffer (current-buffer))
			     nil mother nil 'mime-show-message-mode)))
		  (with-current-buffer pbuf
		    (make-local-variable 'mime-view-temp-message-buffer)
		    (setq mime-view-temp-message-buffer buf))
		  (set-window-buffer pwin pbuf)
		  (select-window pwin)
		  )))))
      )))


;;; @ message/external-body
;;;

(defvar mime-raw-dired-function
  (if (and (>= emacs-major-version 19) window-system)
      (function dired-other-frame)
    (function mime-raw-dired-function-for-one-frame)
    ))

(defun mime-raw-dired-function-for-one-frame (dir)
  (let ((win (or (get-buffer-window mime-preview-buffer)
		 (get-largest-window))))
    (select-window win)
    (dired dir)
    ))

(defun mime-view-message/external-anon-ftp (entity cal)
  (let* ((site (cdr (assoc "site" cal)))
	 (directory (cdr (assoc "directory" cal)))
	 (name (cdr (assoc "name" cal)))
	 (pathname (concat "/anonymous@" site ":" directory)))
    (message "%s" (concat "Accessing " (expand-file-name name pathname) "..."))
    (funcall mime-raw-dired-function pathname)
    (goto-char (point-min))
    (search-forward name)
    ))

(defvar mime-raw-browse-url-function mime-browse-url-function)

(defun mime-view-message/external-url (entity cal)
  (let ((url (cdr (assoc "url" cal))))
    (message "%s" (concat "Accessing " url "..."))
    (funcall mime-raw-browse-url-function url)))


;;; @ rot13-47
;;;

(defun mime-view-caesar (entity situation)
  "Internal method for mime-view to display ROT13-47-48 message."
  (let ((buf (get-buffer-create
	      (format "%s-%s" (buffer-name) (mime-entity-number entity)))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (mime-insert-text-content entity)
      (mule-caesar-region (point-min) (point-max))
      (set-buffer-modified-p nil)
      )
    (let ((win (get-buffer-window (current-buffer))))
      (or (eq (selected-window) win)
	  (select-window (or win (get-largest-window)))
	  ))
    (view-buffer buf)
    (goto-char (point-min))
    ))


;;; @ end
;;;

(provide 'mime-play)

;;; mime-play.el ends here
