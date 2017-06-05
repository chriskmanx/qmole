;;; semi-def.el --- definition module for SEMI -*- coding: iso-8859-4; -*-

;; Copyright (C) 1995,96,97,98,99,2000,01,03 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: definition, MIME, multimedia, mail, news

;; This file is part of SEMI (Sample of Emacs MIME Implementation).

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

(eval-when-compile (require 'cl))

(require 'custom)

(defconst mime-user-interface-product ["SEMI" (1 14 6) "Maruoka"]
  "Product name, version number and code name of MIME-kernel package.")

(autoload 'mule-caesar-region "mule-caesar"
  "Caesar rotation of current region." t)


;;; @ constants
;;;

(defconst mime-echo-buffer-name "*MIME-echo*"
  "Name of buffer to display MIME-playing information.")

(defconst mime-temp-buffer-name " *MIME-temp*")


;;; @ button
;;;

(defcustom mime-button-face 'bold
  "Face used for content-button or URL-button of MIME-Preview buffer."
  :group 'mime
  :type 'face)

(defcustom mime-button-mouse-face 'highlight
  "Face used for MIME-preview buffer mouse highlighting."
  :group 'mime
  :type 'face)

(defsubst mime-add-button (from to function &optional data)
  "Create a button between FROM and TO with callback FUNCTION and DATA."
  (and mime-button-face
       (put-text-property from to 'face mime-button-face))
  (and mime-button-mouse-face
       (put-text-property from to 'mouse-face mime-button-mouse-face))
  (put-text-property from to 'mime-button-callback function)
  (and data
       (put-text-property from to 'mime-button-data data))
  )

(defsubst mime-insert-button (string function &optional data)
  "Insert STRING as button with callback FUNCTION and DATA."
  (save-restriction
    (narrow-to-region (point)(point))
    (insert (concat "[" string "]\n"))
    (mime-add-button (point-min)(point-max) function data)
    ))

(defvar mime-button-mother-dispatcher nil)

(defun mime-button-dispatcher (event)
  "Select the button under point."
  (interactive "e")
  (let (buf point func data)
    (save-window-excursion
      (mouse-set-point event)
      (setq buf (current-buffer)
	    point (point)
	    func (get-text-property (point) 'mime-button-callback)
	    data (get-text-property (point) 'mime-button-data)
	    ))
    (save-excursion
      (set-buffer buf)
      (goto-char point)
      (if func
	  (apply func data)
	(if (fboundp mime-button-mother-dispatcher)
	    (funcall mime-button-mother-dispatcher event)
	  )))))


;;; @ for URL
;;;

(defcustom mime-browse-url-regexp
  (concat "\\(https?\\|ftps?\\|file\\|gopher\\|news\\|nntps?\\|telnets?\\|wais\\|mailto\\):"
	  "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
	  "[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]")
  "*Regexp to match URL in text body."
  :group 'mime
  :type 'regexp)

(defcustom mime-browse-url-function (function browse-url)
  "*Function to browse URL."
  :group 'mime
  :type 'function)

(defsubst mime-add-url-buttons ()
  "Add URL-buttons for text body."
  (goto-char (point-min))
  (while (re-search-forward mime-browse-url-regexp nil t)
    (let ((beg (match-beginning 0))
	  (end (match-end 0)))
      (mime-add-button beg end mime-browse-url-function
		       (list (buffer-substring beg end))))))


;;; @ menu
;;;

(static-cond ((featurep 'xemacs)
	      (defun mime-should-use-popup-menu ()
		(and window-system
		     (mouse-event-p last-command-event)))
	      (defun mime-select-menu-alist (title menu-alist)
		(if (mime-should-use-popup-menu)
		    (let (ret)
		      (popup-menu
		       (list* title
			      "---"
			      (mapcar (function
				       (lambda (cell)
					 (vector (car cell)
						 `(progn
						    (setq ret ',(cdr cell))
						    (throw 'exit nil))
						 t)))
				      menu-alist)))
		      (recursive-edit)
		      ret)
		  (cdr
		   (assoc (completing-read (concat title " : ") menu-alist)
			  menu-alist)))))
	     (t
	      (defun mime-should-use-popup-menu ()
		(and window-system
		     (memq (event-basic-type last-command-event)
			   '(mouse-1 mouse-2 mouse-3))))
	      (defun mime-select-menu-alist (title menu-alist)
		(if (mime-should-use-popup-menu)
		    (x-popup-menu
		     (list '(1 1) (selected-window))
		     (list title (cons title menu-alist)))
		  (cdr
		   (assoc (completing-read (concat title " : ") menu-alist)
			  menu-alist))))))

;;; @ Other Utility
;;;

(defvar mime-condition-type-alist
  '((preview . mime-preview-condition)
    (action . mime-acting-condition)))

(defvar mime-condition-mode-alist
  '((with-default . ctree-set-calist-with-default)
    (t . ctree-set-calist-strictly)))

(defun mime-add-condition (target-type condition &optional mode file)
  "Add CONDITION to database specified by TARGET-TYPE.
TARGET-TYPE must be 'preview or 'action.  
If optional argument MODE is 'strict or nil (omitted), CONDITION is
added strictly.
If optional argument MODE is 'with-default, CONDITION is added with
default rule.
If optional argument FILE is specified, it is loaded when CONDITION is
activate."
  (let ((sym (cdr (assq target-type mime-condition-type-alist))))
    (if sym
	(let ((func (cdr (or (assq mode mime-condition-mode-alist)
			     (assq t mime-condition-mode-alist)))))
	  (if (fboundp func)
	      (progn
		(funcall func sym condition)
		(if file
		    (let ((method (cdr (assq 'method condition))))
		      (autoload method file)
		      ))
		)
	    (error "Function for mode `%s' is not found." mode)
	    ))
      (error "Variable for target-type `%s' is not found." target-type)
      )))


;;; @ end
;;;

(provide 'semi-def)

;;; semi-def.el ends here
