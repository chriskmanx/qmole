;;; poe-xemacs.el --- poe submodule for XEmacs

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995,1996,1997,1998 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, XEmacs

;; This file is part of APEL (A Portable Emacs Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Code:

(require 'pym)


;;; @ color
;;;

(defun-maybe set-cursor-color (color-name)
  "Set the text cursor color of the selected frame to COLOR.
When called interactively, prompt for the name of the color to use."
  (interactive "sColor: ")
  (set-frame-property (selected-frame) 'cursor-color
                      (if (color-instance-p color-name)
                          color-name
                        (make-color-instance color-name))))


;;; @ face
;;;

(defalias-maybe 'face-list 'list-faces)

(or (memq 'underline (face-list))
    (and (fboundp 'make-face)
	 (make-face 'underline)))

(or (face-differs-from-default-p 'underline)
    (set-face-underline-p 'underline t))


;;; @ overlay
;;;

(condition-case nil
    (require 'overlay)
  (error
   (defalias 'make-overlay 'make-extent)
   (defalias 'overlayp 'extentp)
   (defalias 'overlay-put 'set-extent-property)
   (defalias 'overlay-buffer 'extent-buffer)
   (defun move-overlay (extent start end &optional buffer)
     (set-extent-endpoints extent start end))
   (defalias 'delete-overlay 'detach-extent)))


;;; @ dired
;;;

(defun-maybe dired-other-frame (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but makes a new frame."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (switch-to-buffer-other-frame (dired-noselect dirname switches)))


;;; @ timer
;;;

(condition-case nil
    (require 'timer-funcs)
  (error nil))
(condition-case nil
    (require 'timer)
  (error nil))
(or
 (or (featurep 'timer-funcs) (featurep 'timer))
 (progn
   (require 'itimer)
   (if (and (= emacs-major-version 19) (<= emacs-minor-version 14))
       (defun-maybe run-at-time (time repeat function &rest args)
	 (start-itimer (make-temp-name "rat")
		       `(lambda ()
			  (,function ,@args))
		       time repeat))
     (defun-maybe run-at-time (time repeat function &rest args)
       "Function emulating the function of the same name of Emacs.
TIME should be nil meaning now, or a number of seconds from now.
Return an itimer object which can be used in either `delete-itimer'
or `cancel-timer'."
       (apply #'start-itimer "run-at-time"
	      function (if time (max time 1e-9) 1e-9)
	      repeat nil t args)))
   (defalias 'cancel-timer 'delete-itimer)
   (defun with-timeout-handler (tag)
     (throw tag 'timeout))
   (defmacro-maybe with-timeout (list &rest body)
     (let ((seconds (car list))
	   (timeout-forms (cdr list)))
       `(let ((with-timeout-tag (cons nil nil))
	      with-timeout-value with-timeout-timer)
	  (if (catch with-timeout-tag
		(progn
		  (setq with-timeout-timer
			(run-at-time ,seconds nil
				     'with-timeout-handler
				     with-timeout-tag))
		  (setq with-timeout-value (progn . ,body))
		  nil))
	      (progn . ,timeout-forms)
	    (cancel-timer with-timeout-timer)
	    with-timeout-value))))))

(require 'broken)

(broken-facility run-at-time-tick-tock
  "`run-at-time' is not punctual."
  ;; Note that it doesn't support XEmacsen prior to the version 19.15
  ;; since `start-itimer' doesn't pass arguments to a timer function.
  (or (and (= emacs-major-version 19) (<= emacs-minor-version 14))
      (condition-case nil
	  (progn
	    (unless (or itimer-process itimer-timer)
	      (itimer-driver-start))
	    ;; Check whether there is a bug to which the difference of
	    ;; the present time and the time when the itimer driver was
	    ;; woken up is subtracted from the initial itimer value.
	    (let* ((inhibit-quit t)
		   (ctime (current-time))
		   (itimer-timer-last-wakeup
		    (prog1
			ctime
		      (setcar ctime (1- (car ctime)))))
		   (itimer-list nil)
		   (itimer (start-itimer "run-at-time" 'ignore 5)))
	      (sleep-for 0.1) ;; Accept the timeout interrupt.
	      (prog1
		  (> (itimer-value itimer) 0)
		(delete-itimer itimer))))
	(error nil))))

(when-broken run-at-time-tick-tock
  (defalias 'run-at-time
    (lambda (time repeat function &rest args)
      "Function emulating the function of the same name of Emacs.
It works correctly for TIME even if there is a bug in the XEmacs core.
TIME should be nil meaning now, or a number of seconds from now.
Return an itimer object which can be used in either `delete-itimer'
or `cancel-timer'."
      (let ((itimers (list nil)))
	(setcar
	 itimers
	 (apply #'start-itimer "fixed-run-at-time"
		(lambda (itimers repeat function &rest args)
		  (let ((itimer (car itimers)))
		    (if repeat
			(progn
			  (set-itimer-function
			   itimer
			   (lambda (itimer repeat function &rest args)
			     (set-itimer-restart itimer repeat)
			     (set-itimer-function itimer function)
			     (set-itimer-function-arguments itimer args)
			     (apply function args)))
			  (set-itimer-function-arguments
			   itimer
			   (append (list itimer repeat function) args)))
		      (set-itimer-function
		       itimer
		       (lambda (itimer function &rest args)
			 (delete-itimer itimer)
			 (apply function args)))
		      (set-itimer-function-arguments
		       itimer
		       (append (list itimer function) args)))))
		1e-9 (if time (max time 1e-9) 1e-9)
		nil t itimers repeat function args))))))


;;; @ to avoid bug of XEmacs 19.14
;;;

(or (string-match "^../"
		  (file-relative-name "/usr/local/share" "/usr/local/lib"))
    ;; This function was imported from Emacs 19.33.
    (defun file-relative-name (filename &optional directory)
      "Convert FILENAME to be relative to DIRECTORY
(default: default-directory)."
      (setq filename (expand-file-name filename)
	    directory (file-name-as-directory
		       (expand-file-name
			(or directory default-directory))))
      (let ((ancestor ""))
	(while (not (string-match (concat "^" (regexp-quote directory))
				  filename))
	  (setq directory (file-name-directory (substring directory 0 -1))
		ancestor (concat "../" ancestor)))
	(concat ancestor (substring filename (match-end 0))))))


;;; @ Emacs 20.3 emulation
;;;

(defalias-maybe 'line-beginning-position 'point-at-bol)
(defalias-maybe 'line-end-position 'point-at-eol)

;;; @ XEmacs 21 emulation
;;;

;; XEmacs 20.5 and later: (set-extent-properties EXTENT PLIST)
(defun-maybe set-extent-properties (extent plist)
  "Change some properties of EXTENT.
PLIST is a property list.
For a list of built-in properties, see `set-extent-property'."
  (while plist
    (set-extent-property extent (car plist) (cadr plist))
    (setq plist (cddr plist))))  

;;; @ end
;;;

(require 'product)
(product-provide (provide 'poe-xemacs) (require 'apel-ver))

;;; poe-xemacs.el ends here
