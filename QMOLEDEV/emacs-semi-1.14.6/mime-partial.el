;;; mime-partial.el --- Grabbing all MIME "message/partial"s.

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: OKABE Yasuo @ Kyoto University
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: message/partial, MIME, multimedia, mail, news

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

(require 'mime-view)
(require 'mime-play)

(defun mime-combine-message/partial-pieces-automatically (entity situation)
  "Internal method for mime-view to combine message/partial messages
automatically."
  (interactive)
  (let* ((id (cdr (assoc "id" situation)))
	 (target (cdr (assq 'major-mode situation)))
	 (subject-buf (eval (cdr (assq 'summary-buffer-exp situation))))
	 (mother (current-buffer))
	 subject-id
	 (root-dir (expand-file-name
		    (concat "m-prts-" (user-login-name))
		    temporary-file-directory))
	 (request-partial-message-method
	  (cdr (assq 'request-partial-message-method situation)))
	 full-file)
    (setq root-dir (concat root-dir "/" (replace-as-filename id)))
    (setq full-file (concat root-dir "/FULL"))
    
    (if (null target)
	(error "%s is not supported. Sorry." target)
      )
    
    ;; if you can't parse the subject line, try simple decoding method
    (if (or (file-exists-p full-file)
	    (not (y-or-n-p "Merge partials?"))
	    )
	(mime-store-message/partial-piece entity situation)
      (setq subject-id (mime-entity-read-field entity 'Subject))
      (if (string-match "[0-9\n]+" subject-id)
	  (setq subject-id (substring subject-id 0 (match-beginning 0)))
	)
      (save-excursion
	(set-buffer subject-buf)
	(while (search-backward subject-id nil t))
	(catch 'tag
	  (while t
	    (let* ((message
		    ;; request message at the cursor in Subject buffer.
		    (save-window-excursion
		      (funcall request-partial-message-method)
		      ))
		   (situation (mime-entity-situation message))
		   (the-id (cdr (assoc "id" situation))))
	      (when (string= the-id id)
		(with-current-buffer mother
		  (mime-store-message/partial-piece message situation)
		  )
		(if (file-exists-p full-file)
		    (throw 'tag nil)
		  ))
	      (if (not (progn
			 (end-of-line)
			 (search-forward subject-id nil t)
			 ))
		  (error "not found")
		)
	      ))
	  )))))


;;; @ end
;;;

(provide 'mime-partial)

(run-hooks 'mime-partial-load-hook)

;;; mime-partial.el ends here
