;;; mime-conf.el --- mailcap parser and MIME playback configuration

;; Copyright (C) 1997,1998,1999,2000,2004 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Created: 1997-06-27
;; Original: 1997-06-27 mailcap.el by MORIOKA Tomohiko
;;	Renamed: 2000-11-24 to mime-conf.el by MORIOKA Tomohiko
;; Keywords: mailcap, setting, configuration, MIME, multimedia

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'mime-def)


;;; @ comment
;;;

(defsubst mime-mailcap-skip-comment ()
  (let ((chr (char-after (point))))
    (when (and chr
	       (or (= chr ?\n)
		   (= chr ?#)))
      (forward-line)
      t)))


;;; @ token
;;;

(defsubst mime-mailcap-look-at-token ()
  (if (looking-at mime-token-regexp)
      (let ((beg (match-beginning 0))
	    (end (match-end 0)))
	(goto-char end)
	(buffer-substring beg end)
	)))


;;; @ typefield
;;;

(defsubst mime-mailcap-look-at-type-field ()
  (let ((type (mime-mailcap-look-at-token)))
    (if type
	(if (eq (char-after (point)) ?/)
	    (progn
	      (forward-char)
	      (let ((subtype (mime-mailcap-look-at-token)))
		(if subtype
		    (cons (cons 'type (intern type))
			  (unless (string= subtype "*")
			    (list (cons 'subtype (intern subtype)))
			    )))))
	  (list (cons 'type (intern type)))
	  ))))


;;; @ field separator
;;;

(defsubst mime-mailcap-skip-field-separator ()
  (let ((ret (looking-at "\\([ \t]\\|\\\\\n\\)*;\\([ \t]\\|\\\\\n\\)*")))
    (when ret
      (goto-char (match-end 0))
      t)))


;;; @ mtext
;;;

(defsubst mime-mailcap-look-at-schar ()
  (let ((chr (char-after (point))))
    (if (and chr
	     (>= chr ? )
	     (/= chr ?\;)
	     (/= chr ?\\)
	     )
	(prog1
	    chr
	  (forward-char)))))

(defsubst mime-mailcap-look-at-qchar ()
  (when (eq (char-after (point)) ?\\)
    (prog2
	(forward-char)
	(char-after (point))
      (forward-char))))

(defsubst mime-mailcap-look-at-mtext ()
  (let ((beg (point)))
    (while (or (mime-mailcap-look-at-qchar)
	       (mime-mailcap-look-at-schar)))
    (buffer-substring beg (point))
    ))


;;; @ field
;;;

(defsubst mime-mailcap-look-at-field ()
  (let ((token (mime-mailcap-look-at-token)))
    (if token
	(if (looking-at "[ \t]*=[ \t]*")
	    (let ((value (progn
			   (goto-char (match-end 0))
			   (mime-mailcap-look-at-mtext))))
	      (if value
		  (cons (intern token) value)
		))
	  (list (intern token))
	  ))))


;;; @ mailcap entry
;;;

(defun mime-mailcap-look-at-entry ()
  (let ((type (mime-mailcap-look-at-type-field)))
    (if (and type (mime-mailcap-skip-field-separator))
	(let ((view (mime-mailcap-look-at-mtext))
	      fields field)
	  (when view
	    (while (and (mime-mailcap-skip-field-separator)
			(setq field (mime-mailcap-look-at-field))
			)
	      (setq fields (cons field fields))
	      )
	    (nconc type
		   (list (cons 'view view))
		   fields))))))


;;; @ main
;;;

;;;###autoload
(defun mime-parse-mailcap-buffer (&optional buffer order)
  "Parse BUFFER as a mailcap, and return the result.
If optional argument ORDER is a function, result is sorted by it.
If optional argument ORDER is not specified, result is sorted original
order.  Otherwise result is not sorted."
  (save-excursion
    (if buffer
	(set-buffer buffer))
    (goto-char (point-min))
    (let (entries entry)
      (while (progn
	       (while (mime-mailcap-skip-comment))
	       (setq entry (mime-mailcap-look-at-entry))
	       )
	(setq entries (cons entry entries))
	(forward-line)
	)
      (cond ((functionp order) (sort entries order))
	    ((null order) (nreverse entries))
	    (t entries)
	    ))))


;;;###autoload
(defvar mime-mailcap-file "~/.mailcap"
  "*File name of user's mailcap file.")

;;;###autoload
(defun mime-parse-mailcap-file (&optional filename order)
  "Parse FILENAME as a mailcap, and return the result.
If optional argument ORDER is a function, result is sorted by it.
If optional argument ORDER is not specified, result is sorted original
order.  Otherwise result is not sorted."
  (or filename
      (setq filename mime-mailcap-file))
  (with-temp-buffer
    (insert-file-contents filename)
    (mime-parse-mailcap-buffer (current-buffer) order)
    ))


;;;###autoload
(defun mime-format-mailcap-command (mtext situation)
  "Return formated command string from MTEXT and SITUATION.

MTEXT is a command text of mailcap specification, such as
view-command.

SITUATION is an association-list about information of entity.  Its key
may be:

	'type		primary media-type
	'subtype	media-subtype
	'filename	filename
	STRING		parameter of Content-Type field"
  (let ((i 0)
	(len (length mtext))
	(p 0)
	dest)
    (while (< i len)
      (let ((chr (aref mtext i)))
	(cond ((eq chr ?%)
	       (setq i (1+ i)
		     chr (aref mtext i))
	       (cond ((eq chr ?s)
		      (let ((file (cdr (assq 'filename situation))))
			(if (null file)
			    (error "'filename is not specified in situation.")
			  (setq dest (concat dest
					     (substring mtext p (1- i))
					     (shell-quote-argument file))
				i (1+ i)
				p i)
			  )))
		     ((eq chr ?t)
		      (let ((type (or (mime-type/subtype-string
				       (cdr (assq 'type situation))
				       (cdr (assq 'subtype situation)))
				      "text/plain")))
			(setq dest (concat dest
					   (substring mtext p (1- i))
					   type)
			      i (1+ i)
			      p i)
			))
		     ((eq chr ?\{)
		      (setq i (1+ i))
		      (if (not (string-match "}" mtext i))
			  (error "parse error!!!")
			(let* ((me (match-end 0))
			       (attribute (substring mtext i (1- me)))
			       (parameter (cdr (assoc attribute situation))))
			  (if (null parameter)
			      (error "\"%s\" is not specified in situation."
				     attribute)
			    (setq dest (concat dest
					       (substring mtext p (- i 2))
					       parameter)
				  i me
				  p i)
			    )
			  )))
		     (t (error "Invalid sequence `%%%c'." chr))
		     ))
	      ((eq chr ?\\)
	       (setq dest (concat dest (substring mtext p i))
		     p (1+ i)
		     i (+ i 2))
	       )
	      (t (setq i (1+ i)))
	      )))
    (concat dest (substring mtext p))
    ))


;;; @ end
;;;

(provide 'mime-conf)

;;; mime-conf.el ends here
