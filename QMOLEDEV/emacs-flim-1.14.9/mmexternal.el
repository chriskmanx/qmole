;;; mmexternal.el --- MIME entity module for external buffer

;; Copyright (C) 1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: MIME, multimedia, mail, news

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

(require 'mmgeneric)
(require 'mime)

(eval-and-compile
  (luna-define-class mime-external-entity (mime-entity)
		     (body-buffer
		      body-file))
  (luna-define-internal-accessors 'mime-external-entity)

  ;; In an external entity, information of media-type or other
  ;; information which are represented in a header in a non-external
  ;; entity are in the body of the parent entity.
  )

(luna-define-method mime-entity-name ((entity mime-external-entity))
  (concat "child of "
	  (mime-entity-name
	   (mime-entity-parent-internal entity))))


(defun mmexternal-require-file-name (entity)
  (condition-case nil
      (or (mime-external-entity-body-file-internal entity)
	  (let* ((ct (mime-entity-content-type
		      (mime-entity-parent-internal entity)))
		 (access-type
		  (mime-content-type-parameter ct "access-type")))
	    (if (and access-type
		     (string= access-type "anon-ftp"))
		(let ((site (mime-content-type-parameter ct "site"))
		      (directory
		       (mime-content-type-parameter ct "directory"))
		      (name (mime-content-type-parameter ct "name")))
		  (mime-external-entity-set-body-file-internal
		   entity
		   (expand-file-name
		    name
		    (concat "/anonymous@" site ":"
			    (file-name-as-directory directory))))))))
    (error (message "Can't make file-name of external-body."))))

(defun mmexternal-require-buffer (entity)
  (unless (and (mime-external-entity-body-buffer-internal entity)
	       (buffer-live-p
		(mime-external-entity-body-buffer-internal entity)))
    (condition-case nil
	(progn
	  (mmexternal-require-file-name entity)
	  (mime-external-entity-set-body-buffer-internal
	   entity
	   (with-current-buffer (get-buffer-create
				 (concat " *Body of "
					 (mime-entity-name entity)
					 "*"))
	     (binary-insert-encoded-file
	      (mime-external-entity-body-file-internal entity))
	     (current-buffer))))
      (error (message "Can't get external-body.")))))


;;; @ entity
;;;

(luna-define-method mime-insert-entity ((entity mime-external-entity))
  (mime-insert-entity-body (mime-entity-parent-internal entity))
  (insert "\n")
  (mime-insert-entity-body entity))

(luna-define-method mime-write-entity ((entity mime-external-entity) filename)
  (with-temp-buffer
    (mime-insert-entity entity)
    (let ((coding-system-for-write 'raw-text-dos))
      (write-region (point-min) (point-max) filename))))


;;; @ entity header
;;;


;;; @ entity body
;;;

(luna-define-method mime-entity-body ((entity mime-external-entity))
  (mmexternal-require-buffer entity)
  (with-current-buffer (mime-external-entity-body-buffer-internal entity)
    (buffer-string)))

(luna-define-method mime-insert-entity-body ((entity mime-external-entity))
  (mmexternal-require-buffer entity)
  (insert-buffer-substring
   (mime-external-entity-body-buffer-internal entity)))

(luna-define-method mime-write-entity-body ((entity mime-external-entity)
					    filename)
  (mmexternal-require-buffer entity)
  (with-current-buffer (mime-external-entity-body-buffer-internal entity)
    (binary-write-decoded-region (point-min) (point-max) filename)))


;;; @ entity content
;;;

(luna-define-method mime-entity-content ((entity mime-external-entity))
  (let ((ret (mime-entity-body entity)))
    (if ret
	(mime-decode-string ret (mime-entity-encoding entity))
      (message "Cannot get content")
      nil)))

(luna-define-method mime-insert-entity-content ((entity mime-external-entity))
  (insert (mime-entity-content entity)))

(luna-define-method mime-write-entity-content ((entity mime-external-entity)
					       filename)
  (mmexternal-require-buffer entity)
  (with-current-buffer (mime-external-entity-body-buffer-internal entity)
    (mime-write-decoded-region (point-min) (point-max)
			       filename
			       (or (mime-entity-encoding entity) "7bit"))))


;;; @ header field
;;;

(luna-define-method mime-entity-fetch-field :around
  ((entity mime-external-entity) field-name)
  (or (luna-call-next-method)
      (with-temp-buffer
	(mime-insert-entity-body (mime-entity-parent-internal entity))
	(let ((ret (std11-fetch-field field-name)))
	  (when ret
	    (or (symbolp field-name)
		(setq field-name (intern (capitalize field-name))))
	    (mime-entity-set-original-header-internal
	     entity
	     (put-alist field-name ret
			(mime-entity-original-header-internal entity)))
	    ret)))))

(luna-define-method mime-insert-header ((entity mime-external-entity)
					&optional invisible-fields
					visible-fields)
  (let ((the-buf (current-buffer))
	buf p-min p-max)
    (with-temp-buffer
      (mime-insert-entity-body (mime-entity-parent-internal entity))
      (setq buf (current-buffer)
	    p-min (point-min)
	    p-max (point-max))
      (set-buffer the-buf)
      (mime-insert-header-from-buffer buf p-min p-max
				      invisible-fields visible-fields))))


;;; @ end
;;;

(provide 'mmexternal)

;;; mmexternal.el ends here
