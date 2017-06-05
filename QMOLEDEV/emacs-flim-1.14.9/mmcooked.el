;;; mmcooked.el --- MIME entity implementation for binary buffer

;; Copyright (C) 1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

(require 'mmbuffer)

(mm-define-backend cooked (buffer))

(mm-define-method entity-cooked-p ((entity cooked)) t)

(mm-define-method write-entity-content ((entity cooked) filename)
  (save-excursion
    (set-buffer (mime-buffer-entity-buffer-internal entity))
    (let ((encoding (or (mime-entity-encoding entity) "7bit")))
      (if (member encoding '("7bit" "8bit" "binary"))
	  (write-region (mime-buffer-entity-body-start-internal entity)
			(mime-buffer-entity-body-end-internal entity) filename)
	(mime-write-decoded-region
	 (mime-buffer-entity-body-start-internal entity)
	 (mime-buffer-entity-body-end-internal entity)
	 filename encoding)
	))))

(mm-define-method write-entity ((entity cooked) filename)
  (save-excursion
    (set-buffer (mime-buffer-entity-buffer-internal entity))
    (write-region (mime-buffer-entity-header-start-internal entity)
		  (mime-buffer-entity-body-end-internal entity)
		  filename)
    ))

(mm-define-method write-entity-body ((entity cooked) filename)
  (save-excursion
    (set-buffer (mime-buffer-entity-buffer-internal entity))
    (write-region (mime-buffer-entity-body-start-internal entity)
		  (mime-buffer-entity-body-end-internal entity)
		  filename)
    ))

(luna-define-method mime-insert-header ((entity mime-cooked-entity)
					&optional invisible-fields
					visible-fields)
  (let (default-mime-charset)
    (funcall (car (luna-class-find-functions
		   (luna-find-class 'mime-buffer-entity)
		   'mime-insert-header))
	     entity invisible-fields visible-fields)
    ))

(mm-define-method insert-text-content ((entity cooked))
  (let ((str (mime-entity-content entity)))
    (insert
     (if (member (mime-entity-encoding entity)
		 '(nil "7bit" "8bit" "binary"))
	 str
       (decode-mime-charset-string str
				   (or (mime-content-type-parameter
					(mime-entity-content-type entity)
					"charset")
				       default-mime-charset)
				   'CRLF)
       ))))


;;; @ end
;;;

(provide 'mmcooked)

;;; mmcooked.el ends here
