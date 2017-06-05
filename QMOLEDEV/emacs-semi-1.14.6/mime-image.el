;;; mime-image.el --- mime-view filter to display images

;; Copyright (C) 1995,1996,1997,1998 MORIOKA Tomohiko
;; Copyright (C) 1996 Dan Rich

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;	Dan Rich <drich@morpheus.corp.sgi.com>
;;	Daiki Ueno <ueno@ueda.info.waseda.ac.jp>
;;	Katsumi Yamaoka  <yamaoka@jpl.org>
;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1995/12/15
;;	Renamed: 1997/2/21 from tm-image.el

;; Keywords: image, picture, X-Face, MIME, multimedia, mail, news

;; This file is part of SEMI (Showy Emacs MIME Interfaces).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;	If you use this program with MULE, please install
;;	etl8x16-bitmap.bdf font included in tl package.

;;; Code:

(eval-when-compile (require 'cl))

(eval-when-compile (require 'static))

(require 'mime-view)
(require 'alist)
(require 'path-util)

(defsubst mime-image-normalize-xbm-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (let ((case-fold-search t) width height xbytes right margin)
      (goto-char (point-min))
      (or (re-search-forward "_width[\t ]+\\([0-9]+\\)" nil t)
	  (error "!! Illegal xbm file format" (current-buffer)))
      (setq width (string-to-int (match-string 1))
	    xbytes (/ (+ width 7) 8))
      (goto-char (point-min))
      (or (re-search-forward "_height[\t ]+\\([0-9]+\\)" nil t)
	  (error "!! Illegal xbm file format" (current-buffer)))
      (setq height (string-to-int (match-string 1)))
      (goto-char (point-min))
      (re-search-forward "0x[0-9a-f][0-9a-f],")
      (delete-region (point-min) (match-beginning 0))
      (goto-char (point-min))
      (while (re-search-forward "[\n\r\t ,;}]" nil t)
	(replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "0x" nil t)
	(replace-match "\\x" nil t))
      (goto-char (point-min))
      (insert "(" (number-to-string width) " "
	      (number-to-string height) " \"")
      (goto-char (point-max))
      (insert "\")")
      (goto-char (point-min))
      (read (current-buffer)))))

(static-if (featurep 'xemacs)
    (progn
      (defun mime-image-type-available-p (type)
	(memq type (image-instantiator-format-list)))

      (defun mime-image-create (file-or-data &optional type data-p &rest props)
	(when (and data-p (eq type 'xbm))
	  (with-temp-buffer
	    (insert file-or-data)
	    (setq file-or-data
		  (mime-image-normalize-xbm-buffer (current-buffer)))))
	(let ((glyph
	       (make-glyph
		(if (and type (mime-image-type-available-p type))
		    (vconcat
		     (list type (if data-p :data :file) file-or-data)
		     props)
		  file-or-data))))
	  (if (nothing-image-instance-p (glyph-image-instance glyph)) nil
	    glyph)))

      (defun mime-image-insert (image &optional string area)
	(let ((extent (make-extent (point)
				   (progn (and string
					       (insert string))
					  (point)))))
	  (set-extent-property extent 'invisible t)
	  (set-extent-end-glyph extent image))))
  (condition-case nil
      (progn
	(require 'image)
	(defalias 'mime-image-type-available-p 'image-type-available-p)
	(defun mime-image-create
	  (file-or-data &optional type data-p &rest props)
	  (if (and data-p (eq type 'xbm))
	      (with-temp-buffer
		(insert file-or-data)
		(setq file-or-data
		      (mime-image-normalize-xbm-buffer (current-buffer)))
		(apply #'create-image (nth 2 file-or-data) type data-p
		       (nconc
			(list :width (car file-or-data)
			      :height (nth 1 file-or-data))
			props)))
	    (apply #'create-image file-or-data type data-p props)))
	(defalias 'mime-image-insert 'insert-image))
    (error
     (condition-case nil
	 (progn
	   (require (if (featurep 'mule) 'bitmap ""))
	   (defun mime-image-read-xbm-buffer (buffer)
	     (condition-case nil
		 (mapconcat #'bitmap-compose
			    (append (bitmap-decode-xbm
				     (bitmap-read-xbm-buffer
				      (current-buffer))) nil) "\n")
	       (error nil)))
	   (defun mime-image-insert (image &optional string area)
	     (insert image)))
       (error
	(defalias 'mime-image-read-xbm-buffer
	  'mime-image-normalize-xbm-buffer)
	(defun mime-image-insert (image &optional string area)
	  (save-restriction
	    (narrow-to-region (point)(point))
	    (let ((face (gensym "mii")))
	      (or (facep face) (make-face face))
	      (set-face-stipple face image)
	      (let ((row (make-string (/ (car image)  (frame-char-width)) ? ))
		  (height (/ (nth 1 image)  (frame-char-height)))
		  (i 0))
		(while (< i height)
		  (set-text-properties (point) (progn (insert row)(point))
				       (list 'face face))
		  (insert "\n")
		  (setq i (1+ i)))))))))

     (defun mime-image-type-available-p (type)
       (eq type 'xbm))

     (defun mime-image-create (file-or-data &optional type data-p &rest props)
       (when (or (null type) (eq type 'xbm))
	 (with-temp-buffer
	   (if data-p
	       (insert file-or-data)
	     (insert-file-contents file-or-data))
	   (mime-image-read-xbm-buffer (current-buffer))))))))

(defvar mime-image-format-alist
  '((image jpeg		jpeg)
    (image gif		gif)
    (image tiff		tiff)
    (image x-tiff	tiff)
    (image xbm		xbm)
    (image x-xbm	xbm)
    (image x-xpixmap	xpm)
    (image png		png)))

(dolist (rule mime-image-format-alist)
  (when (mime-image-type-available-p (nth 2 rule))
    (ctree-set-calist-strictly
     'mime-preview-condition
     (list (cons 'type (car rule))(cons 'subtype (nth 1 rule))
	   '(body . visible)
	   (cons 'body-presentation-method #'mime-display-image)
	   (cons 'image-format (nth 2 rule))))))
    

;;; @ content filter for images
;;;
;;    (for XEmacs 19.12 or later)

(defun mime-display-image (entity situation)
  (message "Decoding image...")
  (let ((format (cdr (assq 'image-format situation)))
	image)
    (setq image (mime-image-create (mime-entity-content entity) format 'data))
    (if (null image)
	(message "Invalid glyph!")
      (save-excursion
	(mime-image-insert image)
	(insert "\n")
	(message "Decoding image...done")))))

;;; @ end
;;;

(provide 'mime-image)

;;; mime-image.el ends here
