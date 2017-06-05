;;; mel-b-dl.el --- Base64 encoder/decoder using DL module.

;; Copyright (C) 1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: MIME, Base64

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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'mime-def)

(defvar base64-dl-handle
  (and (stringp base64-dl-module)
       (file-exists-p base64-dl-module)
       (dynamic-link base64-dl-module)))

(dynamic-call "emacs_base64_init" base64-dl-handle)

;; base64-dl-module provides `encode-base64-string' and `decode-base64-string'.
(defalias 'base64-encode-string 'encode-base64-string)
(defalias 'base64-decode-string 'decode-base64-string)

(defun base64-encode-region (start end)
  "Encode current region by base64.
START and END are buffer positions."
  (interactive "*r")
  (insert
    (prog1
	(base64-encode-string
	 (buffer-substring start end))
      (delete-region start end)))
  (or (bolp) (insert ?\n)))

(defun base64-decode-region (start end)
  "Decode current region by base64.
START and END are buffer positions."
  (interactive "*r")
  (insert
   (prog1
       (base64-decode-string
	(buffer-substring start end))
     (delete-region start end))))


(mel-define-method-function (mime-encode-string string (nil "base64"))
			    'base64-encode-string)
(mel-define-method-function (mime-decode-string string (nil "base64"))
			    'base64-decode-string)
(mel-define-method-function (mime-encode-region start end (nil "base64"))
			    'base64-encode-region)
(mel-define-method-function (mime-decode-region start end (nil "base64"))
			    'base64-decode-region)

(mel-define-method-function (encoded-text-encode-string string (nil "B"))
			    'base64-encode-string)

(mel-define-method encoded-text-decode-string (string (nil "B"))
  (if (string-match (eval-when-compile
		      (concat "\\`" B-encoded-text-regexp "\\'"))
		    string)
      (base64-decode-string string)
    (error "Invalid encoded-text %s" string)))


;;; @ base64 encoder/decoder for file
;;;

(mel-define-method mime-insert-encoded-file (filename (nil "base64"))
  "Encode contents of file FILENAME to base64, and insert the result.
It calls external base64 encoder specified by
`base64-external-encoder'.  So you must install the program (maybe
mmencode included in metamail or XEmacs package)."
  (interactive "*fInsert encoded file: ")
  (insert (base64-encode-string
	   (with-temp-buffer
	     (set-buffer-multibyte nil)
	     (insert-file-contents-as-binary filename)
	     (buffer-string))))
  (or (bolp) (insert ?\n)))

;; (mel-define-method mime-write-decoded-region (start end filename
;;                                                     (nil "base64"))
;;   "Decode and write current region encoded by base64 into FILENAME.
;; START and END are buffer positions."
;;   (interactive "*r\nFWrite decoded region to file: ")
;;   (let ((str (buffer-substring start end)))
;;     (with-temp-buffer
;;       (insert (base64-decode-string str))
;;       (write-region-as-binary (point-min)(point-max) filename))))


;;; @ end
;;;

(provide 'mel-b-dl)

;;; mel-b-dl.el ends here.
