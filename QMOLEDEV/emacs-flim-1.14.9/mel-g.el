;;; mel-g.el --- Gzip64 encoder/decoder.

;; Copyright (C) 1995,96,97,98,99,2001  Free Software Foundation, Inc.

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;;	MORIOKA Tomohiko <tomo@m17n.org>
;; Maintainer: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Created: 1995/10/25
;; Keywords: Gzip64, base64, gzip, MIME

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

;;; Commentary:

;;; NOTE: Gzip64 is an experimental Content-Transfer-Encoding and its
;;; use is STRONGLY DISCOURAGED except for private communication.

;;; Code:

(require 'mime-def)
(require 'path-util)


;;; @ variables
;;;

(defvar gzip64-external-encoder '("sh" "-c" "gzip -c | mmencode")
  "*list of gzip64 encoder program name and its arguments.")

(defvar gzip64-external-decoder '("sh" "-c" "mmencode -u | gzip -dc")
  "*list of gzip64 decoder program name and its arguments.")


;;; @ encoder/decoder for region
;;;

(defun gzip64-external-encode-region (beg end)
  (interactive "*r")
  (save-excursion
    (let ((coding-system-for-write 'binary))
      (apply (function call-process-region)
	     beg end (car gzip64-external-encoder)
	     t t nil
	     (cdr gzip64-external-encoder)))
    ;; for OS/2
    ;;   regularize line break code
    ;;(goto-char (point-min))
    ;;(while (re-search-forward "\r$" nil t)
    ;;  (replace-match ""))
    ))

(defun gzip64-external-decode-region (beg end)
  (interactive "*r")
  (save-excursion
    (let ((coding-system-for-read 'binary))
      (apply (function call-process-region)
	     beg end (car gzip64-external-decoder)
	     t t nil
	     (cdr gzip64-external-decoder)))))

(mel-define-method-function (mime-encode-region start end (nil "x-gzip64"))
			    'gzip64-external-encode-region)
(mel-define-method-function (mime-decode-region start end (nil "x-gzip64"))
			    'gzip64-external-decode-region)


;;; @ encoder/decoder for string
;;;

(mel-define-method mime-encode-string (string (nil "x-gzip64"))
  (with-temp-buffer
    (insert string)
    (gzip64-external-encode-region (point-min)(point-max))
    (buffer-string)))

(mel-define-method mime-decode-string (string (nil "x-gzip64"))
  (with-temp-buffer
    (insert string)
    (gzip64-external-decode-region (point-min)(point-max))
    (buffer-string)))


;;; @ encoder/decoder for file
;;;

(mel-define-method mime-insert-encoded-file (filename (nil "x-gzip64"))
  (interactive "*fInsert encoded file: ")
  (apply (function call-process)
	 (car gzip64-external-encoder)
	 filename t nil
	 (cdr gzip64-external-encoder)))

(mel-define-method mime-write-decoded-region (start end filename
						    (nil "x-gzip64"))
  "Decode and write current region encoded by gzip64 into FILENAME.
START and END are buffer positions."
  (interactive "*r\nFWrite decoded region to file: ")
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (apply (function call-process-region)
	   start end (car gzip64-external-decoder)
	   nil nil nil
	   (let ((args (cdr gzip64-external-decoder)))
	     (append (butlast args)
		     (list (concat (car (last args)) ">" filename)))))))


;;; @ end
;;;

(provide 'mel-g)

;;; mel-g.el ends here
