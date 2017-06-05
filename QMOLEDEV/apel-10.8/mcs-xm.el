;;; mcs-xm.el --- MIME charset implementation for XEmacs-mule

;; Copyright (C) 1997,1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: MIME-charset, coding-system, emulation, compatibility, Mule

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;    This module requires Emacs 20.0.93, XEmacs 20.3-b5 (with mule)
;;    or later.

;;; Code:

(require 'poem)


(defun encode-mime-charset-region (start end charset &optional lbt)
  "Encode the text between START and END as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset lbt)))
    (if cs
	(encode-coding-region start end cs)
      )))


(defcustom mime-charset-decoder-alist
  (let ((alist
	 '((hz-gb-2312 . decode-mime-charset-region-for-hz)
	   (t . decode-mime-charset-region-default))))
    (if (featurep 'utf-2000)
	alist
      (list*
       '(iso-2022-jp . decode-mime-charset-region-with-iso646-unification)
       '(iso-2022-jp-2 . decode-mime-charset-region-with-iso646-unification)
       alist)))
  "Alist MIME-charset vs. decoder function."
  :group 'i18n
  :type '(repeat (cons mime-charset function)))

(defsubst decode-mime-charset-region-default (start end charset lbt)
  (let ((cs (mime-charset-to-coding-system charset lbt)))
    (if cs
	(decode-coding-region start end cs)
      )))

(unless (featurep 'utf-2000)
  (require 'mcs-xmu))

(defun decode-mime-charset-region-for-hz (start end charset lbt)
  (if lbt
      (save-restriction
	(narrow-to-region start end)
	(decode-coding-region (point-min)(point-max)
			      (mime-charset-to-coding-system 'raw-text lbt))
	(decode-hz-region (point-min)(point-max)))
    (decode-hz-region start end)))

(defun decode-mime-charset-region (start end charset &optional lbt)
  "Decode the text between START and END as MIME CHARSET."
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (let ((func (cdr (or (assq charset mime-charset-decoder-alist)
		       (assq t mime-charset-decoder-alist)))))
    (funcall func start end charset lbt)))

(defun encode-mime-charset-string (string charset &optional lbt)
  "Encode the STRING as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset lbt)))
    (if cs
	(encode-coding-string string cs)
      string)))

;; (defsubst decode-mime-charset-string (string charset)
;;   "Decode the STRING as MIME CHARSET."
;;   (let ((cs (mime-charset-to-coding-system charset)))
;;     (if cs
;;         (decode-coding-string string cs)
;;       string)))
(defun decode-mime-charset-string (string charset &optional lbt)
  "Decode the STRING as MIME CHARSET."
  (with-temp-buffer
    (insert string)
    (decode-mime-charset-region (point-min)(point-max) charset lbt)
    (buffer-string)))


(defvar charsets-mime-charset-alist
  (delq
   nil
   `(((ascii)						. us-ascii)
     ((ascii latin-iso8859-1)				. iso-8859-1)
     ((ascii latin-iso8859-2)				. iso-8859-2)
     ((ascii latin-iso8859-3)				. iso-8859-3)
     ((ascii latin-iso8859-4)				. iso-8859-4)
     ((ascii cyrillic-iso8859-5)			. iso-8859-5)
     ;;((ascii cyrillic-iso8859-5)			. koi8-r)
     ((ascii arabic-iso8859-6)				. iso-8859-6)
     ((ascii greek-iso8859-7)				. iso-8859-7)
     ((ascii hebrew-iso8859-8)				. iso-8859-8)
     ((ascii latin-iso8859-9)				. iso-8859-9)
     ,(if (find-coding-system 'iso-8859-14)
	  '((ascii latin-iso8859-14)			. iso-8859-14))
     ,(if (find-coding-system 'iso-8859-15)
	  '((ascii latin-iso8859-15)			. iso-8859-15))
     ,(if (featurep 'utf-2000)
	  '((ascii latin-jisx0201
		   japanese-jisx0208-1978
		   japanese-jisx0208
		   japanese-jisx0208-1990)		. iso-2022-jp)
	'((ascii latin-jisx0201
		 japanese-jisx0208-1978 japanese-jisx0208)
	  . iso-2022-jp))
     ,(if (featurep 'utf-2000)
	  '((ascii latin-jisx0201
		   japanese-jisx0208-1978
		   japanese-jisx0208
		   japanese-jisx0208-1990
		   japanese-jisx0213-1
		   japanese-jisx0213-2)			. iso-2022-jp-3)
	'((ascii latin-jisx0201
		 japanese-jisx0208-1978 japanese-jisx0208
		 japanese-jisx0213-1
		 japanese-jisx0213-2)			. iso-2022-jp-3))
     ,(if (featurep 'utf-2000)
	  '((ascii latin-jisx0201 katakana-jisx0201
		   japanese-jisx0208-1990)		. shift_jis)
	'((ascii latin-jisx0201
		 katakana-jisx0201 japanese-jisx0208)	. shift_jis))
     ((ascii korean-ksc5601)				. euc-kr)
     ((ascii chinese-gb2312)				. gb2312)
     ((ascii chinese-big5-1 chinese-big5-2)		. big5)
     ((ascii thai-xtis)					. tis-620)
     ,(if (featurep 'utf-2000)
	  '((ascii latin-jisx0201 latin-iso8859-1
		   greek-iso8859-7
		   japanese-jisx0208-1978 japanese-jisx0208
		   japanese-jisx0208-1990
		   japanese-jisx0212
		   chinese-gb2312
		   korean-ksc5601)		. iso-2022-jp-2)
	'((ascii latin-jisx0201 latin-iso8859-1
		 greek-iso8859-7
		 japanese-jisx0208-1978 japanese-jisx0208
		 japanese-jisx0212
		 chinese-gb2312
		 korean-ksc5601)			. iso-2022-jp-2))
     ;;((ascii latin-iso8859-1 greek-iso8859-7
     ;;        latin-jisx0201 japanese-jisx0208-1978
     ;;        chinese-gb2312 japanese-jisx0208
     ;;        korean-ksc5601 japanese-jisx0212
     ;;        chinese-cns11643-1 chinese-cns11643-2)      . iso-2022-int-1)
     )))


(defun coding-system-to-mime-charset (coding-system)
  "Convert CODING-SYSTEM to a MIME-charset.
Return nil if corresponding MIME-charset is not found."
  (setq coding-system
	(coding-system-name (coding-system-base coding-system)))
  (or (car (rassq coding-system mime-charset-coding-system-alist))
      coding-system))

(defun mime-charset-list ()
  "Return a list of all existing MIME-charset."
  (let ((dest (mapcar (function car) mime-charset-coding-system-alist))
	(rest (coding-system-list))
	cs)
    (while rest
      (setq cs (coding-system-name (coding-system-base (car rest))))
      (or (rassq cs mime-charset-coding-system-alist)
	  (memq cs dest)
	  (setq dest (cons cs dest)))
      (setq rest (cdr rest)))
    dest))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'mcs-xm) (require 'apel-ver))

;;; mcs-xm.el ends here
