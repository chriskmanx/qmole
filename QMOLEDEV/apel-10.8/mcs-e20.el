;;; mcs-e20.el --- MIME charset implementation for Emacs 20.1 and 20.2

;; Copyright (C) 1996,1997,1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: emulation, compatibility, Mule

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

;;    This module requires Emacs 20.1 and 20.2.

;;; Code:

(require 'pces)
(eval-when-compile (require 'static))

(defsubst encode-mime-charset-region (start end charset &optional lbt)
  "Encode the text between START and END as MIME CHARSET."
  (let (cs)
    (if (and enable-multibyte-characters
	     (setq cs (mime-charset-to-coding-system charset lbt)))
	(encode-coding-region start end cs)
      )))

(defsubst decode-mime-charset-region (start end charset &optional lbt)
  "Decode the text between START and END as MIME CHARSET."
  (let (cs)
    (if (and enable-multibyte-characters
	     (setq cs (mime-charset-to-coding-system charset lbt)))
	(decode-coding-region start end cs)
      )))


(defsubst encode-mime-charset-string (string charset &optional lbt)
  "Encode the STRING as MIME CHARSET."
  (let (cs)
    (if (and enable-multibyte-characters
	     (setq cs (mime-charset-to-coding-system charset lbt)))
	(encode-coding-string string cs)
      string)))

(defsubst decode-mime-charset-string (string charset &optional lbt)
  "Decode the STRING as MIME CHARSET."
  (let (cs)
    (if (and enable-multibyte-characters
	     (setq cs (mime-charset-to-coding-system charset lbt)))
	(decode-coding-string string cs)
      string)))


(defvar charsets-mime-charset-alist
  (delq
   nil
   `(((ascii)						. us-ascii)
     ((ascii latin-iso8859-1)				. iso-8859-1)
     ((ascii latin-iso8859-2)				. iso-8859-2)
     ((ascii latin-iso8859-3)				. iso-8859-3)
     ((ascii latin-iso8859-4)				. iso-8859-4)
     ;;((ascii cyrillic-iso8859-5)			. iso-8859-5)
     ((ascii cyrillic-iso8859-5)			. koi8-r)
     ((ascii arabic-iso8859-6)				. iso-8859-6)
     ((ascii greek-iso8859-7)				. iso-8859-7)
     ((ascii hebrew-iso8859-8)				. iso-8859-8)
     ((ascii latin-iso8859-9)				. iso-8859-9)
     ,(if (find-coding-system 'iso-8859-14)
	  '((ascii latin-iso8859-14)			. iso-8859-14))
     ,(if (find-coding-system 'iso-8859-15)
	  '((ascii latin-iso8859-15)			. iso-8859-15))
     ((ascii latin-jisx0201
	     japanese-jisx0208-1978 japanese-jisx0208)	. iso-2022-jp)
     ((ascii latin-jisx0201
	     katakana-jisx0201 japanese-jisx0208)	. shift_jis)
     ((ascii korean-ksc5601)				. euc-kr)
     ((ascii chinese-gb2312)				. gb2312)
     ((ascii chinese-big5-1 chinese-big5-2)		. big5)
     ((ascii thai-tis620 composition)			. tis-620)
     ((ascii latin-iso8859-1 greek-iso8859-7
	     latin-jisx0201 japanese-jisx0208-1978
	     chinese-gb2312 japanese-jisx0208
	     korean-ksc5601 japanese-jisx0212)		. iso-2022-jp-2)
     ;;((ascii latin-iso8859-1 greek-iso8859-7
     ;;        latin-jisx0201 japanese-jisx0208-1978
     ;;        chinese-gb2312 japanese-jisx0208
     ;;        korean-ksc5601 japanese-jisx0212
     ;;        chinese-cns11643-1 chinese-cns11643-2)	. iso-2022-int-1)
     ;;((ascii latin-iso8859-1 latin-iso8859-2
     ;;        cyrillic-iso8859-5 greek-iso8859-7
     ;;        latin-jisx0201 japanese-jisx0208-1978
     ;;        chinese-gb2312 japanese-jisx0208
     ;;        korean-ksc5601 japanese-jisx0212
     ;;        chinese-cns11643-1 chinese-cns11643-2
     ;;        chinese-cns11643-3 chinese-cns11643-4
     ;;        chinese-cns11643-5 chinese-cns11643-6
     ;;        chinese-cns11643-7)			. iso-2022-int-1)
     )))

(defun-maybe coding-system-get (coding-system prop)
  "Extract a value from CODING-SYSTEM's property list for property PROP."
  (plist-get (coding-system-plist coding-system) prop)
  )

(defun coding-system-to-mime-charset (coding-system)
  "Convert CODING-SYSTEM to a MIME-charset.
Return nil if corresponding MIME-charset is not found."
  (or (car (rassq coding-system mime-charset-coding-system-alist))
      (coding-system-get coding-system 'mime-charset)
      ))

(defun-maybe-cond mime-charset-list ()
  "Return a list of all existing MIME-charset."
  ((boundp 'coding-system-list)
   (let ((dest (mapcar (function car) mime-charset-coding-system-alist))
	 (rest coding-system-list)
	 cs)
     (while rest
       (setq cs (car rest))
       (unless (rassq cs mime-charset-coding-system-alist)
	 (if (setq cs (coding-system-get cs 'mime-charset))
	     (or (rassq cs mime-charset-coding-system-alist)
		 (memq cs dest)  
		 (setq dest (cons cs dest))
		 )))
       (setq rest (cdr rest)))
     dest))
   (t
    (let ((dest (mapcar (function car) mime-charset-coding-system-alist))
	  (rest (coding-system-list))
	  cs)
      (while rest
	(setq cs (car rest))
	(unless (rassq cs mime-charset-coding-system-alist)
	  (when (setq cs (or (coding-system-get cs 'mime-charset)
			     (and
			      (setq cs (aref
					(coding-system-get cs 'coding-spec)
					2))
			      (string-match "(MIME:[ \t]*\\([^,)]+\\)" cs)
			      (match-string 1 cs))))
	    (setq cs (intern (downcase cs)))
	    (or (rassq cs mime-charset-coding-system-alist)
		(memq cs dest)
		(setq dest (cons cs dest))
		)))
	(setq rest (cdr rest)))
      dest)
    ))

(static-when (and (string= (decode-coding-string "\e.A\eN!" 'ctext) "\eN!")
		  (or (not (find-coding-system 'x-ctext))
		      (coding-system-get 'x-ctext 'apel)))
  (unless (find-coding-system 'x-ctext)
    (make-coding-system
     'x-ctext 2 ?x
     "Compound text based generic encoding for decoding unknown messages."
     '((ascii t) (latin-iso8859-1 t) t t
       nil ascii-eol ascii-cntl nil locking-shift single-shift nil nil nil
       init-bol nil nil)
     '((safe-charsets . t)
       (mime-charset . x-ctext)))
    (coding-system-put 'x-ctext 'apel t)
    ))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'mcs-e20) (require 'apel-ver))

;;; mcs-e20.el ends here
