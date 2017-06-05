;;; mel-b-el.el --- Base64 encoder/decoder.

;; Copyright (C) 1992,95,96,97,98,99,2001 Free Software Foundation, Inc.

;; Author: ENAMI Tsugutomo <enami@sys.ptg.sony.co.jp>
;;         MORIOKA Tomohiko <tomo@m17n.org>
;; Created: 1995/6/24
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
(eval-when-compile
  ;; XXX: the macro `as-binary-process' should be provided when compiling.
  (require 'pces))


;;; @ variables
;;;

(defgroup base64 nil
  "Base64 encoder/decoder"
  :group 'mime)

(defcustom base64-external-encoder '("mmencode")
  "*list of base64 encoder program name and its arguments."
  :group 'base64
  :type '(cons (file :tag "Command")(repeat :tag "Arguments" string)))

(defcustom base64-external-decoder '("mmencode" "-u")
  "*list of base64 decoder program name and its arguments."
  :group 'base64
  :type '(cons (file :tag "Command")(repeat :tag "Arguments" string)))

(defcustom base64-external-decoder-option-to-specify-file '("-o")
  "*list of options of base64 decoder program to specify file.
If the base64 decoder program does not have such option, set this as nil."
  :group 'base64
  :type '(repeat :tag "Arguments" string))

(defcustom base64-internal-encoding-limit 1000
  "*limit size to use internal base64 encoder.
If size of input to encode is larger than this limit,
external encoder is called."
  :group 'base64
  :type '(choice (const :tag "Always use internal encoder" nil)
		 (integer :tag "Size")))

(defcustom base64-internal-decoding-limit (if (and (featurep 'xemacs)
						   (featurep 'mule))
					      1000
					    7600)
  "*limit size to use internal base64 decoder.
If size of input to decode is larger than this limit,
external decoder is called."
  :group 'base64
  :type '(choice (const :tag "Always use internal decoder" nil)
		 (integer :tag "Size")))


;;; @ utility function
;;;

(defun pack-sequence (seq size)
  "Split sequence SEQ into SIZE elements packs, and return list of packs.
\[mel-b-el; tl-seq function]"
  (let ((len (length seq))
	(p 0)
	dest unit)
    (while (< p len)
      (setq unit (cons (elt seq p) unit))
      (setq p (1+ p))
      (when (zerop (mod p size))
	(setq dest (cons (nreverse unit) dest))
	(setq unit nil)))
    (if unit
	(nreverse (cons (nreverse unit) dest))
      (nreverse dest))))


;;; @ internal base64 encoder
;;;	based on base64 decoder by Enami Tsugutomo

(eval-and-compile
  (defconst base64-characters
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
  )

(defmacro base64-num-to-char (n)
  `(aref base64-characters ,n))

(defun base64-encode-1 (pack)
  (let ((buf (make-string 4 ?=)))
    (aset buf 0 (base64-num-to-char (ash (car pack) -2)))
    (if (nth 1 pack)
	(progn
	  (aset buf 1 (base64-num-to-char
		       (logior (ash (logand (car pack) 3) 4)
			       (ash (nth 1 pack) -4))))
	  (if (nth 2 pack)
	      (progn
		(aset buf 2 (base64-num-to-char
			     (logior (ash (logand (nth 1 pack) 15) 2)
				     (ash (nth 2 pack) -6))))
		(aset buf 3 (base64-num-to-char
			     (logand (nth 2 pack) 63))))
	    (aset buf 2 (base64-num-to-char
			 (ash (logand (nth 1 pack) 15) 2)))))
      (aset buf 1 (base64-num-to-char
		   (ash (logand (car pack) 3) 4))))
    buf))

(defun-maybe base64-encode-string (string &optional no-line-break)
  "Base64-encode STRING and return the result.
Optional second argument NO-LINE-BREAK means do not break long lines
into shorter lines."
  (let* ((len (length string))
	 (b 0)(e 57)
	 (dest ""))
    (while (< e len)
      (setq dest
	    (concat dest
		    (mapconcat
		     (function base64-encode-1)
		     (pack-sequence (substring string b e) 3)
		     "")
		    (if (not no-line-break) "\n")))
      (setq b e
	    e (+ e 57)))
    (concat dest
	    (mapconcat
	     (function base64-encode-1)
	     (pack-sequence (substring string b) 3)
	     ""))))

(defun base64-internal-encode-region (beg end &optional no-line-break)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (insert
       (prog1
	   (base64-encode-string (buffer-substring beg end) no-line-break)
	 (delete-region beg end))))))


;;; @ internal base64 decoder
;;;

(defconst base64-numbers
  (eval-when-compile
    (let ((len (length base64-characters))
	  (vec (make-vector 123 nil))
	  (i 0))
      (while (< i len)
	(aset vec (aref base64-characters i) i)
	(setq i (1+ i)))
      vec)))

(defmacro base64-char-to-num (c)
  `(aref base64-numbers ,c))

(defsubst base64-internal-decode (string buffer)
  (let* ((len (length string))
	 (i 0)(j 0)
	 v1 v2 v3)
    (catch 'tag
      (while (< i len)
	(when (prog1 (setq v1 (base64-char-to-num (aref string i)))
		(setq i (1+ i)))
	  (setq v2 (base64-char-to-num (aref string i))
		i (1+ i)
		v3 (base64-char-to-num (aref string i))
		i (1+ i))
	  (aset buffer j (logior (lsh v1 2)(lsh v2 -4)))
	  (setq j (1+ j))
	  (if v3
	      (let ((v4 (base64-char-to-num (aref string i))))
		(setq i (1+ i))
		(aset buffer j (logior (lsh (logand v2 15) 4)(lsh v3 -2)))
		(setq j (1+ j))
		(if v4
		    (aset buffer (prog1 j (setq j (1+ j)))
			  (logior (lsh (logand v3 3) 6) v4))
		  (throw 'tag nil)))
	    (throw 'tag nil)))))
    (substring buffer 0 j)))

(defun base64-internal-decode-string (string)
  (base64-internal-decode string (make-string (length string) 0)))

;; (defsubst base64-decode-string! (string)
;;   (setq string (string-as-unibyte string))
;;   (base64-internal-decode string string))

(defun base64-internal-decode-region (beg end)
  (save-excursion
    (let ((str (string-as-unibyte (buffer-substring beg end))))
      (insert
       (prog1
	   (base64-internal-decode str str)
	 (delete-region beg end))))))

;; (defun base64-internal-decode-region2 (beg end)
;;   (save-excursion
;;     (let ((str (buffer-substring beg end)))
;;       (delete-region beg end)
;;       (goto-char beg)
;;       (insert (base64-decode-string! str)))))

;; (defun base64-internal-decode-region3 (beg end)
;;   (save-excursion
;;     (let ((str (buffer-substring beg end)))
;;       (delete-region beg end)
;;       (goto-char beg)
;;       (insert (base64-internal-decode-string str)))))


;;; @ external encoder/decoder
;;;

(defun base64-external-encode-region (beg end &optional no-line-break)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (as-binary-process
       (apply (function call-process-region)
	      beg end (car base64-external-encoder)
	      t t nil
	      (cdr base64-external-encoder)))
      ;; for OS/2
      ;;   regularize line break code
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
	(replace-match ""))
      (if no-line-break
	  (progn
	    (goto-char (point-min))
	    (while (search-forward "\n" nil t)
	      (replace-match "")))))))

(defun base64-external-decode-region (beg end)
  (save-excursion
    (as-binary-process
     (apply (function call-process-region)
	    beg end (car base64-external-decoder)
	    t t nil
	    (cdr base64-external-decoder)))))

(defun base64-external-decode-string (string)
  (with-temp-buffer
    (insert string)
    (as-binary-process
     (apply (function call-process-region)
	    (point-min)(point-max) (car base64-external-decoder)
	    t t nil
	    (cdr base64-external-decoder)))
    (buffer-string)))


;;; @ application interfaces
;;;

(defun-maybe base64-encode-region (start end &optional no-line-break)
  "Base64-encode the region between START and END.
Return the length of the encoded text.
Optional third argument NO-LINE-BREAK means do not break long lines
into shorter lines.
This function calls internal base64 encoder if size of region is
smaller than `base64-internal-encoding-limit', otherwise it calls
external base64 encoder specified by `base64-external-encoder'.  In
this case, you must install the program (maybe mmencode included in
metamail or XEmacs package)."
  (interactive "*r")
  (if (and base64-internal-encoding-limit
	   (> (- end start) base64-internal-encoding-limit))
      (base64-external-encode-region start end no-line-break)
    (base64-internal-encode-region start end no-line-break)))

(defun-maybe base64-decode-region (start end)
  "Decode current region by base64.
START and END are buffer positions.
This function calls internal base64 decoder if size of region is
smaller than `base64-internal-decoding-limit', otherwise it calls
external base64 decoder specified by `base64-external-decoder'.  In
this case, you must install the program (maybe mmencode included in
metamail or XEmacs package)."
  (interactive "*r")
  (if (and base64-internal-decoding-limit
	   (> (- end start) base64-internal-decoding-limit))
      (base64-external-decode-region start end)
    (base64-internal-decode-region start end)))

(defun-maybe base64-decode-string (string)
  "Decode STRING which is encoded in base64, and return the result.
This function calls internal base64 decoder if size of STRING is
smaller than `base64-internal-decoding-limit', otherwise it calls
external base64 decoder specified by `base64-external-decoder'.  In
this case, you must install the program (maybe mmencode included in
metamail or XEmacs package)."
  (if (and base64-internal-decoding-limit
	   (> (length string) base64-internal-decoding-limit))
      (base64-external-decode-string string)
    (base64-internal-decode-string string)))


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

(defun base64-insert-encoded-file (filename)
  "Encode contents of file FILENAME to base64, and insert the result.
It calls external base64 encoder specified by
`base64-external-encoder'.  So you must install the program (maybe
mmencode included in metamail or XEmacs package)."
  (interactive "*fInsert encoded file: ")
  (if (and base64-internal-encoding-limit
	   (> (nth 7 (file-attributes filename))
	      base64-internal-encoding-limit))
      (apply (function call-process)
	     (car base64-external-encoder)
	     filename t nil
	     (cdr base64-external-encoder))
    (insert
     (base64-encode-string
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(insert-file-contents-as-binary filename)
	(buffer-string))))
    (or (bolp) (insert ?\n))))

(mel-define-method-function (mime-insert-encoded-file filename (nil "base64"))
			    'base64-insert-encoded-file)

(defun base64-write-decoded-region (start end filename)
  "Decode and write current region encoded by base64 into FILENAME.
START and END are buffer positions."
  (interactive "*r\nFWrite decoded region to file: ")
  (if (and base64-internal-decoding-limit
	   (> (- end start) base64-internal-decoding-limit))
      (progn
	(as-binary-process
	 (apply (function call-process-region)
		start end (car base64-external-decoder)
		(null base64-external-decoder-option-to-specify-file)
		(unless base64-external-decoder-option-to-specify-file
		  (list (current-buffer) nil))
		nil
		(delq nil
		      (append
		       (cdr base64-external-decoder)
		       base64-external-decoder-option-to-specify-file
		       (when base64-external-decoder-option-to-specify-file
			 (list filename))))))
	(unless base64-external-decoder-option-to-specify-file
	  (write-region-as-binary (point-min) (point-max) filename)))
    (let ((str (buffer-substring start end)))
      (with-temp-buffer
	(insert (base64-internal-decode-string str))
	(write-region-as-binary (point-min) (point-max) filename)))))

(mel-define-method-function
 (mime-write-decoded-region start end filename (nil "base64"))
 'base64-write-decoded-region)


;;; @ end
;;;

(provide 'mel-b-el)

;;; mel-b-el.el ends here.
