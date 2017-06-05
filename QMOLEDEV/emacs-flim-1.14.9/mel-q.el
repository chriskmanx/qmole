;;; mel-q.el --- Quoted-Printable encoder/decoder.

;; Copyright (C) 1995,96,97,98,99,2000,2001 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Created: 1995/6/25
;; Keywords: MIME, Quoted-Printable, Q-encoding

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
(require 'path-util)
(eval-when-compile
  ;; XXX: should provide char-list instead of string-to-char-list.
  ;; XXx: and also the macro `as-binary-process' should be provided
  ;; XXx: by the module "pces" which will be loaded by way of "poem".
  (require 'poem))


;;; @ Quoted-Printable encoder
;;;

(defsubst quoted-printable-quote-char (character)
  (concat
   "="
   (char-to-string (aref quoted-printable-hex-chars (ash character -4)))
   (char-to-string (aref quoted-printable-hex-chars (logand character 15)))))

(defun quoted-printable-internal-encode-region (start end)
  (save-excursion
    (save-restriction
      (narrow-to-region (goto-char start) end)
      (let ((col 0)
	    chr)
	(while (not (eobp))
	  (cond
	   ((>= col 75)			; soft line break.
	    (insert "=\n")
	    (setq col 0))
	   ((eolp)			; end of line.
	    (forward-char)
	    (setq col 0))
	   (t
	    (setq chr (char-after (point)))
	    (cond
	     ((and (memq chr '(?  ?\t))	; encode WSP char before CRLF.
		   (eq (char-after (1+ (point))) ?\n))
	      (forward-char)
	      (insert "=\n")
	      (forward-char)
	      (setq col 0))
	     ((and (bolp)		; "^From " is not safe.
		   (eq chr			  ?F)
		   (eq (char-after (1+  (point))) ?r)
		   (eq (char-after (+ 2 (point))) ?o)
		   (eq (char-after (+ 3 (point))) ?m)
		   (eq (char-after (+ 4 (point))) ? ))
	      (delete-region (point)(1+ (point)))
	      (insert "=46")		; moved to ?r.
	      (forward-char 4)		; skip "rom ".
	      (setq col 7))
	     ((or (= chr ?\t)		; skip safe char.
		  (and (<= 32 chr)(/= chr ?=)(< chr 127)))
	      (forward-char)
	      (setq col (1+ col)))
	     ((>= col 73)		; soft line break.
	      (insert "=\n")
	      (setq col 0))
	     (t				; encode unsafe char.
	      (delete-region (point)(1+ (point)))
	      ;; (insert (quoted-printable-quote-char chr))
	      (insert
	       ?=
	       (aref quoted-printable-hex-chars (ash chr -4))
	       (aref quoted-printable-hex-chars (logand chr 15)))
	      (setq col (+ col 3)))))))))))


(defvar quoted-printable-external-encoder '("mmencode" "-q")
  "*list of quoted-printable encoder program name and its arguments.")

(defun quoted-printable-external-encode-region (start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (as-binary-process
       (apply (function call-process-region)
	      start end (car quoted-printable-external-encoder)
	      t t nil
	      (cdr quoted-printable-external-encoder)))
      ;; for OS/2
      ;;   regularize line break code
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
	(replace-match "")))))


(defvar quoted-printable-internal-encoding-limit
  (if (and (featurep 'xemacs)(featurep 'mule))
      0
    (require 'path-util)
    (if (exec-installed-p "mmencode")
	1000
      ;; XXX: Fix this message, or simply remove it.
      ;; (message "Don't found external encoder for Quoted-Printable!")
      nil))
  "*limit size to use internal quoted-printable encoder.
If size of input to encode is larger than this limit,
external encoder is called.")

(defun quoted-printable-encode-region (start end)
  "Encode current region by quoted-printable.
START and END are buffer positions.
This function calls internal quoted-printable encoder if size of
region is smaller than `quoted-printable-internal-encoding-limit',
otherwise it calls external quoted-printable encoder specified by
`quoted-printable-external-encoder'.  In this case, you must install
the program (maybe mmencode included in metamail or XEmacs package)."
  (interactive "*r")
  (if (and quoted-printable-internal-encoding-limit
	   (> (- end start) quoted-printable-internal-encoding-limit))
      (quoted-printable-external-encode-region start end)
    (quoted-printable-internal-encode-region start end)))

(defun quoted-printable-encode-string (string)
  "Encode STRING to quoted-printable, and return the result."
  (with-temp-buffer
    (insert string)
    (quoted-printable-encode-region (point-min)(point-max))
    (buffer-string)))


(mel-define-method-function
 (mime-encode-string string (nil "quoted-printable"))
 'quoted-printable-encode-string)

(mel-define-method-function
 (mime-encode-region start end (nil "quoted-printable"))
 'quoted-printable-encode-region)

(mel-define-method mime-insert-encoded-file (filename (nil "quoted-printable"))
  "Encode contents of file FILENAME to quoted-printable, and insert the result.
It calls external quoted-printable encoder specified by
`quoted-printable-external-encoder'.  So you must install the program
\(maybe mmencode included in metamail or XEmacs package)."
  (interactive "*fInsert encoded file: ")
  (apply (function call-process)
	 (car quoted-printable-external-encoder)
	 filename t nil
	 (cdr quoted-printable-external-encoder)))


;;; @ Quoted-Printable decoder
;;;

(defsubst quoted-printable-hex-char-to-num (chr)
  (cond ((<= ?a chr) (+ (- chr ?a) 10))
	((<= ?A chr) (+ (- chr ?A) 10))
	((<= ?0 chr) (- chr ?0))
	))

(defun quoted-printable-internal-decode-region (start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (search-forward "=" nil t)
	(cond
	 ((eolp)
	  ;; unfold soft line break.
	  (delete-region (1- (point))(1+ (point))))
	 ((and (memq (char-after (point))
		     (eval-when-compile
		       ;; XXX: should provide char-list instead.
		       (string-to-char-list quoted-printable-hex-chars)))
	       (memq (char-after (1+ (point)))
		     (eval-when-compile
		       ;; XXX: should provide char-list instead.
		       (string-to-char-list quoted-printable-hex-chars))))
	  ;; encoded char.
	  (insert
	   (prog1
	       (logior
		(ash (quoted-printable-hex-char-to-num (char-after (point))) 4)
		(quoted-printable-hex-char-to-num (char-after (1+ (point)))))
	     (delete-region (1- (point))(+ 2 (point))))))
	 (t
	  ;; invalid encoding.
	  ))))))

(defvar quoted-printable-external-decoder '("mmencode" "-q" "-u")
  "*list of quoted-printable decoder program name and its arguments.")

(defun quoted-printable-external-decode-region (start end)
  (save-excursion
    (as-binary-process
     (apply (function call-process-region)
	    start end (car quoted-printable-external-decoder)
	    t t nil
	    (cdr quoted-printable-external-decoder)))))


(defvar quoted-printable-internal-decoding-limit nil
  "*limit size to use internal quoted-printable decoder.
If size of input to decode is larger than this limit,
external decoder is called.")

(defun quoted-printable-decode-region (start end)
  "Decode current region by quoted-printable.
START and END are buffer positions.
This function calls internal quoted-printable decoder if size of
region is smaller than `quoted-printable-internal-decoding-limit',
otherwise it calls external quoted-printable decoder specified by
`quoted-printable-external-decoder'.  In this case, you must install
the program (maybe mmencode included in metamail or XEmacs package)."
  (interactive "*r")
  (if (and quoted-printable-internal-decoding-limit
	   (> (- end start) quoted-printable-internal-decoding-limit))
      (quoted-printable-external-decode-region start end)
    (quoted-printable-internal-decode-region start end)))

(defun quoted-printable-decode-string (string)
  "Decode STRING which is encoded in quoted-printable, and return the result."
  (with-temp-buffer
    (insert string)
    (quoted-printable-decode-region (point-min)(point-max))
    (buffer-string)))


(mel-define-method-function
 (mime-decode-string string (nil "quoted-printable"))
 'quoted-printable-decode-string)

(mel-define-method-function
 (mime-decode-region start end (nil "quoted-printable"))
 'quoted-printable-decode-region)


(defvar quoted-printable-external-decoder-option-to-specify-file '("-o")
  "*list of options of quoted-printable decoder program to specify file.
If the quoted-printable decoder does not have such option, set this as nil.")

(mel-define-method mime-write-decoded-region (start end filename
						    (nil "quoted-printable"))
  "Decode and write current region encoded by quoted-printable into FILENAME.
START and END are buffer positions."
  (interactive "*r\nFWrite decoded region to file: ")
  (as-binary-process
   (apply (function call-process-region)
	  start end (car quoted-printable-external-decoder)
	  (null quoted-printable-external-decoder-option-to-specify-file)
	  (unless quoted-printable-external-decoder-option-to-specify-file
	    (list (current-buffer) nil))
	  nil
	  (delq nil
		(append
		 (cdr quoted-printable-external-decoder)
		 quoted-printable-external-decoder-option-to-specify-file
		 (when quoted-printable-external-decoder-option-to-specify-file
		   (list filename))))))
  (unless quoted-printable-external-decoder-option-to-specify-file
    (write-region-as-binary (point-min) (point-max) filename)))


;;; @ Q-encoding encode/decode string
;;;

(defconst q-encoding-special-chars-alist
  '((text	?= ?? ?_)
    (comment	?= ?? ?_ ?\( ?\) ?\\)
    (phrase	?= ?? ?_ ?\( ?\) ?\\ ?\" ?# ?$ ?% ?& ?' ?, ?. ?/
		?: ?\; ?< ?> ?@ ?\[ ?\] ?^ ?` ?{ ?| ?} ?~)
    ))

(defun q-encoding-encode-string (string &optional mode)
  "Encode STRING to Q-encoding of encoded-word, and return the result.
MODE allows `text', `comment', `phrase' or nil.  Default value is
`phrase'."
  (let ((specials (cdr (or (assq mode q-encoding-special-chars-alist)
			   (assq 'phrase q-encoding-special-chars-alist)))))
    (mapconcat (function
		(lambda (chr)
		  (cond ((eq chr ? ) "_")
			((or (< chr 32) (< 126 chr)
			     (memq chr specials))
			 (quoted-printable-quote-char chr))
			(t
			 (char-to-string chr)))))
	       string "")))

(defun q-encoding-decode-string (string)
  "Decode STRING which is encoded in Q-encoding and return the result."
  (let (q h l)
    (mapconcat (function
		(lambda (chr)
		  (cond ((eq chr ?_) " ")
			((eq chr ?=)
			 (setq q t)
			 "")
			(q (setq h (quoted-printable-hex-char-to-num chr))
			   (setq q nil)
			   "")
			(h (setq l (quoted-printable-hex-char-to-num chr))
			   (prog1
			       (char-to-string (logior (ash h 4) l))
			     (setq h nil)))
			(t (char-to-string chr)))))
	       string "")))

(mel-define-method-function (encoded-text-encode-string string (nil "Q"))
			    'q-encoding-encode-string)

(mel-define-method encoded-text-decode-string (string (nil "Q"))
  (if (string-match (eval-when-compile
		      (concat "\\`" Q-encoded-text-regexp "\\'"))
		    string)
      (q-encoding-decode-string string)
    (error "Invalid encoded-text %s" string)))


;;; @ end
;;;

(provide 'mel-q)

;;; mel-q.el ends here.
