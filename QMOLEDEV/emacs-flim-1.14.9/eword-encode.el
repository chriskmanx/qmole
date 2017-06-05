;;; eword-encode.el --- RFC 2047 based encoded-word encoder for GNU Emacs

;; Copyright (C) 1995,1996,1997,1998,1999,2000,2002,2003,2004 Free
;;   Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: encoded-word, MIME, multilingual, header, mail, news

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
(require 'mel)
(require 'std11)
(require 'eword-decode)


;;; @ variables
;;;

;; User options are defined in mime-def.el.

(defvar mime-header-charset-encoding-alist
  '((us-ascii		. nil)
    (iso-8859-1		. "Q")
    (iso-8859-2		. "Q")
    (iso-8859-3		. "Q")
    (iso-8859-4		. "Q")
    (iso-8859-5		. "Q")
    (koi8-r		. "Q")
    (iso-8859-7		. "Q")
    (iso-8859-8		. "Q")
    (iso-8859-9		. "Q")
    (iso-8859-14	. "Q")
    (iso-8859-15	. "Q")
    (iso-2022-jp	. "B")
    (iso-2022-jp-3	. "B")
    (iso-2022-kr	. "B")
    (gb2312		. "B")
    (cn-gb		. "B")
    (cn-gb-2312		. "B")
    (euc-kr		. "B")
    (tis-620		. "B")
    (iso-2022-jp-2	. "B")
    (iso-2022-int-1	. "B")
    (utf-8		. "B")
    ))

(defvar mime-header-default-charset-encoding "Q")

(defvar mime-header-encode-method-alist
  '((eword-encode-address-list
     . (Reply-To
	From Sender
	Resent-Reply-To Resent-From
	Resent-Sender To Resent-To
	Cc Resent-Cc Bcc Resent-Bcc
	Dcc))
    (eword-encode-in-reply-to . (In-Reply-To))
    (eword-encode-structured-field-body . (Mime-Version User-Agent))
    (eword-encode-unstructured-field-body)))

;;; @ encoded-text encoder
;;;

(defun eword-encode-text (charset encoding string &optional mode)
  "Encode STRING as an encoded-word, and return the result.
CHARSET is a symbol to indicate MIME charset of the encoded-word.
ENCODING allows \"B\" or \"Q\".
MODE is allows `text', `comment', `phrase' or nil.  Default value is
`phrase'."
  (let ((text (encoded-text-encode-string string encoding mode)))
    (if text
	(concat "=?" (upcase (symbol-name charset)) "?"
		encoding "?" text "?=")
      )))


;;; @ charset word
;;;

(defsubst eword-encode-char-type (character)
  (if (memq character '(?  ?\t ?\n))
      nil
    (char-charset character)
    ))

(defun eword-encode-divide-into-charset-words (string)
  (let ((len (length string))
	dest)
    (while (> len 0)
      (let* ((chr (aref string 0))
             ;; (chr (sref string 0))
	     (charset (eword-encode-char-type chr))
             (i 1)
	     ;; (i (char-length chr))
	     )
	(while (and (< i len)
		    (setq chr (aref string i))
                    ;; (setq chr (sref string i))
		    (eq charset (eword-encode-char-type chr)))
	  (setq i (1+ i))
          ;; (setq i (char-next-index chr i))
	  )
	(setq dest (cons (cons charset (substring string 0 i)) dest)
	      string (substring string i)
	      len (- len i))))
    (nreverse dest)))


;;; @ word
;;;

(defun eword-encode-charset-words-to-words (charset-words)
  (let (dest)
    (while charset-words
      (let* ((charset-word (car charset-words))
	     (charset (car charset-word))
	     )
	(if charset
	    (let ((charsets (list charset))
		  (str (cdr charset-word))
		  )
	      (catch 'tag
		(while (setq charset-words (cdr charset-words))
		  (setq charset-word (car charset-words)
			charset (car charset-word))
		  (if (null charset)
		      (throw 'tag nil)
		    )
		  (or (memq charset charsets)
		      (setq charsets (cons charset charsets))
		      )
		  (setq str (concat str (cdr charset-word)))
		  ))
	      (setq dest (cons (cons charsets str) dest))
	      )
	  (setq dest (cons charset-word dest)
		charset-words (cdr charset-words)
		))))
    (nreverse dest)
    ))


;;; @ rule
;;;

(defmacro make-ew-rword (text charset encoding type)
  (` (list (, text)(, charset)(, encoding)(, type))))
(defmacro ew-rword-text (rword)
  (` (car (, rword))))
(defmacro ew-rword-charset (rword)
  (` (car (cdr (, rword)))))
(defmacro ew-rword-encoding (rword)
  (` (car (cdr (cdr (, rword))))))
(defmacro ew-rword-type (rword)
  (` (car (cdr (cdr (cdr (, rword)))))))

(defun ew-find-charset-rule (charsets)
  (if charsets
      (let* ((charset (find-mime-charset-by-charsets charsets))
	     (encoding
	      (cdr (or (assq charset mime-header-charset-encoding-alist)
		       (cons charset mime-header-default-charset-encoding)))))
	(list charset encoding))))

;; [tomo:2002-11-05] The following code is a quick-fix for emacsen
;; which is not depended on the Mule model.  We should redesign
;; `eword-encode-split-string' to avoid to depend on the Mule model.
(if (featurep 'utf-2000)
;; for CHISE Architecture
(defun tm-eword::words-to-ruled-words (wl &optional mode)
  (let (mcs)
    (mapcar (function
	     (lambda (word)
	       (setq mcs (detect-mime-charset-string (cdr word)))
	       (make-ew-rword
		(cdr word)
		mcs
		(cdr (or (assq mcs mime-header-charset-encoding-alist)
			 (cons mcs mime-header-default-charset-encoding)))
		mode)
	       ))
	    wl)))

;; for legacy Mule
(defun tm-eword::words-to-ruled-words (wl &optional mode)
  (mapcar (function
	   (lambda (word)
	     (let ((ret (ew-find-charset-rule (car word))))
	       (make-ew-rword (cdr word) (car ret)(nth 1 ret) mode)
	       )))
	  wl))
)

(defun ew-space-process (seq)
  (let (prev a ac b c cc)
    (while seq
      (setq b (car seq))
      (setq seq (cdr seq))
      (setq c (car seq))
      (setq cc (ew-rword-charset c))
      (if (and (null (ew-rword-charset b))
	       (not (eq (ew-rword-type b) 'special)))
	  (progn
	    (setq a (car prev))
	    (setq ac (ew-rword-charset a))
	    (if (and (ew-rword-encoding a)
		     (ew-rword-encoding c))
		(cond ((eq ac cc)
		       (setq prev (cons
				   (cons (concat (car a)(car b)(car c))
					 (cdr a))
				   (cdr prev)
				   ))
		       (setq seq (cdr seq))
		       )
		      (t
		       (setq prev (cons
				   (cons (concat (car a)(car b))
					 (cdr a))
				   (cdr prev)
				   ))
		       ))
	      (setq prev (cons b prev))
	      ))
	(setq prev (cons b prev))
	))
    (reverse prev)
    ))

(defun eword-encode-split-string (str &optional mode)
  (ew-space-process
   (tm-eword::words-to-ruled-words
    (eword-encode-charset-words-to-words
     (eword-encode-divide-into-charset-words str))
    mode)))


;;; @ length
;;;

(defun tm-eword::encoded-word-length (rword)
  (let ((string   (ew-rword-text     rword))
	(charset  (ew-rword-charset  rword))
	(encoding (ew-rword-encoding rword))
	ret)
    (setq ret
	  (cond ((string-equal encoding "B")
		 (setq string (encode-mime-charset-string string charset))
		 (base64-encoded-length string)
		 )
		((string-equal encoding "Q")
		 (setq string (encode-mime-charset-string string charset))
		 (Q-encoded-text-length string (ew-rword-type rword))
		 )))
    (if ret
	(cons (+ 7 (length (symbol-name charset)) ret) string)
      )))


;;; @ encode-string
;;;

(defun ew-encode-rword-1 (column rwl &optional must-output)
  (catch 'can-not-output
    (let* ((rword (car rwl))
	   (ret (tm-eword::encoded-word-length rword))
	   string len)
      (if (null ret)
	  (cond ((and (setq string (car rword))
		      (or (<= (setq len (+ (length string) column)) 76)
			  (<= column 1))
		      )
		 (setq rwl (cdr rwl))
		 )
		((memq (aref string 0) '(?  ?\t))
		 (setq string (concat "\n" string)
		       len (length string)
		       rwl (cdr rwl))
		 )
		(must-output
		 (setq string "\n "
		       len 1)
		 )
		(t
		 (throw 'can-not-output nil)
		 ))
	(cond ((and (setq len (car ret))
		    (<= (+ column len) 76)
		    )
	       (setq string
		     (eword-encode-text
		      (ew-rword-charset rword)
		      (ew-rword-encoding rword)
		      (cdr ret)
		      (ew-rword-type rword)
		      ))
	       (setq len (+ (length string) column))
	       (setq rwl (cdr rwl))
	       )
	      (t
	       (setq string (car rword))
	       (let* ((p 0) np
		      (str "") nstr)
		 (while (and (< p len)
			     (progn
			       (setq np (1+ p))
			       ;;(setq np (char-next-index (sref string p) p))
			       (setq nstr (substring string 0 np))
			       (setq ret (tm-eword::encoded-word-length
					  (cons nstr (cdr rword))
					  ))
			       (setq nstr (cdr ret))
			       (setq len (+ (car ret) column))
			       (<= len 76)
			       ))
		   (setq str nstr
			 p np))
		 (if (string-equal str "")
		     (if must-output
			 (setq string "\n "
			       len 1)
		       (throw 'can-not-output nil))
		   (setq rwl (cons (cons (substring string p) (cdr rword))
				   (cdr rwl)))
		   (setq string
			 (eword-encode-text
			  (ew-rword-charset rword)
			  (ew-rword-encoding rword)
			  str
			  (ew-rword-type rword)))
		   (setq len (+ (length string) column))
		   )
		 )))
	)
      (list string len rwl)
      )))

(defun eword-encode-rword-list (column rwl)
  (let (ret dest str ew-f pew-f folded-points)
    (while rwl
      (setq ew-f (nth 2 (car rwl)))
      (if (and pew-f ew-f)
	  (setq rwl (cons '(" ") rwl)
		pew-f nil)
	(setq pew-f ew-f)
	)
      (if (null (setq ret (ew-encode-rword-1 column rwl)))
	  (let ((i (1- (length dest)))
		c s r-dest r-column)
	    (catch 'success
	      (while (catch 'found
		       (while (>= i 0)
			 (cond ((memq (setq c (aref dest i)) '(?  ?\t))
				(if (memq i folded-points)
				    (throw 'found nil)
				  (setq folded-points (cons i folded-points))
				  (throw 'found i))
				)
			       ((eq c ?\n)
				(throw 'found nil)
				))
			 (setq i (1- i))))
		(setq s (substring dest i)
		      r-column (length s)
		      r-dest (concat (substring dest 0 i) "\n" s))
		(when (setq ret (ew-encode-rword-1 r-column rwl))
		  (setq dest r-dest
			column r-column)
		  (throw 'success t)
		  ))
	      (setq ret (ew-encode-rword-1 column rwl 'must-output))
	      )))
      (setq str (car ret))
      (setq dest (concat dest str))
      (setq column (nth 1 ret)
	    rwl (nth 2 ret))
      )
    (list dest column)
    ))


;;; @ converter
;;;

(defun eword-encode-phrase-to-rword-list (phrase)
  (let (token type dest str)
    (while phrase
      (setq token (car phrase))
      (setq type (car token))
      (cond ((eq type 'quoted-string)
	     (setq str (concat "\"" (cdr token) "\""))
	     (setq dest
		   (append dest
			   (list
			    (let ((ret (ew-find-charset-rule
					(find-charset-string str))))
			      (make-ew-rword
			       str (car ret)(nth 1 ret) 'phrase)
			      )
			    )))
	     )
	    ((eq type 'comment)
	     (setq dest
		   (append dest
			   '(("(" nil nil special))
			   (tm-eword::words-to-ruled-words
			    (eword-encode-charset-words-to-words
			     (eword-encode-divide-into-charset-words
			      (cdr token)))
			    'comment)
			   '((")" nil nil special))
			   ))
	     )
	    (t
	     (setq dest
		   (append dest
			   (tm-eword::words-to-ruled-words
			    (eword-encode-charset-words-to-words
			     (eword-encode-divide-into-charset-words
			      (cdr token))
			     ) 'phrase)))
	     ))
      (setq phrase (cdr phrase))
      )
    (ew-space-process dest)
    ))

(defun eword-encode-addr-seq-to-rword-list (seq)
  (let (dest pname)
    (while seq
      (let* ((token (car seq))
	     (name (car token))
	     )
	(cond ((eq name 'spaces)
	       (setq dest (nconc dest (list (list (cdr token) nil nil))))
	       )
	      ((eq name 'comment)
	       (setq dest
		     (nconc
		      dest
		      (list (list "(" nil nil))
		      (eword-encode-split-string (cdr token) 'comment)
		      (list (list ")" nil nil))
		      ))
	       )
	      ((eq name 'quoted-string)
	       (setq dest
		     (nconc
		      dest
		      (list
		       (list (concat "\"" (cdr token) "\"") nil nil)
		       )))
	       )
	      (t
	       (setq dest
		     (if (or (eq pname 'spaces)
			     (eq pname 'comment))
			 (nconc dest (list (list (cdr token) nil nil)))
		       (nconc (nreverse (cdr (reverse dest)))
			      ;; (butlast dest)
			      (list
			       (list (concat (car (car (last dest)))
					     (cdr token))
				     nil nil)))))
	       ))
	(setq seq (cdr seq)
	      pname name))
      )
    dest))

(defun eword-encode-phrase-route-addr-to-rword-list (phrase-route-addr)
  (if (eq (car phrase-route-addr) 'phrase-route-addr)
      (let ((phrase (nth 1 phrase-route-addr))
	    (route (nth 2 phrase-route-addr))
	    dest)
        ;; (if (eq (car (car phrase)) 'spaces)
        ;;     (setq phrase (cdr phrase))
        ;;   )
	(setq dest (eword-encode-phrase-to-rword-list phrase))
	(if dest
	    (setq dest (append dest '((" " nil nil))))
	  )
	(append
	 dest
	 (eword-encode-addr-seq-to-rword-list
	  (append '((specials . "<"))
		  route
		  '((specials . ">"))))
	 ))))

(defun eword-encode-addr-spec-to-rword-list (addr-spec)
  (if (eq (car addr-spec) 'addr-spec)
      (eword-encode-addr-seq-to-rword-list (cdr addr-spec))
    ))

(defun eword-encode-mailbox-to-rword-list (mbox)
  (let ((addr (nth 1 mbox))
	(comment (nth 2 mbox))
	dest)
    (setq dest (or (eword-encode-phrase-route-addr-to-rword-list addr)
		   (eword-encode-addr-spec-to-rword-list addr)
		   ))
    (if comment
	(setq dest
	      (append dest
		      '((" " nil nil)
			("(" nil nil))
		      (eword-encode-split-string comment 'comment)
		      (list '(")" nil nil))
		      )))
    dest))

(defsubst eword-encode-mailboxes-to-rword-list (mboxes)
  (let ((dest (eword-encode-mailbox-to-rword-list (car mboxes))))
    (if dest
	(while (setq mboxes (cdr mboxes))
	  (setq dest
		(nconc dest
		       (list '("," nil nil))
		       (eword-encode-mailbox-to-rword-list
			(car mboxes))))))
    dest))

(defsubst eword-encode-address-to-rword-list (address)
  (cond
   ((eq (car address) 'mailbox)
    (eword-encode-mailbox-to-rword-list address))
   ((eq (car address) 'group)
    (nconc
     (eword-encode-phrase-to-rword-list (nth 1 address))
     (list (list ":" nil nil))
     (eword-encode-mailboxes-to-rword-list (nth 2 address))
     (list (list ";" nil nil))))))

(defsubst eword-encode-addresses-to-rword-list (addresses)
  (let ((dest (eword-encode-address-to-rword-list (car addresses))))
    (if dest
	(while (setq addresses (cdr addresses))
	  (setq dest
		(nconc dest
		       (list '("," nil nil))
		       ;; (list '(" " nil nil))
		       (eword-encode-address-to-rword-list (car addresses))))))
    dest))

(defsubst eword-encode-msg-id-to-rword-list (msg-id)
  (list
   (list
    (concat "<"
	    (caar (eword-encode-addr-seq-to-rword-list (cdr msg-id)))
	    ">")
    nil nil)))

(defsubst eword-encode-in-reply-to-to-rword-list (in-reply-to)
  (let (dest)
    (while in-reply-to
      (setq dest
	    (append dest
		    (let ((elt (car in-reply-to)))
		      (if (eq (car elt) 'phrase)
			  (eword-encode-phrase-to-rword-list (cdr elt))
			(eword-encode-msg-id-to-rword-list elt)
			))))
      (setq in-reply-to (cdr in-reply-to)))
    dest))


;;; @ application interfaces
;;;

(defvar eword-encode-default-start-column 10
  "Default start column if it is omitted.")

(defun eword-encode-string (string &optional column mode)
  "Encode STRING as encoded-words, and return the result.
Optional argument COLUMN is start-position of the field.
Optional argument MODE allows `text', `comment', `phrase' or nil.
Default value is `phrase'."
  (car (eword-encode-rword-list
	(or column eword-encode-default-start-column)
	(eword-encode-split-string string mode))))

(defun eword-encode-address-list (string &optional column)
  "Encode header field STRING as list of address, and return the result.
Optional argument COLUMN is start-position of the field."
  (car (eword-encode-rword-list
	(or column eword-encode-default-start-column)
	(eword-encode-addresses-to-rword-list
	 (std11-parse-addresses-string string))
	)))

(defun eword-encode-in-reply-to (string &optional column)
  "Encode header field STRING as In-Reply-To field, and return the result.
Optional argument COLUMN is start-position of the field."
  (car (eword-encode-rword-list
	(or column 13)
	(eword-encode-in-reply-to-to-rword-list
	 (std11-parse-msg-ids-string string)))))

(defun eword-encode-structured-field-body (string &optional column)
  "Encode header field STRING as structured field, and return the result.
Optional argument COLUMN is start-position of the field."
  (car (eword-encode-rword-list
	(or column eword-encode-default-start-column)
	(eword-encode-addr-seq-to-rword-list (std11-lexical-analyze string))
	)))

(defun eword-encode-unstructured-field-body (string &optional column)
  "Encode header field STRING as unstructured field, and return the result.
Optional argument COLUMN is start-position of the field."
  (car (eword-encode-rword-list
	(or column eword-encode-default-start-column)
	(eword-encode-split-string string 'text))))

;;;###autoload
(defun mime-encode-field-body (field-body field-name)
  "Encode FIELD-BODY as FIELD-NAME, and return the result.
A lexical token includes non-ASCII character is encoded as MIME
encoded-word.  ASCII token is not encoded."
  (setq field-body (std11-unfold-string field-body))
  (if (string= field-body "")
      ""
    (let ((method-alist mime-header-encode-method-alist)
	  start ret)
      (if (symbolp field-name)
	  (setq start (1+ (length (symbol-name field-name))))
	(setq start (1+ (length field-name))
	      field-name (intern (capitalize field-name))))
      (while (car method-alist)
	(if (or (not (cdr (car method-alist)))
		(memq field-name
		      (cdr (car method-alist))))
	    (progn
	      (setq ret
		    (apply (caar method-alist) (list field-body start)))
	      (setq method-alist nil)))
	(setq method-alist (cdr method-alist)))
      ret)))
(defalias 'eword-encode-field-body 'mime-encode-field-body)
(make-obsolete 'eword-encode-field-body 'mime-encode-field-body)

(defun eword-in-subject-p ()
  (let ((str (std11-field-body "Subject")))
    (if (and str (string-match eword-encoded-word-regexp str))
	str)))
(make-obsolete 'eword-in-subject-p "Don't use it.")

(defsubst eword-find-field-encoding-method (field-name)
  (setq field-name (downcase field-name))
  (let ((alist mime-field-encoding-method-alist))
    (catch 'found
      (while alist
	(let* ((pair (car alist))
	       (str (car pair)))
	  (if (and (stringp str)
		   (string= field-name (downcase str)))
	      (throw 'found (cdr pair))
	    ))
	(setq alist (cdr alist)))
      (cdr (assq t mime-field-encoding-method-alist))
      )))

;;;###autoload
(defun mime-encode-header-in-buffer (&optional code-conversion)
  "Encode header fields to network representation, such as MIME encoded-word.
It refers the `mime-field-encoding-method-alist' variable."
  (interactive "*")
  (save-excursion
    (save-restriction
      (std11-narrow-to-header mail-header-separator)
      (goto-char (point-min))
      (let ((default-cs (mime-charset-to-coding-system default-mime-charset))
	    bbeg end field-name)
	(while (re-search-forward std11-field-head-regexp nil t)
	  (setq bbeg (match-end 0)
		field-name (buffer-substring-no-properties (match-beginning 0)
							   (1- bbeg))
		end (std11-field-end))
	  (and (delq 'ascii (find-charset-region bbeg end))
	       (let ((method (eword-find-field-encoding-method
			      (downcase field-name))))
		 (cond ((eq method 'mime)
			(let* ((field-body
				(buffer-substring-no-properties bbeg end))
			       (encoded-body
				(mime-encode-field-body
				 field-body field-name)))
			  (if (not encoded-body)
			      (error "Cannot encode %s:%s"
				     field-name field-body)
			    (delete-region bbeg end)
			    (insert encoded-body))))
		       (code-conversion
			(let ((cs
			       (or (mime-charset-to-coding-system
				    method)
				   default-cs)))
			  (encode-coding-region bbeg end cs)))))))))))
(defalias 'eword-encode-header 'mime-encode-header-in-buffer)
(make-obsolete 'eword-encode-header 'mime-encode-header-in-buffer)


;;; @ end
;;;

(provide 'eword-encode)

;;; eword-encode.el ends here
