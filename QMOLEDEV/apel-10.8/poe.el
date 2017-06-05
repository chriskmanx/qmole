;;; poe.el --- Portable Outfit for Emacsen

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2003, 2005,
;;   2008 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;	Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: emulation, compatibility, Nemacs, MULE, Emacs/mule, XEmacs

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

;;; Code:

(require 'product)
(product-provide (provide 'poe) (require 'apel-ver))

(require 'pym)


;;; @ Version information.
;;;

(static-when (= emacs-major-version 18)
  (require 'poe-18))

;; Some ancient version of XEmacs did not provide 'xemacs.
(static-when (string-match "XEmacs" emacs-version)
  (provide 'xemacs))

;; `file-coding' was appeared in the spring of 1998, just before XEmacs
;; 21.0. Therefore it is not provided in XEmacs with MULE versions 20.4
;; or earlier.
(static-when (featurep 'xemacs)
  ;; must be load-time check to share .elc between w/ MULE and w/o MULE.
  (when (featurep 'mule)
    (provide 'file-coding)))

(static-when (featurep 'xemacs)
  (require 'poe-xemacs))

;; must be load-time check to share .elc between different systems.
(or (fboundp 'open-network-stream)
    (require 'tcp))


;;; @ C primitives emulation.
;;;

;; Emacs 20.3 and earlier: (require FEATURE &optional FILENAME)
;; Emacs 20.4 and later: (require FEATURE &optional FILENAME NOERROR)
(static-condition-case nil
    ;; compile-time check.
    (progn
      (require 'nofeature "nofile" 'noerror)
      (if (get 'require 'defun-maybe)
	  (error "`require' is already redefined")))
  (error
   ;; load-time check.
   (or (fboundp 'si:require)
       (progn
	 (fset 'si:require (symbol-function 'require))
	 (defun require (feature &optional filename noerror)
	   "\
If feature FEATURE is not loaded, load it from FILENAME.
If FEATURE is not a member of the list `features', then the feature
is not loaded; so load the file FILENAME.
If FILENAME is omitted, the printname of FEATURE is used as the file name,
but in this case `load' insists on adding the suffix `.el' or `.elc'.
If the optional third argument NOERROR is non-nil,
then return nil if the file is not found.
Normally the return value is FEATURE."
	   (if noerror
	       (condition-case nil
		   (si:require feature filename)
		 (file-error))
	     (si:require feature filename)))
	 ;; for `load-history'.
	 (setq current-load-list (cons 'require current-load-list))
	 (put 'require 'defun-maybe t)))))

;; Emacs 19.29 and later: (plist-get PLIST PROP)
;; (defun-maybe plist-get (plist prop)
;;   (while (and plist
;;               (not (eq (car plist) prop)))
;;     (setq plist (cdr (cdr plist))))
;;   (car (cdr plist)))
(static-unless (and (fboundp 'plist-get)
		    (not (get 'plist-get 'defun-maybe)))
  (or (fboundp 'plist-get)
      (progn
	(defvar plist-get-internal-symbol)
	(defun plist-get (plist prop)
	  "\
Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...\).  This function returns the value
corresponding to the given PROP, or nil if PROP is not
one of the properties on the list."
	  (setplist 'plist-get-internal-symbol plist)
	  (get 'plist-get-internal-symbol prop))
	;; for `load-history'.
	(setq current-load-list (cons 'plist-get current-load-list))
	(put 'plist-get 'defun-maybe t))))

;; Emacs 19.29 and later: (plist-put PLIST PROP VAL)
;; (defun-maybe plist-put (plist prop val)
;;   (catch 'found
;;     (let ((tail plist)
;;           (prev nil))
;;       (while (and tail (cdr tail))
;;         (if (eq (car tail) prop)
;;             (progn
;;               (setcar (cdr tail) val)
;;               (throw 'found plist))
;;           (setq prev tail
;;                 tail (cdr (cdr tail)))))
;;       (if prev
;;           (progn
;;             (setcdr (cdr prev) (list prop val))
;;             plist)
;;         (list prop val)))))
(static-unless (and (fboundp 'plist-put)
		    (not (get 'plist-put 'defun-maybe)))
  (or (fboundp 'plist-put)
      (progn
	(defvar plist-put-internal-symbol)
	(defun plist-put (plist prop val)
	  "\
Change value in PLIST of PROP to VAL.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2 ...\).  PROP is a symbol and VAL is any object.
If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `\(setq x \(plist-put x prop val\)\)' to be sure to use the new value.
The PLIST is modified by side effects."
	  (setplist 'plist-put-internal-symbol plist)
	  (put 'plist-put-internal-symbol prop val)
	  (symbol-plist 'plist-put-internal-symbol))
	;; for `load-history'.
	(setq current-load-list (cons 'plist-put current-load-list))
	(put 'plist-put 'defun-maybe t))))

;; Emacs 19.23 and later: (minibuffer-prompt-width)
(defun-maybe minibuffer-prompt-width ()
  "Return the display width of the minibuffer prompt."
  (save-excursion
    (set-buffer (window-buffer (minibuffer-window)))
    (current-column)))

;; (read-string PROMPT &optional INITIAL-INPUT HISTORY)
;; Emacs 19.29/XEmacs 19.14(?) and later takes optional 3rd arg HISTORY.
(static-unless (or (featurep 'xemacs)
		   (>= emacs-major-version 20)
		   (and (= emacs-major-version 19)
			(>= emacs-minor-version 29)))
  (or (fboundp 'si:read-string)
      (progn
	(fset 'si:read-string (symbol-function 'read-string))
	(defun read-string (prompt &optional initial-input history)
	  "\
Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
The third arg HISTORY, is dummy for compatibility.
See `read-from-minibuffer' for details of HISTORY argument."
	  (si:read-string prompt initial-input)))))

;; (completing-read prompt table &optional
;; FSF Emacs
;;      --19.7  : predicate require-match init
;; 19.7 --19.34 : predicate require-match init hist
;; 20.1 --      : predicate require-match init hist def inherit-input-method
;; XEmacs
;;      --19.(?): predicate require-match init
;;      --21.2  : predicate require-match init hist
;; 21.2 --      : predicate require-match init hist def
;; )

;; We support following API.
;; (completing-read prompt table
;;                  &optional predicate require-match init hist def)
(static-cond
 ;; add 'hist' and 'def' argument.
 ((< emacs-major-version 19)
  (or (fboundp 'si:completing-read)
      (progn
	(fset 'si:completing-read (symbol-function 'completing-read))
	(defun completing-read
	  (prompt table &optional predicate require-match init
		                  hist def)
	"Read a string in the minibuffer, with completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
TABLE is an alist whose elements' cars are strings, or an obarray.
PREDICATE limits completion to a subset of TABLE.
See `try-completion' and `all-completions' for more details
 on completion, TABLE, and PREDICATE.

If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
 the input is (or completes to) an element of TABLE or is null.
 If it is also not t, Return does not exit if it does non-null completion.
If the input is null, `completing-read' returns an empty string,
 regardless of the value of REQUIRE-MATCH.

If INIT is non-nil, insert it in the minibuffer initially.
  If it is (STRING . POSITION), the initial input
  is STRING, but point is placed POSITION characters into the string.
HIST is ignored in this implementation.
DEF, if non-nil, is the default value.

Completion ignores case if the ambient value of
  `completion-ignore-case' is non-nil."
	(let ((string (si:completing-read prompt table predicate
					  require-match init)))
	  (if (and (string= string "") def)
	      def string))))))
 ;; add 'def' argument.
 ((or (and (featurep 'xemacs)
	   (or (and (eq emacs-major-version 21)
		    (< emacs-minor-version 2))
	       (< emacs-major-version 21)))
      (< emacs-major-version 20))
  (or (fboundp 'si:completing-read)
      (progn
	(fset 'si:completing-read (symbol-function 'completing-read))
	(defun completing-read
	  (prompt table &optional predicate require-match init
		                  hist def)
	"Read a string in the minibuffer, with completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
TABLE is an alist whose elements' cars are strings, or an obarray.
PREDICATE limits completion to a subset of TABLE.
See `try-completion' and `all-completions' for more details
 on completion, TABLE, and PREDICATE.

If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
 the input is (or completes to) an element of TABLE or is null.
 If it is also not t, Return does not exit if it does non-null completion.
If the input is null, `completing-read' returns an empty string,
 regardless of the value of REQUIRE-MATCH.

If INIT is non-nil, insert it in the minibuffer initially.
  If it is (STRING . POSITION), the initial input
  is STRING, but point is placed POSITION characters into the string.
HIST, if non-nil, specifies a history list
  and optionally the initial position in the list.
  It can be a symbol, which is the history list variable to use,
  or it can be a cons cell (HISTVAR . HISTPOS).
  In that case, HISTVAR is the history list variable to use,
  and HISTPOS is the initial position (the position in the list
  which INIT corresponds to).
  Positions are counted starting from 1 at the beginning of the list.
DEF, if non-nil, is the default value.

Completion ignores case if the ambient value of
  `completion-ignore-case' is non-nil."
	(let ((string (si:completing-read prompt table predicate
					  require-match init hist)))
	  (if (and (string= string "") def)
	      def string)))))))

;; v18:	(string-to-int STRING)
;; v19:	(string-to-number STRING)
;; v20:	(string-to-number STRING &optional BASE)
;;
;; XXX: `string-to-number' of Emacs 20.3 and earlier is broken.
;;	(string-to-number "1e1" 16) => 10.0, should be 481.
(static-condition-case nil
    ;; compile-time check.
    (if (= (string-to-number "1e1" 16) 481)
	(if (get 'string-to-number 'defun-maybe)
	    (error "`string-to-number' is already redefined"))
      (error "`string-to-number' is broken"))
  (error
   ;; load-time check.
   (or (fboundp 'si:string-to-number)
       (progn
	 (if (fboundp 'string-to-number)
	     (fset 'si:string-to-number (symbol-function 'string-to-number))
	   (fset 'si:string-to-number (symbol-function 'string-to-int))
	   ;; XXX: In v18, this causes infinite loop while byte-compiling.
	   ;; (defalias 'string-to-int 'string-to-number)
	   )
	 (put 'string-to-number 'defun-maybe t)
	 (defun string-to-number (string &optional base)
	   "\
Convert STRING to a number by parsing it as a decimal number.
This parses both integers and floating point numbers.
It ignores leading spaces and tabs.

If BASE, interpret STRING as a number in that base.  If BASE isn't
present, base 10 is used.  BASE must be between 2 and 16 (inclusive).
If the base used is not 10, floating point is not recognized."
	   (if (or (null base) (= base 10))
	       (si:string-to-number string)
	     (if (or (< base 2)(> base 16))
		 (signal 'args-out-of-range (cons base nil)))
	     (let ((len (length string))
		   (pos 0))
	       ;; skip leading whitespace.
	       (while (and (< pos len)
			   (memq (aref string pos) '(?\  ?\t)))
		 (setq pos (1+ pos)))
	       (if (= pos len)
		   0
		 (let ((number 0)(negative 1)
		       chr num)
		   (if (eq (aref string pos) ?-)
		       (setq negative -1
			     pos (1+ pos))
		     (if (eq (aref string pos) ?+)
			 (setq pos (1+ pos))))
		   (while (and (< pos len)
			       (setq chr (aref string pos)
				     num (cond
					  ((and (<= ?0 chr)(<= chr ?9))
					   (- chr ?0))
					  ((and (<= ?A chr)(<= chr ?F))
					   (+ (- chr ?A) 10))
					  ((and (<= ?a chr)(<= chr ?f))
					   (+ (- chr ?a) 10))
					  (t nil)))
			       (< num base))
		     (setq number (+ (* number base) num)
			   pos (1+ pos)))
		   (* negative number))))))))))

;; Emacs 20.1 and 20.2: (concat-chars &rest CHARS)
;; Emacs 20.3/XEmacs 21.0 and later: (string &rest CHARS)
(static-cond
 ((and (fboundp 'string)
       (subrp (symbol-function 'string)))
  ;; Emacs 20.3/XEmacs 21.0 and later.
  )
 ((and (fboundp 'concat-chars)
       (subrp (symbol-function 'concat-chars)))
  ;; Emacs 20.1 and 20.2.
  (defalias 'string 'concat-chars))
 (t
  ;; Use `defun-maybe' to update `load-history'.
  (defun-maybe string (&rest chars)
    "Concatenate all the argument characters and make the result a string."
    ;; We cannot use (apply 'concat chars) here because `concat' does not
    ;; work with multibyte chars on Mule 1.* and 2.*.
    (mapconcat (function char-to-string) chars ""))))

;; Mule: (char-before POS)
;; v20: (char-before &optional POS)
(static-condition-case nil
    ;; compile-time check.
    (progn
      (char-before)
      (if (get 'char-before 'defun-maybe)
	  (error "`char-before' is already defined")))
  (wrong-number-of-arguments            ; Mule.
   ;; load-time check.
   (or (fboundp 'si:char-before)
       (progn
         (fset 'si:char-before (symbol-function 'char-before))
         (put 'char-before 'defun-maybe t)
         ;; takes IGNORED for backward compatibility.
         (defun char-before (&optional pos ignored)
           "\
Return character in current buffer preceding position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
           (si:char-before (or pos (point)))))))
  (void-function                        ; non-Mule.
   ;; load-time check.
   (defun-maybe char-before (&optional pos)
     "\
Return character in current buffer preceding position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
     (if pos
         (save-excursion
           (and (= (goto-char pos) (point))
                (not (bobp))
                (preceding-char)))
       (and (not (bobp))
            (preceding-char)))))
  (error                                ; found our definition at compile-time.
   ;; load-time check.
   (condition-case nil
       (char-before)
     (wrong-number-of-arguments         ; Mule.
      (or (fboundp 'si:char-before)
          (progn
            (fset 'si:char-before (symbol-function 'char-before))
            (put 'char-before 'defun-maybe t)
            ;; takes IGNORED for backward compatibility.
            (defun char-before (&optional pos ignored)
              "\
Return character in current buffer preceding position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
              (si:char-before (or pos (point)))))))
     (void-function                     ; non-Mule.
      (defun-maybe char-before (&optional pos)
        "\
Return character in current buffer preceding position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
        (if pos
            (save-excursion
              (and (= (goto-char pos) (point))
                   (not (bobp))
                   (preceding-char)))
          (and (not (bobp))
               (preceding-char))))))))

;; v18, v19: (char-after POS)
;; v20: (char-after &optional POS)
(static-condition-case nil
    ;; compile-time check.
    (progn
      (char-after)
      (if (get 'char-after 'defun-maybe)
	  (error "`char-after' is already redefined")))
  (wrong-number-of-arguments		; v18, v19
   ;; load-time check.
   (or (fboundp 'si:char-after)
       (progn
         (fset 'si:char-after (symbol-function 'char-after))
         (put 'char-after 'defun-maybe t)
         (defun char-after (&optional pos)
           "\
Return character in current buffer at position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
           (si:char-after (or pos (point)))))))
  (void-function			; NEVER happen?
   ;; load-time check.
   (defun-maybe char-after (&optional pos)
     "\
Return character in current buffer at position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
     (if pos
         (save-excursion
           (and (= (goto-char pos) (point))
                (not (eobp))
                (following-char)))
       (and (not (eobp))
            (following-char)))))
  (error                                ; found our definition at compile-time.
   ;; load-time check.
   (condition-case nil
       (char-after)
     (wrong-number-of-arguments         ; v18, v19
      (or (fboundp 'si:char-after)
          (progn
            (fset 'si:char-after (symbol-function 'char-after))
            (put 'char-after 'defun-maybe t)
	    (defun char-after (&optional pos)
	      "\
Return character in current buffer at position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
	      (si:char-after (or pos (point)))))))
     (void-function                     ; NEVER happen?
      (defun-maybe char-after (&optional pos)
	"\
Return character in current buffer at position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
	(if pos
	    (save-excursion
	      (and (= (goto-char pos) (point))
		   (not (eobp))
		   (following-char)))
	  (and (not (eobp))
	       (following-char))))))))

;; Emacs 19.29 and later: (buffer-substring-no-properties START END)
(defun-maybe buffer-substring-no-properties (start end)
  "Return the characters of part of the buffer, without the text properties.
The two arguments START and END are character positions;
they can be in either order."
  (let ((string (buffer-substring start end)))
    (set-text-properties 0 (length string) nil string)
    string))

;; Emacs 19.31 and later: (buffer-live-p OBJECT)
(defun-maybe buffer-live-p (object)
  "Return non-nil if OBJECT is a buffer which has not been killed.
Value is nil if OBJECT is not a buffer or if it has been killed."
  (and object
       (get-buffer object)
       (buffer-name (get-buffer object))
       t))

;; Emacs 20: (line-beginning-position &optional N)
(defun-maybe line-beginning-position (&optional n)
  "Return the character position of the first character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
  (save-excursion
    (forward-line (1- (or n 1)))
    (point)))

;; Emacs 20: (line-end-position &optional N)
(defun-maybe line-end-position (&optional n)
  "Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
  (save-excursion
    (end-of-line (or n 1))
    (point)))

;; FSF Emacs 19.29 and later
;; (read-file-name PROMPT &optional DIR DEFAULT-FILENAME MUSTMATCH INITIAL)
;; XEmacs 19.14 and later:
;; (read-file-name (PROMPT &optional DIR DEFAULT MUST-MATCH INITIAL-CONTENTS
;;                         HISTORY)

;; In FSF Emacs 19.28 and earlier (except for v18) or XEmacs 19.13 and
;; earlier, this function is incompatible with the other Emacsen.
;; For instance, if DEFAULT-FILENAME is nil, INITIAL is not and user
;; enters a null string, it returns the visited file name of the current
;; buffer if it is non-nil.

;; It does not assimilate the different numbers of the optional arguments
;; on various Emacsen (yet).
(static-cond
 ((and (not (featurep 'xemacs))
       (eq emacs-major-version 19)
       (< emacs-minor-version 29))
  (if (fboundp 'si:read-file-name)
      nil
    (fset 'si:read-file-name (symbol-function 'read-file-name))
    (defun read-file-name (prompt &optional dir default-filename mustmatch
				  initial)
      "Read file name, prompting with PROMPT and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Default name to DEFAULT-FILENAME if user enters a null string.
 (If DEFAULT-FILENAME is omitted, the visited file name is used,
  except that if INITIAL is specified, that combined with DIR is used.)
Fourth arg MUSTMATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL specifies text to start with.
DIR defaults to current buffer's directory default."
      (si:read-file-name prompt dir
			 (or default-filename
			     (if initial
				 (expand-file-name initial dir)))
			 mustmatch initial))))
 ((and (featurep 'xemacs)
       (eq emacs-major-version 19)
       (< emacs-minor-version 14))
  (if (fboundp 'si:read-file-name)
      nil
    (fset 'si:read-file-name (symbol-function 'read-file-name))
    (defun read-file-name (prompt &optional dir default must-match
				  initial-contents history)
      "Read file name, prompting with PROMPT and completing in directory DIR.
This will prompt with a dialog box if appropriate, according to
 `should-use-dialog-box-p'.
Value is not expanded---you must call `expand-file-name' yourself.
Value is subject to interpreted by substitute-in-file-name however.
Default name to DEFAULT if user enters a null string.
 (If DEFAULT is omitted, the visited file name is used,
  except that if INITIAL-CONTENTS is specified, that combined with DIR is
  used.)
Fourth arg MUST-MATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL-CONTENTS specifies text to start with.
Sixth arg HISTORY specifies the history list to use.  Default is
 `file-name-history'.
DIR defaults to current buffer's directory default."
      (si:read-file-name prompt dir
			 (or default
			     (if initial-contents
				 (expand-file-name initial-contents dir)))
			 must-match initial-contents history)))))


;;; @ Basic lisp subroutines emulation. (lisp/subr.el)
;;;

;;; @@ Lisp language features.

(defmacro-maybe push (newelt listname)
  "Add NEWELT to the list stored in the symbol LISTNAME.
This is equivalent to (setq LISTNAME (cons NEWELT LISTNAME)).
LISTNAME must be a symbol."
  (list 'setq listname
	(list 'cons newelt listname)))

(defmacro-maybe pop (listname)
  "Return the first element of LISTNAME's value, and remove it from the list.
LISTNAME must be a symbol whose value is a list.
If the value is nil, `pop' returns nil but does not actually
change the list."
  (list 'prog1 (list 'car listname)
	(list 'setq listname (list 'cdr listname))))

(defmacro-maybe when (cond &rest body)
  "If COND yields non-nil, do BODY, else return nil."
  (list 'if cond (cons 'progn body)))
;; (def-edebug-spec when (&rest form))

(defmacro-maybe unless (cond &rest body)
  "If COND yields nil, do BODY, else return nil."
  (cons 'if (cons cond (cons nil body))))
;; (def-edebug-spec unless (&rest form))

(defsubst-maybe caar (x)
  "Return the car of the car of X."
  (car (car x)))

(defsubst-maybe cadr (x)
  "Return the car of the cdr of X."
  (car (cdr x)))

(defsubst-maybe cdar (x)
  "Return the cdr of the car of X."
  (cdr (car x)))

(defsubst-maybe cddr (x)
  "Return the cdr of the cdr of X."
  (cdr (cdr x)))

(defun-maybe last (x &optional n)
  "Return the last link of the list X.  Its car is the last element.
If X is nil, return nil.
If N is non-nil, return the Nth-to-last link of X.
If N is bigger than the length of X, return X."
  (if n
      (let ((m 0) (p x))
	(while (consp p)
	  (setq m (1+ m) p (cdr p)))
	(if (<= n 0) p
	  (if (< n m) (nthcdr (- m n) x) x)))
    (while (cdr x)
      (setq x (cdr x)))
    x))

;; Actually, `butlast' and `nbutlast' are defined in lisp/cl.el.
(defun-maybe butlast (x &optional n)
  "Returns a copy of LIST with the last N elements removed."
  (if (and n (<= n 0)) x
    (nbutlast (copy-sequence x) n)))

(defun-maybe nbutlast (x &optional n)
  "Modifies LIST to remove the last N elements."
  (let ((m (length x)))
    (or n (setq n 1))
    (and (< n m)
	 (progn
	   (if (> n 0) (setcdr (nthcdr (- (1- m) n) x) nil))
	   x))))

;; Emacs 20.3 and later: (assoc-default KEY ALIST &optional TEST DEFAULT)
(defun-maybe assoc-default (key alist &optional test default)
  "Find object KEY in a pseudo-alist ALIST.
ALIST is a list of conses or objects.  Each element (or the element's car,
if it is a cons) is compared with KEY by evaluating (TEST (car elt) KEY).
If that is non-nil, the element matches;
then `assoc-default' returns the element's cdr, if it is a cons,
or DEFAULT if the element is not a cons.

If no element matches, the value is nil.
If TEST is omitted or nil, `equal' is used."
  (let (found (tail alist) value)
    (while (and tail (not found))
      (let ((elt (car tail)))
	(when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
	  (setq found t value (if (consp elt) (cdr elt) default))))
      (setq tail (cdr tail)))
    value))

;; The following two function use `compare-strings', which we don't
;; support yet.
;; (defun assoc-ignore-case (key alist))
;; (defun assoc-ignore-representation (key alist))

;; Emacs 19.29/XEmacs 19.13 and later: (rassoc KEY LIST)
;; Actually, `rassoc' is defined in src/fns.c.
(defun-maybe rassoc (key list)
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.
The value is actually the element of LIST whose cdr equals KEY.
Elements of LIST that are not conses are ignored."
  (catch 'found
    (while list
      (cond ((not (consp (car list))))
	    ((equal (cdr (car list)) key)
	     (throw 'found (car list))))
      (setq list (cdr list)))))

;; XEmacs 19.13 and later: (remassoc KEY ALIST)
(defun-maybe remassoc (key alist)
  "Delete by side effect any elements of ALIST whose car is `equal' to KEY.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `equal' to KEY, there is no way to remove it by side effect;
therefore, write `(setq foo (remassoc key foo))' to be sure of changing
the value of `foo'."
  (while (and (consp alist)
              (or (not (consp (car alist)))
                  (equal (car (car alist)) key)))
    (setq alist (cdr alist)))
  (if (consp alist)
      (let ((prev alist)
            (tail (cdr alist)))
        (while (consp tail)
          (if (and (consp (car alist))
                   (equal (car (car tail)) key))
              ;; `(setcdr CELL NEWCDR)' returns NEWCDR.
              (setq tail (setcdr prev (cdr tail)))
            (setq prev (cdr prev)
                  tail (cdr tail))))))
  alist)

;; XEmacs 19.13 and later: (remassq KEY ALIST)
(defun-maybe remassq (key alist)
  "Delete by side effect any elements of ALIST whose car is `eq' to KEY.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `eq' to KEY, there is no way to remove it by side effect;
therefore, write `(setq foo (remassq key foo))' to be sure of changing
the value of `foo'."
  (while (and (consp alist)
              (or (not (consp (car alist)))
                  (eq (car (car alist)) key)))
    (setq alist (cdr alist)))
  (if (consp alist)
      (let ((prev alist)
            (tail (cdr alist)))
        (while (consp tail)
          (if (and (consp (car tail))
                   (eq (car (car tail)) key))
              ;; `(setcdr CELL NEWCDR)' returns NEWCDR.
              (setq tail (setcdr prev (cdr tail)))
            (setq prev (cdr prev)
                  tail (cdr tail))))))
  alist)

;; XEmacs 19.13 and later: (remrassoc VALUE ALIST)
(defun-maybe remrassoc (value alist)
  "Delete by side effect any elements of ALIST whose cdr is `equal' to VALUE.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `equal' to VALUE, there is no way to remove it by side effect;
therefore, write `(setq foo (remrassoc value foo))' to be sure of changing
the value of `foo'."
  (while (and (consp alist)
              (or (not (consp (car alist)))
                  (equal (cdr (car alist)) value)))
    (setq alist (cdr alist)))
  (if (consp alist)
      (let ((prev alist)
            (tail (cdr alist)))
        (while (consp tail)
          (if (and (consp (car tail))
                   (equal (cdr (car tail)) value))
              ;; `(setcdr CELL NEWCDR)' returns NEWCDR.
              (setq tail (setcdr prev (cdr tail)))
            (setq prev (cdr prev)
                  tail (cdr tail))))))
  alist)

;; XEmacs 19.13 and later: (remrassq VALUE ALIST)
(defun-maybe remrassq (value alist)
  "Delete by side effect any elements of ALIST whose cdr is `eq' to VALUE.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `eq' to VALUE, there is no way to remove it by side effect;
therefore, write `(setq foo (remrassq value foo))' to be sure of changing
the value of `foo'."
  (while (and (consp alist)
              (or (not (consp (car alist)))
                  (eq (cdr (car alist)) value)))
    (setq alist (cdr alist)))
  (if (consp alist)
      (let ((prev alist)
            (tail (cdr alist)))
        (while (consp tail)
          (if (and (consp (car tail))
                   (eq (cdr (car tail)) value))
              ;; `(setcdr CELL NEWCDR)' returns NEWCDR.
              (setq tail (setcdr prev (cdr tail)))
            (setq prev (cdr prev)
                  tail (cdr tail))))))
  alist)

;;; Define `functionp' here because "localhook" uses it.

;; Emacs 20.1/XEmacs 20.3 (but first appeared in Epoch?): (functionp OBJECT)
(defun-maybe functionp (object)
  "Non-nil if OBJECT is a type of object that can be called as a function."
  (or (subrp object) (byte-code-function-p object)
      (eq (car-safe object) 'lambda)
      (and (symbolp object) (fboundp object))))

;;; @@ Hook manipulation functions.

;; "localhook" package is written for Emacs 19.28 and earlier.
;; `run-hooks' was a lisp function in Emacs 19.29 and earlier.
;; So, in Emacs 19.29, `run-hooks' and others will be overrided.
;; But, who cares it?
(static-unless (subrp (symbol-function 'run-hooks))
  (require 'localhook))

;; Emacs 19.29/XEmacs 19.14(?) and later: (add-to-list LIST-VAR ELEMENT)
(defun-maybe add-to-list (list-var element)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (or (member element (symbol-value list-var))
      (set list-var (cons element (symbol-value list-var)))))

;; (eval-after-load FILE FORM)
;; Emacs 19.28 and earlier do not evaluate FORM if FILE is already loaded.
;; XEmacs 20.2 and earlier have `after-load-alist', but refuse to support
;; `eval-after-load'. (see comments in XEmacs/lisp/subr.el.)
(static-cond
 ((featurep 'xemacs)
  ;; for XEmacs 20.2 and earlier.
  (defun-maybe eval-after-load (file form)
    "Arrange that, if FILE is ever loaded, FORM will be run at that time.
This makes or adds to an entry on `after-load-alist'.
If FILE is already loaded, evaluate FORM right now.
It does nothing if FORM is already on the list for FILE.
FILE should be the name of a library, with no directory name."
    ;; Make sure there is an element for FILE.
    (or (assoc file after-load-alist)
	(setq after-load-alist (cons (list file) after-load-alist)))
    ;; Add FORM to the element if it isn't there.
    (let ((elt (assoc file after-load-alist)))
      (or (member form (cdr elt))
	  (progn
	    (nconc elt (list form))
	    ;; If the file has been loaded already, run FORM right away.
	    (and (assoc file load-history)
		 (eval form)))))
    form))
 ((>= emacs-major-version 20))
 ((and (= emacs-major-version 19)
       (< emacs-minor-version 29))
  ;; for Emacs 19.28 and earlier.
  (defun eval-after-load (file form)
    "Arrange that, if FILE is ever loaded, FORM will be run at that time.
This makes or adds to an entry on `after-load-alist'.
If FILE is already loaded, evaluate FORM right now.
It does nothing if FORM is already on the list for FILE.
FILE should be the name of a library, with no directory name."
    ;; Make sure there is an element for FILE.
    (or (assoc file after-load-alist)
	(setq after-load-alist (cons (list file) after-load-alist)))
    ;; Add FORM to the element if it isn't there.
    (let ((elt (assoc file after-load-alist)))
      (or (member form (cdr elt))
	  (progn
	    (nconc elt (list form))
	    ;; If the file has been loaded already, run FORM right away.
	    (and (assoc file load-history)
		 (eval form)))))
    form))
 (t
  ;; should emulate for v18?
  ))

(defun-maybe eval-next-after-load (file)
  "Read the following input sexp, and run it whenever FILE is loaded.
This makes or adds to an entry on `after-load-alist'.
FILE should be the name of a library, with no directory name."
  (eval-after-load file (read)))

;;; @@ Input and display facilities.

;; XXX: (defun read-passwd (prompt &optional confirm default))

;;; @@ Miscellanea.

;; Avoid compiler warnings about this variable,
;; which has a special meaning on certain system types.
(defvar-maybe buffer-file-type nil
  "Non-nil if the visited file is a binary file.
This variable is meaningful on MS-DOG and Windows NT.
On those systems, it is automatically local in every buffer.
On other systems, this variable is normally always nil.")

;; Emacs 20.3 or later.
(defvar-maybe minor-mode-overriding-map-alist nil
  "Alist of keymaps to use for minor modes, in current major mode.
APEL provides this as dummy for compatibility.")

;; Emacs 20.1/XEmacs 20.3(?) and later: (save-current-buffer &rest BODY)
;;
;; v20 defines `save-current-buffer' as a C primitive (in src/editfns.c)
;; and introduces a new bytecode Bsave_current_buffer(_1), replacing an
;; obsolete bytecode Bread_char.  To make things worse, Emacs 20.1 and
;; 20.2 have a bug that it will restore the current buffer without
;; confirming that it is alive.
;;
;; This is a source of incompatibility of .elc between v18/v19 and v20.
;; (XEmacs compiler takes care of it if compatibility mode is enabled.)
(defmacro-maybe save-current-buffer (&rest body)
  "Save the current buffer; execute BODY; restore the current buffer.
Executes BODY just like `progn'."
  (` (let ((orig-buffer (current-buffer)))
       (unwind-protect
	   (progn (,@ body))
	 (if (buffer-live-p orig-buffer)
	     (set-buffer orig-buffer))))))

;; Emacs 20.1/XEmacs 20.3(?) and later: (with-current-buffer BUFFER &rest BODY)
(defmacro-maybe with-current-buffer (buffer &rest body)
  "Execute the forms in BODY with BUFFER as the current buffer.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  (` (save-current-buffer
       (set-buffer (, buffer))
       (,@ body))))

;; Emacs 20.1/XEmacs 20.3(?) and later: (with-temp-file FILE &rest FORMS)
(defmacro-maybe with-temp-file (file &rest forms)
  "Create a new buffer, evaluate FORMS there, and write the buffer to FILE.
The value of the last form in FORMS is returned, like `progn'.
See also `with-temp-buffer'."
  (let ((temp-file (make-symbol "temp-file"))
	(temp-buffer (make-symbol "temp-buffer")))
    (` (let (((, temp-file) (, file))
	     ((, temp-buffer)
	      (get-buffer-create (generate-new-buffer-name " *temp file*"))))
	 (unwind-protect
	     (prog1
		 (with-current-buffer (, temp-buffer)
		   (,@ forms))
	       (with-current-buffer (, temp-buffer)
		 (widen)
		 (write-region (point-min) (point-max) (, temp-file) nil 0)))
	   (and (buffer-name (, temp-buffer))
		(kill-buffer (, temp-buffer))))))))

;; Emacs 20.4 and later: (with-temp-message MESSAGE &rest BODY)
;; This macro uses `current-message', which appears in v20.
(static-when (and (fboundp 'current-message)
		  (subrp (symbol-function 'current-message)))
  (defmacro-maybe with-temp-message (message &rest body)
    "\
Display MESSAGE temporarily if non-nil while BODY is evaluated.
The original message is restored to the echo area after BODY has finished.
The value returned is the value of the last form in BODY.
MESSAGE is written to the message log buffer if `message-log-max' is non-nil.
If MESSAGE is nil, the echo area and message log buffer are unchanged.
Use a MESSAGE of \"\" to temporarily clear the echo area."
    (let ((current-message (make-symbol "current-message"))
	  (temp-message (make-symbol "with-temp-message")))
      (` (let (((, temp-message) (, message))
	       ((, current-message)))
	   (unwind-protect
	       (progn
		 (when (, temp-message)
		   (setq (, current-message) (current-message))
		   (message "%s" (, temp-message))
		   (,@ body))
		 (and (, temp-message) (, current-message)
		      (message "%s" (, current-message))))))))))

;; Emacs 20.1/XEmacs 20.3(?) and later: (with-temp-buffer &rest FORMS)
(defmacro-maybe with-temp-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'.
See also `with-temp-file' and `with-output-to-string'."
  (let ((temp-buffer (make-symbol "temp-buffer")))
    (` (let (((, temp-buffer)
	      (get-buffer-create (generate-new-buffer-name " *temp*"))))
	 (unwind-protect
	     (with-current-buffer (, temp-buffer)
	       (,@ forms))
	   (and (buffer-name (, temp-buffer))
		(kill-buffer (, temp-buffer))))))))

;; Emacs 20.1/XEmacs 20.3(?) and later: (with-output-to-string &rest BODY)
(defmacro-maybe with-output-to-string (&rest body)
  "Execute BODY, return the text it sent to `standard-output', as a string."
  (` (let ((standard-output
	    (get-buffer-create (generate-new-buffer-name " *string-output*"))))
       (let ((standard-output standard-output))
	 (,@ body))
       (with-current-buffer standard-output
	 (prog1
	     (buffer-string)
	   (kill-buffer nil))))))

;; Emacs 20.1 and later: (combine-after-change-calls &rest BODY)
(defmacro-maybe combine-after-change-calls (&rest body)
  "Execute BODY, but don't call the after-change functions till the end.
If BODY makes changes in the buffer, they are recorded
and the functions on `after-change-functions' are called several times
when BODY is finished.
The return value is the value of the last form in BODY.

If `before-change-functions' is non-nil, then calls to the after-change
functions can't be deferred, so in that case this macro has no effect.

Do not alter `after-change-functions' or `before-change-functions'
in BODY.

This emulating macro does not support after-change functions at all,
just execute BODY."
  (cons 'progn body))

;; Emacs 19.29/XEmacs 19.14(?) and later: (match-string NUM &optional STRING)
(defun-maybe match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

;; Emacs 20.3 and later: (match-string-no-properties NUM &optional STRING)
(defun-maybe match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (let ((result
		 (substring string (match-beginning num) (match-end num))))
	    (set-text-properties 0 (length result) nil result)
	    result)
	(buffer-substring-no-properties (match-beginning num)
					(match-end num)))))

;; Emacs 19.28 and earlier
;;  (replace-match NEWTEXT &optional FIXEDCASE LITERAL)
;; Emacs 20.x (?) and later
;;  (replace-match NEWTEXT &optional FIXEDCASE LITERAL STRING SUBEXP)
;; XEmacs 21:
;;  (replace-match NEWTEXT &optional FIXEDCASE LITERAL STRING STRBUFFER)
;; We support following API.
;;  (replace-match NEWTEXT &optional FIXEDCASE LITERAL STRING)
(static-condition-case nil
    ;; compile-time check
    (progn
      (string-match "" "")
      (replace-match "" nil nil "")
      (if (get 'replace-match 'defun-maybe)
	  (error "`replace-match' is already defined")))
  (wrong-number-of-arguments ; Emacs 19.28 and earlier
   ;; load-time check.
   (or (fboundp 'si:replace-match)
       (progn
	 (fset 'si:replace-match (symbol-function 'replace-match))
	 (put 'replace-match 'defun-maybe t)
	 (defun replace-match (newtext &optional fixedcase literal string)
	   "Replace text matched by last search with NEWTEXT.
If second arg FIXEDCASE is non-nil, do not alter case of replacement text.
Otherwise maybe capitalize the whole text, or maybe just word initials,
based on the replaced text.
If the replaced text has only capital letters
and has at least one multiletter word, convert NEWTEXT to all caps.
If the replaced text has at least one word starting with a capital letter,
then capitalize each word in NEWTEXT.

If third arg LITERAL is non-nil, insert NEWTEXT literally.
Otherwise treat `\' as special:
  `\&' in NEWTEXT means substitute original matched text.
  `\N' means substitute what matched the Nth `\(...\)'.
       If Nth parens didn't match, substitute nothing.
  `\\' means insert one `\'.
FIXEDCASE and LITERAL are optional arguments.
Leaves point at end of replacement text.

The optional fourth argument STRING can be a string to modify.
In that case, this function creates and returns a new string
which is made by replacing the part of STRING that was matched."
	   (if string
	       (with-temp-buffer
		(save-match-data
		  (insert string)
		  (let* ((matched (match-data))
			 (beg (nth 0 matched))
			 (end (nth 1 matched)))
		    (store-match-data
		     (list
		      (if (markerp beg)
			  (move-marker beg (1+ (match-beginning 0)))
			(1+ (match-beginning 0)))
		      (if (markerp end)
			  (move-marker end (1+ (match-end 0)))
			(1+ (match-end 0))))))
		  (si:replace-match newtext fixedcase literal)
		  (buffer-string)))
	     (si:replace-match newtext fixedcase literal))))))
  (error ; found our definition at compile-time.
   ;; load-time check.
   (condition-case nil
    (progn
      (string-match "" "")
      (replace-match "" nil nil ""))
    (wrong-number-of-arguments ; Emacs 19.28 and earlier
     ;; load-time check.
     (or (fboundp 'si:replace-match)
	 (progn
	   (fset 'si:replace-match (symbol-function 'replace-match))
	   (put 'replace-match 'defun-maybe t)
	   (defun replace-match (newtext &optional fixedcase literal string)
	     "Replace text matched by last search with NEWTEXT.
If second arg FIXEDCASE is non-nil, do not alter case of replacement text.
Otherwise maybe capitalize the whole text, or maybe just word initials,
based on the replaced text.
If the replaced text has only capital letters
and has at least one multiletter word, convert NEWTEXT to all caps.
If the replaced text has at least one word starting with a capital letter,
then capitalize each word in NEWTEXT.

If third arg LITERAL is non-nil, insert NEWTEXT literally.
Otherwise treat `\' as special:
  `\&' in NEWTEXT means substitute original matched text.
  `\N' means substitute what matched the Nth `\(...\)'.
       If Nth parens didn't match, substitute nothing.
  `\\' means insert one `\'.
FIXEDCASE and LITERAL are optional arguments.
Leaves point at end of replacement text.

The optional fourth argument STRING can be a string to modify.
In that case, this function creates and returns a new string
which is made by replacing the part of STRING that was matched."
	     (if string
		 (with-temp-buffer
		  (save-match-data
		    (insert string)
		    (let* ((matched (match-data))
			   (beg (nth 0 matched))
			   (end (nth 1 matched)))
		      (store-match-data
		       (list
			(if (markerp beg)
			    (move-marker beg (1+ (match-beginning 0)))
			  (1+ (match-beginning 0)))
			(if (markerp end)
			    (move-marker end (1+ (match-end 0)))
			  (1+ (match-end 0))))))
		    (si:replace-match newtext fixedcase literal)
		    (buffer-string)))
	       (si:replace-match newtext fixedcase literal)))))))))

;; Emacs 20: (format-time-string FORMAT &optional TIME UNIVERSAL)
;; Those format constructs are yet to be implemented.
;;   %c, %C, %j, %U, %W, %x, %X
;; Not fully compatible especially when invalid format is specified.
(static-unless (and (fboundp 'format-time-string)
		    (not (get 'format-time-string 'defun-maybe)))
  (or (fboundp 'format-time-string)
  (progn
  (defconst format-time-month-list
    '(( "Zero" . ("Zero" . 0))
      ("Jan" . ("January" . 1)) ("Feb" . ("February" . 2))
      ("Mar" . ("March" . 3)) ("Apr" . ("April" . 4)) ("May" . ("May" . 5))
      ("Jun" . ("June" . 6))("Jul" . ("July" . 7)) ("Aug" . ("August" . 8))
      ("Sep" . ("September" . 9)) ("Oct" . ("October" . 10))
      ("Nov" . ("November" . 11)) ("Dec" . ("December" . 12)))
    "Alist of months and their number.")

  (defconst format-time-week-list
    '(("Sun" . ("Sunday" . 0)) ("Mon" . ("Monday" . 1))
      ("Tue" . ("Tuesday" . 2)) ("Wed" . ("Wednesday" . 3))
      ("Thu" . ("Thursday" . 4)) ("Fri" . ("Friday" . 5))
      ("Sat" . ("Saturday" . 6)))
    "Alist of weeks and their number.")

  (defun format-time-string (format &optional time universal)
    "Use FORMAT-STRING to format the time TIME, or now if omitted.
TIME is specified as (HIGH LOW . IGNORED) or (HIGH . LOW), as returned by
`current-time' or `file-attributes'.
The third, optional, argument UNIVERSAL, if non-nil, means describe TIME
as Universal Time; nil means describe TIME in the local time zone.
The value is a copy of FORMAT-STRING, but with certain constructs replaced
by text that describes the specified date and time in TIME:

%Y is the year, %y within the century, %C the century.
%G is the year corresponding to the ISO week, %g within the century.
%m is the numeric month.
%b and %h are the locale's abbreviated month name, %B the full name.
%d is the day of the month, zero-padded, %e is blank-padded.
%u is the numeric day of week from 1 (Monday) to 7, %w from 0 (Sunday) to 6.
%a is the locale's abbreviated name of the day of week, %A the full name.
%U is the week number starting on Sunday, %W starting on Monday,
 %V according to ISO 8601.
%j is the day of the year.

%H is the hour on a 24-hour clock, %I is on a 12-hour clock, %k is like %H
 only blank-padded, %l is like %I blank-padded.
%p is the locale's equivalent of either AM or PM.
%M is the minute.
%S is the second.
%Z is the time zone name, %z is the numeric form.
%s is the number of seconds since 1970-01-01 00:00:00 +0000.

%c is the locale's date and time format.
%x is the locale's \"preferred\" date format.
%D is like \"%m/%d/%y\".

%R is like \"%H:%M\", %T is like \"%H:%M:%S\", %r is like \"%I:%M:%S %p\".
%X is the locale's \"preferred\" time format.

Finally, %n is a newline, %t is a tab, %% is a literal %.

Certain flags and modifiers are available with some format controls.
The flags are `_' and `-'.  For certain characters X, %_X is like %X,
but padded with blanks; %-X is like %X, but without padding.
%NX (where N stands for an integer) is like %X,
but takes up at least N (a number) positions.
The modifiers are `E' and `O'.  For certain characters X,
%EX is a locale's alternative version of %X;
%OX is like %X, but uses the locale's number symbols.

For example, to produce full ISO 8601 format, use \"%Y-%m-%dT%T%z\".

Compatibility Note.

Those format constructs are yet to be implemented.
  %c, %C, %j, %U, %W, %x, %X
Not fully compatible especially when invalid format is specified."
    (let ((fmt-len (length format))
	  (ind 0)
	  prev-ind
	  cur-char
	  (prev-char nil)
	  strings-so-far
	  (result "")
	  field-width
	  field-result
	  pad-left change-case
	  (paren-level 0)
	  hour ms ls
	  (tz (car (current-time-zone)))
	  time-string)
      (if universal
	  (progn
	    (or time
		(setq time (current-time)))
	    (setq ms (car time)
		  ls (- (nth 1 time) tz))
	    (cond ((< ls 0)
		   (setq ms (1- ms)
			 ls (+ ls 65536)))
		  ((>= ls 65536)
		   (setq ms (1+ ms)
			 ls (- ls 65536))))
	    (setq time (append (list ms ls) (nth 2 time)))))
      (setq time-string (current-time-string time)
	    hour (string-to-int (substring time-string 11 13)))
      (while (< ind fmt-len)
	(setq cur-char (aref format ind))
	(setq
	 result
	 (concat result
	(cond
	 ((eq cur-char ?%)
	  ;; eat any additional args to allow for future expansion, not!!
	  (setq pad-left nil change-case nil field-width "" prev-ind ind
		strings-so-far "")
;	  (catch 'invalid
	  (while (progn
		   (setq ind (1+ ind))
		   (setq cur-char (if (< ind fmt-len)
				      (aref format ind)
				    ?\0))
		   (or (eq ?- cur-char) ; pad on left
		       (eq ?# cur-char) ; case change
		       (if (and (string-equal field-width "")
				(<= ?0 cur-char) (>= ?9 cur-char))
			   ;; get format width
			   (let ((field-index ind))
			     (while (progn
				      (setq ind (1+ ind))
				      (setq cur-char (if (< ind fmt-len)
							 (aref format ind)
						       ?\0))
				      (and (<= ?0 cur-char) (>= ?9 cur-char))))
			     (setq field-width
				   (substring format field-index ind))
			     (setq ind (1- ind)
				   cur-char nil)
			     t))))
	    (setq prev-char cur-char
		  strings-so-far (concat strings-so-far
					 (if cur-char
					     (char-to-string cur-char)
					   field-width)))
	    ;; characters we actually use
	    (cond ((eq cur-char ?-)
		   ;; padding to left must be specified before field-width
		   (setq pad-left (string-equal field-width "")))
		  ((eq cur-char ?#)
		   (setq change-case t))))
	  (setq field-result
		(cond
		 ((eq cur-char ?%)
		  "%")
		 ;; the abbreviated name of the day of week.
		 ((eq cur-char ?a)
		  (substring time-string 0 3))
		 ;; the full name of the day of week
		 ((eq cur-char ?A)
		  (cadr (assoc (substring time-string 0 3)
			       format-time-week-list)))
		 ;; the abbreviated name of the month
		 ((eq cur-char ?b)
		  (substring time-string 4 7))
		 ;; the full name of the month
		 ((eq cur-char ?B)
		  (cadr (assoc (substring time-string 4 7)
			       format-time-month-list)))
		 ;; a synonym for `%x %X' (yet to come)
		 ((eq cur-char ?c)
		  "")
		 ;; locale specific (yet to come)
		 ((eq cur-char ?C)
		  "")
		 ;; the day of month, zero-padded
		 ((eq cur-char ?d)
		  (format "%02d" (string-to-int (substring time-string 8 10))))
		 ;; a synonym for `%m/%d/%y'
		 ((eq cur-char ?D)
		  (format "%02d/%02d/%s"
			  (cddr (assoc (substring time-string 4 7)
				       format-time-month-list))
			  (string-to-int (substring time-string 8 10))
			  (substring time-string -2)))
		 ;; the day of month, blank-padded
		 ((eq cur-char ?e)
		  (format "%2d" (string-to-int (substring time-string 8 10))))
		 ;; a synonym for `%b'
		 ((eq cur-char ?h)
		  (substring time-string 4 7))
		 ;; the hour (00-23)
		 ((eq cur-char ?H)
		  (substring time-string 11 13))
		 ;; the hour (00-12)
		 ((eq cur-char ?I)
		  (format "%02d" (if (> hour 12) (- hour 12) hour)))
		 ;; the day of the year (001-366) (yet to come)
		 ((eq cur-char ?j)
		  "")
		 ;; the hour (0-23), blank padded
		 ((eq cur-char ?k)
		  (format "%2d" hour))
		 ;; the hour (1-12), blank padded
		 ((eq cur-char ?l)
		  (format "%2d" (if (> hour 12) (- hour 12) hour)))
		 ;; the month (01-12)
		 ((eq cur-char ?m)
		  (format "%02d" (cddr (assoc (substring time-string 4 7)
					      format-time-month-list))))
		 ;; the minute (00-59)
		 ((eq cur-char ?M)
		  (substring time-string 14 16))
		 ;; a newline
		 ((eq cur-char ?n)
		  "\n")
		 ;; `AM' or `PM', as appropriate
		 ((eq cur-char ?p)
		  (setq change-case (not change-case))
		  (if (> hour 12) "pm" "am"))
		 ;; a synonym for `%I:%M:%S %p'
		 ((eq cur-char ?r)
		  (format "%02d:%s:%s %s"
			  (if (> hour 12) (- hour 12) hour)
			  (substring time-string 14 16)
			  (substring time-string 17 19)
			  (if (> hour 12) "PM" "AM")))
		 ;; a synonym for `%H:%M'
		 ((eq cur-char ?R)
		  (format "%s:%s"
			  (substring time-string 11 13)
			  (substring time-string 14 16)))
		 ;; the seconds (00-60)
		 ((eq cur-char ?S)
		  (substring time-string 17 19))
		 ;; a tab character
		 ((eq cur-char ?t)
		  "\t")
		 ;; a synonym for `%H:%M:%S'
		 ((eq cur-char ?T)
		  (format "%s:%s:%s"
			  (substring time-string 11 13)
			  (substring time-string 14 16)
			  (substring time-string 17 19)))
		 ;; the week of the year (01-52), assuming that weeks
		 ;; start on Sunday (yet to come)
		 ((eq cur-char ?U)
		  "")
		 ;; the numeric day of week (0-6).  Sunday is day 0
		 ((eq cur-char ?w)
		  (format "%d" (cddr (assoc (substring time-string 0 3)
					    format-time-week-list))))
		 ;; the week of the year (01-52), assuming that weeks
		 ;; start on Monday (yet to come)
		 ((eq cur-char ?W)
		  "")
		 ;; locale specific (yet to come)
		 ((eq cur-char ?x)
		  "")
		 ;; locale specific (yet to come)
		 ((eq cur-char ?X)
		  "")
		 ;; the year without century (00-99)
		 ((eq cur-char ?y)
		  (substring time-string -2))
		 ;; the year with century
		 ((eq cur-char ?Y)
		  (substring time-string -4))
		 ;; the time zone abbreviation
		 ((eq cur-char ?Z)
		  (if universal
		      "UTC"
		    (setq change-case (not change-case))
		    (downcase (cadr (current-time-zone)))))
		 ((eq cur-char ?z)
		  (if universal
		      "+0000"
		    (if (< tz 0)
			(format "-%02d%02d"
				(/ (- tz) 3600) (/ (% (- tz) 3600) 60))
		      (format "+%02d%02d"
			      (/ tz 3600) (/ (% tz 3600) 60)))))
		 (t
		  (concat
		   "%"
		   strings-so-far
		   (char-to-string cur-char)))))
;		  (setq ind prev-ind)
;		  (throw 'invalid "%"))))
	  (if (string-equal field-width "")
	      (if change-case (upcase field-result) field-result)
	    (let ((padded-result
		   (format (format "%%%s%s%c"
				   ""	; pad on left is ignored
;				   (if pad-left "-" "")
				   field-width
				   ?s)
			   (or field-result ""))))
	      (let ((initial-length (length padded-result))
		    (desired-length (string-to-int field-width)))
		(when (and (string-match "^0" field-width)
			   (string-match "^ +" padded-result))
		  (setq padded-result
			(replace-match
			 (make-string
			  (length (match-string 0 padded-result)) ?0)
			 nil nil padded-result)))
		(if (> initial-length desired-length)
		    ;; truncate strings on right, years on left
		    (if (stringp field-result)
			(substring padded-result 0 desired-length)
		      (if (eq cur-char ?y)
			  (substring padded-result (- desired-length))
			padded-result))) ;non-year numbers don't truncate
		(if change-case (upcase padded-result) padded-result))))) ;)
	 (t
	  (char-to-string cur-char)))))
	(setq ind (1+ ind)))
      result))
  ;; for `load-history'.
  (setq current-load-list (cons 'format-time-string current-load-list))
  (put 'format-time-string 'defun-maybe t))))

;; Emacs 19.29-19.34/XEmacs: `format-time-string' neither supports the
;; format string "%z" nor the third argument `universal'.
(unless (string-match "\\`[---+][0-9]+\\'"
		      (format-time-string "%z" (current-time)))
  (defadvice format-time-string
    (before support-timezone-in-numeric-form-and-3rd-arg
	    (format-string &optional time universal) activate compile)
    "Advice to support the construct `%z' and the third argument `universal'."
    (let ((tz (car (current-time-zone)))
	  case-fold-search ms ls)
      (while (string-match "\\(\\(\\`\\|[^%]\\)\\(%%\\)*\\)%z" format-string)
	(setq format-string
	      (concat (substring format-string 0 (match-end 1))
		      (if universal
			  "+0000"
			(if (< tz 0)
			    (format "-%02d%02d"
				    (/ (- tz) 3600) (/ (% (- tz) 3600) 60))
			  (format "+%02d%02d"
				  (/ tz 3600) (/ (% tz 3600) 60))))
		      (substring format-string (match-end 0)))))
      (if universal
	  (progn
	    (while (string-match "\\(\\(\\`\\|[^%]\\)\\(%%\\)*\\)%Z"
				 format-string)
	      (setq format-string
		    (concat (substring format-string 0 (match-end 1))
			    "UTC"
			    (substring format-string (match-end 0)))))
	    (or time
		(setq time (current-time)))
	    (setq ms (car time)
		  ls (- (nth 1 time) tz))
	    (cond ((< ls 0)
		   (setq ms (1- ms)
			 ls (+ ls 65536)))
		  ((>= ls 65536)
		   (setq ms (1+ ms)
			 ls (- ls 65536))))
	    (setq time (append (list ms ls) (nth 2 time))))))))

(defconst-maybe split-string-default-separators "[ \f\t\n\r\v]+"
  "The default value of separators for `split-string'.

A regexp matching strings of whitespace.  May be locale-dependent
\(as yet unimplemented).  Should not match non-breaking spaces.

Warning: binding this to a different value and using it as default is
likely to have undesired semantics.")

;; Here is a Emacs 22 version. OMIT-NULLS
(defun-maybe split-string (string &optional separators omit-nulls)
  "Split STRING into substrings bounded by matches for SEPARATORS.

The beginning and end of STRING, and each match for SEPARATORS, are
splitting points.  The substrings matching SEPARATORS are removed, and
the substrings between the splitting points are collected as a list,
which is returned.

If SEPARATORS is non-nil, it should be a regular expression matching text
which separates, but is not part of, the substrings.  If nil it defaults to
`split-string-default-separators', normally \"[ \\f\\t\\n\\r\\v]+\", and
OMIT-NULLS is forced to t.

If OMIT-NULLS is t, zero-length substrings are omitted from the list \(so
that for the default value of SEPARATORS leading and trailing whitespace
are effectively trimmed).  If nil, all zero-length substrings are retained,
which correctly parses CSV format, for example.

Note that the effect of `(split-string STRING)' is the same as
`(split-string STRING split-string-default-separators t)').  In the rare
case that you wish to retain zero-length substrings when splitting on
whitespace, use `(split-string STRING split-string-default-separators)'.

Modifies the match data; use `save-match-data' if necessary."
  (let ((keep-nulls (not (if separators omit-nulls t)))
	(rexp (or separators split-string-default-separators))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< start (length string)))
      (setq notfirst t)
      (if (or keep-nulls (< start (match-beginning 0)))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (if (or keep-nulls (< start (length string)))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))


;;; @ Window commands emulation. (lisp/window.el)
;;;

(defmacro-maybe save-selected-window (&rest body)
  "Execute BODY, then select the window that was selected before BODY."
  (list 'let
	'((save-selected-window-window (selected-window)))
	(list 'unwind-protect
	      (cons 'progn body)
	      (list 'select-window 'save-selected-window-window))))

;; Emacs 19.31 and later:
;;  (get-buffer-window-list &optional BUFFER MINIBUF FRAME)
(defun-maybe get-buffer-window-list (buffer &optional minibuf frame)
  "Return windows currently displaying BUFFER, or nil if none.
See `walk-windows' for the meaning of MINIBUF and FRAME."
  (let ((buffer (if (bufferp buffer) buffer (get-buffer buffer))) windows)
    (walk-windows
     (function (lambda (window)
		 (if (eq (window-buffer window) buffer)
		     (setq windows (cons window windows)))))
     minibuf frame)
    windows))


;;; @ Frame commands emulation. (lisp/frame.el)
;;;

;; XEmacs 21.0 and later:
;;  (save-selected-frame &rest BODY)
(defmacro-maybe save-selected-frame (&rest body)
  "Execute forms in BODY, then restore the selected frame."
  (list 'let
	'((save-selected-frame-frame (selected-frame)))
	(list 'unwind-protect
	      (cons 'progn body)
	      (list 'select-frame 'save-selected-frame-frame))))


;;; @ Basic editing commands emulation. (lisp/simple.el)
;;;


;;; @ File input and output commands emulation. (lisp/files.el)
;;;

(defvar-maybe temporary-file-directory
  (file-name-as-directory
   (cond ((memq system-type '(ms-dos windows-nt))
	  (or (getenv "TEMP") (getenv "TMPDIR") (getenv "TMP") "c:/temp"))
	 ((memq system-type '(vax-vms axp-vms))
	  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "SYS$SCRATCH:"))
	 (t
	  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp"))))
  "The directory for writing temporary files.")

;; Emacs 21 CVS         ; nothing to do.
;;  (make-temp-file PREFIX &optional DIR-FLAG SUFFIX)
;;
;; Emacs 21.1-21.3      ; replace with CVS version of `make-temp-file'.
;;  (make-temp-file PREFIX &optional DIR-FLAG)
;;
;; Emacs 20 and earlier ; install our version of `make-temp-file', for
;;  or XEmacs		; single-user system or for multi-user system.
(eval-when-compile
  (cond
   ((get 'make-temp-file 'defun-maybe)
    ;; this form is already evaluated during compilation.
    )
   ((not (fboundp 'make-temp-file))
    ;; Emacs 20 and earlier, or XEmacs.
    (put 'make-temp-file 'defun-maybe 'none))
   (t
    (let* ((object (symbol-function 'make-temp-file))
           (arglist (cond
                     ((byte-code-function-p object)
                      (if (fboundp 'compiled-function-arglist)
                          (compiled-function-arglist object)
                        (aref object 0)))
                     ((eq (car-safe object) 'lambda)
                      (nth 1 object))
                     ;; `make-temp-file' is a built-in.
                     )))
      ;; arglist: (prefix &optional dir-flag suffix)
      (cond
       ((not arglist)
        ;; `make-temp-file' is a built-in; expects 3-args.
        (put 'make-temp-file 'defun-maybe '3-args))
       ((> (length arglist) 3)
        ;; Emacs 21 CVS.
        (put 'make-temp-file 'defun-maybe '3-args))
       (t
        ;; Emacs 21.1-21.3
        (put 'make-temp-file 'defun-maybe '2-args)))))))

(static-cond
 ((eq (get 'make-temp-file 'defun-maybe) '3-args)
  (put 'make-temp-file 'defun-maybe '3-args))
 ((eq (get 'make-temp-file 'defun-maybe) '2-args)
  (put 'make-temp-file 'defun-maybe '2-args)
  (or (fboundp 'si:make-temp-file)
      (fset 'si:make-temp-file (symbol-function 'make-temp-file)))
  (setq current-load-list (cons 'make-temp-file current-load-list))
  (defun make-temp-file (prefix &optional dir-flag suffix)
    "\
Create a temporary file.
The returned file name (created by appending some random characters at the end
of PREFIX, and expanding against `temporary-file-directory' if necessary),
is guaranteed to point to a newly created empty file.
You can then use `write-region' to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file.

If SUFFIX is non-nil, add that at the end of the file name."
    (let ((umask (default-file-modes))
          file)
      (unwind-protect
          (progn
            ;; Create temp files with strict access rights.  
            ;; It's easy toloosen them later, whereas it's impossible
            ;;  to close the time-window of loose permissions otherwise.
            (set-default-file-modes 448)
            (while (condition-case ()
                       (progn
                         (setq file
                               (make-temp-name
                                (expand-file-name
                                 prefix temporary-file-directory)))
                         (if suffix
                             (setq file (concat file suffix)))
                         (if dir-flag
                             (make-directory file)
                           (write-region "" nil file nil
                                         'silent nil 'excl))
                         nil)
                     (file-already-exists t))
              ;; the file was somehow created by someone else between
              ;; `make-temp-name' and `write-region', let's try again.
              nil)
            file)
        ;; Reset the umask.
        (set-default-file-modes umask)))))
 ((eq (get 'make-temp-file 'defun-maybe) 'none)
  (put 'make-temp-file 'defun-maybe 'none)
  (setq current-load-list (cons 'make-temp-file current-load-list))
  ;; must be load-time check to share .elc between different systems.
  (cond
   ((memq system-type '(windows-nt ms-dos OS/2 emx))
    ;; for single-user systems.
    (defun make-temp-file (prefix &optional dir-flag suffix)
      "Create a temporary file.
The returned file name (created by appending some random characters at the end
of PREFIX, and expanding against `temporary-file-directory' if necessary),
is guaranteed to point to a newly created empty file.
You can then use `write-region' to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file.

If SUFFIX is non-nil, add that at the end of the file name."
      (let ((file (make-temp-name
                   (expand-file-name prefix temporary-file-directory))))
        (if suffix
            (setq file (concat file suffix)))
        (if dir-flag
            (make-directory file)
          (write-region "" nil file nil 'silent))
        file)))
   (t
    ;; for multi-user systems.
    (defun make-temp-file (prefix &optional dir-flag suffix)
      "Create a temporary file.
The returned file name (created by appending some random characters at the end
of PREFIX, and expanding against `temporary-file-directory' if necessary),
is guaranteed to point to a newly created empty file.
You can then use `write-region' to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file.

If SUFFIX is non-nil, add that at the end of the file name."
      (let ((prefix (expand-file-name prefix temporary-file-directory)))
        (if dir-flag
            ;; Create a new empty directory.
            (let (dir)
              (while (condition-case ()
                         (progn
                           (setq dir (make-temp-name prefix))
                           (if suffix
                               (setq dir (concat dir suffix)))
                           ;; `make-directory' returns nil for success,
                           ;; otherwise signals an error.
                           (make-directory dir))
                       ;; the dir was somehow created by someone else
                       ;; between `make-temp-name' and `make-directory',
                       ;; let's try again.
                       (file-already-exists t)))
              (set-file-modes dir 448)
              dir)
          ;; Create a new empty file.
          (let (tempdir tempfile)
            (unwind-protect
                (let (file)
                  ;; First, create a temporary directory.
                  (while (condition-case ()
                             (progn
                               (setq tempdir (make-temp-name
                                              (concat
                                               (file-name-directory prefix)
                                               "DIR")))
                               ;; return nil or signal an error.
                               (make-directory tempdir))
                           ;; let's try again.
                           (file-already-exists t)))
                  (set-file-modes tempdir 448)
                  ;; Second, create a temporary file in the tempdir.
                  ;; There *is* a race condition between `make-temp-name'
                  ;; and `write-region', but we don't care it since we are
                  ;; in a private directory now.
                  (setq tempfile (make-temp-name (concat tempdir "/EMU")))
                  (write-region "" nil tempfile nil 'silent)
                  (set-file-modes tempfile 384)
                  ;; Finally, make a hard-link from the tempfile.
                  (while (condition-case ()
                             (progn
                               (setq file (make-temp-name prefix))
                               (if suffix
                                   (setq file (concat file suffix)))
                               ;; return nil or signal an error.
                               (add-name-to-file tempfile file))
                           ;; let's try again.
                           (file-already-exists t)))
                  file)
              ;; Cleanup the tempfile.
              (and tempfile
                   (file-exists-p tempfile)
                   (delete-file tempfile))
              ;; Cleanup the tempdir.
              (and tempdir
                   (file-directory-p tempdir)
                   (delete-directory tempdir)))))))))))

;; Actually, `path-separator' is defined in src/emacs.c and overrided
;; in dos-w32.el.
(defvar-maybe path-separator ":"
  "The directory separator in search paths, as a string.")

;; `convert-standard-filename' is defined in lisp/files.el and overrided
;; in lisp/dos-fns.el and lisp/w32-fns.el for each environment.
(cond
 ;; must be load-time check to share .elc between different systems.
 ((fboundp 'convert-standard-filename))
 ((memq system-type '(windows-nt ms-dos))
  ;; should we do (require 'filename) at load-time ?
  ;; (require 'filename)
  ;; filename.el requires many modules, so we do not want to load it
  ;; at compile-time. Instead, suppress warnings by these autoloads.
  (eval-when-compile
    (autoload 'filename-maybe-truncate-by-size "filename")
    (autoload 'filename-special-filter "filename"))
  (defun convert-standard-filename (filename)
    "Convert a standard file's name to something suitable for the current OS.
This function's standard definition is trivial; it just returns the argument.
However, on some systems, the function is redefined
with a definition that really does change some file names.
Under `windows-nt' or `ms-dos', it refers `filename-replacement-alist' and
`filename-limit-length' for the basic filename and each parent directory name."
    (require 'filename)
    (let* ((names (split-string filename "/"))
	   (drive-name (car names))
	   (filter (function
		    (lambda (string)
		      (filename-maybe-truncate-by-size
		       (filename-special-filter string))))))
      (cond
       ((eq 1 (length names))
	(funcall filter drive-name))
       ((string-match "^[^/]:$" drive-name)
	(concat drive-name "/" (mapconcat filter (cdr names) "/")))
       (t
	(mapconcat filter names "/"))))))
 (t
  (defun convert-standard-filename (filename)
    "Convert a standard file's name to something suitable for the current OS.
This function's standard definition is trivial; it just returns the argument.
However, on some systems, the function is redefined
with a definition that really does change some file names.
Under `windows-nt' or `ms-dos', it refers `filename-replacement-alist' and
`filename-limit-length' for the basic filename and each parent directory name."
    filename)))

(static-cond
 ((fboundp 'insert-file-contents-literally))
 ((boundp 'file-name-handler-alist)
  ;; Use `defun-maybe' to update `load-history'.
  (defun-maybe insert-file-contents-literally (filename &optional visit
							beg end replace)
    "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
    (let (file-name-handler-alist)
      (insert-file-contents filename visit beg end replace))))
 (t
  (defalias 'insert-file-contents-literally 'insert-file-contents)))

(defun-maybe file-name-sans-extension (filename)
  "Return FILENAME sans final \"extension\".
The extension, in a file name, is the part that follows the last `.'."
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename)))
	  directory)
      (if (string-match "\\.[^.]*\\'" file)
	  (if (setq directory (file-name-directory filename))
	      (expand-file-name (substring file 0 (match-beginning 0))
				directory)
	    (substring file 0 (match-beginning 0)))
	filename))))


;;; @ Miscellanea.

;; Emacs 19.29 and later: (current-fill-column)
(defun-maybe current-fill-column ()
  "Return the fill-column to use for this line."
  fill-column)

;; Emacs 19.29 and later: (current-left-margin)
(defun-maybe current-left-margin ()
  "Return the left margin to use for this line."
  left-margin)


;;; @ XEmacs emulation.
;;;

(defun-maybe find-face (face-or-name)
  "Retrieve the face of the given name.
If FACE-OR-NAME is a face object, it is simply returned.
Otherwise, FACE-OR-NAME should be a symbol.  If there is no such face,
nil is returned.  Otherwise the associated face object is returned."
  (car (memq face-or-name (face-list))))

;; Emacs 21.1 defines this as an alias for `line-beginning-position'.
;; Therefore, optional 2nd arg BUFFER is not portable.
(defun-maybe point-at-bol (&optional n buffer)
  "Return the character position of the first character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
  (save-excursion
    (if buffer (set-buffer buffer))
    (forward-line (1- (or n 1)))
    (point)))

;; Emacs 21.1 defines this as an alias for `line-end-position'.
;; Therefore, optional 2nd arg BUFFER is not portable.
(defun-maybe point-at-eol (&optional n buffer)
  "Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
  (save-excursion
    (if buffer (set-buffer buffer))
    (end-of-line (or n 1))
    (point)))

(defsubst-maybe define-obsolete-function-alias (oldfun newfun)
  "Define OLDFUN as an obsolete alias for function NEWFUN.
This makes calling OLDFUN equivalent to calling NEWFUN and marks OLDFUN
as obsolete."
  (defalias oldfun newfun)
  (make-obsolete oldfun newfun))

;; XEmacs 21: (character-to-event CH &optional EVENT DEVICE)
(defun-maybe character-to-event (ch)
  "Convert keystroke CH into an event structure, replete with bucky bits.
Note that CH (the keystroke specifier) can be an integer, a character
or a symbol such as 'clear."
  ch)

;; XEmacs 21: (event-to-character EVENT
;;             &optional ALLOW-EXTRA-MODIFIERS ALLOW-META ALLOW-NON-ASCII)
(defun-maybe-cond event-to-character (event)
  "Return the character approximation to the given event object.
If the event isn't a keypress, this returns nil."
  ((and (fboundp 'read-event)
	(subrp (symbol-function 'read-event)))
   ;; Emacs 19 and later.
   (cond
    ((symbolp event)
     ;; mask is (BASE-TYPE MODIFIER-BITS) or nil.
     (let ((mask (get event 'event-symbol-element-mask)))
       (if mask
	   (let ((base (get (car mask) 'ascii-character)))
	     (if base
		 (logior base (car (cdr mask))))))))
    ((integerp event) event)))
  (t
   ;; v18. Is this correct?
   event))

;; v18: no event; (read-char)
;; Emacs 19, 20.1 and 20.2: (read-event)
;; Emacs 20.3: (read-event &optional PROMPT SUPPRESS-INPUT-METHOD)
;; Emacs 20.4: (read-event &optional PROMPT INHERIT-INPUT-METHOD)
;; XEmacs: (next-event &optional EVENT PROMPT),
;;         (next-command-event &optional EVENT PROMPT)
(defun-maybe-cond next-command-event (&optional event prompt)
  "Read an event object from the input stream.
If EVENT is non-nil, it should be an event object and will be filled
in and returned; otherwise a new event object will be created and
returned.
If PROMPT is non-nil, it should be a string and will be displayed in
the echo area while this function is waiting for an event."
  ((and (>= emacs-major-version 20)
	(>= emacs-minor-version 4))
   ;; Emacs 20.4 and later.
   (read-event prompt))			; should specify 2nd arg?
  ((and (= emacs-major-version 20)
	(= emacs-minor-version 3))
   ;; Emacs 20.3.
   (read-event prompt))			; should specify 2nd arg?
  ((and (fboundp 'read-event)
	(subrp (symbol-function 'read-event)))
   ;; Emacs 19, 20.1 and 20.2.
   (if prompt (message "%s" prompt))
   (read-event))
  (t
   (if prompt (message "%s" prompt))
   (read-char)))


;;; @ MULE 2 emulation.
;;;

(defun-maybe-cond cancel-undo-boundary ()
  "Cancel undo boundary."
  ((boundp 'buffer-undo-list)
   ;; for Emacs 19 and later.
   (if (and (consp buffer-undo-list)
	    (null (car buffer-undo-list)))
       (setq buffer-undo-list (cdr buffer-undo-list)))))


;;; @ End.
;;;

;;; poe.el ends here
