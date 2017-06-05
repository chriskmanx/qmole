;;; mime-def.el --- definition module about MIME -*- coding: iso-8859-4; -*-

;; Copyright (C) 1995,96,97,98,99,2000,2001,2002,2003,2004,2005,2006
;;   Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;	Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: definition, MIME, multimedia, mail, news

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

(require 'custom)
(require 'mcharset)
(require 'alist)

(eval-when-compile (require 'luna))	; luna-arglist-to-arguments

(eval-and-compile
  (defconst mime-library-product ["FLIM" (1 14 9) "Gojò"]
    "Product name, version number and code name of MIME-library package."))

(defmacro mime-product-name (product)
  `(aref ,product 0))

(defmacro mime-product-version (product)
  `(aref ,product 1))

(defmacro mime-product-code-name (product)
  `(aref ,product 2))

(defconst mime-library-version
  (eval-when-compile
    (concat (mime-product-name mime-library-product) " "
	    (mapconcat #'number-to-string
		       (mime-product-version mime-library-product) ".")
	    " - \"" (mime-product-code-name mime-library-product) "\"")))


;;; @ variables
;;;

(defgroup mime '((default-mime-charset custom-variable))
  "Emacs MIME Interfaces"
  :group 'news
  :group 'mail)

(defcustom mime-uuencode-encoding-name-list '("x-uue" "x-uuencode")
  "*List of encoding names for uuencode format."
  :group 'mime
  :type '(repeat string))


;;; @@ for encoded-word
;;;

(defgroup mime-header nil
  "Header representation, specially encoded-word"
  :group 'mime)

;;; @@@ decoding
;;;

(defcustom mime-field-decoding-max-size 1000
  "*Max size to decode header field."
  :group 'mime-header
  :type '(choice (integer :tag "Limit (bytes)")
		 (const :tag "Don't limit" nil)))

(defcustom mime-header-accept-quoted-encoded-words nil
  "*Accept encoded-words in quoted-strings."
  :group 'mime-header
  :type 'boolean)


;;; @@@ encoding
;;;

(defcustom mime-field-encoding-method-alist
  '(("X-Nsubject" . iso-2022-jp-2)
    ("Newsgroups" . nil)
    ("Message-ID" . nil)
    (t            . mime)
    )
  "*Alist to specify field encoding method.
Its key is field-name, value is encoding method.

If method is `mime', this field will be encoded into MIME format.

If method is a MIME-charset, this field will be encoded as the charset
when it must be convert into network-code.

If method is `default-mime-charset', this field will be encoded as
variable `default-mime-charset' when it must be convert into
network-code.

If method is nil, this field will not be encoded."
  :group 'mime-header
  :type '(repeat (cons (choice :tag "Field"
			       (string :tag "Name")
			       (const :tag "Default" t))
		       (choice :tag "Method"
			       (const :tag "MIME conversion" mime)
			       (symbol :tag "non-MIME conversion")
			       (const :tag "no-conversion" nil)))))


;;; @ required functions
;;;

(defsubst regexp-* (regexp)
  (concat regexp "*"))

(defsubst regexp-or (&rest args)
  (concat "\\(" (mapconcat (function identity) args "\\|") "\\)"))

(or (fboundp 'char-int)
    (defalias 'char-int 'identity))


;;; @ MIME constants
;;;

(defconst mime-tspecial-char-list
  '(?\] ?\[ ?\( ?\) ?< ?> ?@ ?, ?\; ?: ?\\ ?\" ?/ ?? ?=))
(defconst mime-token-regexp
  (concat "[^" mime-tspecial-char-list "\000-\040]+"))
(defconst mime-attribute-char-regexp
  (concat "[^" mime-tspecial-char-list "\000-\040"
	  "*'%"				; introduced in RFC 2231.
	  "]"))

(defconst mime-charset-regexp
  (concat "[^" mime-tspecial-char-list "\000-\040"
	  "*'%"				; should not include "%"?
	  "]+"))

;; More precisely, length of "[A-Za-z]+" is limited to at most 8.
;; (defconst mime-language-regexp "[A-Za-z]+\\(-[A-Za-z]+\\)*")
(defconst mime-language-regexp "[-A-Za-z]+")

(defconst mime-encoding-regexp mime-token-regexp)


;;; @@ base64 / B
;;;

(defconst base64-token-regexp "[A-Za-z0-9+/]")
(defconst base64-token-padding-regexp "[A-Za-z0-9+/=]")

(defconst B-encoded-text-regexp
  (concat "\\(\\("
	  base64-token-regexp
	  base64-token-regexp
	  base64-token-regexp
	  base64-token-regexp
	  "\\)*"
	  base64-token-regexp
	  base64-token-regexp
	  base64-token-padding-regexp
	  base64-token-padding-regexp
          "\\)"))

;; (defconst eword-B-encoding-and-encoded-text-regexp
;;   (concat "\\(B\\)\\?" eword-B-encoded-text-regexp))


;;; @@ Quoted-Printable / Q
;;;

(defconst quoted-printable-hex-chars "0123456789ABCDEF")

(defconst quoted-printable-octet-regexp
  (concat "=[" quoted-printable-hex-chars
	  "][" quoted-printable-hex-chars "]"))

(defconst Q-encoded-text-regexp
  (concat "\\([^=?]\\|" quoted-printable-octet-regexp "\\)+"))

;; (defconst eword-Q-encoding-and-encoded-text-regexp
;;   (concat "\\(Q\\)\\?" eword-Q-encoded-text-regexp))


;;; @ Content-Type
;;;

(defsubst make-mime-content-type (type subtype &optional parameters)
  (cons (cons 'type type)
	(cons (cons 'subtype subtype)
	      parameters)))

(defsubst mime-content-type-primary-type (content-type)
  "Return primary-type of CONTENT-TYPE."
  (cdr (car content-type)))

(defsubst mime-content-type-subtype (content-type)
  "Return subtype of CONTENT-TYPE."
  (cdr (car (cdr content-type))))

(defsubst mime-content-type-parameters (content-type)
  "Return parameters of CONTENT-TYPE."
  (cdr (cdr content-type)))

(defsubst mime-content-type-parameter (content-type parameter)
  "Return PARAMETER value of CONTENT-TYPE."
  (cdr (assoc parameter (cdr (cdr content-type)))))


(defsubst mime-type/subtype-string (type &optional subtype)
  "Return type/subtype string from TYPE and SUBTYPE."
  (if type
      (if subtype
	  (format "%s/%s" type subtype)
	(format "%s" type))))


;;; @ Content-Disposition
;;;

(defsubst make-mime-content-disposition (type &optional parameters)
  (cons (cons 'type type)
	parameters))

(defsubst mime-content-disposition-type (content-disposition)
  "Return disposition-type of CONTENT-DISPOSITION."
  (cdr (car content-disposition)))

(defsubst mime-content-disposition-parameters (content-disposition)
  "Return disposition-parameters of CONTENT-DISPOSITION."
  (cdr content-disposition))

(defsubst mime-content-disposition-parameter (content-disposition parameter)
  "Return PARAMETER value of CONTENT-DISPOSITION."
  (cdr (assoc parameter (cdr content-disposition))))

(defsubst mime-content-disposition-filename (content-disposition)
  "Return filename of CONTENT-DISPOSITION."
  (mime-content-disposition-parameter content-disposition "filename"))


;;; @ message structure
;;;

(defvar mime-message-structure nil
  "Information about structure of message.
Please use reference function `mime-entity-SLOT' to get value of SLOT.

Following is a list of slots of the structure:

node-id			node-id (list of integers)
content-type		content-type (content-type)
content-disposition	content-disposition (content-disposition)
encoding		Content-Transfer-Encoding (string or nil)
children		entities included in this entity (list of entity)

If an entity includes other entities in its body, such as multipart or
message/rfc822, `mime-entity' structures of them are included in
`children', so the `mime-entity' structure become a tree.")

(make-variable-buffer-local 'mime-message-structure)

(make-obsolete-variable 'mime-message-structure "should not use it.")


;;; @ for mel-backend
;;;

(defvar mel-service-list nil)

(defmacro mel-define-service (name &optional args &rest rest)
  "Define NAME as a service for Content-Transfer-Encodings.
If ARGS is specified, NAME is defined as a generic function for the
service."
  `(progn
     (add-to-list 'mel-service-list ',name)
     (defvar ,(intern (format "%s-obarray" name)) (make-vector 7 0))
     ,@(if args
	   `((defun ,name ,args
	       ,@rest
	       (funcall (mel-find-function ',name ,(car (last args)))
			,@(luna-arglist-to-arguments (butlast args)))
	       )))
     ))

(put 'mel-define-service 'lisp-indent-function 'defun)


(defvar mel-encoding-module-alist nil)

(defsubst mel-find-function-from-obarray (ob-array encoding)
  (let* ((f (intern-soft encoding ob-array)))
    (or f
	(let ((rest (cdr (assoc encoding mel-encoding-module-alist))))
	  (while (and rest
		      (progn
			(require (car rest))
			(null (setq f (intern-soft encoding ob-array)))
			))
	    (setq rest (cdr rest))
	    )
	  f))))

(defsubst mel-copy-method (service src-backend dst-backend)
  (let* ((oa (symbol-value (intern (format "%s-obarray" service))))
	 (f (mel-find-function-from-obarray oa src-backend))
	 sym)
    (when f
      (setq sym (intern dst-backend oa))
      (or (fboundp sym)
	  (fset sym (symbol-function f))
	  ))))
       
(defsubst mel-copy-backend (src-backend dst-backend)
  (let ((services mel-service-list))
    (while services
      (mel-copy-method (car services) src-backend dst-backend)
      (setq services (cdr services)))))

(defmacro mel-define-backend (type &optional parents)
  "Define TYPE as a mel-backend.
If PARENTS is specified, TYPE inherits PARENTS.
Each parent must be backend name (string)."
  (cons 'progn
	(mapcar (lambda (parent)
		  `(mel-copy-backend ,parent ,type)
		  )
		parents)))

(defmacro mel-define-method (name args &rest body)
  "Define NAME as a method function of (nth 1 (car (last ARGS))) backend.
ARGS is like an argument list of lambda, but (car (last ARGS)) must be
specialized parameter.  (car (car (last ARGS))) is name of variable
and (nth 1 (car (last ARGS))) is name of backend (encoding)."
  (let* ((specializer (car (last args)))
	 (class (nth 1 specializer)))
    `(progn
       (mel-define-service ,name)
       (fset (intern ,class ,(intern (format "%s-obarray" name)))
	     (lambda ,(butlast args)
	       ,@body)))))

(put 'mel-define-method 'lisp-indent-function 'defun)

(defmacro mel-define-method-function (spec function)
  "Set SPEC's function definition to FUNCTION.
First element of SPEC is service.
Rest of ARGS is like an argument list of lambda, but (car (last ARGS))
must be specialized parameter.  (car (car (last ARGS))) is name of
variable and (nth 1 (car (last ARGS))) is name of backend (encoding)."
  (let* ((name (car spec))
	 (args (cdr spec))
	 (specializer (car (last args)))
	 (class (nth 1 specializer)))
    `(let (sym)
       (mel-define-service ,name)
       (setq sym (intern ,class ,(intern (format "%s-obarray" name))))
       (or (fboundp sym)
	   (fset sym (symbol-function ,function))))))

(defmacro mel-define-function (function spec)
  (let* ((name (car spec))
	 (args (cdr spec))
	 (specializer (car (last args)))
	 (class (nth 1 specializer)))
    `(progn
       (define-function ,function
	 (intern ,class ,(intern (format "%s-obarray" name))))
       )))

(defvar base64-dl-module
  (if (and (fboundp 'base64-encode-string)
	   (subrp (symbol-function 'base64-encode-string)))
      nil
    (if (fboundp 'dynamic-link)
	(let ((path (expand-file-name "base64.so" exec-directory)))
	  (and (file-exists-p path)
	       path)
	  ))))


;;; @ end
;;;

(provide 'mime-def)

;;; mime-def.el ends here
