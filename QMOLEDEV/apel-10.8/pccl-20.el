;;; pccl-20.el --- Portable CCL utility for Emacs 20 and XEmacs-21-mule

;; Copyright (C) 1998 Free Software Foundation, Inc.
;; Copyright (C) 1998 Tanaka Akira

;; Author: Tanaka Akira  <akr@jaist.ac.jp>
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

;;; Code:

(eval-when-compile (require 'ccl))
(require 'broken)

(broken-facility ccl-accept-symbol-as-program
  "Emacs does not accept symbol as CCL program."
  (progn
    (define-ccl-program test-ccl-identity
      '(1 ((read r0) (loop (write-read-repeat r0)))))
    (condition-case nil
        (progn
          (funcall
	   (if (fboundp 'ccl-vector-execute-on-string)
	       'ccl-vector-execute-on-string
	     'ccl-execute-on-string)
           'test-ccl-identity
           (make-vector 9 nil)
           "")
          t)
      (error nil)))
  t)

(eval-and-compile

  (static-if (featurep 'xemacs)
      (defadvice make-coding-system (before ccl-compat (name type &rest ad-subr-args) activate)
	(when (and (integerp type)
		   (eq type 4)
		   (characterp (ad-get-arg 2))
		   (stringp (ad-get-arg 3))
		   (consp (ad-get-arg 4))
		   (symbolp (car (ad-get-arg 4)))
		   (symbolp (cdr (ad-get-arg 4))))
	  (setq type 'ccl)
	  (setq ad-subr-args
		(list
		 (ad-get-arg 3)
		 (append
		  (list
		   'mnemonic (char-to-string (ad-get-arg 2))
		   'decode (symbol-value (car (ad-get-arg 4)))
		   'encode (symbol-value (cdr (ad-get-arg 4))))
		  (ad-get-arg 5)))))))

  (if (featurep 'xemacs)
      (defun make-ccl-coding-system (name mnemonic docstring decoder encoder)
	"\
Define a new CODING-SYSTEM by CCL programs DECODER and ENCODER.

CODING-SYSTEM, DECODER and ENCODER must be symbol."
	(make-coding-system
	 name 'ccl docstring
	 (list 'mnemonic (char-to-string mnemonic)
	       'decode (symbol-value decoder)
	       'encode (symbol-value encoder))))
    (defun make-ccl-coding-system
      (coding-system mnemonic docstring decoder encoder)
      "\
Define a new CODING-SYSTEM by CCL programs DECODER and ENCODER.

CODING-SYSTEM, DECODER and ENCODER must be symbol."
      (when-broken ccl-accept-symbol-as-program
	(setq decoder (symbol-value decoder))
	(setq encoder (symbol-value encoder)))
      (make-coding-system coding-system 4 mnemonic docstring
			  (cons decoder encoder)))
    )

  (when-broken ccl-accept-symbol-as-program

    (when (subrp (symbol-function 'ccl-execute))
      (fset 'ccl-vector-program-execute
	    (symbol-function 'ccl-execute))
      (defun ccl-execute (ccl-prog reg)
	"\
Execute CCL-PROG with registers initialized by REGISTERS.
If CCL-PROG is symbol, it is dereferenced."
	(ccl-vector-program-execute
	 (if (symbolp ccl-prog) (symbol-value ccl-prog) ccl-prog)
	 reg)))

    (when (subrp (symbol-function 'ccl-execute-on-string))
      (fset 'ccl-vector-program-execute-on-string
	    (symbol-function 'ccl-execute-on-string))
      (defun ccl-execute-on-string (ccl-prog status string &optional contin)
	"\
Execute CCL-PROG with initial STATUS on STRING.
If CCL-PROG is symbol, it is dereferenced."
	(ccl-vector-program-execute-on-string
	 (if (symbolp ccl-prog) (symbol-value ccl-prog) ccl-prog)
	 status string contin)))
    )
  )

(eval-when-compile
  (define-ccl-program test-ccl-eof-block
    '(1
      ((read r0)
       (write r0)
       (read r0))
      (write "[EOF]")))

  (make-ccl-coding-system
   'test-ccl-eof-block-cs ?T "CCL_EOF_BLOCK tester"
   'test-ccl-eof-block 'test-ccl-eof-block)
  )

(broken-facility ccl-execute-eof-block-on-encoding-null
  "Emacs forgets executing CCL_EOF_BLOCK with encoding on empty input. (Fixed on Emacs 20.4)"
  (equal (encode-coding-string "" 'test-ccl-eof-block-cs) "[EOF]"))

(broken-facility ccl-execute-eof-block-on-encoding-some
  "Emacs forgets executing CCL_EOF_BLOCK with encoding on non-empty input. (Fixed on Emacs 20.3)"
  (equal (encode-coding-string "a" 'test-ccl-eof-block-cs) "a[EOF]"))

(broken-facility ccl-execute-eof-block-on-decoding-null
  "Emacs forgets executing CCL_EOF_BLOCK with decoding on empty input. (Fixed on Emacs 20.4)"
  (equal (decode-coding-string "" 'test-ccl-eof-block-cs) "[EOF]"))

(broken-facility ccl-execute-eof-block-on-decoding-some
  "Emacs forgets executing CCL_EOF_BLOCK with decoding on non-empty input. (Fixed on Emacs 20.4)"
  (equal (decode-coding-string "a" 'test-ccl-eof-block-cs) "a[EOF]"))

(broken-facility ccl-execute-eof-block-on-encoding
  "Emacs may forget executing CCL_EOF_BLOCK with encoding."
  (not (or (broken-p 'ccl-execute-eof-block-on-encoding-null)
	   (broken-p 'ccl-execute-eof-block-on-encoding-some)))
  t)

(broken-facility ccl-execute-eof-block-on-decoding
  "Emacs may forget executing CCL_EOF_BLOCK with decoding."
  (not (or (broken-p 'ccl-execute-eof-block-on-decoding-null)
	   (broken-p 'ccl-execute-eof-block-on-decoding-some)))
  t)

(broken-facility ccl-execute-eof-block
  "Emacs may forget executing CCL_EOF_BLOCK."
  (not (or (broken-p 'ccl-execute-eof-block-on-encoding)
	   (broken-p 'ccl-execute-eof-block-on-decoding)))
  t)


;;; @ end
;;;

(require 'product)
(product-provide (provide 'pccl-20) (require 'apel-ver))

;;; pccl-20.el ends here
