;;; pccl-om.el --- Portable CCL utility for Mule 2.*

;; Copyright (C) 1998 Free Software Foundation, Inc.
;; Copyright (C) 1998 Tanaka Akira

;; Author: Tanaka Akira <akr@jaist.ac.jp>
;;	Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
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
  "Emacs does not accept symbol as CCL program.")

(eval-and-compile
  (defun make-ccl-coding-system
    (coding-system mnemonic doc-string decoder encoder)
    "\
Define a new CODING-SYSTEM by CCL programs DECODER and ENCODER.

CODING-SYSTEM, DECODER and ENCODER must be symbol."
    (setq decoder (symbol-value decoder)
	  encoder (symbol-value encoder))
    (make-coding-system coding-system 4 mnemonic doc-string
			nil		; Mule takes one more optional argument: EOL-TYPE.
			(cons decoder encoder)))
  )

(defun ccl-execute (ccl-prog reg)
  "Execute CCL-PROG with registers initialized by REGISTERS.
If CCL-PROG is symbol, it is dereferenced."
  (exec-ccl
   (if (symbolp ccl-prog) (symbol-value ccl-prog) ccl-prog)
   reg))

(defun ccl-execute-on-string (ccl-prog status string &optional contin)
  "Execute CCL-PROG with initial STATUS on STRING.
If CCL-PROG is symbol, it is dereferenced."
  (exec-ccl-string
   (if (symbolp ccl-prog) (symbol-value ccl-prog) ccl-prog)
   status string))

(broken-facility ccl-execute-on-string-ignore-contin
  "CONTIN argument for ccl-execute-on-string is ignored.")

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
  "Emacs forgets executing CCL_EOF_BLOCK with encoding on empty input."
  (equal (code-convert-string "" *internal* 'test-ccl-eof-block-cs) "[EOF]"))

(broken-facility ccl-execute-eof-block-on-encoding-some
  "Emacs forgets executing CCL_EOF_BLOCK with encoding on non-empty input."
  (equal (code-convert-string "a" *internal* 'test-ccl-eof-block-cs) "a[EOF]"))

(broken-facility ccl-execute-eof-block-on-decoding-null
  "Emacs forgets executing CCL_EOF_BLOCK with decoding on empty input."
  (equal (code-convert-string "" 'test-ccl-eof-block-cs *internal*) "[EOF]"))

(broken-facility ccl-execute-eof-block-on-decoding-some
  "Emacs forgets executing CCL_EOF_BLOCK with decoding on non-empty input."
  (equal (code-convert-string "a" 'test-ccl-eof-block-cs *internal*) "a[EOF]"))

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

(broken-facility ccl-cascading-read
  "Emacs CCL read command does not accept more than 2 arguments."
  (condition-case nil
      (progn
        (define-ccl-program cascading-read-test
          '(1
            (read r0 r1 r2)))
        t)
    (error nil)))

;;; @ end
;;;

(require 'product)
(product-provide (provide 'pccl-om) (require 'apel-ver))

;;; pccl-om.el ends here
