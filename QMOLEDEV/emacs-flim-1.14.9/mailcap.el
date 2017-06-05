;;; mailcap.el --- mailcap parser

;; Copyright (C) 1997,1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Created: 1997-06-27
;;	2000-11-24	Rewrote to use mime-conf.el.
;; Keywords: mailcap, setting, configuration, MIME, multimedia
;; Status: obsolete

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

(require 'mime-conf)
(require 'poe) ; define-obsolete-function-alias

(define-obsolete-function-alias
  'mailcap-parse-buffer 'mime-parse-mailcap-buffer)

(define-obsolete-function-alias
  'mailcap-format-command 'mime-format-mailcap-command)
  
(cond
 ((featurep 'xemacs)
  (define-obsolete-variable-alias
    'mailcap-file 'mime-mailcap-file)
  (define-obsolete-function-alias
    'mailcap-parse-file 'mime-parse-mailcap-file)
  )
 (t
  (defvar mailcap-file mime-mailcap-file)
  (defun mailcap-parse-file (&optional filename order)
    "Parse FILENAME as a mailcap, and return the result.
If optional argument ORDER is a function, result is sorted by it.
If optional argument ORDER is not specified, result is sorted original
order.  Otherwise result is not sorted.
This function is obsolete.  Please use mime-parse-mailcap-file instead."
    (if filename
	(mime-parse-mailcap-file filename order)
      (let ((mime-mailcap-file mailcap-file))
	(mime-parse-mailcap-file nil order))))
  (make-obsolete 'mailcap-parse-file 'mime-parse-mailcap-file)
  ))


;;; @ end
;;;

(provide 'mailcap)

;;; mailcap.el ends here
