;;; postpet.el --- Postpet support for GNU Emacs

;; Copyright (C) 1999,2000 Free Software Foundation, Inc.

;; Author: Tanaka Akira  <akr@jaist.ac.jp>
;; Keywords: Postpet, MIME, multimedia, mail, news

;; This file is part of SEMI (Sample of Elastic MIME Interfaces).

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'mime)
(require 'alist)

(put 'unpack 'lisp-indent-function 1)
(defmacro unpack (string &rest body)
  `(let* ((*unpack*string* (string-as-unibyte ,string))
	  (*unpack*index* 0))
     ,@body))

(defun unpack-skip (len)
  (setq *unpack*index* (+ len *unpack*index*)))

(defun unpack-fixed (len)
  (prog1
      (substring *unpack*string* *unpack*index* (+ *unpack*index* len))
    (unpack-skip len)))

(defun unpack-byte ()
  (char-int (aref (unpack-fixed 1) 0)))

(defun unpack-short ()
  (let* ((b0 (unpack-byte))
	 (b1 (unpack-byte)))
    (+ (* 256 b0) b1)))

(defun unpack-long ()
  (let* ((s0 (unpack-short))
	 (s1 (unpack-short)))
    (+ (* 65536 s0) s1)))

(defun unpack-string ()
  (let ((len (unpack-byte)))
    (unpack-fixed len)))

(defun unpack-string-sjis ()
  (decode-mime-charset-string (unpack-string) 'shift_jis))

;;;###autoload
(defun postpet-decode (string)
  (condition-case nil
      (unpack string
	(let (res)
	  (unpack-skip 4)
	  (set-alist 'res 'carryingcount (unpack-long))
	  (unpack-skip 8)
	  (set-alist 'res 'sentyear (unpack-short))
	  (set-alist 'res 'sentmonth (unpack-short))
	  (set-alist 'res 'sentday (unpack-short))
	  (unpack-skip 8)
	  (set-alist 'res 'petname (unpack-string-sjis))
	  (set-alist 'res 'owner (unpack-string-sjis))
	  (set-alist 'res 'pettype (unpack-fixed 4))
	  (set-alist 'res 'health (unpack-short))
	  (unpack-skip 2)
	  (set-alist 'res 'sex (unpack-long))
	  (unpack-skip 1)
	  (set-alist 'res 'brain (unpack-byte))
	  (unpack-skip 39)
	  (set-alist 'res 'happiness (unpack-byte))
	  (unpack-skip 14)
	  (set-alist 'res 'petbirthyear (unpack-short))
	  (set-alist 'res 'petbirthmonth (unpack-short))
	  (set-alist 'res 'petbirthday (unpack-short))
	  (unpack-skip 8)
	  (set-alist 'res 'from (unpack-string))
	  (unpack-skip 5)
	  (unpack-skip 160)
	  (unpack-skip 4)
	  (unpack-skip 8)
	  (unpack-skip 8)
	  (unpack-skip 26)
	  (set-alist 'res 'treasure (unpack-short))
	  (set-alist 'res 'money (unpack-long))
	  res))
    (error nil)))

;;;###autoload
(defun mime-display-application/x-postpet (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (let ((pet (postpet-decode (mime-entity-content entity))))
      (if pet
	  (insert
	   "Petname: " (cdr (assq 'petname pet))
	   "\n"
	   "Owner: " (cdr (assq 'owner pet))
	   "\n"
	   "Pettype: " (cdr (assq 'pettype pet))
	   "\n"
	   "From: " (cdr (assq 'from pet))
	   "\n"
	   "CarryingCount: " (int-to-string (cdr (assq 'carryingcount pet)))
	   "\n"
	   "SentYear: " (int-to-string (cdr (assq 'sentyear pet)))
	   "\n"
	   "SentMonth: " (int-to-string (cdr (assq 'sentmonth pet)))
	   "\n"
	   "SentDay: " (int-to-string (cdr (assq 'sentday pet)))
	   "\n"
	   "PetbirthYear: " (int-to-string (cdr (assq 'petbirthyear pet)))
	   "\n"
	   "PetbirthMonth: " (int-to-string (cdr (assq 'petbirthmonth pet)))
	   "\n"
	   "PetbirthDay: " (int-to-string (cdr (assq 'petbirthday pet)))
	   "\n"
	   "Health: " (int-to-string (cdr (assq 'health pet)))
	   "\n"
	   "Sex: " (int-to-string (cdr (assq 'sex pet)))
	   "\n"
	   "Brain: " (int-to-string (cdr (assq 'brain pet)))
	   "\n"
	   "Happiness: " (int-to-string (cdr (assq 'happiness pet)))
	   "\n"
	   "Treasure: " (int-to-string (cdr (assq 'treasure pet)))
	   "\n"
	   "Money: " (int-to-string (cdr (assq 'money pet)))
	   "\n")
	(insert "Invalid format\n"))
      (run-hooks 'mime-display-application/x-postpet-hook))))


;;; @ end
;;;

(provide 'postpet)

;;; postpet.el ends here
