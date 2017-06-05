;;; mail-mime-setup.el --- setup file for mail-mode.

;; Copyright (C) 1994,1995,1996,1997,1998,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: mail-mode, MIME, multimedia, multilingual, encoded-word

;; This file is part of SEMI (Setting for Emacs MIME Interfaces).

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

(require 'semi-setup)
(require 'alist)


(autoload 'turn-on-mime-edit "mime-edit"
  "Unconditionally turn on MIME-Edit minor mode." t)

;; (autoload 'eword-decode-header "eword-decode"
;;   "Decode MIME encoded-words in header fields." t)


;;; @ for mail-mode, RMAIL and VM
;;;

;; (add-hook 'mail-setup-hook 'eword-decode-header)
(add-hook 'mail-setup-hook 'turn-on-mime-edit 'append)
(add-hook 'mail-send-hook  'mime-edit-maybe-translate)
(set-alist 'mime-edit-split-message-sender-alist
           'mail-mode (function
                       (lambda ()
                         (interactive)
                         (funcall send-mail-function)
                         )))


;;; @ for signature
;;;

(if mime-setup-use-signature
    (setq mail-signature nil)
  )


;;; @ end
;;;

(provide 'mail-mime-setup)

;;; mail-mime-setup.el ends here
