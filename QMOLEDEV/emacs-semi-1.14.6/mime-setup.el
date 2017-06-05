;;; mime-setup.el --- setup file for MIME viewer and composer.

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: MIME, multimedia, multilingual, mail, news

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

(load "mail-mime-setup")

(condition-case nil
    (load "gnus-mime-setup")
  (error (message "gnus-mime-setup is not found."))
  )

(condition-case nil
    (load "emh-setup")
  (error (message "emh-setup is not found."))
  )


;;; @ end
;;;

(provide 'mime-setup)

(run-hooks 'mime-setup-load-hook)

;;; mime-setup.el ends here
