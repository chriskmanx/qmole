;;; aplus.el -- Loads the Aplus specific settings for xemacs 

;; Copyright (C) 1998,2001 Colin Rafferty

;; Author: Colin Rafferty <colin@xemacs.org>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to colin@xemacs.org) or from
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Send bug reports to colin@xemacs.org

;;; Code:

;; 
;;
(defvar aplus-set-load-path t
  "*Non-nil means set the load-path for the user.")

(if aplus-set-load-path
    (setq load-path 
	  (append 
	   '("/usr/local/aplus-fsf-4.20/lisp.1") load-path 
	   )	
	  ))
;;
;; Load Aplus and set defaults
;;
(defvar aplus-setup-global-bindings t
  "*Non-nil means to set up the global keybindings for the user.")

(if aplus-setup-global-bindings
    (load "xa" nil t))

(require 'a-font)
(require 'keyb)
(require 'a)
(setq a-prog "/usr/local/bin/a+")
(setq a-plus t)

(cond ((file-readable-p "~/.custom/a-options.el")
       (load-file "~/.custom/a-options.el")
       )
      )

(provide 'aplus)

;;; aplus.el ends here
