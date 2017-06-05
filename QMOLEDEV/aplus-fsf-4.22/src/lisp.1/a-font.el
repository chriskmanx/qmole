;;; a-font.el --- Font lock for APlus.

;; Copyright (C) 1998,1999,2001 Colin Rafferty

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

(require 'font-lock)

(provide 'a-font)

(defconst a-font-lock-keywords-1 nil
  "Helper for a-font-lock-keywords")

(let ((ctoken "\\(\\sw\\|\\s_\\|[:~*&]\\)+")
      (ows "[ \t]*"))
  (setq
   a-font-lock-keywords-1
   (list
    ;; Should have function names highlighted (font-lock-function-name-face)
    (list (concat
	   "^\\("
	   "\\("
	   "\\("
	   "\\(" ctoken ows "\\)?"	; context
	   "\\." ows
	   "\\)?"
	   "\\(" ctoken "\\)"		; name
	   "\\)"
	   ows "{" ows
	   "\\("
	   "\\(" ctoken "\\)" ows	; first argument
	   "\\)?"
	   "\\)")
	  ;; context and name together are the function name
	  '(2 font-lock-function-name-face)
	  ;; first argument is a variable name.
	  '(9 font-lock-variable-name-face nil t)
	  ;; any remaining arguments are also variable names.
	  (list (concat "\\(;" ows "\\(" ctoken "\\)" ows "\\)")
		nil nil '(2 font-lock-variable-name-face nil t)))
    ;; All keywords:
    ;; (regexp-opt '("case" "do" "else" "if" "while") nil t)
    (cons "\\<\\(case\\|do\\|else\\|if\\|while\\)\\>" 'font-lock-keyword-face)
    )))

(defvar a-font-lock-keywords a-font-lock-keywords-1
  "Default expressions to highlight in A+ modes.")

(provide 'a-font)

;;; a-font.el ends here
