;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                        ;
;                        TypeRex OCaml Studio                            ;
;                                                                        ;
;                           Wojciech Meyer                               ;
;                                                                        ;
;  Copyright 2011-2012 INRIA Saclay - Ile-de-France / OCamlPro           ;
;  All rights reserved.  This file is distributed under the terms of     ;
;  the GNU Public License version 3.0.                                   ;
;                                                                        ;
;  TypeRex is distributed in the hope that it will be useful,            ;
;  but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;  GNU General Public License for more details.                          ;
;                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The simplest incremental compilation using flymake.
;; Uses ocamlbuild even when the project doesn't use it.
;; The root of the project is where the .typerex file is found.
;; This implementation handles OCaml style multiline diagnostics,
;; and doesn't need any external scripts.

;; Interesting package to install would be flymake-cursor which provides
;; a mode-line updates with the last diagnostic.

(defcustom ocp-flymake-available nil
  "Enable \"on-the-fly compilation\" button "
  :group 'typerex-misc)

(eval-when-compile (require 'flymake))

(when ocp-flymake-available

(require 'flymake)

  (progn
    ;; Add OCaml regex error message patterns
    (add-to-list 
     'flymake-err-line-patterns
     `(,(concat "File \"\\(.*\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)"
                "--?\\([0-9]+\\):\\(Error\\|Warning\\): \\(.*\\)$") 1 2 3 6))

    ;; Be sensitive on .ml files
    (add-to-list 
     'flymake-allowed-file-name-masks 
     '("\\.ml\\'" ocp-flymake-init))

    ;; Finally this will enable flymake support immediately after typerex is loaded
    ;; in particila set the local multiline flags, to allow multiline OCaml
    ;; diagnostics
    (add-hook
     'typerex-mode-hook
     '(lambda ()
        (set (make-local-variable 'multiline-flymake-mode) t))))

(defun ocp-flymake-init ()
  "Create syntax check ocamlbuild command line for TypeRex projects."
  (let* ((args nil)
         ;; first find the root of the project, this is where .typerex file
         (dir (locate-dominating-file buffer-file-name ".typerex"))
	 (buildfile-dir (flymake-init-find-buildfile-dir dir ".typerex"))
         ;; we need to provide a relative path for ocamlbuild
         (source-file-name (file-relative-name buffer-file-name dir))
         ;; we replace the source code name with the target for ocamlbuild
         (source (replace-regexp-in-string "\.ml$" ".cmo" source-file-name)))
    ;; finally build the command it consists of the list name of the command, 
    ;; list of flags, and the calculated working directory
    (list "ocamlbuild" (list "-use-ocamlfind" source) dir)))


;; Code below is taken from the Emacs wiki: http://www.emacswiki.org/emacs/FlymakeHaskell
;; OCaml compiler diagnostics are multiline and flymake that I checked (0.3) that comes
;; with Emacs assumes single line error/warnings messages
;; We use buffer local variable to enable that behavior.

(defvar multiline-flymake-mode
  "Decides whetever to join lines, during flymake parsing of the build command output."
  nil)

(defvar flymake-split-output-multiline nil)

;; This advice will set the special flags during split-output invocation
;; that deals with splitting multiline output
(defadvice flymake-split-output
  (around flymake-split-output-multiline activate protect)
  (if multiline-flymake-mode
      (let ((flymake-split-output-multiline t))
        ad-do-it)
    ad-do-it))
    
;; here we handle, what was set before, dynamic scoping is not nice, but that's
;; how Emacs works
(defadvice flymake-split-string
  (before flymake-split-string-multiline activate)
  (when flymake-split-output-multiline
    (ad-set-arg 1 "^\\s *$")))

)

(provide 'ocp-flymake)
