;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                        ;
;                        TypeRex OCaml Studio                            ;
;                                                                        ;
;                           Tiphaine Turpin                              ;
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


; If we keep tuareg separately, we could use 'typerex-load-hook
;(add-hook 'typerex-mode-hook 'start-ocp-wizard-server)

(ocp-restart-server)

;; Plugin with menus
;;;;;;;;;;;;;;;;;;;;

(defcustom ocp-menu-trigger nil
  "mouse event to trigger the contextual menu (default nil)"
  :group 'typerex-misc)

(defcustom ocp-prefix-key [(control o)]
  "key combination to trigger the command menu (default nil)"
  :group 'typerex-misc)

(defun ocp-wrap-grep (grep)
  "Show all definitions and references for the current ident"
  (interactive)
  (let ((res (checked-string-command grep)))
    (when res
      (let* ((res (read res))
             (root (car res))
             (contents (cadr res))
             (overlays (eval (cadr (cdr res))))
             (local-overlays (eval (cadr (cdr (cdr res)))))
             (buffer (buffer-name))
             (grep-buffer "*ocp-wizard-grep*"))
        (set-cleared-buffer grep-buffer)
        (compilation-minor-mode 1)
        (cd root)
        (insert contents)
        (display-buffer grep-buffer)
        (highlight-regions t overlays)
        (set-buffer buffer)
        (highlight-regions nil local-overlays)
        ))))

(defun ocp-grep ()
  "Show all definitions and references for the current ident"
  (interactive)
  (ocp-wrap-grep "grep"))

(defun ocp-grep-toplevel ()
  "Grep the toplevel module defined by the current source file"
  (interactive)
  (ocp-wrap-grep "grep-toplevel"))

(defun ocp-wizard-menu-plugin ()
  "Register the commands as a keyboard menu"

  (defvar ocp-prefix (make-sparse-keymap "OCP"))
  (defvar ocp-prefix-mouse (make-sparse-keymap "OCP"))
  (define-key typerex-mode-map ocp-prefix-key ocp-prefix)
  (when ocp-menu-trigger
    (define-key typerex-mode-map ocp-menu-trigger ocp-prefix-mouse))

  (defvar ocp-prefix-top (make-sparse-keymap "Toplevel"))
  (define-key ocp-prefix-top [(r)] '("Rename" . ocp-rename-toplevel))
  (define-key ocp-prefix-top [(g)] '("Grep" . ocp-grep-toplevel))

  (define-key ocp-prefix [(u)] `("Undo (global)" . ocp-undo))
  (define-key ocp-prefix [(q)] '("Qualify" . ocp-eliminate-open))
  (define-key ocp-prefix [(p)] '("Prune" . ocp-prune-lids))
  (define-key ocp-prefix [(t)] `("Toplevel-(Rename/Grep)" . ,ocp-prefix-top))
  (define-key ocp-prefix [(r)] '("Rename" . ocp-rename))
  (define-key ocp-prefix [(g)] '("Grep" . ocp-grep))
  (define-key ocp-prefix [(a)] '("Alternate definitions" . ocp-cycle-definitions))
  (define-key ocp-prefix [(d)] '("Definition" . ocp-goto-definition))
  (define-key ocp-prefix [(c)] '("Comment" . ocp-comment-definition))

  (define-key ocp-prefix-mouse [(u)] `("Undo (global)" . ocp-undo))
  (define-key ocp-prefix-mouse [(q)] '("Qualify" . ocp-eliminate-open))
  (define-key ocp-prefix-mouse [(p)] '("Prune" . ocp-prune-lids))
  (define-key ocp-prefix-mouse [(x)] '("Toplevel Rename" . ocp-rename-toplevel))
  (define-key ocp-prefix-mouse [(y)] '("Toplevel Grep" . ocp-grep-toplevel))
  (define-key ocp-prefix-mouse [(r)] '("Rename" . ocp-rename))
  (define-key ocp-prefix-mouse [(g)] '("Grep" . ocp-grep))
  (define-key ocp-prefix-mouse [(a)] '("Alternate definitions" . ocp-cycle-definitions))
  (define-key ocp-prefix-mouse [(d)] '("Definition" . ocp-goto-definition))
  (define-key ocp-prefix-mouse [(c)] '("Comment" . ocp-comment-definition))
  )

(defun ocp-action-item (c)
  `[,(cadr c) ,(car c) ,:help ,(documentation (car c))])

(defun typerex-build-menu ()
  (easy-menu-define
   typerex-mode-menu (list typerex-mode-map)
   "TypeRex Mode Menu."
   (append
   '("TypeRex")
   (mapcar
    'ocp-action-item
    '(
      (ocp-grep-toplevel "Toplevel Grep")
      (ocp-rename-toplevel "Toplevel Rename")
      (ocp-comment-definition "Comment")
      (ocp-goto-definition "Definition")
      (ocp-cycle-definitions "Alternate definitions")
      (ocp-grep "Grep")
      (ocp-rename "Rename")
      (ocp-prune-lids "Prune")
      (ocp-eliminate-open "Qualify")
      (ocp-undo "Undo (global)")
    ))
   `(
   "---"
     [ "Show type at point" caml-types-show-type
       typerex-with-caml-mode-p]
     [ "Help for identifier" caml-help
       typerex-with-caml-mode-p]
     [ "Show fully qualified ident at point" caml-types-show-ident
       typerex-with-caml-mode-p]
     ["Switch .ml/.mli" typerex-find-alternate-file t]
   "---"
     ["Compile..." compile t]
     ["On-the-fly compilation" flymake-mode :style toggle :selected flymake-mode
      :help "Turn on/off on-the-fly compilation with ocamlbuild"
      :visible ocp-flymake-available]

     ("Interactive Mode"
      ["Run Caml Toplevel" typerex-run-caml t]
      ["Interrupt Caml Toplevel" typerex-interrupt-caml
       :active (comint-check-proc typerex-interactive-buffer-name)]
      ["Kill Caml Toplevel" typerex-kill-caml
       :active (comint-check-proc typerex-interactive-buffer-name)]
      ["Evaluate Region" typerex-eval-region
       ;; Region-active-p for XEmacs and mark-active for Emacs
       :active (if (fboundp 'region-active-p) (region-active-p) mark-active)]
      ["Evaluate Phrase" typerex-eval-phrase t]
      ["Evaluate Buffer" typerex-eval-buffer t])
   "---"

     ("Caml Forms"
      ["try .. with .." typerex-insert-try-form t]
      ["match .. with .." typerex-insert-match-form t]
      ["let .. in .." typerex-insert-let-form t]
      ["if .. then .. else .." typerex-insert-if-form t]
      ["while .. do .. done" typerex-insert-while-form t]
      ["for .. do .. done" typerex-insert-for-form t]
      ["begin .. end" typerex-insert-begin-form t])

     ("Definitions"
      ["Scan..." typerex-list-definitions t])
     ;; [ "Complete identifier" caml-complete
     ;;   typerex-with-caml-mode-p]
     [ "Add path for documentation" ocaml-add-path
       typerex-with-caml-mode-p]
     [ "Open module for documentation" ocaml-open-module
       typerex-with-caml-mode-p]
     [ "Close module for documentation" ocaml-close-module
       typerex-with-caml-mode-p]
     "---"
     ("More"
     ,(ocp-action-item '(ocp-restart-server "Restart"))
     ["Customize TypeRex Mode..." (customize-group 'ocp) t]
     ("TypeRex Options" ["Dummy" nil t])
     ("TypeRex Interactive Options" ["Dummy" nil t])
     ["TypeRex User Manual" typerex-browse-typerex-manual t]
     ["TypeRex Short Cuts" typerex-help t]
;;     ["Short Cuts" typerex-short-cuts]
     ["OCaml Reference Manual..." typerex-browse-manual t]
     ["OCaml Library..." typerex-browse-library t]
     ["About" typerex-about t])

     )))
  (easy-menu-add typerex-mode-menu)
  (typerex-update-options-menu)
  ;; Save and update definitions menu
  (if typerex-with-xemacs
      (add-hook 'activate-menubar-hook 'typerex-update-definitions-menu)
    (when (functionp 'easy-menu-create-menu)
      ;; Patch for Emacs
      (add-hook 'menu-bar-update-hook
                'typerex-with-emacs-update-definitions-menu)
      (make-local-variable 'typerex-definitions-keymaps)
      (setq typerex-definitions-keymaps
            (cdr (easy-menu-create-menu
                  "Definitions" typerex-definitions-menu)))
      (setq typerex-definitions-menu-last-buffer nil))))

(ocp-wizard-menu-plugin)
