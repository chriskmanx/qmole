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

;; Auto-completion using Auto Complete Mode

;; This file defines a completion source for the auto complete mode,
;; which rellies on ocp-wizard tokenization and semantic analysis.

(defcustom auto-complete-keys nil
  "If set, simultaneously specifies a set of keys for auto-completion"
  :group 'typerex-auto-complete :type '(symbol))

;;(eval-when-compile
  (require 'auto-complete-config nil t)
;;)

(defvar last-ocp-completion-point nil
  "the point at which completion data is available, if any")
(make-variable-buffer-local 'last-ocp-completion-point)
(defvar last-ocp-completion-data nil
  "the data for the last completion point, if any")
(make-variable-buffer-local 'last-ocp-completion-data)

(defun discard-ocp-completion-data ()
  "Flush the completion data. This function is called by the
modification hook in tokenize.el."
  (setq last-ocp-completion-point nil)
  (setq last-ocp-completion-data nil))

(defun compute-ocp-completion-data ()
;;  (message "computing candidates...")
  (let*
      ((pos (- (position-bytes (point)) 1))
        (result
        (owz-string-command
         (concat
          "completion " (buffer-name) " " (int-to-string pos)))))
;;    (message "candidates: %s" result)
    (let
        ((candidates (read result)))
;;      (mapc
;;       (lambda (c) (message "candidate: %s" c))
;;       candidates)
      candidates))
  )

(defun get-ocp-completion-data ()
  (ocp-modify-changed-region)
  (if (eq last-ocp-completion-point (point))
      last-ocp-completion-data
    (setq last-ocp-completion-point (point))
    (setq last-ocp-completion-data (compute-ocp-completion-data))
    last-ocp-completion-data))

(defun ocp-prefix () (eval (car (get-ocp-completion-data))))
(defun ocp-candidates () (cadr (get-ocp-completion-data)))

(defun ocp-candidates-names ()
  (let ((candidates (ocp-candidates)))
;;    (message "candidates: %s" candidates)
    (let ((candidates (mapcar 'car candidates)))
;;      (message "candidates: %s" candidates)
      candidates)))

;;(defun ocp-ac-documentation (candidate)
;;  (message "getting doc for %s" candidate)
;;  (let ((doc (cadr (assoc candidate (ocp-candidates)))))
;;    (message "doc=%s" doc)
;;    doc))

(defun ocp-ac-documentation (candidate)
;;  (message "getting doc for %s" candidate)
  (let ((doc
         (owz-string-command
          (concat "completion-doc " (buffer-name) " " candidate))))
;;    (message "doc=%s" doc)
    doc))

;; This is not how symbol works
;;(defun ocp-ac-symbol (candidate)
;;  (message "getting symbol for %s" candidate)
;;  (let ((symb (cadr (assoc candidate (ocp-candidates)))))
;;    (message "symb=%s" symb)
;;    symb))

(defun ac-keys-backquote-backslash ()
  "configuration using backquote to complete longest common
  prefix, backslash to accept current candidate, and C-n, C-p to
  cycle between candidates"

     ;; Using <`> to complete whatever the context, and <C-`> for `
     (define-key ac-mode-map "`" 'auto-complete)
     ;;(ac-set-trigger-key "`")
     ;; Workaround a bug in auto complete mode (I dont' kno how to avoid
     ;; altering the global map):
     (define-key (current-global-map) "`" nil)
     (define-key (current-global-map) [?\C-`] (lambda () (interactive) (insert "`")))
;;     (define-key ac-mode-map [?\C-`] (lambda () (interactive) (insert "`")))
     ;; Using <\> to accept the selected completion
     (define-key ac-completing-map "\\" 'ac-complete)
     ;;AutoComplete keymap: keeping normal behavior of RET and TAB, up and down
     (define-key ac-completing-map "\C-n" 'ac-next)
     (define-key ac-completing-map "\C-p" 'ac-previous)
     (define-key ac-completing-map "\r" nil)
     (define-key ac-completing-map "\t" nil)
     (define-key ac-completing-map [up] nil)
     (define-key ac-completing-map [down] nil)
  )

(defun ac-keys-two-dollar ()
  "configuration using ² to complete longest common prefix, $
  to accept current candidate, and C-n, C-p to cycle between
  candidates"

     ;; Using <`> to complete whatever the context, and <C-`> for `
     (define-key ac-mode-map "²" 'auto-complete)
     (define-key (current-global-map) "²" nil)
     (define-key ac-completing-map "$" 'ac-complete)
     ;; AutoComplete keymap: keeping normal behavior of RET and TAB, up and down
     (define-key ac-completing-map "\C-n" 'ac-next)
     (define-key ac-completing-map "\C-p" 'ac-previous)
     (define-key ac-completing-map "\r" nil)
     (define-key ac-completing-map "\t" nil)
     (define-key ac-completing-map [up] nil)
     (define-key ac-completing-map [down] nil)
  )

(defun ac-keys-default-start-with-c-tab ()
  "configuration using standard keys (RET - TAB), but with C-TAB
as trigger key"
  (ac-set-trigger-key "<C-tab>")
  )

(when ocp-auto-complete
      (require 'auto-complete-config)
      ;; Defining ac-source-ocp-wizard and ac-complete-ocp-wizard
      (ac-define-source ocp-wizard
        '((candidates . ocp-candidates-names)
          (prefix . ocp-prefix)
;;          (symbol . ocp-ac-symbol)
          (document . ocp-ac-documentation)
          ))
      ;; Set auto-completion source to ocp-wizard when in TypeRex mode
      (add-hook
       'typerex-mode-hook
       (lambda ()
         (setq ac-sources '(ac-source-ocp-wizard))
         (auto-complete-mode))
       t)
      ;; Enable auto-completion in all OCaml buffers
      (add-to-list 'ac-modes 'typerex-mode)
      (when auto-complete-keys
        (add-hook 'auto-complete-mode-hook auto-complete-keys))
      )
