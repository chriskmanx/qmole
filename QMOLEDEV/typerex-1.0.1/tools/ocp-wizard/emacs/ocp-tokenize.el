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

;; Tokenization and Font-lock with ocp-wizard

;; This file registers hooks to let ocp-wizard know about the current
;; buffer contents, enabling completion and a fontification.

(defcustom ocp-auto-complete nil
  "If true, enable TypeRex auto-completion"
  :group 'typerex-auto-complete :type '(boolean))

(defcustom ocp-pre-cache t
  "If true, pre-cache the cmt to prevent pausing at first
completion. We use a counter to avoid the deadlocks that used to
happen when either:
 - Emacs is started on several files simultaneously
 - User begins typing during Emacs startup."
  :group 'typerex-auto-complete :type '(boolean))

;; If ocp-buffer-bytes = 0, then next update will (re)set the buffer
;; contents on the server.
(defvar ocp-buffer-bytes 0 "length as known to ocp-wizard")
(make-variable-buffer-local 'ocp-buffer-bytes)

;; If start <= end then this region has been modified
(defvar ocp-buffer-modified-start 1)
(make-variable-buffer-local 'ocp-buffer-modified-start)
(defvar ocp-buffer-modified-end 1)
(make-variable-buffer-local 'ocp-buffer-modified-end)

(defun ocp-update-modified (begin end &optional old-len)
  "Enlarge the recorded modified region according to the given
parameters. This function is set as an after-change hook, as well
as a find-file-hook (with point-min and point-max as parameters)."
(unless (eq ocp-buffer-bytes 0)
  (if (eq old-len nil) (setq old-len 0))
;;  (message "ocp-update-modified [%d, %d[ (old-len=%d)" begin end old-len)
  (if (<= ocp-buffer-modified-start ocp-buffer-modified-end)
      (progn
        (setq ocp-buffer-modified-start (min ocp-buffer-modified-start begin))
        (setq ocp-buffer-modified-end
              (max end
                   (if (<= begin ocp-buffer-modified-end)
                       (let ((growth (- (- end begin) old-len)))
                         (+ ocp-buffer-modified-end growth))
                     ocp-buffer-modified-end))))
    (setq ocp-buffer-modified-start begin)
    (setq ocp-buffer-modified-end end)
    ;; See auto-completion
    (if (fboundp 'discard-completion-data) (discard-completion-data)))
;;  (message "-> [%d, %d["  ocp-buffer-modified-start  ocp-buffer-modified-end)
  ))

(defun reset-tokenization ()
  "Ensure that the next change committed to the server
will (re-)load the whole buffer"
  (setq ocp-buffer-bytes 0))

;; We use this hack to get the absolute filename before the
;; buffer-local variable has been initialized (when fontifying for the
;; first time).
(defun filename-of-buffer-name (buffer-name directory)
  "Try to get the absolute filename for a buffer name and directory name"
  (let* ((len (length buffer-name))
         (filename
          (if (and (> len 3)
                   (char-equal (elt buffer-name (- len 1)) ?>)
                   (char-equal (elt buffer-name (- len 3)) ?<))
              (substring buffer-name 0 (- len 3))
            buffer-name)))
    (expand-file-name filename directory)))

;; (defcustom typerex-extension-list '("ml" "mli" "mll" "mly")
;;   "File extensions which enable TypeRex"
;;   :group 'ocp)

;; (defun is-ocaml-buffer ()
;;   (let* ((filename (filename-of-buffer-name (buffer-name) default-directory))
;;          (len (length filename)))
;;     (member (file-name-extension filename) typerex-extension-list)))

(defun is-ocaml-buffer ()
  (not (string= (buffer-name) typerex-interactive-buffer-name)))

(defun ocp-modify-region
  (start end start-bytes end-bytes old-length-bytes first-time)
  "Commit a region modification to the server, and return the
string result, which is either the fontification command, or
OK. All positions count from 1."
;;  (message "ocp-modify-region [%d, %d[, old=%d, first-time=%s"
;;           start-bytes end-bytes old-length-bytes first-time)
  (let* ((filename (filename-of-buffer-name (buffer-name) default-directory))
         (time-before (float-time)))
    (owz-string-command
     (concat "modify-buffer "
             (buffer-name) " "
             filename " "
             (int-to-string (- start-bytes 1)) " "
             (int-to-string (- end-bytes 1)) " "
             (int-to-string old-length-bytes)
             (if first-time " true" " false")
             "\n"
             (buffer-substring start end)))))

(defun ocp-try-once-modify-changed-region ()
  "Commit the currently changed region (and set the state to
unchanged)."
;;  (message "typerex-fontify-changed-region")
  (if (eq ocp-buffer-bytes 0)
      (progn
        (setq ocp-buffer-modified-start (point-min))
        (setq ocp-buffer-modified-end (point-max))))
  (let* ((last-pos (+ (buffer-size) 1))
         (last-pos-bytes (position-bytes last-pos))
         (new-ocp-buffer-bytes (- last-pos-bytes 1))
         (growth-bytes (- new-ocp-buffer-bytes ocp-buffer-bytes))
         (first-time (eq ocp-buffer-bytes 0)))
;;  (message "modified-start=%d, modified-end=%d" ocp-buffer-modified-start ocp-buffer-modified-end)
    (if (<= ocp-buffer-modified-start ocp-buffer-modified-end)
        (let*
            ((begin-bytes (position-bytes ocp-buffer-modified-start))
             (end-bytes (position-bytes ocp-buffer-modified-end))
             (old-length-bytes (- (- end-bytes begin-bytes) growth-bytes)))
          (setq ocp-buffer-bytes new-ocp-buffer-bytes)
          (let ((res
                 (ocp-modify-region
                  ocp-buffer-modified-start ocp-buffer-modified-end
                  begin-bytes end-bytes old-length-bytes first-time)))
            (setq ocp-buffer-modified-start last-pos)
            (setq ocp-buffer-modified-end 1)
            res))
      nil)))

(defun ocp-modify-changed-region ()
  "Same as ocp-try-once-modify-change-region, but in case of error,
try to reset tokenization. Actual arguments are ignored; the
modifications are tracked explicitely thanks to ocp-update-modified."
  (condition-case e
      (ocp-try-once-modify-changed-region)
    (error
     (progn
       (message "Error during tokenization: %s" e)
       (message "Trying to reset tokenization")
       (reset-tokenization)
       (condition-case e
           (ocp-try-once-modify-changed-region)
         (error
          (message "Error again: %s\nAbort" e)
          nil))))))

(defun typerex-fontify-changed-region (begin end &optional verbose)
  "Commit any pending modifications and re-fontify the modified part"
  (ocp-modify-changed-region)
  (let ((command
         (owz-string-command
          (concat "fontify-buffer " (buffer-name)))))
    (condition-case e
        (eval (read command))
      (error (message "Error during fontification: %s" e)))))

;; This is a nice trick which prevents long pauses.
(defun ocp-pre-cache-buffer (buffer)
  (run-with-idle-timer
   0 nil
   (lambda (buffer) (owz-string-command (concat "pre-cache-buffer " buffer)))
   buffer))

;; Install modification hooks and font-lock function
(add-hook
 'typerex-mode-hook
 (lambda ()
   (when (and ocp-syntax-coloring (string= ocp-theme "caml"))
     (if (require 'caml-font nil t)
         (eval '(caml-font-set-font-lock))
       (message "caml-font not found")))
   (if (is-ocaml-buffer)
       (progn
         ;; If either coloring or completion are enabled, then track
         ;; modifications in each TypeRex-enabled buffer.
         (if (or (and ocp-syntax-coloring
                      (not (member ocp-theme '("tuareg" "caml"))))
                 ocp-auto-complete)
             (progn
               ;; Record each modification
               (add-hook 'after-change-functions 'ocp-update-modified nil t)
               ;; Reset fontification when visiting a file, since it may be
               ;; a C-x C-w and change the buffer name
               (add-hook 'find-file-hook 'reset-tokenization nil t)))
         ;; If coloring is enabled, modification are commited by the
         ;; typerex-fontify-changed-region, which we register by redefining
         ;; typerex-install-font-lock.
         (make-local-variable 'font-lock-fontify-region-function)
         (if (and ocp-syntax-coloring
                  (not (member ocp-theme '("tuareg" "caml"))))
             (setq font-lock-fontify-region-function
                   'typerex-fontify-changed-region))
         ;; If auto-completion is enabled, modifications are also
         ;; commited when completion is invoked (see
         ;; ocp-get-completion-data)

         ;; If auto-completion is enabled, we pre-load the cmt to give
         ;; a smoother impression.
         (if (and ocp-auto-complete ocp-pre-cache)
             (add-hook
              'find-file-hook
              (lambda () (ocp-pre-cache-buffer (buffer-name)))
              t t))
         ))))

;; Flymake workaround (incompatible with syntax coloring)
(defadvice flymake-get-file-name-mode-and-masks (around ocp-flymake-fix activate)
  (condition-case nil
      ad-do-it
    (error nil)))
