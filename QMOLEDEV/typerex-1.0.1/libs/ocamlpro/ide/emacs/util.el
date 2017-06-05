;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                        ;
;                        TypeRex OCaml Studio                            ;
;                                                                        ;
;                 Thomas Gazagnaire, Fabrice Le Fessant                  ;
;                                                                        ;
;  Copyright 2011-2012 OCamlPro                                          ;
;  All rights reserved.  This file is distributed under the terms of     ;
;  the GNU Public License version 3.0.                                   ;
;                                                                        ;
;  TypeRex is distributed in the hope that it will be useful,            ;
;  but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;  GNU General Public License for more details.                          ;
;                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "LOADING TYPEREX")
;;(setq debug-on-error t)

(defun exec-in-buffer (tmpbuf command)
  (let ((commandstring (mapconcat 'identity command " ")))
    (message commandstring)
    (let* ((process-name (car command))
           (process-args (cdr command))
           (exit-status 
            (apply
             'call-process
             (append 
              (list process-name nil tmpbuf nil)
              process-args))))
      (message "command exited with code %d (output=%s)" exit-status
               (if (equal tmpbuf nil) "<none>" tmpbuf))
      (if (not (eq exit-status 0))
          (if (equal tmpbuf nil)
              (error process-name)
              (error (with-current-buffer tmpbuf
                       (concat commandstring "\n" (buffer-string)))))))))

(defun check-position (pos)
  (min (point-max) (max (point-min) pos)))

(defun check-byte (pos)
  (min (position-bytes (point-max)) (max (position-bytes (point-min)) pos)))

(defun line-column-bytes ()
  (let ((l (line-number-at-pos))
        (c (- (position-bytes (point))
              (position-bytes (line-beginning-position)))))
    (format "%d %d" l c)))

(defun line-column-to-pos (l c)
  (save-excursion
    (goto-char (point-min))
    (byte-to-position (+ (position-bytes (line-beginning-position l)) c))))

(defun highlight-overlay (face start end)
  "put an overlay on the given range, in the current buffer"
  (let ((overlay (make-overlay (check-position start) (check-position end))))
    (overlay-put overlay 'face face)
    overlay))

(defun highlight-region (pos)
  (highlight-overlay (car pos) (cadr pos) (cadr (cdr pos))))

(defun do-at-next-input (f arg)
  (let ((inhibited inhibit-quit))
    (setq inhibit-quit t)
    (sit-for 10000)
    (apply f (list arg))
    (setq inhibit-quit inhibited)))

(defun highlight (face start end)
  (let ((overlay (highlight-region (list face start end))))
    (do-at-next-input
     'delete-overlay overlay)))

(defun highlight-regions (forever regions)
  (let ((overlays (mapcar 'highlight-region regions)))
    (unless (eq forever t)
      (do-at-next-input
       (lambda (ovs) (mapc 'delete-overlay ovs)) overlays))
    nil))

(defun display-temp (buffer-name contents)
  "like display-message-or-buffer, but not permanent"
  (display-message-or-buffer contents buffer-name)
  (do-at-next-input
   (lambda (buffer-name)
     (condition-case nil
         (kill-buffer buffer-name)
        (error ())))
   buffer-name))

;; Copied from Emacs itself (simple.el) !
(defun fits-in-echo-area ()
  "tests whether a buffer's contents fits in the echo area"
  (let ((lines
         (if (= (buffer-size) 0)
             0
           (count-screen-lines nil nil nil (minibuffer-window)))))
    (or (<= lines 1)
        (<= lines
            (if resize-mini-windows
                (cond ((floatp max-mini-window-height)
                       (* (frame-height)
                          max-mini-window-height))
                      ((integerp max-mini-window-height)
                       max-mini-window-height)
                      (t
                       1))
              1)))))

(defun y-or-n-check-height (prompt)
  "prompt for a single character answer as y-or-n-p, but use a
buffer to display the question if it is too long."
  (with-temp-buffer
    (insert prompt)
    (if (fits-in-echo-area)
        (y-or-n-p prompt)
      (display-buffer (current-buffer))
      (let ((answer (y-or-n-p prompt)))
        (kill-buffer (current-buffer))
        answer))))

(defun set-text-properties-region (pos)
  (add-text-properties (car pos) (cadr pos) (cadr (cdr pos))))

(defun propertize-regions (regions)
  (mapc 'set-text-properties-region regions)
  nil)

(defun propertize-region-list (regions)
  (let ((props (car regions)))
    (mapc
     (lambda (pos)
       (add-text-properties (car pos) (cadr pos) props))
     (cadr regions)))
  nil)

(defun propertize-region-list-byte (regions)
  (let ((props (car regions)))
    (mapc
     (lambda (pos)
       (let ((start (check-byte (car pos)))
             (end (check-byte (cadr pos))))
         (add-text-properties
          (byte-to-position start) (byte-to-position end) props)))
     (cadr regions)))
  nil)

(defun propertize-region-lists-char (regions)
  (mapc 'propertize-region-list regions)
  nil)

(defun propertize-region-lists-byte (regions)
  (mapc 'propertize-region-list-byte regions)
  nil)

(defun get-buffer-create-clear (name)
  "return a buffer with the given name, clearing it if necessary"
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (widen)
      (delete-region (point-min) (point-max)))
    buffer))

(defun create-buffer (buffer-name) (get-buffer-create-clear buffer-name))

(defun set-cleared-buffer (buffer-name)
  (set-buffer (get-buffer-create-clear buffer-name)))
  
(defun string-of-buffer (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun int-of-string (str)
  (if (equal str "0")
      0
    (let ((i (string-to-number str)))
      (if (equal i 0)
          nil
        i))))

(defun current-line ()
  "Return the vertical position of point..."
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 1 0)
     -1))

(defun strings-of-buffer (buffer)
  (with-current-buffer buffer
    (split-string (buffer-string) "[\n]+")))

(defun goto-position (pos)
  (forward-char (- (+ 1 pos) (point))))

(defun current-ident ()
  (save-excursion
    (let (start end)
    (search-backward-regexp "[^a-zA-Z._0-9]")
    (forward-char 1)
    (setq start (point))
    (search-forward-regexp "[^a-zA-Z._0-9]" nil 0)
    (if (eq (point) (point-max))
       (setq end (point))
       (setq end (- (point) 1)))
    (buffer-substring start end))))

(defun delete-current-ident ()
  (let (start end end-buffer)
  (search-backward-regexp "[^a-zA-Z._0-9]")
  (forward-char 1)
  (setq start (point))
  (search-forward-regexp "[^a-zA-Z._0-9]" nil 0)
  (if (eq (point) (point-max))
      (progn
        (setq end (point))
        (setq end-buffer 1))
    (progn
      (setq end (- (point) 1))
      (setq end-buffer 0)))
  (delete-region start end)
  (if (equal end-buffer 0)
      (backward-char 1))))

(defun revert-with-history ()
  "revert the current buffer while keeping its history"
  (let ((pos (point)))
    (save-excursion
;;	  (setq scroll (window-vscroll))
;;	  (setq auto-window-vscroll nil)
      (clear-visited-file-modtime)
      (widen)
      (setq inhibit-modification-hooks t)
       (delete-region (point-min) (point-max))
       (setq inhibit-modification-hooks nil)
       (insert-file-contents (buffer-file-name))
      )
    (goto-char pos)
    (set-buffer-modified-p nil)
    (set-visited-file-modtime)
    ))

(defun revert-buffer-visiting (filename)
  "revert the buffer visiting filename, if any"
  (let ((buffer (find-buffer-visiting filename)))
    (unless (eq buffer nil)
      (with-current-buffer buffer
;; workaround for the duplicate insertion bug
;;        (revert-buffer t t t))
        (revert-with-history)
        (setq buffer-undo-list nil)
        (push '(apply ocp-undo) buffer-undo-list)
      ))))

(defun save-buffer-visiting (filename)
  "revert the buffer visiting filename, if any"
  (let ((buffer (find-buffer-visiting filename)))
    (unless (eq buffer nil)
      (with-current-buffer buffer
        (save-buffer)))))

(defun renamed-file (old-filename new-filename)
  "update the buffer visiting a renamed filename, if any"
  (let ((buffer (find-buffer-visiting old-filename)))
    (unless (eq buffer nil)
      (with-current-buffer buffer
        (set-visited-file-name new-filename t t)
;; workaround for the duplicate insertion bug
;;       (revert-buffer t t t))
        (revert-with-history)
        (setq buffer-undo-list nil)
        (push '(apply ocp-undo) buffer-undo-list)
        (reset-tokenization)
        ))))
