;; Copyright (c) 2001 Morgan Stanley Dean Witter and Co. All rights reserved.  
;; See .../src/LICENSE for terms of distribution.

(require 'keyb)
(require 'a)

; Interactively set a-prog
(global-set-key "\C-ca" 'set-a-prog-builtin)
(global-set-key 'f1 'a-send-region)
(global-set-key 'f2 'a-send-line) 
(global-set-key 'f3 'a-send-func) 
(global-set-key 'f4 'a-other-window) 
(global-set-key 'f5 'delete-other-windows)
(global-set-key 'f6 'other-window-or-pop) 
(global-set-key 'f7 'next-buffer) 
(global-set-key 'f8 'previous-buffer)
(global-set-key 'f9 'enlarge-window) 
(global-set-key 'f10 'shrink-window)
(global-set-key 'scroll 'scroll-down-in-place) 
(global-set-key '(shift scroll) 'scroll-up-in-place)
(global-set-key 'prsc 'print-buffer)


(global-set-key 'props 'toggle-a-minor-mode)

(global-set-key 'backspace 'backward-delete-char-untabify)
(define-key esc-map "h" 'help-for-help)

(defun other-window-or-pop()
  "Goto next window or if only one window pop previous buffer in other window"
  (interactive)
  (let* ((win1 (selected-window))
	 (win2 (progn (other-window 1) (selected-window))))
    (and (eq win1 win2) (switch-to-buffer-other-window nil))))

(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(defun previous-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun next-buffer ()
  "Switch to next buffer."
  (interactive)
  (switch-to-buffer
   (if (eq last-command 'next-buffer)
       (first-normal-buffer (reverse (buffer-list)))
       (other-buffer))))

(defun first-normal-buffer (buflist)
  (if (null buflist)
      nil
      (let
	  ((buf1 (buffer-name (car buflist))))
	  (if (equal " " (substring buf1 0 1))
	      (first-normal-buffer (cdr buflist))
	      buf1))))

(defun kill-current-buffer ()
  "Kills the current buffer if no process."
  (interactive)
  (if (get-buffer-process (current-buffer)) 
	  (message "Buffer not killed -- Buffer has process")
	(if buffer-file-name 
		(if (buffer-modified-p (current-buffer)) 
			(message "Buffer not killed -- Buffer is modified")
		  (kill-this-buffer))
	  (kill-this-buffer)
	  )
	)
  )
