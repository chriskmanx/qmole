;; Copyright (c) 2001 Morgan Stanley Dean Witter and Co. All rights reserved.
;; See ..../src/LICENSE for terms of distribution.

(defvar a-key-string nil "Apl characters corresponding to raw keyboard input")
(defvar xev nil)
(defvar key nil)

(defun a-self-insert (n) (interactive "p")
	(or n (setq n 1))
	(setq xev (copy-event last-command-event))
	(setq key (event-to-character xev t t t))
	(setq last-command-char (aref a-key-string key))
	(self-insert-command n))

(setq a-minor-map nil)
(if a-minor-map nil
  (setq a-minor-map (make-sparse-keymap))
  (setq a-key-string (make-string 256 ?\000))
  (let ((n 32))
    (while (< n 256)
      (aset a-key-string n n)
      (setq n (1+ n))))
  (mapcar #'(lambda (akeydef)
	      (let ((key (car akeydef))
		    (apl (cdr akeydef)))
		(define-key a-minor-map (list 'meta (- key 128)) 'a-self-insert)
		(aset a-key-string key apl)))
	  '((?\M-a . ?\301) ; A (2)
	    (?\M-b . ?\302) ; B
	    (?\M-c . ?\303) ;
	    (?\M-d . ?\304) ; D
	    (?\M-e . ?\305) ;
	    (?\M-f . ?_)    ; _
	    (?\M-g . ?\307) ; G
	    (?\M-h . ?\310) ; H
	    (?\M-i . ?\311) ;
	    (?\M-j . ?\312) ; J
	    (?\M-k . ?\')   ; '
	    (?\M-l . ?\314) ; L
	    (?\M-m . ?|)    ; |
	    (?\M-n . ?\316) ; N
	    (?\M-o . ?\317) ;
	    (?\M-p . ?\*)   ; *
	    (?\M-q . ?\?)   ; ?
	    (?\M-r . ?\322) ; R
	    (?\M-s . ?\323) ;
	    (?\M-t . ?\~)   ; ~
	    (?\M-u . ?\325) ;
	    (?\M-v . ?\326) ; V
	    (?\M-w . ?\327) ; W (1)
	    (?\M-x . ?\330) ;
	    (?\M-y . ?\331) ;
	    (?\M-z . ?\332) ;
	    (?\M-1 . ?\241) ; !
	    (?\M-2 . ?\242) ;
	    (?\M-3 . ?<)    ; <
	    (?\M-4 . ?\244) ; $
	    (?\M-5 . ?=)    ; =
	    (?\M-6 . ?\246) ;
	    (?\M-7 . ?>)    ; >
	    (?\M-8 . ?\250) ; (
	    (?\M-9 . ?\251) ; )
	    (?\M-0 . ?^)    ; ^
	    (?\M-- . ?\253) ; +
	    (?\M-= . ?\337) ;
	    (?\M-\\ . ?\334);
	    (?\M-` . ?\376) ; ~
	    (?\M-, . ?\254) ; ,
	    (?\M-[ . ?\373) ; {
	    (?\M-] . ?\375) ; }
	    (?\M-; . ?\333) ; [
	    (?\M-' . ?\335) ;
	    (?\M-. . ?\334) ;
	    (?\M-/ . ?\257) ; /
	    (?\M-! . ?\340) ; `
	    (?\M-@ . ?\346) ;
	    (?\M-# . ?\347) ;
	    (?\M-$ . ?\350) ; h
	    (?\M-% . ?\367) ; w (1)
	    (?\M-^ . ?\364) ; t
	    (?\M-& . ?\341) ; a (2)
	    (?\M-* . ?\360) ; p
	    (?\M-\( . ?\271); 9
	    (?\M-\) . ?\260); 0
	    (?\M-_ . ?!)    ; !
	    (?\M-+ . ?\255) ; -
	    (?\M-| . ?\374) ; | (check syntax table entry!)
	    (?\M-O . ?\357) ;
	    (?\M-J . ?\352) ;
	    (?\M-F . ?\275) ; =
	    (?\M-E . ?\345) ; e
	    (?\M-I . ?\351) ; i
	    (?\M-{ . ?\335) ;
	    (?\M-} . ?\333) ; [
	    (?\M-S . ?\276) ; >
	    (?\M-G . ?\347) ;
	    (?\M-H . ?\350) ; h
	    (?\M-L . ?\354) ; l
	    (?\M-: . ?\274) ;
	    (?\M-\" . ?\273); ;
	    (?\M-C . ?\343) ; c
	    (?\M-B . ?\342) ; b
	    (?\M-N . ?\356) ; n
	    (?\M-M . ?\315) ;
	    (?\M-> . ?\256) ; . *** ???
	    (?\M-P . ?\263) ; 3
	    (?\M-Y . ?\264) ; 4
	    (?\M-Z . ?\372) ; z
	    (?\M-Q . ?\277) ;
	    (?\M-W . ?W)    ; W
	    (?\M-R . ?R)    ; R
	    (?\M-T . ?T)    ; T
	    (?\M-U . ?U)    ; U
	    (?\M-A . ?A)    ; A
	    (?\M-D . ?D)    ; D
	    (?\M-K . ?K)    ; K
	    (?\M-X . ?X)    ; X
	    (?\M-V . ?V)    ; V
	    (?\M-? . ?\?)   ; ?
	    (?\M-~ . ?~)    ; ~
	    (?\M-< . ?<)    ; <
	    (?\M-\040 . ?\040)
	    ) ; end of translation table
	   ) ; end of mapcar
	  
   ;; if we are running off of a tty, window-system is nil
   ;; in such a case, we need to handle all those events that get passed
   ;; as escape sequences
  (if window-system nil
    (global-unset-key "\C-h")
    (global-set-key "\C-h" 'backward-delete-char) 
    (define-key esc-map "[" nil)
    (define-key esc-map "O" nil)
    (mapcar #'(lambda (akeydef)
		(let ((key (car akeydef))
		      (apl (cdr akeydef)))
		  (define-key esc-map key apl)
		  ))
	    '(
	      ("[224z" . a-send-region)		; f1
	      ("[225z" . a-send-line)		; f2
	      ("[226z" . a-send-func)		; f3
	      ("[227z" . a-other-window)	; f4
	      ("[228z" . delete-other-windows)	; f5
	      ("[229z" . other-window-or-pop)	; f6
	      ("[230z" . next-buffer)		; f7
	      ("[231z" . previous-buffer)	; f8
	      ("[232z" . enlarge-window)	; f9
	      ("[233z" . shrink-window)		; f10
	      ("[209z" . print-buffer)		; PrintScreen
	      ("[210z" . scroll-down-in-place)	; Scroll
	      ("[5z" . scroll-down)		; pgup
	      ("[216z" . scroll-down)		; pgup
	      ("[6z" . scroll-up)		; pgdn
	      ("[222z" . scroll-up)		; pgdn
	      ("[214z" . beginning-of-line)	; home
	      ("[220z" . end-of-line)		; end
	      ("[2z" . overwrite-mode)		; insert
	      ("OA" . previous-line)		; up-arrow
	      ("OB" . next-line)		; down-arrow
	      ("OC" . forward-char)		; right-arrow
	      ("OD" . backward-char)		; left-arrow
	      ) ; end of translation table
	    ) ; end of mapcar
    ) ; end of if window-system nil
) ; end of if a-minor-map

(define-key a-minor-map "\C-c\C-a" 'a)
(define-key a-minor-map "\C-c\C-r" 'a-send-region)
(define-key a-minor-map "\C-c\C-l" 'a-send-line)
(define-key a-minor-map "\C-c\C-f" 'a-send-func)
(define-key a-minor-map "\C-c\C-b" 'a-send-buffer)
(define-key a-minor-map '(escape)  esc-map)

(setq a-mode-map nil)
(if a-mode-map nil
  (setq a-mode-map (make-sparse-keymap)))
(define-key a-mode-map "\C-m" 'a-send-input)
(define-key a-mode-map "\C-c\C-c" 'a-interrupt-process)
(define-key a-mode-map "\C-c\C-k" 'a-kill-process)
(define-key a-mode-map "\C-c\C-q" 'a-quit-process)
(define-key a-mode-map "\C-c\C-s" 'a-show-output) 
(define-key a-mode-map "\C-c\C-o" 'a-kill-output) 
(define-key a-mode-map "\C-c\C-p" 'a-goto-last-input)
(define-key a-mode-map "\C-c\C-m" 'a-recall-last-input)
  
(if (not (assq 'a-minor minor-mode-alist))
	(setq minor-mode-alist (cons '(a-minor " A") minor-mode-alist)))

(provide 'keyb)

