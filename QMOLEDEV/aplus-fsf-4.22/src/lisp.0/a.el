;; Copyright (c) 2001 Morgan Stanley Dean Witter and Co. All rights reserved.
;; See .../src/LICENSE for terms of distribution.


(defvar a+-font "kaplscreen-bold"  "Font used in A+ mode.")
(defvar old-default-font (face-font-name 'default))

; (defvar apr-command  "/usr/local/bin/apr" "Command to print A+ code")
;(defvar apr-command  "/ms/dist/aurora/bin/apr" "Command to print A+ code")
(defvar apr-command  "/usr/bin/lpr" "Command to print A+ code")
(defvar lpr-command  "/usr/bin/lpr" "Command to print A+ code")
(defvar old-lpr-command lpr-command)

(defvar apr-switches (list "-2r") "Switches for apr-command")
(defvar lpr-switches (list "-2r") "Switches for apr-command")
(defvar old-lpr-switches lpr-switches)

(defvar a-minor nil "*t if in A minor mode")
(make-variable-buffer-local 'a-minor)
(setq-default a-minor nil)

(defvar a-alt-parents nil "Keymap parents for A minor mode toggle")
(make-variable-buffer-local 'a-alt-parents)
(setq-default a-alt-parents nil)

(defvar a-alt-popup nil "Popup menu for A minor mode toggle")
(make-variable-buffer-local 'a-alt-popup)
(setq-default a-alt-popup nil)

(defvar a-mode-syntax-table nil "Syntax table in use in a-minor mode.")

(defun set-a-comment-parms ()
  "Assigns a+ comment parameters"
  (setq comment-multi-line nil)
  (setq comment-start (char-to-string ?\343))
  (setq comment-end "")
  (setq comment-column 1)
  (setq parse-sexp-ignore-comments t)
  )

(if a-mode-syntax-table
    ()
  (setq a-mode-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?\n ">" a-mode-syntax-table)   ; \n is end of comment
  (modify-syntax-entry ?\\ "." a-mode-syntax-table)   ; \ is punctuation
  (modify-syntax-entry ?_ "w" a-mode-syntax-table)    ; _ is word constituent
  (modify-syntax-entry ?' "\"" a-mode-syntax-table)   ; ' is string quote
  (modify-syntax-entry ?\; "." a-mode-syntax-table)
  (modify-syntax-entry ?# "." a-mode-syntax-table)
  (modify-syntax-entry ?, "." a-mode-syntax-table)
  (modify-syntax-entry ?@ "w" a-mode-syntax-table)
  (modify-syntax-entry ?. "_" a-mode-syntax-table) 
  (modify-syntax-entry ?\241 "." a-mode-syntax-table) ; dieresis (each)
  (modify-syntax-entry ?\242 "_" a-mode-syntax-table) ; overbar
  (modify-syntax-entry ?\244 "." a-mode-syntax-table) ; less than or equal
  (modify-syntax-entry ?\246 "." a-mode-syntax-table) ; greater than or equal
  (modify-syntax-entry ?\250 "." a-mode-syntax-table) ; not equal
  (modify-syntax-entry ?\251 "." a-mode-syntax-table) ; down caret
  (modify-syntax-entry ?\253 "." a-mode-syntax-table) ; times
  (modify-syntax-entry ?\255 "." a-mode-syntax-table) ; domino
  (modify-syntax-entry ?\256 "." a-mode-syntax-table) ; dotted del
  (modify-syntax-entry ?\257 "." a-mode-syntax-table) ; slash bar
  (modify-syntax-entry ?\271 "." a-mode-syntax-table) ; nor
  (modify-syntax-entry ?\273 "." a-mode-syntax-table) ; quad jot
  (modify-syntax-entry ?\274 "." a-mode-syntax-table) ; quad backslash
  (modify-syntax-entry ?\275 "." a-mode-syntax-table) ; equal underbar
  (modify-syntax-entry ?\276 "." a-mode-syntax-table) ; squad
  (modify-syntax-entry ?\301 "." a-mode-syntax-table) ; alpha
  (modify-syntax-entry ?\302 "." a-mode-syntax-table) ; base
  (modify-syntax-entry ?\303 "." a-mode-syntax-table) ; cap
  (modify-syntax-entry ?\304 "." a-mode-syntax-table) ; down stile
  (modify-syntax-entry ?\305 "." a-mode-syntax-table) ; epsilon
  (modify-syntax-entry ?\307 "." a-mode-syntax-table) ; del
  (modify-syntax-entry ?\310 "." a-mode-syntax-table) ; delta
  (modify-syntax-entry ?\311 "." a-mode-syntax-table) ; iota
  (modify-syntax-entry ?\312 "." a-mode-syntax-table) ; jot
  (modify-syntax-entry ?\314 "." a-mode-syntax-table) ; quad
  (modify-syntax-entry ?\316 "." a-mode-syntax-table) ; top
  (modify-syntax-entry ?\317 "." a-mode-syntax-table) ; circle
  (modify-syntax-entry ?\322 "." a-mode-syntax-table) ; rho
  (modify-syntax-entry ?\323 "." a-mode-syntax-table) ; up stile
  (modify-syntax-entry ?\325 "." a-mode-syntax-table) ; del stile
  (modify-syntax-entry ?\326 "." a-mode-syntax-table) ; cup
  (modify-syntax-entry ?\327 "." a-mode-syntax-table) ; omega
  (modify-syntax-entry ?\330 "." a-mode-syntax-table) ; right shoe
  (modify-syntax-entry ?\331 "." a-mode-syntax-table) ; take
  (modify-syntax-entry ?\332 "." a-mode-syntax-table) ; left shoe
  (modify-syntax-entry ?\333 "." a-mode-syntax-table) ; right
  (modify-syntax-entry ?\334 "." a-mode-syntax-table) ; backslash bar
  (modify-syntax-entry ?\335 "." a-mode-syntax-table) ; left
  (modify-syntax-entry ?\337 "." a-mode-syntax-table) ; divide
  (modify-syntax-entry ?\340 "." a-mode-syntax-table) ; I-beam
  (modify-syntax-entry ?\341 "." a-mode-syntax-table) ; circle bar
  (modify-syntax-entry ?\342 "." a-mode-syntax-table) ; execute
  (modify-syntax-entry ?\343 "<" a-mode-syntax-table) ; lamp comment
  (modify-syntax-entry ?\345 "." a-mode-syntax-table) ; epsilon underbar
  (modify-syntax-entry ?\346 "." a-mode-syntax-table) ; del tilde
  (modify-syntax-entry ?\347 "." a-mode-syntax-table) ; del stile
  (modify-syntax-entry ?\350 "." a-mode-syntax-table) ; delta stile
  (modify-syntax-entry ?\351 "." a-mode-syntax-table) ; iota underbar
  (modify-syntax-entry ?\354 "." a-mode-syntax-table) ; quote quad
  (modify-syntax-entry ?\356 "." a-mode-syntax-table) ; format
  (modify-syntax-entry ?\364 "." a-mode-syntax-table) ; flip
  (modify-syntax-entry ?\367 "." a-mode-syntax-table) ; rotate
  (modify-syntax-entry ?\373 "." a-mode-syntax-table) ; left arrow
  (modify-syntax-entry ?\374 "." a-mode-syntax-table) ; delta underbar
  (modify-syntax-entry ?\375 "." a-mode-syntax-table) ; right arrow
  (modify-syntax-entry ?\376 "." a-mode-syntax-table) ; diamond
)

(defvar narrowed nil "Flagged of whether narrowed")
(make-variable-buffer-local 'narrowed)
(setq-default narrowed nil)

(setq global-popup-menu  
	  '("A+ menu"
		["Toggle A+ Keyboard" toggle-a-minor-mode t]
		["Start A+/Goto A+ Buffer" a t]
		["Print Region" lpr-region (mark)]
		["Comment Region" comment-region (mark)]
		["Uncomment Region" (comment-region (region-beginning) (region-end) -1) (mark)]
		["Narrow-to-Region" (progn (setq narrowed t) (narrow-to-region (region-beginning) (region-end))) (mark)]
		["Widen" (progn (setq narrowed nil) (widen)) narrowed]
		))

(defvar a-minor-popup-menu nil  "Popup menu for a-minor mode.")

(setq a-minor-popup-menu
  '("A+ Utils"
	"---"
	["Send Line to A+" a-send-line t]
	["Send Region to A+" a-send-region (mark)]
	["Send Buffer to A+" a-send-buffer t]
	["Send Function to A+" a-send-func t]
	))

(defvar a-menubar nil  "A+ menubar entry.")

(setq a-menubar
	  (nconc  
	   '("A+"
		 ["Toggle A+ Keyboard" toggle-a-minor-mode t]
		 ["Start A+/Goto A+ Buffer" a t]
		 ["Print Region" lpr-region (mark)]
		 ["Comment Region" comment-region (mark)]
		 ["Uncomment Region" (comment-region (region-beginning) (region-end) -1) (mark)]
		 ["Narrow-to-Region" (progn (setq narrowed t) (narrow-to-region (region-beginning) (region-end))) (mark)]
		 ["Widen" (progn (setq narrowed nil) (widen)) narrowed]
		 ) 
	   (cdr a-minor-popup-menu))
	  )

(add-submenu nil a-menubar)

(setq a-options-menubar
	  (nconc  
	   '("A+Options"
		 ["A+ Program..."
		  (setq a-prog
				(read-file-name "Executable for A+ Program: " "" a-prog t a-prog)) t]
		 ["Workspace Size..."
		  (progn
			(setq a-mbytes (read-number "Enter initial workspace size in megabytes: "))
			(if (lt a-mbytes 4) (setq a-mbytes 4))) t]
		 ["Size Threshold..."
		  (progn
			(setq a-mbytes-threshold (read-number "Enter incremental workspace size in megabytes: "))
			(if (lt a-mbytes 16) (setq a-mbytes 16))) t]
		 ["Log File..." (setq a-log (read-file-name "Log File for A+ Session: " "" a-log nil a-log)) t]
		 ["Temp dir..." (setq a-tmp-dir (read-string "Temp dir for A+ xemacs operations: " a-tmp-dir )) t]
		 ["A+ Buffer..." (setq a-buffer-name (read-string "A+ Buffer Name: " a-buffer-name)) t]
		 ["apr Program..." 
		  (setq apr-command (read-file-name "Executable for printing A+: " "" apr-command t apr-command)) t]
		 ["apr Switches..." (setq apr-switches 
								  (list (read-string "Switches to pass to apr program: " (car apr-switches)))) t]
		 ["Font..." (setq a+-font (read-string "Font for A+: " "kaplscreen-bold")) t]
		 "----"
		 ["Host..." (setq a-host (read-string "Host on which to run A+: " a-host)) t]
		 ["Init String..." (setq a-init-string (read-string "Init string: " a-init-string)) t]
		 ["Load Prefix..." (setq a-load-prefix (read-string "Load prefix: " a-load-prefix)) t]
		 ["Load Comment..." (setq a-load-comment (read-string "Load comment: " a-load-comment)) t]
		 ["Load Sequence..." (setq a-load-sequence (read-string "Load sequence: " a-load-sequence)) t]
		 ["Prompt Pattern..." (setq a-prompt-pattern (read-string "Prompt pattern: " a-prompt-pattern)) t]
		 ["Quiet" (setq a-quiet (not a-quiet)) :style toggle :selected a-quiet]
		 ["Invocation..." (setq a-invocation (read-number "A+ Invocation: " a-invocation)) t]
		 "----"
		 ["A+ Options File..." 
		  (setq save-a-options-file 
				(read-file-name "Log File for A+ Session: " "" "~/.custom/a-options.el" nil save-a-options-file)) t]
		 ["Save A+ Options" save-a-options-menu-settings t]
		 )
	   )
	  )

(add-submenu nil a-options-menubar)

;;; The A+ Options menu

(defconst a-options-menu-saved-forms
  ;; This is really quite a kludge, but it gets the job done.
  (purecopy
   '(a-prog			; #### - does this WORK???
     a-mbytes
     a-mbytes-threshold
     a-log
     a-tmp-dir
     a-buffer-name
     apr-command
     apr-switches
     a+-font
     a-host
     a-init-string
     a-load-prefix
     a-load-comment
     a-load-sequence
     a-prompt-pattern
     a-quiet
     a-invocation
	 ))
  "The variables to save; or forms to evaluate to get forms to write out.")

(defvar save-a-options-file (concat "~" init-file-user "/.custom/a-options.el")
	"File to save options into.")

(defun save-a-options-menu-settings ()
  "Saves the current settings of the `A+Options' menu to your `a-options.el' file."
  (interactive)
  (let ((output-buffer (find-file-noselect
				(expand-file-name save-a-options-file)))
				output-marker)
    (save-excursion
      (set-buffer output-buffer)
      ;;
      ;; Find and delete the previously saved data, and position to write.
      ;;
      (goto-char (point-min))
      (if (re-search-forward "^;; APLUS Options Menu Settings *\n" nil 'move)
	  (let ((p (match-beginning 0)))
	    (goto-char p)
	    (or (re-search-forward
		 "^;; End of APLUS Options Menu Settings *\\(\n\\|\\'\\)"
		 nil t)
		(error "can't find END of saved state in a-options.el"))
	    (delete-region p (match-end 0)))
	(goto-char (point-max))
	(insert "\n"))
      (setq output-marker (point-marker))

      ;; run with current-buffer unchanged so that variables are evaluated in
      ;; the current context, instead of in the context of the "a-options.el" buffer.
      (let ((print-readably t)
	    (print-escape-newlines t)
	    (standard-output output-marker))
	(princ ";; APLUS Options Menu Settings\n")
	(princ ";; ============================\n")
	(princ "(cond\n")
	(princ " ((and (string-match \"XEmacs\" emacs-version)\n")
	(princ "       (boundp 'emacs-major-version)\n")
	(princ "       (>= emacs-major-version 19))\n")
	(mapcar #'(lambda (var)
		    (princ "  ")
		    (if (symbolp var)
			(prin1 (list 'setq var
				     (let ((val (symbol-value var)))
				       (if (or (memq val '(t nil))
					       (stringp val))
					   val
					 (list 'quote val)))))
		      (setq var (eval var))
		      (cond ((eq (car-safe var) 'progn)
			     (while (setq var (cdr var))
			       (prin1 (car var))
			       (princ "\n")
			       (if (cdr var) (princ "  "))
			       ))
			    (var
			     (prin1 var))))
		    (if var (princ "\n")))
		a-options-menu-saved-forms)
	(princ "  ))\n")
	(princ ";; ==================================\n")
	(princ ";; End of APLUS Options Menu Settings\n")
	))
    (set-marker output-marker nil)
    (save-excursion
      (set-buffer output-buffer)
      (save-buffer))
    ))

(defvar a-popup-menu nil  "Popup menu for a-mode.")

(setq a-popup-menu
	  (nconc
	   '("A+ Process Menu"
		 ["Recall Last Input" a-recall-last-input t]
		 ["Goto Last Input" a-goto-last-input t]
		 "---"
		 ["Interrupt A+" a-interrupt-process (get-buffer-process (current-buffer))]
		 ["Quit A+" a-quit-process (get-buffer-process (current-buffer))]
		 ["Kill A+" a-kill-process (get-buffer-process (current-buffer))]
		 "---")
	   a-minor-popup-menu))

(defun a-mode ()
  "Starts a-mode"
  (interactive)
  (setq major-mode 'a-mode)
  (setq mode-name "A")
  (auto-fill-mode -1)
  (use-local-map a-mode-map)
  (setq a-minor t)
  (set-syntax-table a-mode-syntax-table)
  (setq mode-popup-menu a-popup-menu)
  (set-a-comment-parms)
  (setq lpr-command apr-command)
  (setq lpr-switches apr-switches)
  (set-keymap-parents a-mode-map a-minor-map)
  (redraw-modeline)
  )

(defun toggle-a-minor-mode ()
  "Toggles minor mode providing enhanced APL Union Keyboard."
  (interactive)
  (make-variable-buffer-local 'meta-prefix-char)
  (setq a-minor (not a-minor))
  (if a-minor 
	  (progn
		(set-a-comment-parms)
		(setq a-alt-popup mode-popup-menu)
		(setq mode-popup-menu a-minor-popup-menu)
		(set-syntax-table a-mode-syntax-table)
		(setq old-lpr-command lpr-command)
		(setq old-lpr-switches lpr-switches)
		(setq lpr-command apr-command)
		(setq lpr-switches apr-switches)
   	    (setq meta-prefix-char -1)
        (set-face-font 'default a+-font (selected-frame))
		(auto-fill-mode -1)
		(if (current-local-map) 
			(progn
			  (setq a-alt-parents (keymap-parents (current-local-map)))
			  (set-keymap-parents (current-local-map) (cons a-minor-map a-alt-parents)))
		  (use-local-map a-minor-map))
		)
	(progn 
	  (setq lpr-command old-lpr-command)
	  (setq lpr-switches old-lpr-switches)
	  (set-face-font 'default old-default-font (selected-frame))
   	  (setq meta-prefix-char 27)
	  (if (equal a-minor-map (current-local-map))
		(use-local-map nil) 
	  (progn		 
		(set-keymap-parents (current-local-map) a-alt-parents)
		(setq a-alt-parents nil)
		)
	  )))
  (redraw-modeline)
  )

(defun a-minor-mode ()
  "Turns on a-minor-mode minor mode providing enhanced APL Union Keyboard."
  (interactive)
  (if (not a-minor) (toggle-a-minor-mode))
  (add-submenu nil a-menubar)
  (add-submenu nil a-options-menubar)
) 

(defun defarg (arg defval prompt)
"arguments are  ARG DEFVAL PROMPT: If ARG is non-null and is not equal
to \"\" then ARG is returned. Otherwise prompt (with PROMPT) for a new
value.  If this new value is equal to \"\" then return DEFVAL."
  (if (and (or (null arg) (equal arg ""))
	   (equal (setq arg (read-from-minibuffer
			     (concat prompt "[" defval "]: "))) ""))
      defval arg))

(defvar a-prog "/usr/local/bin/a" "*Program to run for `a' command")
(defvar a-log "~/.emacs_a" "*Log file for `a' sessions")
(defvar a-tmp-dir "~" "Temp directory for A+ xemacs operations")
(defvar a-init-string "$echo `date` on `uname -n`\n" "*Sent to `a' on startup")
(defvar a-mbytes 4 "*Default number of megabytes")
(defvar a-mbytes-threshold 16 "*Max default size before prompting")
(defvar a-prompt-pattern "^\\** *" "*Prompt for `a'")
(defvar a-invocation 0 "Random Number of `a' invocation") (random t)
(defvar a-load-sequence 0 "Sequence number used in name of temp file")
(defvar a-load-prefix "$loadrm " "*Load prefix")
(defvar a-load-comment "\343 Loading: " "*Load comment")
(defvar a-quiet nil "*If true, shut up")
(defvar a-host nil "*Host on which to run `a'")
(defvar a-track-beginning-of-expression nil "*If non-nil, a-recall-last-input
will leave the cursor at the beginning of the last input")
(defvar a-buffer-name "*a*" "*Name of buffer in which to run the `a' process")
(defvar a-plus nil "*t if A+ style command line")
(defvar a-plus-without-s nil "*t if no s is desired")
(defvar a-plus-rest nil "*remaining arguments to A+")
(defvar a-startup-hook nil "Function to run after a-buffer initialization")
(defvar a-first-output-hook nil "Function to run after first a output")

(defun standard-a()
  (interactive)
  (setq a-prog "/usr/local/bin/z")
  (setq a-log "~/.emacs_a")
  (setq a-tmp-dir "~")
  (setq a-init-string "$echo `date` on `hostname`\n")
  (setq a-mbytes 4)
  (setq a-mbytes-threshold 16)
  (setq a-prompt-pattern "^\\** *")
  (setq a-invocation 0) (random t)
  (setq a-load-sequence 0)
  (setq a-load-prefix "$load ")
  (setq a-load-comment "\343 Loading: ")
  (setq a-quiet nil)
  (setq a-host nil)
  (message "standard `a' defaults set"))

(defun setup-a()
  (interactive)
  (setq a-prog (defarg nil a-prog "a-prog"))
  (setq a-mbytes
	(string-to-int (defarg nil (int-to-string a-mbytes) "a-mbytes")))
  (setq a-init-string (defarg nil a-init-string "a-init-string"))
  (setq a-load-prefix (defarg nil a-load-prefix "a-load-prefix"))
  (setq a-load-comment (defarg nil a-load-comment "a-load-comment")))

(defvar a-no-output-yet nil)

(defun a (&optional mbytes loadfile newbuf)
  "Interactively runs an `a' session."
  (interactive)
  (let* ((process-connection-type nil)
		 (mbytes (or mbytes a-mbytes))
		 (loadfile (or loadfile (getenv "ALOADFILE")
					   (if a-plus a-plus-rest)))
		 aproc astat)
    (setq newbuf (or newbuf (null (get-buffer a-buffer-name))))
    (if (and (interactive-p) a-host)
		(message (concat "Running `a' on " a-host "...")))
    (switch-to-buffer a-buffer-name)
    (make-variable-buffer-local 'meta-prefix-char)
	(setq meta-prefix-char -1)
    (make-variable-buffer-local 'next-line-add-newlines)
	(setq next-line-add-newlines t)
    (set-face-font 'default a+-font (selected-frame))
    (setq aproc (get-buffer-process (current-buffer)))
    (setq astat (if aproc (process-status aproc)))
    (if (or (null astat) (not (memq astat '(run stop))))
		(progn
		  (setq mbytes 
				(if (<= mbytes a-mbytes-threshold)
					mbytes
				  (string-to-int
				   (defarg
					 (and mbytes (int-to-string mbytes))
					 (int-to-string mbytes)
					 "megabytes"))))
		  (setq modeline-process '(": %s"))
		  (a-mode)
		  (make-local-variable 'last-input-start)
		  (setq last-input-start (point-max-marker))
		  (make-local-variable 'last-input-end)
		  (setq last-input-end (point-max-marker))
		  (cond ((and a-log (= (point-min) (point-max)))
				 (and (file-exists-p a-log) (insert-file-contents a-log))
				 (setq buffer-file-name a-log)))
		  (insert "Starting...\n")
		  (setq a-invocation (random))
		  (setq a-no-output-yet t)
		  (setq aproc
				(apply 'start-process "`a'"
					   (current-buffer)
					   (append
						(if (and a-host (not (equal a-host (system-name))))
							(append (list "rsh" a-host)
									(define-DISPLAY)
									(var-defs
									 "PRINTER" "PLOTTER" "PATH"
									 "XAPPLRESDIR" "XENVIRONMENT"
									 "XFILESEARCHPATH" "XUSERFILESEARCHPATH")))
						(list a-prog)
						(if (and a-plus a-plus-without-s) (list "-s"))
						(if a-plus (list "-w"))
						(list (int-to-string mbytes))
						(if (listp loadfile) loadfile (list loadfile)))))
		  (set-marker (process-mark aproc) (point-max))
		  (set-process-filter aproc 'a-proc-filter)
		  (goto-char (point-max))
		  (if newbuf (run-hooks 'a-startup-hook))))))

(defun define-DISPLAY ()
  (let ((val (getenv "DISPLAY")))
    (if val
	(set-shell-variable "DISPLAY"
		 (if (or (equal "unix:" (substring (concat val "     ") 0 5))
			 (equal ":" (substring (concat val " ") 0 1)))
			  (concat (system-name)
				  (substring val (string-match ":" val) nil))
			val)))))

(defun set-shell-variable (var value)
  (let ((shell (getenv "SHELL")))
    (if (equal "/bin/csh" shell)
	(list (concat "setenv " var " " value " ; "))
      (list (concat var "=" value)))))

(defun var-defs (&rest arg)
  (let (result)
    (while arg
      (let* ((var (car arg))
	     (val (getenv var)))
	(if val
	    (setq result (append result (set-shell-variable var val)))))
      (setq arg (cdr arg)))
    result))
      
(defun a-proc-filter (aproc output)
  "If point is at end of buffer, place output in buffer
and move point to new end.  Otherwise, just place output in buffer."
  (let ((initial a-no-output-yet))
    (if a-no-output-yet (progn (setq a-no-output-yet nil)
			       (process-send-string aproc a-init-string)))
    (let* ((cur-win (selected-window))
	   (aproc-buf (process-buffer aproc))
	   (aproc-wins (if (equal aproc-buf (current-buffer))
			   (list (cons cur-win (point-marker)))))
	   aproc-buf-point)
      (while (not (equal cur-win
			 (select-window (next-window (selected-window)))))
	(if (equal aproc-buf (current-buffer))
	    (setq aproc-wins
		  (append aproc-wins
			  (list (cons (selected-window) (point-marker)))))))
      (save-excursion
	(set-buffer aproc-buf)
	(setq aproc-buf-point (point-marker))
	(goto-char (process-mark aproc))
	(insert-before-markers output)
	(set-marker (process-mark aproc) (point))
	(if initial (run-hooks 'a-first-output-hook))
	(goto-char aproc-buf-point))
      (mapcar
       '(lambda (glom) (set-window-point (car glom) (cdr glom)))
       aproc-wins))))

(defun a-other-window (&optional mbytes loadfile)
  "Starts `a' interpreter in other window"
  (interactive)
  (let ((newbuf (null (get-buffer a-buffer-name))))
    (pop-to-buffer a-buffer-name)
    (a mbytes loadfile newbuf)
    (add-submenu nil a-menubar)
    (add-submenu nil a-options-menubar)
  )
)

(defun a-adjacent-window (&optional mbytes loadfile)
  "Starts `a' interpreter in other window but stays in current window"
  (interactive)
  (let ((cbuf (current-buffer)))
    (a-other-window mbytes loadfile)
    (pop-to-buffer cbuf)))

(defun a-interrupt-process()(interactive)(interrupt-process))
(defun a-quit-process()(interactive)(quit-process))
(defun a-kill-process ()
  (interactive)
  (if a-host
      (quit-process)
    (kill-process)))

(defun a-goto-last-input()(interactive)(goto-char last-input-start))
(defun a-recall-last-input ()
  (interactive)
  (let ((here (point)))
    (insert (buffer-substring last-input-start last-input-end))
    (if a-track-beginning-of-expression (goto-char here))))

(defvar last-input-start nil "Marker for start of last unit of input.")
(defvar last-input-end nil "Marker for end of last unit of input.")

(defun a-show-output () 
  "Display start of this batch of output at top of window. 
Also put cursor there." 
  (interactive) 
  (goto-char last-input-end) 
  (forward-char) 
  (recenter 0)) 

(defun a-kill-output () 
  "Kill all output since last input." 
  (interactive) 
  (kill-region last-input-end (point-max)) 
  (goto-char (point-max)) 
  (insert "ããã output flushed ããã\n")) 

(defun a-send-input ()
  "Send input to `a' process.
At end of buffer, sends all text after last output
as input to the process, including a newline inserted at the end.
When not at end, copies current line to the end of the buffer,
after first attempting to discard any prompt at the beginning of the line
by matching the regexp that is the value of a-prompt-pattern if possible.
This regexp should start with \"^\"."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
		 (junk (or proc (error "Current buffer has no process")))
		 (cur (point))
		 (beg (progn 
				(beginning-of-line)
				(re-search-forward a-prompt-pattern nil t)
				(point)))
		 (end (progn (end-of-line) (point)))
		 (max (point-max)))
	(if (eobp)
		(progn
		  (move-marker last-input-start (process-mark proc))
		  (move-marker last-input-end (point))
		  (insert ?\n)
		  (process-send-region proc beg (point))
		  (set-marker (process-mark proc) (point)))
	  (goto-char (point-max))
	  (insert (buffer-substring beg end))
	  (goto-char (+ max
					(if (> cur beg)
						(- cur beg)
					  0))))))

(defun a-send-line()
  (interactive)
  (let* ((cur-win (selected-window))
	 (cur-buf (current-buffer))
	 (aproc-buf (get-buffer a-buffer-name))
	 (aproc (get-buffer-process aproc-buf))
	 (aproc-wins (if (equal aproc-buf (current-buffer))
			 (list (cons cur-win (point-marker)))))
	 s)
    (while (not (equal cur-win
		       (select-window (next-window (selected-window)))))
      (if (equal aproc-buf (current-buffer))
	  (setq aproc-wins
		(append aproc-wins
			(list (cons (selected-window) (point-marker)))))))
    (save-excursion
      (setq s (concat (buffer-substring
		       (progn
			 (beginning-of-line)
			 (if (equal aproc-buf (current-buffer))
			     (re-search-forward a-prompt-pattern nil t))
			 (point))
		       (progn (end-of-line) (point)))
		      "\n"))
      (a-other-window)
      (goto-char (point-max))
      (insert-before-markers s)
      (process-send-string aproc s)
      (pop-to-buffer cur-buf))
    (mapcar
     '(lambda (glom) (set-window-point (car glom) (cdr glom)))
     aproc-wins)
    (next-line 1)))

(defun a-send-substr(start end describe)
  (let* ((cbuf (current-buffer))
	 (fname (expand-file-name
		 (format "%s/#emacs_a%d.%d" a-tmp-dir a-invocation
			 (setq a-load-sequence (1+ a-load-sequence)))))
	 (cmd (concat a-load-prefix fname "\n"))
	 (aproc))
    (write-region start end fname nil 'nomessage)
    (a-other-window)
    (setq aproc (get-buffer-process (current-buffer)))
    (goto-char (point-max))
    (or a-quiet (insert (concat a-load-comment describe "\n")))
    (set-marker (process-mark aproc) (point))
    (process-send-string aproc cmd)
    (pop-to-buffer cbuf)))

;;(defun a-send-func ()
;;  (interactive)
;;  (let ((pos (point)))
;;    (end-of-line)
;;    (let ((depth (car (parse-partial-sexp (point-min) (point))))
;;	  (parse-sexp-ignore-comments t)
;;	  (orig (point))
;;	  after-colon
;;	  start)
;;      (if (if (> depth 0)
;;	      (condition-case err
;;		  (up-list depth)
;;		(error (progn
;;			 (goto-char orig)
;;			 (setq parse-sexp-ignore-comments nil)
;;			 (up-list (- depth))
;;			 (push-mark pos)
;;			 'error))))
;;	  (error "Unbalanced parentheses start here (C-x C-x to go back)")
;;	(end-of-line)
;;	(re-search-backward "^[^ã\'\"]+:" nil 'move)
;;	(setq after-colon (match-end 0))
;;	(backward-list (car (parse-partial-sexp (point-min) (point))))
;;	(beginning-of-line)
;;	(setq start (point))
;;	(goto-char after-colon)
;;	(if (eolp) (forward-char))
;;	(end-of-line)
;;	(up-list (car (parse-partial-sexp after-colon (point))))
;;	(forward-line)
;;	(a-send-substr
;;	 start
;;	 (point)
;;	 (buffer-substring start (1- after-colon)))
;;	(goto-char pos)))))

;;;; a-send-func from ik
(defun a-send-func ()
  (interactive)
  (let ((pos (point)))
    (end-of-line)
    (let* ((expr (parse-partial-sexp (point-min) (point)))
	   (depth (car expr))
	   (instring (nth 3 expr))
	   (parse-sexp-ignore-comments t)
	   (orig (point))
	   after-colon
	   start)
      (if instring (goto-char instring))
      (if (if (> depth 0)
	      (condition-case err
		  (up-list depth)
		(error (progn
			 (goto-char orig)
			 (setq parse-sexp-ignore-comments t)
			 (up-list (- depth))
			 (push-mark pos)
			 'error))))
	  (error "Unbalanced parentheses start here (C-x C-x to go back)")
	(end-of-line)
	(re-search-backward "^[^ã\'\"]+:" nil 'move)
	(setq after-colon (match-end 0))
	(backward-list (car (parse-partial-sexp (point-min) (point))))
	(beginning-of-line)
	(setq start (point))
	(goto-char after-colon)
	(if (eolp) (forward-char))
	(end-of-line)
	(up-list (car (parse-partial-sexp after-colon (point))))
	(forward-line)
	(a-send-substr
	 start
	 (point)
	 (buffer-substring start (1- after-colon)))
	(goto-char pos)))))

;;;; !! CURRENTLY BROKEN  (a-send-func) !!
;;;; ====================================================================
;;;; a-send-func.............................................070397 NRL
;;;; ====================================================================
;;;; This defun will try to send A+ function def block to A+ interpreter
;;;; using Emacs Lisp parser. Inevitably, there is a difference between A+
;;;; language syntax against Emacs Lisp s-expression syntax.
;;;; For example, there are two types of strings in A+; In double quoted
;;;; strings, the '\' is an escape char. But in single quoted strings, '\'
;;;; is not. This causes problem in Emacs char syntax definition because
;;;; only one syntax class for each char.
;;;; So, there is problem for Emacs Lisp parser to deal with the following
;;;; A+ single quoted string :
;;;;            'abc''cd\'
;;;; which is an error in Emacs Lisp s-expression parse. In this case,
;;;; I print out a message "Please double check your single quoted string."
;;;; But "abc\"cd" is ok as long as we define '\' with the escape syntax
;;;; clase "\\".
;;;; ============================================================================
;;;; change the syntax class for "\\"; add the following line to syntax table
;;;; (modify-syntax-entry ?\\ "\\" a-mode-syntax-table)
;;;; ============================================================================
;;(defun a-send-func ()
;;  (interactive)
;;  (let ((pos (point))
;;	where
;;	func-start
;;	after-colon)
;;    (debug)
;;    (end-of-line)			; in case the cursor is before ':'
;;    (setq where (point))
;;    (re-search-backward "^[^\'\"]+:" nil 'move)  ; orginal
;;    (re-search-backward "^[^ã\'\"]+:" nil 'move)
;;    (setq func-start (point))
;;    (setq after-colon (match-end 0))
;;    (let* ((s1-state (parse-partial-sexp func-start where))
;;	   (func-end (point))
;;	   (parse-sexp-ignore-comments t)
;;	   (depth (car s1-state))    ; depth of parenthesis
;;	   (q-char (nth 3 s1-state)) ;if non-nil, the starting strng quote char
;;	   (orig (point))
;;	   s2-state
;;	   )
;;      ;; check depth and move to the end of func def
;;      (if (> depth 0)		; check if cursor in unbalanced parenthesis
;;	  (progn		; try to parse more to balance parenthesis
;;	    (setq s2-state (parse-partial-sexp (point) (point-max) (- depth) nil s1-state))
;;	    (setq func-end (point))
;;	    (setq q-char (nth 3 s2-state))
;;	    (cond ((not (null q-char))
;;		   (if (char-equal q-char ?\')
;;		    (message
;;			"PLease double check your single quoted strings.")))
;;		  ((= 0 (car s2-state)) t) ;depth should be zero by now.
;;		  (t (progn	; otherwise error for unbalanced parenthesis
;;		       (goto-char (nth 1 s1-state))
;;		       (push-mark pos)
;;		       (error "Unbalanced parentheses start here (C-x C-x to goback)"))))
;;	    )) ;; end of progn ;; end of if depth > 0
;;      (a-send-substr
;;       func-start
;;       func-end ;; (point)
;;       (buffer-substring func-start (1- after-colon)))
;;      (goto-char pos))))

(defun a-send-region ()
  (interactive)
  (a-send-substr (region-beginning) (region-end) "region"))

(defun a-send-buffer ()
  (interactive)
  (a-send-substr (point-min) (point-max) (concat "buffer " (buffer-name))))

(defun change-assoc (key value alist)
  "usage: (change-assoc KEY VALUE ALIST)
If ALIST contains KEY then replace the associated value with VALUE.
Otherwise, associate KEY and VALUE in the alist"
  (let ((tuple (assoc key alist)))
    (cond (tuple (setcdr tuple value))
	  (t (nconc alist (list (cons key value)))))))

(change-assoc "\\.apl$" 'a-minor-mode auto-mode-alist)
(change-assoc "\\.a$" 'a-minor-mode auto-mode-alist)
(change-assoc "\\.\\+$" 'a-minor-mode auto-mode-alist)
(change-assoc "\\.z$" 'a-minor-mode auto-mode-alist)


(defvar a-prog-default "a+")
(defvar a-path-default "/usr/local/lib/aplus-")

;(cond ((string-equal (getenv "SYS_FIRIS") "FID")
;       (defvar a-path-default "/usr/local/a+_")
;       )
;      ((string-equal (getenv "SYS_FIRIS") "IS" )
;       (defvar a-path-default "/ms/dist/aplus/PROJ/aplus/")
;       )
;)

(defun toggle-a-prog (arg)
  (interactive "P")
  (message
   (setq a-prog (if (equal a-prog new-a-prog)
		    old-a-prog
		  new-a-prog)))
  )

(defun set-a-prog-builtin ()
  (interactive)
  (let ((arg (read-from-minibuffer
	      (concat "Change a-prog to [" a-prog-default "]: ") nil nil nil)))
    (if (equal (length arg) 0) (setq arg a-prog-default))
    (setq a-plus t)
    (message
     (concat
      "Setting a-prog to \""
      (setq a-prog
	    (cond
	     ; a+_1a 1a
	     ((string-match "^\\(a\\+_\\)?\\([0-9]+a\\)$" arg)
	      (build-prog-name a-path-default "/bin/a+" arg 2))
	     ; a+_1b1 1b1
	     ((string-match "^\\(a\\+_\\)?\\([0-9]+b[0-9]+\\)$" arg)
	      (build-prog-name a-path-default "/bin/a+" arg 2))
	     ; a+_1.01 1.01
	     ((string-match "^\\(a\\+_\\)?\\([0-9]+\\.[0-9]+\\)$" arg)
	      (build-prog-name a-path-default "/bin/a+" arg 2))
	     ; a aX a_82 aX_82 a_latest aX_latest
	     ((string-match "^a\\(X\\)?\\(_[0-9a-zA-Z]+\\)?$" arg)
	      (progn
		(setq a-plus nil)
		(concat
		 "/usr/local/bin/a"
		 (if (match-beginning 1) "X")
		 (if (match-beginning 2)
		     (substring arg (match-beginning 2) (match-end 2))))))
	     ; a+_m1.maus m1.maus
	     ((string-match "^\\(a\\+_\\)?m\\([0-9]+\\)\\.\\([a-z]+\\)$" arg)
	      (build-prog-name
	       "/u/src/usr/local/a+_"
	       (concat "/" (substring arg (match-beginning 3) (match-end 3))
		       "/" (getenv "ID_EXEC") "/bin/a+")
	       arg 2))
	     ; a+_m1 m1
	     ((string-match "^\\(a\\+_\\)?m\\([0-9]+\\)$" arg)
	      (build-prog-name "/u/src/usr/local/a+_"
			       (concat "/" (getenv "ID_EXEC") "/bin/a+")
			       arg 2))
	     ; a+_dev.maus dev.maus
	     ((string-match "^\\(a\\+_\\)?dev\\.\\([a-z]+\\)$" arg)
	      (build-prog-name "/u/aplus/"
			       (concat "/" (getenv "ID_EXEC") "/bin/a+")
			       arg 2))
	     ; a+_dev dev
	     ((string-match "^\\(a\\+_\\)?dev$" arg)
	      (concat "/u/aplus/" (getenv "ID_EXEC") "/bin/a+"))
	     ; a+ a+_1
	     ((string-match "^a\\+\\(_[0-9]+\\)?$" arg)
	      (concat "/usr/local/a+"
		      (if (match-beginning 1)
			  (substring arg (match-beginning 1) (match-end 1)))
		      "/bin/a+"))
	     ; 1
	     ((string-match "^\\([0-9]+\\)$" arg)
	      (concat a-path-default arg "/bin/a+"))
	     ; ont
	     ((string-match "^ont$" arg) "/usr/local/a+_ont/bin/a+")
	     ; default
	     ((string-match "^default$" arg) "/ms/dist/aplus/bin/a+")
	     ; x.latest
	     ((string-match "^\\([0-9]+.latest\\)$" arg)
	      (concat a-path-default arg "/bin/a+"))
	     ; x.dev
	     ((string-match "^\\([0-9]+.dev\\)$" arg)
	      (concat a-path-default arg "/bin/a+"))
	     ; anything else
	      (t arg )))
      "\""))))

(defun build-prog-name (before after arg match)
  (concat before
	  (substring arg (match-beginning match) (match-end match))
	  after))

(defvar a-mode-string "A")
(setq-default mode-line-modified '("-%*%*-"))
(setq-default mode-line-format
	      (list ""
		    'a-mode-string
		    'mode-line-modified
		    'mode-line-buffer-identification
		    "   "
		    'global-mode-string
		    "   %[("
		    'mode-name 'minor-mode-alist "%n" 'modeline-process
		    ")%]----"
		    '(-3 . "%p")
		    "-%-"))

;;;=========================================================================
;;; isearch fix for A+
(setq search-exit-option nil) ;  "Non-nil means random control characters terminate incremental search."

(define-key isearch-mode-map "\M-n" nil)
(define-key isearch-mode-map "\M-p" nil)
(define-key isearch-mode-map [(meta right)] 'isearch-ring-advance)
(define-key isearch-mode-map [(meta left)] 'isearch-ring-retreat)

(define-key minibuffer-local-isearch-map "\M-n" nil)
(define-key minibuffer-local-isearch-map "\M-p" nil)
(define-key minibuffer-local-isearch-map [(meta right)] 'isearch-ring-advance)
(define-key minibuffer-local-isearch-map [(meta left)] 'isearch-ring-retreat)

(defun a-isearch-printing-char ()
  "Any other A+ printing character => add it to the search string and search."
  (interactive)
  (setq xev (copy-event last-command-event))
  (setq key (event-to-character xev t t t))
  (setq last-command-char (aref a-key-string key))
  (isearch-process-search-char last-command-char))

(put 'a-isearch-printing-char		'isearch-command t)

;; Redefinition for A+
(defun isearch-maybe-frob-keyboard-macros ()
  ;;
  ;; If the command about to be executed is `self-insert-command' then change
  ;; the command to `isearch-printing-char' instead, meaning add the last-
  ;; typed character to the search string.
  ;;
  ;; If `this-command' is a string or a vector (that is, a keyboard macro)
  ;; and it contains only one command, which is bound to self-insert-command,
  ;; then do the same thing as for self-inserting commands: arrange for that
  ;; character to be added to the search string.  If we didn't do this, then
  ;; typing a compose sequence (a la x-compose.el) would terminate the search
  ;; and insert the character, instead of searching for that character.
  ;;
  ;; We should continue doing this, since it's pretty much the behavior one
  ;; would expect, but it will stop being so necessary once key-translation-
  ;; map exists and is used by x-compose.el and things like it, since the
  ;; translation will have been done before we see the keys.
  ;;
  (cond ((eq this-command 'self-insert-command)
	 (setq this-command 'isearch-printing-char))
		((eq this-command 'a-self-insert)
		 (setq this-command 'a-isearch-printing-char))
		((and (or (stringp this-command) (vectorp this-command))
			  (eq (key-binding this-command) 'self-insert-command))
		 (setq last-command-event (character-to-event (aref this-command 0))
			   last-command-char (and (stringp this-command) (aref this-command 0))
			   this-command 'isearch-printing-char))
	))

;;;=========================================================================
;;; gdb fix for A+

(defun gdb-patch ()
  "Redefines GDB mode keymap to remove conflicts with A+"
  (interactive)
  (define-key gdb-mode-map "\M-i" nil)
  (define-key gdb-mode-map "\M-s" nil)
  (define-key gdb-mode-map "\M-?" nil)
  (define-key gdb-mode-map "\C-c\M-i" 'gdb-stepi)
  (define-key gdb-mode-map "\C-c\M-s" 'gdb-step)
  (define-key gdb-mode-map [(meta tab)] 'comint-dynamic-list-completions)
)

(defvar gdb-mode-hook 'gdb-patch 
  "Function to run after gdb-mode initialization")


;;;=========================================================================
;;; comint fix for A+

(defun comint-patch ()
  "Redefines COMINT mode keymap to remove conflicts with A+"
  (interactive)
  (define-key comint-mode-map "\M-n" nil)
  (define-key comint-mode-map "\M-p" nil)
  (define-key comint-mode-map "\M-r" nil)
  (define-key comint-mode-map "\M-s" nil)
  (define-key comint-mode-map "\M-?" nil)
  (define-key comint-mode-map [(meta right)] 'comint-next-matching-input-from-input)
  (define-key comint-mode-map [(meta left)] 'comint-previous-matching-input-from-input)
  (define-key comint-mode-map [(meta down)] 'comint-next-matching-input)
  (define-key comint-mode-map [(meta up)] 'comint-previous-matching-input)
  (define-key comint-mode-map [(meta tab)] 'comint-dynamic-list-completions)
)

(defvar comint-mode-hook 'comint-patch "Function to run after comint-mode initialization")

(provide 'a)

;; -If you are looking at a read-only file and type C-x C-q to get to write
;; mode, xemacs will also check out and lock the file without telling you.
;;
;; -If you try to edit an rcs file that has been removed, xemacs will check
;; the file out read only without telling you.  For example, if you try to edit
;; /u/d/sts_ny_trader/code2/swfheader.a (which has an rcs log), xemacs will
;; create the file by checking it out of rcs.  This puts an out-of-date file
;; into ny_trader/code2 without your even realizing it.
;;

(setq find-file-not-found-hooks nil)
(define-key global-map [(control x) (control q)] 'toggle-read-only)


