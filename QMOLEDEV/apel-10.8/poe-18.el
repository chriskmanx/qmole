;;; poe-18.el --- poe API implementation for Emacs 18.*

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.
;; Copyright (C) 1999 Yuuichi Teranishi

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;	Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;;	Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: emulation, compatibility

;; This file is part of APEL (A Portable Emacs Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Note to APEL developers and APEL programmers:
;;
;; If old (v18) compiler is used, top-level macros are expanded at
;; *load-time*, not compile-time. Therefore,
;;
;; (1) Definitions with `*-maybe' won't be compiled.
;;
;; (2) you cannot use macros defined with `defmacro-maybe' within function
;;     definitions in the same file.
;;     (`defmacro-maybe' is evaluated at load-time, therefore byte-compiler
;;      treats such use of macros as (unknown) functions and compiles them
;;      into function calls, which will cause errors at run-time.)
;;
;; (3) `eval-when-compile' and `eval-and-compile' are evaluated at
;;     load-time if used at top-level.

;;; Code:

(require 'pym)


;;; @ Compilation.
;;;
(defun defalias (sym newdef)
  "Set SYMBOL's function definition to NEWVAL, and return NEWVAL."
  (fset sym newdef))

(defun byte-code-function-p (object)
  "Return t if OBJECT is a byte-compiled function object."
  (and (consp object) (consp (cdr object))
       (let ((rest (cdr (cdr object)))
	     elt)
	 (if (stringp (car rest))
	     (setq rest (cdr rest)))
	 (catch 'tag
	   (while rest
	     (setq elt (car rest))
	     (if (and (consp elt)
		      (eq (car elt) 'byte-code))
		 (throw 'tag t))
	     (setq rest (cdr rest)))))))

;; (symbol-plist 'cyclic-function-indirection)
(put 'cyclic-function-indirection
     'error-conditions
     '(cyclic-function-indirection error))
(put 'cyclic-function-indirection
     'error-message
     "Symbol's chain of function indirections contains a loop")

;; The following function definition is a direct translation of its
;; C definition in emacs-20.4/src/data.c.
(defun indirect-function (object)
  "Return the function at the end of OBJECT's function chain.
If OBJECT is a symbol, follow all function indirections and return the final
function binding.
If OBJECT is not a symbol, just return it.
Signal a void-function error if the final symbol is unbound.
Signal a cyclic-function-indirection error if there is a loop in the
function chain of symbols."
  (let* ((hare object)
         (tortoise hare))
    (catch 'found
      (while t
        (or (symbolp hare) (throw 'found hare))
        (or (fboundp hare) (signal 'void-function (cons object nil)))
        (setq hare (symbol-function hare))
        (or (symbolp hare) (throw 'found hare))
        (or (fboundp hare) (signal 'void-function (cons object nil)))
        (setq hare (symbol-function hare))

        (setq tortoise (symbol-function tortoise))

        (if (eq hare tortoise)
            (signal 'cyclic-function-indirection (cons object nil)))))
    hare))

;;; Emulate all functions and macros of emacs-20.3/lisp/byte-run.el.
;;; (note: jwz's original compiler and XEmacs compiler have some more
;;;  macros; they are "nuked" by rms in FSF version.)

;; Use `*-maybe' here because new byte-compiler may be installed.
(put 'inline 'lisp-indent-hook 0)
(defmacro-maybe inline (&rest body)
  "Eval BODY forms sequentially and return value of last one.

This emulating macro does not support function inlining because old \(v18\)
compiler does not support inlining feature."
  (cons 'progn body))

(put 'defsubst 'lisp-indent-hook 'defun)
(put 'defsubst 'edebug-form-spec 'defun)
(defmacro-maybe defsubst (name arglist &rest body)
  "Define an inline function.  The syntax is just like that of `defun'.

This emulating macro does not support function inlining because old \(v18\)
compiler does not support inlining feature."
  (cons 'defun (cons name (cons arglist body))))

(defun-maybe make-obsolete (fn new)
  "Make the byte-compiler warn that FUNCTION is obsolete.
The warning will say that NEW should be used instead.
If NEW is a string, that is the `use instead' message.

This emulating function does nothing because old \(v18\) compiler does not
support this feature."
  (interactive "aMake function obsolete: \nxObsoletion replacement: ")
  fn)

(defun-maybe make-obsolete-variable (var new)
  "Make the byte-compiler warn that VARIABLE is obsolete,
and NEW should be used instead.  If NEW is a string, then that is the
`use instead' message.

This emulating function does nothing because old \(v18\) compiler does not
support this feature."
  (interactive "vMake variable obsolete: \nxObsoletion replacement: ")
  var)

(put 'dont-compile 'lisp-indent-hook 0)
(defmacro-maybe dont-compile (&rest body)
  "Like `progn', but the body always runs interpreted \(not compiled\).
If you think you need this, you're probably making a mistake somewhere."
  (list 'eval (list 'quote (if (cdr body) (cons 'progn body) (car body)))))

(put 'eval-when-compile 'lisp-indent-hook 0)
(defmacro-maybe eval-when-compile (&rest body)
  "Like progn, but evaluates the body at compile-time.

This emulating macro does not do compile-time evaluation at all because
of the limitation of old \(v18\) compiler."
  (cons 'progn body))

(put 'eval-and-compile 'lisp-indent-hook 0)
(defmacro-maybe eval-and-compile (&rest body)
  "Like progn, but evaluates the body at compile-time as well as at load-time.

This emulating macro does not do compile-time evaluation at all because
of the limitation of old \(v18\) compiler."
  (cons 'progn body))


;;; @ C primitives emulation.
;;;

(defun member (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with EQUAL.
The value is actually the tail of LIST whose car is ELT."
  (while (and list (not (equal elt (car list))))
    (setq list (cdr list)))
  list)

(defun delete (elt list)
  "Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `equal'.
If the first member of LIST is ELT, deleting it is not a side effect;
it is simply using a different list.
Therefore, write `(setq foo (delete element foo))'
to be sure of changing the value of `foo'."
  (if list
      (if (equal elt (car list))
	  (cdr list)
	(let ((rest list)
	      (rrest (cdr list)))
	  (while (and rrest (not (equal elt (car rrest))))
	    (setq rest rrest
		  rrest (cdr rrest)))
	  (setcdr rest (cdr rrest))
	  list))))

(defun default-boundp (symbol)
  "Return t if SYMBOL has a non-void default value.
This is the value that is seen in buffers that do not have their own values
for this variable."
  (condition-case error
      (progn
	(default-value symbol)
	t)
    (void-variable nil)))

;;; @@ current-time.
;;;

(defvar current-time-world-timezones
  '(("PST" .  -800)("PDT" .  -700)("MST" .  -700)
    ("MDT" .  -600)("CST" .  -600)("CDT" .  -500)
    ("EST" .  -500)("EDT" .  -400)("AST" .  -400)
    ("NST" .  -330)("UT"  .  +000)("GMT" .  +000)
    ("BST" .  +100)("MET" .  +100)("EET" .  +200)
    ("JST" .  +900)("GMT+1"  .  +100)("GMT+2"  .  +200)
    ("GMT+3"  .  +300)("GMT+4"  .  +400)("GMT+5"  .  +500)
    ("GMT+6"  .  +600)("GMT+7"  .  +700)("GMT+8"  .  +800)
    ("GMT+9"  .  +900)("GMT+10" . +1000)("GMT+11" . +1100)
    ("GMT+12" . +1200)("GMT+13" . +1300)("GMT-1"  .  -100)
    ("GMT-2"  .  -200)("GMT-3"  .  -300)("GMT-4"  .  -400)
    ("GMT-5"  .  -500)("GMT-6"  .  -600)("GMT-7"  .  -700)
    ("GMT-8"  .  -800)("GMT-9"  .  -900)("GMT-10" . -1000)
    ("GMT-11" . -1100) ("GMT-12" . -1200))
  "Time differentials of timezone from GMT in +-HHMM form.
Used in `current-time-zone' (Emacs 19 emulating function by APEL).")

(defvar current-time-local-timezone nil 
  "*Local timezone name.
Used in `current-time-zone' (Emacs 19 emulating function by APEL).")

(defun set-time-zone-rule (tz)
  "Set the local time zone using TZ, a string specifying a time zone rule.
If TZ is nil, use implementation-defined default time zone information.
If TZ is t, use Universal Time."
  (cond
   ((stringp tz)
    (setq current-time-local-timezone tz))
   (tz
    (setq current-time-local-timezone "GMT"))
   (t
    (setq current-time-local-timezone
	  (with-temp-buffer
	    ;; We use `date' command to get timezone information.
	    (call-process "date" nil (current-buffer) t)
	    (goto-char (point-min))
	    (if (looking-at 
		 "^.*\\([A-Z][A-Z][A-Z]\\([^ \n\t]*\\)\\).*$")
		(buffer-substring (match-beginning 1)
				  (match-end 1))))))))

(defun current-time-zone (&optional specified-time)
  "Return the offset and name for the local time zone.
This returns a list of the form (OFFSET NAME).
OFFSET is an integer number of seconds ahead of UTC (east of Greenwich).
    A negative value means west of Greenwich.
NAME is a string giving the name of the time zone.
Optional argument SPECIFIED-TIME is ignored in this implementation.
Some operating systems cannot provide all this information to Emacs;
in this case, `current-time-zone' returns a list containing nil for
the data it can't find."
  (let ((local-timezone (or current-time-local-timezone
			    (progn
			      (set-time-zone-rule nil)
			      current-time-local-timezone)))
	timezone abszone seconds)
    (setq timezone
	  (or (cdr (assoc (upcase local-timezone) 
			  current-time-world-timezones))
	      ;; "+900" style or nil.
	      local-timezone))
    (when timezone
      (if (stringp timezone)
	  (setq timezone (string-to-int timezone)))
      ;; Taking account of minute in timezone.
      ;; HHMM -> MM
      (setq abszone (abs timezone))
      (setq seconds (* 60 (+ (* 60 (/ abszone 100)) (% abszone 100))))
      (list (if (< timezone 0) (- seconds) seconds)
	    local-timezone))))

(or (fboundp 'si:current-time-string)
    (fset 'si:current-time-string (symbol-function 'current-time-string)))
(defun current-time-string (&optional specified-time)
  "Return the current time, as a human-readable string.
Programs can use this function to decode a time,
since the number of columns in each field is fixed.
The format is `Sun Sep 16 01:03:52 1973'.
If an argument SPECIFIED-TIME is given, it specifies a time to format
instead of the current time.  The argument should have the form:
  (HIGH . LOW)
or the form:
  (HIGH LOW . IGNORED).
Thus, you can use times obtained from `current-time'
and from `file-attributes'."
  (if (null specified-time)
      (si:current-time-string)
    (or (consp specified-time)
	(error "Wrong type argument %s" specified-time))
    (let ((high (car specified-time))
	  (low  (cdr specified-time))
	  (offset (or (car (current-time-zone)) 0))
	  (mdays '(31 28 31 30 31 30 31 31 30 31 30 31))
	  (mnames '("Jan" "Feb" "Mar" "Apr" "May" "Jun" 
		    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	  (wnames '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
	  days dd yyyy lyear mm HH MM SS)
      (if (consp low)
	  (setq low (car low)))
      (or (integerp high)
	  (error "Wrong type argument %s" high))
      (or (integerp low)
	  (error "Wrong type argument %s" low))
      (setq low (+ low offset))
      (while (> low 65535)
	(setq high (1+ high)
	      low (- low 65536)))
      (setq yyyy 1970)
      (while (or (> high 481)
		 (and (= high 481)
		      (>= low 13184)))
	(if (and (> high 0)
		 (< low 13184))
	    (setq high (1- high)
		  low  (+ 65536 low)))
	(setq high (- high 481)
	      low  (- low 13184))
	(if (and (zerop (% yyyy 4))
		 (or (not (zerop (% yyyy 100)))
		     (zerop (% yyyy 400))))
	    (progn
	      (if (and (> high 0) 
		       (< low 20864))
		  (setq high (1- high)
			low  (+ 65536 low)))
	      (setq high (- high 1)
		    low (- low 20864))))
	(setq yyyy (1+ yyyy)))
      (setq dd 1)
      (while (or (> high 1)
		 (and (= high 1)
		      (>= low 20864)))
	(if (and (> high 0)
		 (< low 20864))
	    (setq high (1- high)
		  low  (+ 65536 low)))
	(setq high (- high 1)
	      low  (- low 20864)
	      dd (1+ dd)))
      (setq days dd)
      (if (= high 1)
	  (setq low (+ 65536 low)))
      (setq mm 0)
      (setq lyear (and (zerop (% yyyy 4))
		       (or (not (zerop (% yyyy 100)))
			   (zerop (% yyyy 400)))))
      (while (> (- dd  (if (and lyear (= mm 1)) 29 (nth mm mdays))) 0)
	(setq dd (- dd (if (and lyear (= mm 1)) 29 (nth mm mdays))))
	(setq mm (1+ mm)))
      (setq HH (/ low 3600)
	    low (% low 3600)
	    MM (/ low 60)
	    SS (% low 60))
      (format "%s %s %2d %02d:%02d:%02d %4d"
	      (nth (% (+ days
			 (- (+ (* (1- yyyy) 365) (/ (1- yyyy) 400) 
			       (/ (1- yyyy) 4)) (/ (1- yyyy) 100))) 7)
		   wnames)
	      (nth mm mnames)
	      dd HH MM SS yyyy))))

(defun current-time ()
  "Return the current time, as the number of seconds since 1970-01-01 00:00:00.
The time is returned as a list of three integers.  The first has the
most significant 16 bits of the seconds, while the second has the
least significant 16 bits.  The third integer gives the microsecond
count.

The microsecond count is zero on systems that do not provide
resolution finer than a second."
  (let* ((str (current-time-string))
	 (yyyy (string-to-int (substring str 20 24)))
	 (mm (length (member (substring str 4 7)
			     '("Dec" "Nov" "Oct" "Sep" "Aug" "Jul"
			       "Jun" "May" "Apr" "Mar" "Feb" "Jan"))))
	 (dd (string-to-int (substring str 8 10)))
	 (HH (string-to-int (substring str 11 13)))
	 (MM (string-to-int (substring str 14 16)))
	 (SS (string-to-int (substring str 17 19)))
	 (offset (or (car (current-time-zone)) 0))
	 dn ct1 ct2 i1 i2
	 year uru)
    (setq ct1 0 ct2 0 i1 0 i2 0)
    (setq year (- yyyy 1970))
    (while (> year 0)
      (setq year (1- year)
	    ct1 (+ ct1 481)
	    ct2 (+ ct2 13184))
      (while (> ct2 65535)
	(setq ct1 (1+ ct1)
	      ct2 (- ct2 65536))))
    (setq year (- yyyy 1))
    (setq uru (- (+ (- (/ year 4) (/ year 100)) 
		    (/ year 400)) 477))
    (while (> uru 0)
      (setq uru (1- uru)
	    i1 (1+ i1)
	    i2 (+ i2 20864))
      (if (> i2 65535)
	  (setq i1 (1+ i1)
		i2 (- i2 65536))))
    (setq ct1 (+ ct1 i1)
	  ct2 (+ ct2 i2))
    (while (> ct2 65535)
      (setq ct1 (1+ ct1)
	    ct2 (- ct2 65536)))
    (setq dn (+ dd (* 31 (1- mm))))
    (if (> mm 2)
	(setq dn (+ (- dn (/ (+ 23 (* 4 mm)) 10))
		    (if (and (zerop (% yyyy 4))
			     (or (not (zerop (% yyyy 100)))
				 (zerop (% yyyy 400))))
			1 0))))
    (setq dn (1- dn)
	  i1 0 
	  i2 0)
    (while (> dn 0)
      (setq dn (1- dn)
	    i1 (1+ i1)
	    i2 (+ i2 20864))
      (if (> i2 65535)
	  (setq i1 (1+ i1)
		i2 (- i2 65536))))
    (setq ct1 (+ (+ (+ ct1 i1) (/ ct2 65536)) 
		 (/ (+ (* HH 3600) (* MM 60) SS)
		    65536))
	  ct2 (+ (+ i2 (% ct2 65536))
		 (% (+ (* HH 3600) (* MM 60) SS)
		    65536)))
    (while (< (- ct2 offset) 0)
      (setq ct1 (1- ct1)
	    ct2 (+ ct2 65536)))
    (setq ct2 (- ct2 offset))
    (while (> ct2 65535)
      (setq ct1 (1+ ct1)
	    ct2 (- ct2 65536)))
    (list ct1 ct2 0)))

;;; @@ Floating point numbers.
;;;

(defun abs (arg)
  "Return the absolute value of ARG."
  (if (< arg 0) (- arg) arg))

;;; @ Basic lisp subroutines.
;;;

(defmacro lambda (&rest cdr)
  "Return a lambda expression.
A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
self-quoting; the result of evaluating the lambda expression is the
expression itself.  The lambda expression may then be treated as a
function, i.e., stored as the function value of a symbol, passed to
funcall or mapcar, etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING is an optional documentation string.
 If present, it should describe how to call the function.
 But documentation strings are usually not useful in nameless functions.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of lisp expressions."
  ;; Note that this definition should not use backquotes; subr.el should not
  ;; depend on backquote.el.
  (list 'function (cons 'lambda cdr)))

(defun force-mode-line-update (&optional all)
  "Force the mode-line of the current buffer to be redisplayed.
With optional non-nil ALL, force redisplay of all mode-lines."
  (if all (save-excursion (set-buffer (other-buffer))))
  (set-buffer-modified-p (buffer-modified-p)))

(defalias 'set-match-data 'store-match-data)

(defvar save-match-data-internal)

;; We use save-match-data-internal as the local variable because
;; that works ok in practice (people should not use that variable elsewhere).
(defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (` (let ((save-match-data-internal (match-data)))
       (unwind-protect (progn (,@ body))
         (set-match-data save-match-data-internal)))))


;;; @ Basic editing commands.
;;;

;; 18.55 does not have these variables.
(defvar-maybe buffer-undo-list nil
  "List of undo entries in current buffer.
APEL provides this as dummy for a compatibility.")

(defvar-maybe auto-fill-function nil
  "Function called (if non-nil) to perform auto-fill.
APEL provides this as dummy for a compatibility.")

(defvar-maybe unread-command-event nil
  "APEL provides this as dummy for a compatibility.")
(defvar-maybe unread-command-events nil
  "List of events to be read as the command input.
APEL provides this as dummy for a compatibility.")

;; (defvar-maybe minibuffer-setup-hook nil
;;   "Normal hook run just after entry to minibuffer.")
;; (defvar-maybe minibuffer-exit-hook nil
;;   "Normal hook run just after exit from minibuffer.")

(defvar-maybe minor-mode-map-alist nil
  "Alist of keymaps to use for minor modes.
APEL provides this as dummy for a compatibility.")

(defalias 'insert-and-inherit 'insert)
(defalias 'insert-before-markers-and-inherit 'insert-before-markers)
(defalias 'number-to-string 'int-to-string)

(defun generate-new-buffer-name (name &optional ignore)
  "Return a string that is the name of no existing buffer based on NAME.
If there is no live buffer named NAME, then return NAME.
Otherwise modify name by appending `<NUMBER>', incrementing NUMBER
until an unused name is found, and then return that name.
Optional second argument IGNORE specifies a name that is okay to use
\(if it is in the sequence to be tried\)
even if a buffer with that name exists."
  (if (get-buffer name)
      (let ((n 2) new)
	(while (get-buffer (setq new (format "%s<%d>" name n)))
	  (setq n (1+ n)))
	new)
    name))

(or (fboundp 'si:mark)
    (fset 'si:mark (symbol-function 'mark)))
(defun mark (&optional force)
  (si:mark))

(defun-maybe window-minibuffer-p (&optional window)
"Return non-nil if WINDOW is a minibuffer window."
  (eq (or window (selected-window)) (minibuffer-window)))

(defun-maybe window-live-p (obj)
  "Returns t if OBJECT is a window which is currently visible."
  (and (windowp obj)
       (or (eq obj (minibuffer-window))
	   (eq obj (get-buffer-window (window-buffer obj))))))

;; Add optinal argument `hist'
(or (fboundp 'si:read-from-minibuffer)
    (progn
      (fset 'si:read-from-minibuffer (symbol-function 'read-from-minibuffer))
      (defun read-from-minibuffer (prompt &optional
					  initial-contents keymap read hist)
	
	"Read a string from the minibuffer, prompting with string PROMPT.
If optional second arg INITIAL-CONTENTS is non-nil, it is a string
  to be inserted into the minibuffer before reading input.
  If INITIAL-CONTENTS is (STRING . POSITION), the initial input
  is STRING, but point is placed at position POSITION in the minibuffer.
Third arg KEYMAP is a keymap to use whilst reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HIST is ignored in this implementation."
	(si:read-from-minibuffer prompt initial-contents keymap read))))

;; Add optional argument `frame'.
(or (fboundp 'si:get-buffer-window)
    (progn
      (fset 'si:get-buffer-window (symbol-function 'get-buffer-window))
      (defun get-buffer-window (buffer &optional frame)
	"Return a window currently displaying BUFFER, or nil if none.
Optional argument FRAME is ignored in this implementation."
	(si:get-buffer-window buffer))))

(defun-maybe walk-windows (proc &optional minibuf all-frames)
  "Cycle through all visible windows, calling PROC for each one.
PROC is called with a window as argument.

Optional second arg MINIBUF t means count the minibuffer window even
if not active.  MINIBUF nil or omitted means count the minibuffer iff
it is active.  MINIBUF neither t nor nil means not to count the
minibuffer even if it is active.
Optional third argument ALL-FRAMES is ignored in this implementation."
  (if (window-minibuffer-p (selected-window))
      (setq minibuf t))
  (let* ((walk-windows-start (selected-window))
	 (walk-windows-current walk-windows-start))
    (unwind-protect
	(while (progn
		 (setq walk-windows-current
		       (next-window walk-windows-current minibuf))
		 (funcall proc walk-windows-current)
		 (not (eq walk-windows-current walk-windows-start))))
      (select-window walk-windows-start))))

(defun buffer-disable-undo (&optional buffer)
  "Make BUFFER stop keeping undo information.
No argument or nil as argument means do this for the current buffer."
   (buffer-flush-undo (or buffer (current-buffer))))


;;; @@ Frame (Emacs 18 cannot make frame)
;;;
;; The following four are frequently used for manipulating the current frame.
;; frame.el has `screen-width', `screen-height', `set-screen-width' and
;; `set-screen-height' for backward compatibility and declare them as obsolete.
(defun frame-width (&optional frame)
  "Return number of columns available for display on FRAME.
If FRAME is omitted, describe the currently selected frame."
  (screen-width))

(defun frame-height (&optional frame)
  "Return number of lines available for display on FRAME.
If FRAME is omitted, describe the currently selected frame."
  (screen-height))

(defun set-frame-width (frame cols &optional pretend)
  "Specify that the frame FRAME has COLS columns.
Optional third arg non-nil means that redisplay should use COLS columns
but that the idea of the actual width of the frame should not be changed."
  (set-screen-width cols pretend))

(defun set-frame-height (frame lines &optional pretend)
  "Specify that the frame FRAME has LINES lines.
Optional third arg non-nil means that redisplay should use LINES lines
but that the idea of the actual height of the frame should not be changed."
  (set-screen-height lines pretend))

;;; @@ Environment variables.
;;;

(autoload 'setenv "env"
  "Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or is
`nil', the environment variable VARIABLE will be removed.
This function works by modifying `process-environment'."
  t)


;;; @ File input and output commands.
;;;

(defvar data-directory exec-directory)

;; In 18.55, `call-process' does not return exit status.
(defun file-executable-p (filename)
  "Return t if FILENAME can be executed by you.
For a directory, this means you can access files in that directory."
  (if (file-exists-p filename)
      (let ((process (start-process "test" nil "test" "-x" filename)))
	(while (eq 'run (process-status process)))
	(zerop (process-exit-status process)))))

(defun make-directory-internal (dirname)
  "Create a directory. One argument, a file name string."
  (let ((dir (expand-file-name dirname)))
    (if (file-exists-p dir)
        (signal 'file-already-exists
                (list "Creating directory: %s already exists" dir))
      (let ((exit-status (call-process "mkdir" nil nil nil dir)))
        (if (or (and (numberp exit-status)
                     (not (zerop exit-status)))
                (stringp exit-status))
            (error "Create directory %s failed.")
          ;; `make-directory' of v19 and later returns nil for success.
          )))))

(defun make-directory (dir &optional parents)
  "Create the directory DIR and any nonexistent parent dirs.
The second (optional) argument PARENTS says whether
to create parent directories if they don't exist."
  (let ((len (length dir))
	(p 0) p1 path)
    (catch 'tag
      (while (and (< p len) (string-match "[^/]*/?" dir p))
	(setq p1 (match-end 0))
	(if (= p1 len)
	    (throw 'tag nil))
	(setq path (substring dir 0 p1))
	(if (not (file-directory-p path))
	    (cond ((file-exists-p path)
		   (error "Creating directory: %s is not directory" path))
		  ((null parents)
		   (error "Creating directory: %s is not exist" path))
		  (t
		   (make-directory-internal path))))
	(setq p p1)))
    (make-directory-internal dir)))

(defun delete-directory (directory)
  "Delete the directory named DIRECTORY.  Does not follow symlinks."
  (let ((exit-status (call-process "rmdir" nil nil nil directory)))
    (when (or (and (numberp exit-status) (not (zerop exit-status)))
	      (stringp exit-status))
      (error "Delete directory %s failed."))))

(defun parse-colon-path (cd-path)
  "Explode a colon-separated list of paths into a string list."
  (and cd-path
       (let (cd-prefix cd-list (cd-start 0) cd-colon)
	 (setq cd-path (concat cd-path path-separator))
	 (while (setq cd-colon (string-match path-separator cd-path cd-start))
	   (setq cd-list
		 (nconc cd-list
			(list (if (= cd-start cd-colon)
				  nil
				(substitute-in-file-name
				 (file-name-as-directory
				  (substring cd-path cd-start cd-colon)))))))
	   (setq cd-start (+ cd-colon 1)))
	 cd-list)))

(defun file-relative-name (filename &optional directory)
  "Convert FILENAME to be relative to DIRECTORY (default: default-directory)."
  (setq filename (expand-file-name filename)
	directory (file-name-as-directory (expand-file-name
					   (or directory default-directory))))
  (let ((ancestor ""))
    (while (not (string-match (concat "^" (regexp-quote directory)) filename))
      (setq directory (file-name-directory (substring directory 0 -1))
	    ancestor (concat "../" ancestor)))
    (concat ancestor (substring filename (match-end 0)))))

(or (fboundp 'si:directory-files)
    (fset 'si:directory-files (symbol-function 'directory-files)))
(defun directory-files (directory &optional full match nosort)
  "Return a list of names of files in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is dummy for compatibility."
  (si:directory-files directory full match))

(or (fboundp 'si:write-region)
    (fset 'si:write-region (symbol-function 'write-region)))
(defun write-region (start end filename &optional append visit)
  "Write current region into specified file.
When called from a program, requires three arguments:
START, END and FILENAME.  START and END are normally buffer positions
specifying the part of the buffer to write.
If START is nil, that means to use the entire buffer contents.
If START is a string, then output that string to the file
instead of any buffer contents; END is ignored.

Optional fourth argument APPEND if non-nil means
  append to existing file contents (if any).  If it is an integer,
  seek to that offset in the file before writing.
Optional fifth argument VISIT if t means
  set the last-save-file-modtime of buffer to this file's modtime
  and mark buffer not modified.
If VISIT is a string, it is a second file name;
  the output goes to FILENAME, but the buffer is marked as visiting VISIT.
  VISIT is also the file name to lock and unlock for clash detection.
If VISIT is neither t nor nil nor a string,
  that means do not display the \"Wrote file\" message."
  (cond
   ((null start)
    (si:write-region (point-min) (point-max) filename append visit))
   ((stringp start)
    (with-temp-buffer
      (insert start)
      (si:write-region (point-min) (point-max) filename append visit)))
   (t
    (si:write-region start end filename append visit))))

;;; @ Process.
;;; 
(or (fboundp 'si:accept-process-output)
    (progn
      (fset 'si:accept-process-output (symbol-function 'accept-process-output))
      (defun accept-process-output (&optional process timeout timeout-msecs)
	"Allow any pending output from subprocesses to be read by Emacs.
It is read into the process' buffers or given to their filter functions.
Non-nil arg PROCESS means do not return until some output has been received
 from PROCESS. Nil arg PROCESS means do not return until some output has
 been received from any process.
TIMEOUT and TIMEOUT-MSECS are ignored in this implementation."
	(si:accept-process-output process))))

;;; @ Text property.
;;;

;; In Emacs 20.4, these functions are defined in src/textprop.c.
(defun text-properties-at (position &optional object))
(defun get-text-property (position prop &optional object))
(defun get-char-property (position prop &optional object))
(defun next-property-change (position &optional object limit))
(defun next-single-property-change (position prop &optional object limit))
(defun previous-property-change (position &optional object limit))
(defun previous-single-property-change (position prop &optional object limit))
(defun add-text-properties (start end properties &optional object))
(defun put-text-property (start end property value &optional object))
(defun set-text-properties (start end properties &optional object))
(defun remove-text-properties (start end properties &optional object))
(defun text-property-any (start end property value &optional object))
(defun text-property-not-all (start end property value &optional object))
;; the following two functions are new in v20.
(defun next-char-property-change (position &optional object))
(defun previous-char-property-change (position &optional object))
;; the following two functions are obsolete.
;; (defun erase-text-properties (start end &optional object)
;; (defun copy-text-properties (start end src pos dest &optional prop)


;;; @ Overlay.
;;;

(defun overlayp (object))
(defun make-overlay (beg end &optional buffer front-advance rear-advance))
(defun move-overlay (overlay beg end &optional buffer))
(defun delete-overlay (overlay))
(defun overlay-start (overlay))
(defun overlay-end (overlay))
(defun overlay-buffer (overlay))
(defun overlay-properties (overlay))
(defun overlays-at (pos))
(defun overlays-in (beg end))
(defun next-overlay-change (pos))
(defun previous-overlay-change (pos))
(defun overlay-lists ())
(defun overlay-recenter (pos))
(defun overlay-get (overlay prop))
(defun overlay-put (overlay prop value))

;;; @ End.
;;;

(require 'product)
(product-provide (provide 'poe-18) (require 'apel-ver))

;;; poe-18.el ends here
