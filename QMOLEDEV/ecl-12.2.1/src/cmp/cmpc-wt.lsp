;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPC-WT -- Routines for writing code to C files.
;;;;

(in-package #-new-cmp "COMPILER" #+new-cmp "C-BACKEND")

(defun wt1 (form)
  (typecase form
    ((or STRING INTEGER CHARACTER)
     (princ form *compiler-output1*))
    ((or DOUBLE-FLOAT SINGLE-FLOAT)
     (format *compiler-output1* "~10,,,,,,'eG" form))
    (LONG-FLOAT
     (format *compiler-output1* "~,,,,,,'eEl" form))
    (VAR (wt-var form))
    (t (wt-loc form)))
  nil)

(defun wt-h1 (form)
  (let ((*compiler-output1* *compiler-output2*))
    (wt1 form)))

(defun wt (&rest forms)
  (mapc #'wt1 forms))

(defun wt-h (&rest forms)
  (mapc #'wt-h1 forms))

(defun wt-nl-h (&rest forms)
  (terpri *compiler-output2*)
  (mapc #'wt-h1 forms))

(defun princ-h (form)
  (princ form *compiler-output2*))

(defun wt-nl (&rest forms)
  (wt1 #\Newline)
  (wt1 #\Tab)
  (mapc #'wt1 forms))

(defun wt-nl1 (&rest forms)
  (wt1 #\Newline)
  (mapc #'wt1 forms))

;;;
;;; LABELS AND JUMPS
;;;

(defun wt-go (label)
  #-new-cmp
  (setf (cdr label) t
        label (car label))
  (wt "goto L" label ";"))

(defun wt-label (label)
  #-new-cmp
  (when (cdr label) (wt-nl1 "L" (car label) ":;"))
  #+new-cmp
  (wt-nl1 "L" label ":;"))

;;;
;;; C/C++ COMMENTS
;;;

(defun wt-filtered-comment (text stream single-line)
  (declare (string text))
  (if single-line
      (progn
	(fresh-line stream)
	(princ "/*	" stream))
      (format stream "~50T/*  "))
  (let* ((l (1- (length text))))
    (declare (fixnum l))
    (dotimes (n l)
      (let* ((c (schar text n))
             (code (char-code c)))
        (cond
          ((or (eq c #\Newline) (eq c #\Tab))
           (princ c stream))
	  ((or (< code 32) (> code 127))
           (format stream "\ux" code))
          ((and (char= c #\*) (char= (schar text (1+ n)) #\/))
           (princ #\\ stream))
          (t
           (princ c stream)))))
    (princ (schar text l) stream))
  (format stream "~70T*/")
  )

(defun do-wt-comment (message-or-format args single-line-p)
  (unless (and (symbolp message-or-format) (not (symbol-package message-or-format)))
    (wt-filtered-comment (if (stringp message-or-format)
                             (if args
                                 (apply #'format nil message-or-format args)
                                 message-or-format)
                             (princ-to-string message-or-format))
                         *compiler-output1*
                         single-line-p)))

(defun wt-comment (message &rest extra)
  (do-wt-comment message extra nil))

(defun wt-comment-nl (message &rest extra)
  (do-wt-comment message extra t))

;;;
;;; STRINGS
;;;
;;; This routine converts lisp data into C-strings. We have to take
;;; care of escaping special characteres with backslashes. We also have
;;; to split long lines using  the fact that multiple strings are joined
;;; together by the compiler.
;;;

(defvar *wt-string-size* 0)

(defun utf8-encoded-string (string)
  (let* ((output (make-array (round (* 1.2 (length string)))
			     :element-type 'base-char
			     :fill-pointer 0))
	 (stream (make-sequence-output-stream output :external-format :utf-8)))
    (write-string string stream)
    output))

(defun wt-filtered-data (string stream &key one-liner (external-format :default))
  #+unicode
  (unless (loop with max = (if (eq external-format :default) 255 127)
	     for c across string always (<= (char-code c) max))
    (setf string (utf8-encoded-string string)))
  (let ((N (length string))
	(wt-data-column 80))
    (incf *wt-string-size* (1+ N)) ; 1+ accounts for a blank space
    (format stream (if one-liner "\"" "~%\""))
    (dotimes (i N)
      (decf wt-data-column)
      (when (< wt-data-column 0)
	(format stream "\"~% \"")
	(setq wt-data-column 79))
      (let ((x (aref string i)))
	(cond
	  ((or (< (char-code x) 32)
	       (> (char-code x) 127))
	   (case x
	     ; We avoid a trailing backslash+newline because some preprocessors
	     ; remove them.
	     (#\Newline (princ "\\n" stream))
	     (#\Tab (princ "\\t" stream))
	     (t (format stream "\\~3,'0o" (char-code x)))))
	  ((char= x #\\)
	   (princ "\\\\" stream))
	  ((char= x #\")
	   (princ "\\\"" stream))
	  (t (princ x stream)))))
    (princ (if one-liner "\""  " \"") stream)
    string))

(defun c-filtered-string (string &key (external-format :utf-8))
  (with-output-to-string (aux-stream)
    (wt-filtered-data string aux-stream :one-liner t
		      :external-format external-format)))
