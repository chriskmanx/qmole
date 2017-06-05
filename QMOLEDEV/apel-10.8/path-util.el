;;; path-util.el --- Emacs Lisp file detection utility

;; Copyright (C) 1996,1997,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: file detection, install, module

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'poe)

(defvar default-load-path load-path
  "*Base of `load-path'.
It is used as default value of target path to search file or
subdirectory under load-path.")

;;;###autoload
(defun add-path (path &rest options)
  "Add PATH to `load-path' if it exists under `default-load-path'
directories and it does not exist in `load-path'.

You can use following PATH styles:
	load-path relative: \"PATH/\"
			(it is searched from `default-load-path')
	home directory relative: \"~/PATH/\" \"~USER/PATH/\"
	absolute path: \"/HOO/BAR/BAZ/\"

You can specify following OPTIONS:
	'all-paths	search from `load-path'
			instead of `default-load-path'
	'append		add PATH to the last of `load-path'"
  (let ((rest (if (memq 'all-paths options)
		  load-path
		default-load-path))
	p)
    (if (and (catch 'tag
	       (while rest
		 (setq p (expand-file-name path (car rest)))
		 (if (file-directory-p p)
		     (throw 'tag p))
		 (setq rest (cdr rest))))
	     (not (or (member p load-path)
		      (if (string-match "/$" p)
			  (member (substring p 0 (1- (length p))) load-path)
			(member (file-name-as-directory p) load-path)))))
	(setq load-path
	      (if (memq 'append options)
		  (append load-path (list p))
		(cons p load-path))))))

;;;###autoload
(defun add-latest-path (pattern &optional all-paths)
  "Add latest path matched by PATTERN to `load-path'
if it exists under `default-load-path' directories
and it does not exist in `load-path'.

If optional argument ALL-PATHS is specified, it is searched from all
of load-path instead of default-load-path."
  (let ((path (get-latest-path pattern all-paths)))
    (if path
	(add-to-list 'load-path path)
      )))

;;;###autoload
(defun get-latest-path (pattern &optional all-paths)
  "Return latest directory in default-load-path
which is matched to regexp PATTERN.
If optional argument ALL-PATHS is specified,
it is searched from all of load-path instead of default-load-path."
  (catch 'tag
    (let ((paths (if all-paths
		    load-path
		  default-load-path))
	  dir)
      (while (setq dir (car paths))
	(if (and (file-exists-p dir)
		 (file-directory-p dir)
		 )
	    (let ((files (sort (directory-files dir t pattern t)
			       (function file-newer-than-file-p)))
		  file)
	      (while (setq file (car files))
		(if (file-directory-p file)
		    (throw 'tag file)
		  )
		(setq files (cdr files))
		)))
	(setq paths (cdr paths))
	))))

;;;###autoload
(defun file-installed-p (file &optional paths)
  "Return absolute-path of FILE if FILE exists in PATHS.
If PATHS is omitted, `load-path' is used."
  (if (null paths)
      (setq paths load-path)
    )
  (catch 'tag
    (let (path)
      (while paths
	(setq path (expand-file-name file (car paths)))
	(if (file-exists-p path)
	    (throw 'tag path)
	  )
	(setq paths (cdr paths))
	))))

;;;###autoload
(defvar exec-suffix-list '("")
  "*List of suffixes for executable.")

;;;###autoload
(defun exec-installed-p (file &optional paths suffixes)
  "Return absolute-path of FILE if FILE exists in PATHS.
If PATHS is omitted, `exec-path' is used.
If suffixes is omitted, `exec-suffix-list' is used."
  (or paths
      (setq paths exec-path)
      )
  (or suffixes
      (setq suffixes exec-suffix-list)
      )
  (let (files)
    (catch 'tag
      (while suffixes
	(let ((suf (car suffixes)))
	  (if (and (not (string= suf ""))
		   (string-match (concat (regexp-quote suf) "$") file))
	      (progn
		(setq files (list file))
		(throw 'tag nil)
		)
	    (setq files (cons (concat file suf) files))
	    )
	  (setq suffixes (cdr suffixes))
	  )))
    (setq files (nreverse files))
    (catch 'tag
      (while paths
	(let ((path (car paths))
	      (files files)
	      )
	  (while files
	    (setq file (expand-file-name (car files) path))
	    (if (file-executable-p file)
		(throw 'tag file)
	      )
	    (setq files (cdr files))
	    )
	  (setq paths (cdr paths))
	  )))))

;;;###autoload
(defun module-installed-p (module &optional paths)
  "Return t if module is provided or exists in PATHS.
If PATHS is omitted, `load-path' is used."
  (or (featurep module)
      (let ((file (symbol-name module)))
	(or paths
	    (setq paths load-path)
	    )
	(catch 'tag
	  (while paths
	    (let ((stem (expand-file-name file (car paths)))
		  (sufs '(".elc" ".el"))
		  )
	      (while sufs
		(let ((file (concat stem (car sufs))))
		  (if (file-exists-p file)
		      (throw 'tag file)
		    ))
		(setq sufs (cdr sufs))
		))
	    (setq paths (cdr paths))
	    )))))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'path-util) (require 'apel-ver))

;;; path-util.el ends here
