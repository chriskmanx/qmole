;;; lunit.el --- simple testing framework for luna

;; Copyright (C) 2000 Daiki Ueno.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: OOP, XP

;; This file is part of FLIM (Faithful Library about Internet Message).

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

;;; Commentary:

;; This module is inspired by "JUnit A Cook's Tour".
;; <URL:http://www.junit.org/junit/doc/cookstour/cookstour.htm>

;; (require 'lunit)
;;
;; (luna-define-class silly-test-case (lunit-test-case))
;;
;; (luna-define-method test-1 ((case silly-test-case))
;;   (lunit-assert (integerp "a")))
;;
;; (luna-define-method test-2 ((case silly-test-case))
;;   (lunit-assert (stringp "b")))
;;
;; (with-output-to-temp-buffer "*Lunit Results*"
;;   (lunit (lunit-make-test-suite-from-class 'silly-test-case)))
;; ______________________________________________________________________
;; Starting test `silly-test-case#test-1'
;; failure: (integerp "a")
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ______________________________________________________________________
;; Starting test `silly-test-case#test-2'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; 2 runs, 1 failures, 0 errors

;;; Code:

(require 'luna)

(eval-when-compile (require 'cl))

;;; @ test
;;;

(eval-and-compile
  (luna-define-class lunit-test ()
		     (name))

  (luna-define-internal-accessors 'lunit-test))

(luna-define-generic lunit-test-number-of-tests (test)
  "Count the number of test cases that will be run by the test.")

(luna-define-generic lunit-test-run (test result)
  "Run the test and collects its result in result.")

(luna-define-generic lunit-test-suite-add-test (suite test)
  "Add the test to the suite.")

;;; @ test listener
;;;

(luna-define-class lunit-test-listener)

;;; @ test result
;;;

(put 'lunit-error 'error-message "test error")
(put 'lunit-error 'error-conditions '(lunit-error error))

(put 'lunit-failure 'error-message "test failure")
(put 'lunit-failure 'error-conditions '(lunit-failure lunit-error error))

(eval-and-compile
  (luna-define-class lunit-test-result ()
		     (errors
		      failures
		      listeners))

  (luna-define-internal-accessors 'lunit-test-result))

(luna-define-generic lunit-test-result-run (result case)
  "Run the test case.")

(luna-define-generic lunit-test-result-notify (result message &rest args)
  "Report the current state of execution.")

(luna-define-generic lunit-test-result-error (result case error)
  "Add error to the list of errors.
The passed in exception caused the error.")

(luna-define-generic lunit-test-result-failure (result case failure)
  "Add failure to the list of failures.
The passed in exception caused the failure.")

(luna-define-generic lunit-test-result-add-listener (result listener)
  "Add listener to the list of listeners.")

(defun lunit-make-test-result (&rest listeners)
  "Return a newly allocated `lunit-test-result' instance with LISTENERS."
  (luna-make-entity 'lunit-test-result :listeners listeners))

(luna-define-method lunit-test-result-notify ((result lunit-test-result)
					      message args)
  (let ((listeners
	 (lunit-test-result-listeners-internal result)))
    (dolist (listener listeners)
      (apply #'luna-send listener message listener args))))

(luna-define-method lunit-test-result-run ((result lunit-test-result) case)
  (lunit-test-result-notify result 'lunit-test-listener-start case)
  (condition-case error
      (lunit-test-case-run case)
    (lunit-failure
     (lunit-test-result-failure result case (nth 1 error)))
    (lunit-error
     (lunit-test-result-error result case (cdr error))))
  (lunit-test-result-notify result 'lunit-test-listener-end case))

(luna-define-method lunit-test-result-error ((result lunit-test-result)
					     case error)
  (let ((errors
	 (lunit-test-result-errors-internal result)))
    (setq errors (nconc errors (list (cons case error))))
    (lunit-test-result-set-errors-internal result errors))
  (lunit-test-result-notify result 'lunit-test-listener-error case error))

(luna-define-method lunit-test-result-failure ((result lunit-test-result)
					       case failure)
  (let ((failures
	 (lunit-test-result-failures-internal result)))
    (setq failures (nconc failures (list (cons case failure))))
    (lunit-test-result-set-failures-internal result failures))
  (lunit-test-result-notify result 'lunit-test-listener-failure case failure))

(luna-define-method lunit-test-result-add-listener ((result lunit-test-result)
						    listener)
  (let ((listeners
	 (lunit-test-result-listeners-internal result)))
    (setq listeners (nconc listeners (list listener)))
    (lunit-test-result-set-listeners-internal result listeners)))

;;; @ test case
;;;

(luna-define-class lunit-test-case (lunit-test))

(luna-define-generic lunit-test-case-run (case)
  "Run the test case.")

(luna-define-generic lunit-test-case-setup (case)
  "Setup the test case.")

(luna-define-generic lunit-test-case-teardown (case)
  "Clear the test case.")

(defun lunit-make-test-case (class name)
  "Return a newly allocated `lunit-test-case'.
CLASS is a symbol for class derived from `lunit-test-case'.
NAME is name of the method to be tested."
  (luna-make-entity class :name name))

(luna-define-method lunit-test-number-of-tests ((case lunit-test-case))
  1)

(luna-define-method lunit-test-run ((case lunit-test-case) result)
  (lunit-test-result-run result case))

(luna-define-method lunit-test-case-setup ((case lunit-test-case)))
(luna-define-method lunit-test-case-teardown ((case lunit-test-case)))

(luna-define-method lunit-test-case-run ((case lunit-test-case))
  (lunit-test-case-setup case)
  (unwind-protect
      (let* ((name
	      (lunit-test-name-internal case))
	     (functions
	      (luna-find-functions case name)))
	(unless functions
	  (error "Method \"%S\" not found" name))
	(condition-case error
	    (funcall (car functions) case)
	  (lunit-failure
	   (signal (car error)(cdr error)))
	  (error
	   (signal 'lunit-error error))))
    (lunit-test-case-teardown case)))

;;; @ test suite
;;;

(eval-and-compile
  (luna-define-class lunit-test-suite (lunit-test)
		     (tests))

  (luna-define-internal-accessors 'lunit-test-suite))

(defun lunit-make-test-suite (&rest tests)
  "Return a newly allocated `lunit-test-suite' instance.
TESTS holds a number of instances of `lunit-test'."
  (luna-make-entity 'lunit-test-suite :tests tests))

(luna-define-method lunit-test-suite-add-test ((suite lunit-test-suite) test)
  (let ((tests (lunit-test-suite-tests-internal suite)))
    (lunit-test-suite-set-tests-internal suite (nconc tests (list test)))))

(luna-define-method lunit-test-number-of-tests ((suite lunit-test-suite))
  (let ((tests (lunit-test-suite-tests-internal suite))
	(accu 0))
    (dolist (test tests)
      (setq accu (+ accu (lunit-test-number-of-tests test))))
    accu))

(luna-define-method lunit-test-run ((suite lunit-test-suite) result)
  (let ((tests (lunit-test-suite-tests-internal suite)))
    (dolist (test tests)
      (lunit-test-run test result))))

;;; @ test runner
;;;

(defmacro lunit-assert (condition-expr)
  "Verify that CONDITION-EXPR returns non-nil; signal an error if not."
  (let ((condition (eval condition-expr)))
    `(when ,(not condition)
       (signal 'lunit-failure (list ',condition-expr)))))

(luna-define-class lunit-test-printer (lunit-test-listener))

(luna-define-method lunit-test-listener-error ((printer lunit-test-printer)
					       case error)
  (princ (format " error: %S" error)))

(luna-define-method lunit-test-listener-failure ((printer lunit-test-printer)
						 case failure)
  (princ (format " failure: %S" failure)))

(luna-define-method lunit-test-listener-start ((printer lunit-test-printer)
					       case)
  (princ (format "Running `%S#%S'..."
		 (luna-class-name case)
		 (lunit-test-name-internal case))))

(luna-define-method lunit-test-listener-end ((printer lunit-test-printer) case)
  (princ "\n"))

(defun lunit-make-test-suite-from-class (class)
  "Make a test suite from all test methods of the CLASS."
  (let (tests)
    (mapatoms
     (lambda (symbol)
       (if (and (fboundp symbol)
		(string-match "^test" (symbol-name symbol))
		(null (get symbol 'luna-method-qualifier)))
	   (push (lunit-make-test-case class symbol) tests)))
     (luna-class-obarray (luna-find-class class)))
    (apply #'lunit-make-test-suite tests)))

(defun lunit (test)
  "Run TEST and display the result."
  (let* ((printer
	  (luna-make-entity 'lunit-test-printer))
	 (result
	  (lunit-make-test-result printer)))
    (lunit-test-run test result)
    (let ((failures
	   (lunit-test-result-failures-internal result))
	  (errors
	   (lunit-test-result-errors-internal result)))
      (princ (format "%d runs, %d failures, %d errors\n"
		     (lunit-test-number-of-tests test)
		     (length failures)
		     (length errors))))))

(defvar imenu-create-index-function)
(defun lunit-create-index-function ()
  (require 'imenu)
  (save-excursion
    (unwind-protect
	(progn
	  (goto-char (point-min))
	  (setq imenu-generic-expression
		'((nil "^\\s-*(def\\(un\\|subst\\|macro\\)\\s-+\\([-A-Za-z0-9+*|:]+\\)" 2)))
	  (funcall imenu-create-index-function))
      (setq imenu-create-index-function lisp-imenu-generic-expression))))

(defun lunit-generate-template (file)
  (interactive "fGenerate lunit template for: ")
  (save-excursion
    (set-buffer (find-file-noselect file))
    (let ((index-alist 
	   (lunit-create-index-function)))
      (with-output-to-temp-buffer "*Lunit template*"
	(let* ((feature
		(file-name-sans-extension
		 (file-name-nondirectory file)))
	       (class
		(concat "test-" feature)))
	  (set-buffer standard-output)
	  (insert "\
\(require 'lunit)
\(require '" feature ")

\(luna-define-class " class " (lunit-test-case))

")
	  (dolist (index index-alist)
	    (insert "\
\(luna-define-method " class "-" (car index) " ((case " class "))
  (lunit-assert nil))

")))))))

(provide 'lunit)

;;; lunit.el ends here
