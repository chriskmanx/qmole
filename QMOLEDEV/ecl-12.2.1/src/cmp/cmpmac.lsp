;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;; ----------------------------------------------------------------------
;;; Macros only used in the code of the compiler itself:

#-new-cmp
(in-package "COMPILER")
#-new-cmp
(import 'sys::arglist "COMPILER")
#+new-cmp
(in-package "C-DATA")

;; ----------------------------------------------------------------------
;; CACHED FUNCTIONS
;;
(defmacro defun-cached (name lambda-list test &body body)
  (let* ((cache-name (intern (concatenate 'string "*" (string name) "-CACHE*")
                             (symbol-package name)))
         (reset-name (intern (concatenate 'string (string name) "-EMPTY-CACHE")
                             (symbol-package name)))
         (hash-function (case test
                          (EQ 'SI::HASH-EQ)
                          (EQL 'SI::HASH-EQL)
                          (EQUAL 'SI::HASH-EQUAL)
                          (t (setf test 'EQUALP) 'SI::HASH-EQUALP))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (defparameter ,cache-name (make-array 1024 :element-type t :adjustable nil)))
       (defun ,reset-name ()
         (make-array 1024 :element-type t :adjustable nil))
       (defun ,name ,lambda-list
         (flet ((,name ,lambda-list ,@body))
           (let* ((hash (logand (,hash-function ,@lambda-list) 1023))
                  (elt (aref ,cache-name hash)))
             (declare (type (integer 0 1023) hash)
                      (type (array t (*)) ,cache-name))
             (if (and elt ,@(loop for arg in lambda-list
                               collect `(,test (pop (truly-the cons elt)) ,arg)))
                 (first (truly-the cons elt))
                 (let ((output (,name ,@lambda-list)))
                   (setf (aref ,cache-name hash) (list ,@lambda-list output))
                   output))))))))

(defmacro defun-equal-cached (name lambda-list &body body)
  `(defun-cached ,name ,lambda-list equal ,@body))

;;; ----------------------------------------------------------------------
;;; CONVENIENCE FUNCTIONS / MACROS
;;;

(defun-cached env-var-name (n) eql
  (format nil "env~D" n))

(defun-cached lex-env-var-name (n) eql
  (format nil "lex~D" n))

(defun same-fname-p (name1 name2) (equal name1 name2))

;;; from cmpenv.lsp
(defmacro next-cmacro () '(incf *next-cmacro*))

;;; from cmplabel.lsp
(defmacro next-label () `(cons (incf *last-label*) nil))

(defmacro next-label* () `(cons (incf *last-label*) t))

(defun next-lcl () (list 'LCL (incf *lcl*)))

(defun next-cfun (&optional (prefix "L~D~A") (lisp-name nil))
  (let ((code (incf *next-cfun*)))
    (format nil prefix code (lisp-to-c-name lisp-name))))

(defun next-temp ()
  (prog1 *temp*
         (incf *temp*)
         (setq *max-temp* (max *temp* *max-temp*))))

(defun next-lex ()
  (prog1 (cons *level* *lex*)
         (incf *lex*)
         (setq *max-lex* (max *lex* *max-lex*))))

(defun next-env () (prog1 *env*
		     (incf *env*)
		     (setq *max-env* (max *env* *max-env*))))

(defmacro with-clean-symbols (symbols &body body)
  "Rewrites the given forms replacing the given symbols with uninterned
ones, which is useful for creating hygienic macros."
  `(progn ,@(sublis (loop for s in symbols
                      collect (cons s (make-symbol (symbol-name s))))
                   body)))

(defmacro reckless (&rest body)
  `(locally (declare (optimize (safety 0)))
     ,@body))
