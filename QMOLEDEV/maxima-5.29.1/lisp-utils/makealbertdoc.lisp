(in-package :cl-user)
(load "defsystem.lisp")
(load "../src/maxima.system")
(defvar *sys* (mk:find-system "maxima"))

(load "albert/albert.asd") ;; change me
(asdf:oos 'asdf:load-op :albert)

(setf (albert:albert-setting '("albert" "presentation" "funcallable" "calledby")) t)
(setf (albert:albert-setting '("albert" "lisp2csf" "display-progress")) t)
(setf (albert:albert-setting '("albert"  "docbook" "dtd")) "/usr/share/sgml/docbook/xml-dtd-4.1.2/docbookx.dtd")

;; let's forward e.g defmvar to defvar, seems to have same semantics
(defmethod lisp2csf:analyse-object ((objtype (eql 'defmvar)) expression)
  (lisp2csf:analyse-object 'cl:defvar (cons 'cl:defvar (cdr expression))))

;; this might be wrong, but defmfun loonks just like a defun, assume that for now
(defmethod lisp2csf:analyse-object ((objtype (eql 'defmfun)) expression)
  (let ((arglist (third expression)))
    (when (symbolp arglist)
      (setf arglist (list arglist)))
    (lisp2csf:analyse-object 'cl:defun (cons 'cl:defun
					     (cons (second expression)
						   (cons arglist (cdddr expression))
						   )))
    ))

(albert:document-files (albert::get-system-files-mkdefsys *sys*))
