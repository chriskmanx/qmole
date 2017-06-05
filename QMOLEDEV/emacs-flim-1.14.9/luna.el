;;; luna.el --- tiny OOP system kernel

;; Copyright (C) 1999,2000,2002 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: OOP

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

;;; Code:

(eval-when-compile (require 'cl))


;;; @ class
;;;

(defmacro luna-find-class (name)
  "Return a luna-class that has NAME."
  `(get ,name 'luna-class))

;; Give NAME (symbol) the luna-class CLASS.
(defmacro luna-set-class (name class)
  `(put ,name 'luna-class ,class))

;; Return the obarray of luna-class CLASS.
(defmacro luna-class-obarray (class)
  `(aref ,class 1))

;; Return the parents of luna-class CLASS.
(defmacro luna-class-parents (class)
  `(aref ,class 2))

;; Return the number of slots of luna-class CLASS.
(defmacro luna-class-number-of-slots (class)
  `(aref ,class 3))

(defmacro luna-define-class (class &optional parents slots)
  "Define CLASS as a luna-class.
CLASS always inherits the luna-class `standard-object'.

The optional 1st arg PARENTS is a list luna-class names.  These
luna-classes are also inheritted by CLASS.

The optional 2nd arg SLOTS is a list of slots CLASS will have."
  `(luna-define-class-function ',class ',(append parents '(standard-object))
			       ',slots))


;; Define CLASS as a luna-class.  PARENTS, if non-nil, is a list of
;; luna-class names inherited by CLASS.  SLOTS, if non-nil, is a list
;; of slots belonging to CLASS.

(defun luna-define-class-function (class &optional parents slots)
  (let ((oa (make-vector 31 0))
	(rest parents)
	parent name
	(i 2)
	b j)
    (while rest
      (setq parent (pop rest)
	    b (- i 2))
      (mapatoms (lambda (sym)
		  (when (setq j (get sym 'luna-slot-index))
		    (setq name (symbol-name sym))
		    (unless (intern-soft name oa)
		      (put (intern name oa) 'luna-slot-index (+ j b))
		      (setq i (1+ i)))))
		(luna-class-obarray (luna-find-class parent))))
    (setq rest slots)
    (while rest
      (setq name (symbol-name (pop rest)))
      (unless (intern-soft name oa)
	(put (intern name oa) 'luna-slot-index i)
	(setq i (1+ i))))
    (luna-set-class class (vector 'class oa parents i))))


;; Return a member (slot or method) of CLASS that has name
;; MEMBER-NAME.

(defun luna-class-find-member (class member-name)
  (or (stringp member-name)
      (setq member-name (symbol-name member-name)))
  (intern-soft member-name (luna-class-obarray class)))


;; Return a member (slot or method) of CLASS that has name
;; MEMBER-NAME.  If CLASS doesnt' have such a member, make it in
;; CLASS.

(defsubst luna-class-find-or-make-member (class member-name)
  (or (stringp member-name)
      (setq member-name (symbol-name member-name)))
  (intern member-name (luna-class-obarray class)))


;; Return the index number of SLOT-NAME in CLASS.

(defmacro luna-class-slot-index (class slot-name)
  `(get (luna-class-find-member ,class ,slot-name) 'luna-slot-index))

(defmacro luna-define-method (name &rest definition)
  "Define NAME as a method of a luna class.

Usage of this macro follows:

  (luna-define-method NAME [METHOD-QUALIFIER] ARGLIST [DOCSTRING] BODY...)

The optional 1st argument METHOD-QUALIFIER specifies when and how the
method is called.

If it is :before, call the method before calling the parents' methods.

If it is :after, call the method after calling the parents' methods.

If it is :around, call the method only.  The parents' methods can be
executed by calling the function `luna-call-next-method' in BODY.

Otherwize, call the method only, and the parents' methods are never
executed.  In this case, METHOD-QUALIFIER is treated as ARGLIST.

ARGLIST has the form ((VAR CLASS) METHOD-ARG ...), where VAR is a
variable name that should be bound to an entity that receives the
message NAME, CLASS is a class name.  The first argument to the method
is VAR, and the remaining arguments are METHOD-ARGs.

If VAR is nil, arguments to the method are METHOD-ARGs.  This kind of
methods can't be called from generic-function (see
`luna-define-generic').

The optional 4th argument DOCSTRING is the documentation of the
method.  If it is not string, it is treated as BODY.

The optional 5th BODY is the body of the method."
  (let ((method-qualifier (pop definition))
	args specializer class self)
    (if (memq method-qualifier '(:before :after :around))
	(setq args (pop definition))
      (setq args method-qualifier
	    method-qualifier nil))
    (setq specializer (car args)
	  class (nth 1 specializer)
	  self (car specializer))
    `(let ((func (lambda ,(if self
			      (cons self (cdr args))
			    (cdr args))
		   ,@definition))
	   (sym (luna-class-find-or-make-member
		 (luna-find-class ',class) ',name))
	   (cache (get ',name 'luna-method-cache)))
       (and cache
	    (fboundp sym)
	    (mapatoms
	     (lambda (s)
	       (if (memq (symbol-function sym) (symbol-value s))
		   (unintern s cache)))
	     cache))
       (fset sym func)
       (put sym 'luna-method-qualifier ,method-qualifier))))

(put 'luna-define-method 'lisp-indent-function 'defun)

(def-edebug-spec luna-define-method
  (&define name [&optional &or ":before" ":after" ":around"]
	   ((arg symbolp)
	    [&rest arg]
	    [&optional ["&optional" arg &rest arg]]
	    &optional ["&rest" arg])
	   def-body))


;; Return a list of method functions named SERVICE registered in the
;; parents of CLASS.

(defun luna-class-find-parents-functions (class service)
  (let ((parents (luna-class-parents class))
	ret)
    (while (and parents
		(null
		 (setq ret (luna-class-find-functions
			    (luna-find-class (pop parents))
			    service)))))
    ret))

;; Return a list of method functions named SERVICE registered in CLASS
;; and the parents..

(defun luna-class-find-functions (class service)
  (let ((sym (luna-class-find-member class service)))
    (if (fboundp sym)
	(cond ((eq (get sym 'luna-method-qualifier) :before)
	       (cons (symbol-function sym)
		     (luna-class-find-parents-functions class service)))
	      ((eq (get sym 'luna-method-qualifier) :after)
	       (nconc (luna-class-find-parents-functions class service)
		      (list (symbol-function sym))))
	      ((eq (get sym 'luna-method-qualifier) :around)
	       (cons sym (luna-class-find-parents-functions class service)))
	      (t
	       (list (symbol-function sym))))
      (luna-class-find-parents-functions class service))))


;;; @ instance (entity)
;;;

(defmacro luna-class-name (entity)
  "Return class-name of the ENTITY."
  `(aref ,entity 0))

(defmacro luna-set-class-name (entity name)
  `(aset ,entity 0 ,name))

(defmacro luna-get-obarray (entity)
  `(aref ,entity 1))

(defmacro luna-set-obarray (entity obarray)
  `(aset ,entity 1 ,obarray))

(defmacro luna-slot-index (entity slot-name)
  `(luna-class-slot-index (luna-find-class (luna-class-name ,entity))
			  ,slot-name))

(defsubst luna-slot-value (entity slot)
  "Return the value of SLOT of ENTITY."
  (aref entity (luna-slot-index entity slot)))

(defsubst luna-set-slot-value (entity slot value)
  "Store VALUE into SLOT of ENTITY."
  (aset entity (luna-slot-index entity slot) value))

(defmacro luna-find-functions (entity service)
  `(luna-class-find-functions (luna-find-class (luna-class-name ,entity))
			      ,service))

(defsubst luna-send (entity message &rest luna-current-method-arguments)
  "Send MESSAGE to ENTITY, and return the result.
ENTITY is an instance of a luna class, and MESSAGE is a method name of
the luna class.
LUNA-CURRENT-METHOD-ARGUMENTS is arguments of the MESSAGE."
  (let ((luna-next-methods (luna-find-functions entity message))
	luna-current-method
	luna-previous-return-value)
    (while (and luna-next-methods
		(progn
		  (setq luna-current-method (pop luna-next-methods)
			luna-previous-return-value
			(apply luna-current-method
			       luna-current-method-arguments))
		  (if (symbolp luna-current-method)
		      (not (eq (get luna-current-method
				    'luna-method-qualifier) :around))
		    t))))
    luna-previous-return-value))

(eval-when-compile
  (defvar luna-next-methods nil)
  (defvar luna-current-method-arguments nil))

(defun luna-call-next-method ()
  "Call the next method in the current method function.
A method function that has :around qualifier should call this function
to execute the parents' methods."
  (let (luna-current-method
	luna-previous-return-value)
    (while (and luna-next-methods
		(progn
		  (setq luna-current-method (pop luna-next-methods)
			luna-previous-return-value
			(apply luna-current-method
			       luna-current-method-arguments))
		  (if (symbolp luna-current-method)
		      (not (eq (get luna-current-method
				    'luna-method-qualifier) :around))
		    t))))
    luna-previous-return-value))

(defun luna-make-entity (class &rest init-args)
  "Make an entity (instance) of luna-class CLASS and return it.
INIT-ARGS is a plist of the form (:SLOT1 VAL1 :SLOT2 VAL2 ...),
where SLOTs are slots of CLASS and the VALs are initial values of
the corresponding SLOTs."
  (let* ((c (get class 'luna-class))
	 (v (make-vector (luna-class-number-of-slots c) nil)))
    (luna-set-class-name v class)
    (luna-set-obarray v (make-vector 7 0))
    (apply #'luna-send v 'initialize-instance v init-args)))


;;; @ interface (generic function)
;;;

;; Find a method of ENTITY that handles MESSAGE, and call it with
;; arguments LUNA-CURRENT-METHOD-ARGUMENTS.

(defun luna-apply-generic (entity message &rest luna-current-method-arguments)
  (let* ((class (luna-class-name entity))
	 (cache (get message 'luna-method-cache))
	 (sym (intern-soft (symbol-name class) cache))
	 luna-next-methods)
    (if sym
	(setq luna-next-methods (symbol-value sym))
      (setq luna-next-methods
	    (luna-find-functions entity message))
      (set (intern (symbol-name class) cache)
	   luna-next-methods))
    (luna-call-next-method)))


;; Convert ARGLIST (argument list spec for a method function) to the
;; actual list of arguments.

(defsubst luna-arglist-to-arguments (arglist)
  (let (dest)
    (while arglist
      (let ((arg (car arglist)))
	(or (memq arg '(&optional &rest))
	    (setq dest (cons arg dest))))
      (setq arglist (cdr arglist)))
    (nreverse dest)))


(defmacro luna-define-generic (name args &optional doc)
  "Define a function NAME that provides a generic interface to the method NAME.
ARGS is the argument list for NAME.  The first element of ARGS is an
entity.

The function handles a message sent to the entity by calling the
method with proper arguments.

The optional 3rd argument DOC is the documentation string for NAME."
  (if doc
      `(progn
	 (defun ,(intern (symbol-name name)) ,args
	   ,doc
	   (luna-apply-generic ,(car args) ',name
			       ,@(luna-arglist-to-arguments args)))
	 (put ',name 'luna-method-cache (make-vector 31 0)))
    `(progn
       (defun ,(intern (symbol-name name)) ,args
	 (luna-apply-generic ,(car args) ',name
			     ,@(luna-arglist-to-arguments args)))
       (put ',name 'luna-method-cache (make-vector 31 0)))))

(put 'luna-define-generic 'lisp-indent-function 'defun)


;;; @ accessor
;;;

(defun luna-define-internal-accessors (class-name)
  "Define internal accessors for instances of the luna class CLASS-NAME.

Internal accessors are macros to refer and set a slot value of the
instances.  For instance, if the class has SLOT, macros
CLASS-NAME-SLOT-internal and CLASS-NAME-set-SLOT-internal are defined.

CLASS-NAME-SLOT-internal accepts one argument INSTANCE, and returns
the value of SLOT.

CLASS-NAME-set-SLOT-internal accepts two arguemnt INSTANCE and VALUE,
and sets SLOT to VALUE."
  (let ((entity-class (luna-find-class class-name))
	parents parent-class)
    (mapatoms
     (lambda (slot)
       (if (luna-class-slot-index entity-class slot)
	   (catch 'derived
	     (setq parents (luna-class-parents entity-class))
	     (while parents
	       (setq parent-class (luna-find-class (car parents)))
	       (if (luna-class-slot-index parent-class slot)
		   (throw 'derived nil))
	       (setq parents (cdr parents)))
	     (eval
	      `(progn
		 (defmacro ,(intern (format "%s-%s-internal"
					    class-name slot))
		   (entity)
		   (list 'aref entity
			 ,(luna-class-slot-index entity-class
						 (intern (symbol-name slot)))))
		 (defmacro ,(intern (format "%s-set-%s-internal"
					    class-name slot))
		   (entity value)
		   (list 'aset entity
			 ,(luna-class-slot-index
			   entity-class (intern (symbol-name slot)))
			 value)))))))
     (luna-class-obarray entity-class))))


;;; @ standard object
;;;

;; Define super class of all luna classes.
(luna-define-class-function 'standard-object)

(luna-define-method initialize-instance ((entity standard-object)
					 &rest init-args)
  "Initialize slots of ENTITY by INIT-ARGS."
  (let* ((c (luna-find-class (luna-class-name entity)))
	 (oa (luna-class-obarray c))
	 s i)
    (while init-args
      (setq s (intern-soft (substring (symbol-name (pop init-args)) 1) oa)
	    i (pop init-args))
      (if s
	  (aset entity (get s 'luna-slot-index) i)))
    entity))


;;; @ end
;;;

(provide 'luna)

;; luna.el ends here
