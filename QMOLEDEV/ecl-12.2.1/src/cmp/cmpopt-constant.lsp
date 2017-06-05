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
;;;; CMPOPT-CONSTANTS  Constant expressions.
;;;;

(in-package "COMPILER")

(defun constant-expression-p (form)
  (or (constantp form)
      (and (consp form)
           (every #'constant-expression-p (rest form))
           (let ((head (car form)))
             (or (member head '(IF OR AND NULL NOT PROGN))
                 (and (get-sysprop head 'pure)
                      (inline-possible head)))))))

(defun extract-constant-value (form &optional failure)
  (if (constant-expression-p form)
      (handler-case (cmp-eval form)
        (error (c) failure))
      failure))

