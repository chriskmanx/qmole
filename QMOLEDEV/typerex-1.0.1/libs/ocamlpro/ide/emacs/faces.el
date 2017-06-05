;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                        ;
;                        TypeRex OCaml Studio                            ;
;                                                                        ;
;                           Tiphaine Turpin                              ;
;                                                                        ;
;  Copyright 2011-2012 INRIA Saclay - Ile-de-France / OCamlPro           ;
;  All rights reserved.  This file is distributed under the terms of     ;
;  the GNU Public License version 3.0.                                   ;
;                                                                        ;
;  TypeRex is distributed in the hope that it will be useful,            ;
;  but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;  GNU General Public License for more details.                          ;
;                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file defines a set of faces for use with new TypeRex syntax
;; coloring, both on dark or light background.

(defgroup typerex-new-faces nil
  "Special faces for the new TypeRex syntax coloring."
  :group 'typerex-syntax-coloring)

(defun ocp-face-name (name)
  (intern (concat "ocp-face-" (symbol-name name))))

(defun ocp-face-same (name desc)
  (progn
  (eval `(defface ,(ocp-face-name name) `(
   (t
    ,desc
    )) ,(format "Face for %s tokens" name)
       :group 'typerex-new-faces))
;  (eval `(defvar ,name `,name))
  ))

(defun ocp-face-light-dark (name light dark)
  (eval `(defface ,(ocp-face-name name) `(
   (((background light))
    ,light
   )(t
    ,dark
    )) ,(format "Face for %s tokens" name)
       :group 'typerex-new-faces)))

(defun ocp-face (f)
  (if (eq (length f) 3)
      (ocp-face-light-dark (car f) (cadr f) (cadr (cdr f)))
    (ocp-face-same (car f) (cadr f))))


;; The following defines a list of faces by means of entries of the
;; following format: (<name> <light background> [<dark background>])
(defconst ocp-faces `(

;; Keywords
  (governing
   (:weight ultra-bold :foreground "#3C323C")
   (:weight ultra-bold :foreground "gold"))
  (keyword
   (:inherit font-lock-keyword-face)
   (:weight ultra-bold :foreground "firebrick"))
  (open-include
   (:foreground "red1" :weight ultra-bold))
;   (:inherit font-lock-warning-face))

;; Symbols and punctuation
  (punctuation
   (:weight ultra-bold)
   (:foreground "gray85" :weight ultra-bold))
  (bar-case
   (:weight ultra-bold :foreground "purple3" :background "linen")
   (:weight ultra-bold :foreground "MediumOrchid3" :background "purple4"))
  (func
   (:weight ultra-bold :foreground "purple3"))
  (op-field
   (:weight ultra-bold)
   (:foreground "gray85" :weight ultra-bold))

;; Operators
  (op
   (:weight ultra-bold :foreground "sienna"))
  (op-bool
   (:weight ultra-bold :foreground "brown"))

;; Literals
  (constant
   (:inherit font-lock-constant-face))
  (string
   (:inherit font-lock-string-face))

;; ', `, ~, ?
  (tag
   (:weight ultra-bold :foreground "sienna"))

;; Idents
  (type-occ
   (:foreground "OliveDrab4"))
  (type-def
   (:weight bold :inherit ,(ocp-face-name 'type-occ)))
  (type-variable
   (:weight bold :foreground "dark violet"))
  (cstr-occ
   (:inherit font-lock-constant-face)
   (:foreground "medium sea green"))
  (cstr-def
   (:weight bold :inherit ,(ocp-face-name 'cstr-occ))
   (:weight bold :foreground "medium sea green"))
  (field-occ
   (:inherit font-lock-constant-face)
   (:foreground "medium sea green"))
  (field-def
   (:weight bold :inherit ,(ocp-face-name 'field-occ))
   (:weight bold :foreground "medium sea green"))
  (variant
   (:inherit font-lock-constant-face))
  (val-occ-fun
   (:foreground "blue1")
   (:foreground "light sky blue"))
  (val-occ
   (:inherit font-lock-variable-name-face)
   (:foreground "peru"))
  (val-def-fun
   (:weight bold :inherit ,(ocp-face-name 'val-occ-fun))
   (:weight bold :foreground "light sky blue"))
  (val-def
   (:weight bold :inherit ,(ocp-face-name 'val-occ)))
  (method-occ
   (:inherit ,(ocp-face-name 'val-occ)))
  (method-def
   (:weight bold :inherit ,(ocp-face-name 'method-occ)))
  (mod-occ
   (:foreground "dark orange"))
  (mod-def
   (:weight bold :inherit ,(ocp-face-name 'mod-occ)))
  (builtin
   (:inherit font-lock-builtin-face))

;; Comments
  (comment
   (:foreground "gray50"))
  (doc
   (:foreground "steel blue"))
  (doc-title
   (:weight bold :foreground "black" :background "light blue"))
  (doc-italic
   (:slant italic :inherit ,(ocp-face-name 'doc)))
  (doc-bold
   (:weight bold :inherit ,(ocp-face-name 'doc)))
  (doc-keyword
   (:foreground "magenta4"))
  (doc-stop
   (:inherit font-lock-warning-face))

;; Lexing error
  (error
   (:inherit typerex-font-lock-error-face))

  (highlight-occ
   (:background "green yellow")
   (:background "coral4"))
  (highlight-def
   (:background "green")
   (:background "purple4"))

))

(mapc 'ocp-face ocp-faces)
