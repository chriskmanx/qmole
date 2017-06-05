;;; product.el --- Functions for product version information.

;; Copyright (C) 1999,2000 Free Software Foundation, Inc.

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;;	Keiichi Suzuki <keiichi@nanap.org>
;; Keywords: compatibility, User-Agent

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This module defines some utility functions for product information,
;; used for User-Agent header field.
;;
;; User-Agent header field first appeared in HTTP [RFC 1945, RFC 2616]
;; and adopted to News Article Format draft [USEFOR].
;;
;; [RFC 1945] Hypertext Transfer Protocol -- HTTP/1.0.
;;  T. Berners-Lee, R. Fielding & H. Frystyk. May 1996.
;;
;; [RFC 2616] Hypertext Transfer Protocol -- HTTP/1.1.
;;  R. Fielding, J. Gettys, J. Mogul, H. Frystyk, L. Masinter, P. Leach,
;;  T. Berners-Lee. June 1999.
;;
;; [USEFOR] News Article Format, <draft-ietf-usefor-article-02.txt>.
;;  USEFOR Working Group. March 1999.

;;; Code:

(defvar product-obarray (make-vector 13 0))

(defvar product-ignore-checkers nil)

(defun product-define (name &optional family version code-name)
  "Define a product as a set of NAME, FAMILY, VERSION, and CODE-NAME.
NAME is a string.  Optional 2nd argument FAMILY is a string of
family product name.  Optional 3rd argument VERSION is a list of
numbers.  Optional 4th argument CODE-NAME is a string."
  (and family
       (product-add-to-family family name))
  (set (intern name product-obarray)
       (vector name family version code-name nil nil nil nil)))

(defun product-name (product)
  "Return the name of PRODUCT, a string."
  (aref product 0))
(defun product-family (product)
  "Return the family name of PRODUCT, a string."
  (aref product 1))
(defun product-version (product)
  "Return the version of PRODUCT, a list of numbers."
  (aref product 2))
(defun product-code-name (product)
  "Return the code-name of PRODUCT, a string."
  (aref product 3))
(defun product-checkers (product)
  "Return the checkers of PRODUCT, a list of functions."
  (aref product 4))
(defun product-family-products (product)
  "Return the family products of PRODUCT, a list of strings."
  (aref product 5))
(defun product-features (product)
  "Return the features of PRODUCT, a list of feature."
  (aref product 6))
(defun product-version-string (product)
  "Return the version string of PRODUCT, a string."
  (aref product 7))

(defun product-set-name (product name)
  "Set name of PRODUCT to NAME."
  (aset product 0 name))
(defun product-set-family (product family)
  "Set family name of PRODUCT to FAMILY."
  (aset product 1 family))
(defun product-set-version (product version)
  "Set version of PRODUCT to VERSION."
  (aset product 2 version))
;; Some people want to translate code-name.
(defun product-set-code-name (product code-name)
  "Set code-name of PRODUCT to CODE-NAME."
  (aset product 3 code-name))
(defun product-set-checkers (product checkers)
  "Set checker functions of PRODUCT to CHECKERS."
  (aset product 4 checkers))
(defun product-set-family-products (product products)
  "Set family products of PRODUCT to PRODUCTS."
  (aset product 5 products))
(defun product-set-features (product features)
  "Set features of PRODUCT to FEATURES."
  (aset product 6 features))
(defun product-set-version-string (product version-string)
  "Set version string of PRODUCT to VERSION-STRING."
  (aset product 7 version-string))

(defun product-add-to-family (family product-name)
  "Add a product to a family.
FAMILY is a product structure which returned by `product-define'.
PRODUCT-NAME is a string of the product's name ."
  (let ((family-product (product-find-by-name family)))
    (if family-product
	(let ((dest (product-family-products family-product)))
	  (or (member product-name dest)
	      (product-set-family-products
	       family-product (cons product-name dest))))
      (error "Family product `%s' is not defined" family))))

(defun product-remove-from-family (family product-name)
  "Remove a product from a family.
FAMILY is a product string which returned by `product-define'.
PRODUCT-NAME is a string of the product's name."
  (let ((family-product (product-find-by-name family)))
    (if family-product
	(product-set-family-products
	 family-product
	 (delete product-name (product-family-products family-product)))
      (error "Family product `%s' is not defined" family))))

(defun product-add-checkers (product &rest checkers)
  "Add checker function(s) to a product.
PRODUCT is a product structure which returned by `product-define'.
The rest arguments CHECKERS should be functions.  These functions
are registered to the product's checkers list, and will be called by
 `product-run-checkers'.
If a checker is `ignore' will be ignored all checkers after this."
  (setq product (product-find product))
  (or product-ignore-checkers
      (let ((dest (product-checkers product))
	    checker)
	(while checkers
	  (setq checker (car checkers)
		checkers (cdr checkers))
	  (or (memq checker dest)
	      (setq dest (cons checker dest))))
	(product-set-checkers product dest))))

(defun product-remove-checkers (product &rest checkers)
  "Remove checker function(s) from a product.
PRODUCT is a product structure which returned by `product-define'.
The rest arguments CHECKERS should be functions.  These functions removed
from the product's checkers list."
  (setq product (product-find product))
  (let ((dest (product-checkers product)))
    (while checkers
      (setq checkers (cdr checkers)
	    dest (delq (car checkers) dest)))
    (product-set-checkers product dest)))

(defun product-add-feature (product feature)
  "Add a feature to the features list of a product.
PRODUCT is a product structure which returned by `product-define'.
FEATURE is a feature in the PRODUCT's."
  (setq product (product-find product))
  (let ((dest (product-features product)))
    (or (memq feature dest)
	(product-set-features product (cons feature dest)))))

(defun product-remove-feature (product feature)
  "Remove a feature from the features list of a product.
PRODUCT is a product structure which returned by `product-define'.
FEATURE is a feature which registered in the products list of PRODUCT."
  (setq product (product-find product))
  (product-set-features product
			(delq feature (product-features product))))

(defun product-run-checkers (product version &optional force)
  "Run checker functions of product.
PRODUCT is a product structure which returned by `product-define'.
VERSION is target version.
If optional 3rd argument FORCE is non-nil then do not ignore
all checkers."
  (let ((checkers (product-checkers product)))
    (if (or force
	    (not (memq 'ignore checkers)))
	(let ((version (or version
			   (product-version product))))
	  (while checkers
	    (funcall (car checkers) version version)
	    (setq checkers (cdr checkers)))))))

(defun product-find-by-name (name)
  "Find product by name and return a product structure.
NAME is a string of the product's name."
  (symbol-value (intern-soft name product-obarray)))

(defun product-find-by-feature (feature)
  "Get a product structure of a feature's product.
FEATURE is a symbol of the feature."
  (get feature 'product))

(defun product-find (product)
  "Find product information.
If PRODUCT is a product structure, then return PRODUCT itself.
If PRODUCT is a string, then find product by name and return a
product structure.  If PRODUCT is symbol of feature, then return
the feature's product."
  (cond
   ((and (symbolp product)
	 (featurep product))
    (product-find-by-feature product))
   ((stringp product)
    (product-find-by-name product))
   ((vectorp product)
    product)
   (t
    (error "Invalid product %s" product))))

(put 'product-provide 'lisp-indent-function 1)
(defmacro product-provide (feature-def product-def)
  "Declare a feature as a part of product.
FEATURE-DEF is a definition of the feature.
PRODUCT-DEF is a definition of the product."
  (let* ((feature feature-def)
	 (product (product-find (eval product-def)))
	 (product-name (product-name product))
	 (product-family (product-family product))
	 (product-version (product-version product))
	 (product-code-name (product-code-name product))
	 (product-version-string (product-version-string product)))
    (` (progn
	 (, product-def)
	 (put (, feature) 'product
	      (let ((product (product-find-by-name (, product-name))))
		(product-run-checkers product '(, product-version))
		(and (, product-family)
		     (product-add-to-family (, product-family)
					    (, product-name)))
		(product-add-feature product (, feature))
		(if (equal '(, product-version) (product-version product))
		    product
		  (vector (, product-name) (, product-family)
			  '(, product-version) (, product-code-name)
			  nil nil nil (, product-version-string)))))
	 (, feature-def)))))

(defun product-version-as-string (product)
  "Return version number of product as a string.
PRODUCT is a product structure which returned by `product-define'.
If optional argument UPDATE is non-nil, then regenerate
`product-version-string' from `product-version'."
  (setq product (product-find product))
  (or (product-version-string product)
      (and (product-version product)
	   (product-set-version-string product
				       (mapconcat (function int-to-string)
						  (product-version product)
						  ".")))))

(defun product-string-1 (product &optional verbose)
  "Return information of product as a string of \"NAME/VERSION\".
PRODUCT is a product structure which returned by `product-define'.
If optional argument VERBOSE is non-nil, then return string of
\"NAME/VERSION (CODE-NAME)\"."
  (setq product (product-find product))
  (concat (product-name product)
	  (let ((version-string (product-version-as-string product)))
	    (and version-string
		 (concat "/" version-string)))
	  (and verbose (product-code-name product)
	       (concat " (" (product-code-name product) ")"))))

(defun product-for-each (product all function &rest args)
  "Apply a function to a product and the product's family with args.
PRODUCT is a product structure which returned by `product-define'.
If ALL is nil, apply function to only products which provided feature.
FUNCTION is a function.  The function called with following arguments.
The 1st argument is a product structure.  The rest arguments are ARGS."
  (setq product (product-find product))
  (let ((family (product-family-products product)))
    (and (or all (product-features product))
	 (apply function product args))
    (while family
      (apply 'product-for-each (car family) all function args)
      (setq family (cdr family)))))

(defun product-string (product)
  "Return information of product as a string of \"NAME/VERSION\".
PRODUCT is a product structure which returned by `product-define'."
  (let (dest)
    (product-for-each product nil
     (function
      (lambda (product)
	(let ((str (product-string-1 product nil)))
	  (if str
	      (setq dest (if dest
			     (concat dest " " str)
			   str)))))))
    dest))

(defun product-string-verbose (product)
  "Return information of product as a string of \"NAME/VERSION (CODE-NAME)\".
PRODUCT is a product structure which returned by `product-define'."
  (let (dest)
    (product-for-each product nil
     (function
      (lambda (product)
	(let ((str (product-string-1 product t)))
	  (if str
	      (setq dest (if dest
			     (concat dest " " str)
			   str)))))))
    dest))

(defun product-version-compare (v1 v2)
  "Compare two versions.
Return an integer greater than, equal to, or less than 0,
according as the version V1 is greater than, equal to, or less
than the version V2.
Both V1 and V2 are a list of integer(s) respectively."
  (while (and v1 v2 (= (car v1) (car v2)))
    (setq v1 (cdr v1)
	  v2 (cdr v2)))
  (if v1 (if v2 (- (car v1) (car v2)) 1) (if v2 -1 0)))

(defun product-version>= (product require-version)
  "Compare product version with required version.
PRODUCT is a product structure which returned by `product-define'.
REQUIRE-VERSION is a list of integer."
  (>= (product-version-compare (product-version (product-find product))
			       require-version)
      0))

(defun product-list-products ()
  "List all products information."
  (let (dest)
    (mapatoms
     (function
      (lambda (sym)
	(setq dest (cons (symbol-value sym) dest))))
     product-obarray)
    dest))

(defun product-parse-version-string (verstr)
  "Parse version string \".*v1.v2... (CODE-NAME)\".
Return list of version, code-name, and version-string.
VERSTR is a string."
  (let (version version-string code-name)
    (and (string-match "\\(\\([0-9.]+\\)[^ ]*\\)[^(]*\\((\\(.+\\))\\)?" verstr)
	 (let ((temp (substring verstr (match-beginning 2) (match-end 2))))
	   (setq version-string (substring verstr
					   (match-beginning 1)
					   (match-end 1))
		 code-name (and (match-beginning 4)
				(substring verstr
					   (match-beginning 4)
					   (match-end 4))))
	   (while (string-match "^\\([0-9]+\\)\\.?" temp)
	     (setq version (cons (string-to-number
				  (substring temp
					     (match-beginning 1)
					     (match-end 1)))
				 version)
		   temp (substring temp (match-end 0))))))
    (list (nreverse version) code-name version-string)))


;;; @ End.
;;;

(provide 'product)			; beware of circular dependency.
(require 'apel-ver)			; these two files depend on each other.
(product-provide 'product 'apel-ver)


;;; @ Define emacs versions.
;;;

(require 'pym)

(defconst-maybe emacs-major-version
  (progn (string-match "^[0-9]+" emacs-version)
	 (string-to-int (substring emacs-version
				   (match-beginning 0)(match-end 0))))
  "Major version number of this version of Emacs.")
(defconst-maybe emacs-minor-version
  (progn (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
	 (string-to-int (substring emacs-version
				   (match-beginning 1)(match-end 1))))
  "Minor version number of this version of Emacs.")

;;(or (product-find "emacs")
;;    (progn
;;      (product-define "emacs")
;;      (cond
;;       ((featurep 'meadow)
;;	(let* ((info (product-parse-version-string (Meadow-version)))
;;	       (version (nth 0 info))
;;	       (code-name (nth 1 info))
;;	       (version-string (nth 2 info)))
;;	  (product-set-version-string
;;	   (product-define "Meadow" "emacs" version code-name)
;;	   version-string)
;;	  (product-provide 'Meadow "Meadow"))
;;	(and (featurep 'mule)
;;	     (let* ((info (product-parse-version-string mule-version))
;;		    (version (nth 0 info))
;;		    (code-name (nth 1 info))
;;		    (version-string (nth 2 info)))
;;	       (product-set-version-string
;;		(product-define "MULE" "Meadow" version code-name)
;;		version-string)
;;	       (product-provide 'mule "MULE")))
;;	(let* ((info (product-parse-version-string emacs-version))
;;	       (version (nth 0 info))
;;	       (code-name (nth 1 info))
;;	       (version-string (nth 2 info)))
;;	  (product-set-version-string
;;	   (product-define "Emacs" "Meadow" version code-name)
;;	   version-string)
;;	  (product-provide 'emacs "Emacs")))
;;       )))

;;; product.el ends here
