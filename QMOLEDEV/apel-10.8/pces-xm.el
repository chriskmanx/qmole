;;; pces-xm.el --- pces module for XEmacs-mule

;; Copyright (C) 1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: emulation, compatibility, Mule

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

;;; @ fix coding-system definition
;;;

;; It seems not bug, but I can not permit it...
(and (coding-system-property 'iso-2022-jp 'input-charset-conversion)
     (copy-coding-system 'iso-2022-7bit 'iso-2022-jp))

(and (coding-system-property 'iso-2022-jp-dos 'input-charset-conversion)
     (copy-coding-system 'iso-2022-7bit-dos 'iso-2022-jp-dos))

(or (find-coding-system 'ctext-dos)
    (make-coding-system
     'ctext 'iso2022
     "Coding-system used in X as Compound Text Encoding."
     '(charset-g0 ascii charset-g1 latin-iso8859-1
		  eol-type nil
		  mnemonic "CText")))

(or (find-coding-system 'iso-2022-jp-2-dos)
    (make-coding-system
     'iso-2022-jp-2 'iso2022
     "ISO-2022 coding system using SS2 for 96-charset in 7-bit code."
     '(charset-g0 ascii
       charset-g2 t ;; unspecified but can be used later.
       seven t
       short t
       mnemonic "ISO7/SS2"
       eol-type nil)))

(or (find-coding-system 'gb2312-dos)
    (copy-coding-system 'cn-gb-2312-dos 'gb2312-dos))
(or (find-coding-system 'gb2312-mac)
    (copy-coding-system 'cn-gb-2312-mac 'gb2312-mac))
(or (find-coding-system 'gb2312-unix)
    (copy-coding-system 'cn-gb-2312-unix 'gb2312-unix))

(or (find-coding-system 'euc-kr-dos)
    (make-coding-system
     'euc-kr 'iso2022
     "Coding-system of Korean EUC (Extended Unix Code)."
     '(charset-g0 ascii charset-g1 korean-ksc5601
		  mnemonic "ko/EUC"
		  eol-type nil)))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'pces-xm) (require 'apel-ver))

;;; pces-xm.el ends here
