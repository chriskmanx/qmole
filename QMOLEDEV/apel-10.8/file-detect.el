;;; file-detect.el --- Path management or file detection utility

;; Copyright (C) 1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: file-detect.el,v 7.1 1997/11/08 07:40:52 morioka Exp $
;; Keywords: file detection, install, module
;; Status: obsoleted

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

;;; Commentary:

;;	This file is existed only for compatibility.  Please use
;;	path-util.el instead of this file.

;;; Code:

(require 'path-util)

(require 'product)
(product-provide (provide 'file-detect) (require 'apel-ver))

;;; file-detect.el ends here
