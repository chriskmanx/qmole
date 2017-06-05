;;;  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll
;;;
;;;    This program is free software; you can redistribute it and/or
;;;    modify it under the terms of the GNU Library General Public
;;;    License as published by the Free Software Foundation; either
;;;    version 2 of the License, or (at your option) any later version.
;;;
;;;    See file '../Copyright' for full details.

(load (merge-pathnames "tools" *load-pathname*))

(defconstant +encodings-root+ (translate-logical-pathname #p"ext:encodings;"))

(loop for entry in +all-mappings+
   for name = (first entry)
   for orig = (make-pathname :name name :type "BIN"
                             :defaults +encodings-root+)
   for copy = (ensure-directories-exist
               (make-pathname :name name :type "BIN"
                              :defaults "build:encodings;"))
   do (progn
	(unless (probe-file orig)
          (error "Missing mapping")
	  (let ((mapping (if (equalp name "JISX0208")
			     (mapcar #'rest (read-mapping name 3))
			     (read-mapping name))))
	    (dump-mapping-array mapping orig)))
	(copy-encoding-file orig copy)))

(defconstant +aliases+
  '((:us-ascii :ascii)
    (:utf8 :utf-8)
    (:ucs-2 :ucs2 :utf-16 :unicode)
    (:ucs-2le :ucs2le :utf-16le)
    (:ucs-2be :ucs2be :utf-16be)
    (:ucs-4 :ucs4 :utf-32)
    (:ucs-4be :ucs4be :utf-32be)
    (:ucs-4le :ucs4le :utf-32le)

    (:koi8-r :koi8r)
    
    (:iso-8859-1 :latin-1 :latin1 :cp819 :ibm819)
    (:iso-8859-2 :latin-2 :latin2)
    (:iso-8859-3 :latin-3 :latin3)
    (:iso-8859-4 :latin-4 :latin4)
    (:iso-8859-5 :latin-5 :latin5 :cyrillic)
    (:iso-8859-6 :latin-6 :latin6 :arabic)
    (:iso-8859-7 :latin-7 :latin7 :greek :ecma-118)
    (:iso-8859-8 :latin-8 :latin8 :hebrew)
    (:iso-8859-9 :latin-9 :latin9)
    (:iso-8859-10 :latin-10 :latin10)
    (:iso-8859-11 :latin-11 :latin11 :thai)
    (:iso-8859-15 :latin-0 :latin0)

    (:dos-cp437 :ibm437)
    (:dos-cp850 :ibm850 :cp850)
    (:dos-cp852 :ibm852)
    (:dos-cp855 :ibm855)
    (:dos-cp857 :ibm857)
    (:dos-cp860 :ibm860)
    (:dos-cp861 :ibm861)
    (:dos-cp862 :ibm862 :cp862)
    (:dos-cp863 :ibm863)
    (:dos-cp864 :ibm864)
    (:dos-cp865 :ibm865)
    (:dos-cp866 :ibm866 :cp866)
    (:dos-cp869 :ibm869)

    (:windows-cp932 :windows-932 :cp932)
    (:windows-cp936 :windows-936 :cp936)
    (:windows-cp949 :windows-949 :cp949)
    (:windows-cp950 :windows-950 :cp950)

    (:windows-cp1250 :windows-1250 :ms-ee)
    (:windows-cp1251 :windows-1251 :ms-cyrl)
    (:windows-cp1252 :windows-1252 :ms-ansi)
    (:windows-cp1253 :windows-1253 :ms-greek)
    (:windows-cp1254 :windows-1254 :ms-turk)
    (:windows-cp1255 :windows-1255 :ms-hebr)
    (:windows-cp1256 :windows-1256 :ms-arab)
    (:windows-cp1257 :windows-1257 :winbaltrim)
    (:windows-cp1258 :windows-1258)
    ))

(loop for (name . aliases) in +aliases+
   do (loop for alias in aliases
	 for filename0 = (make-pathname :name (symbol-name alias)
                                        :defaults "build:encodings;")
	 for filename = (ensure-directories-exist filename0)
	 do (with-open-file (out filename :direction :output :if-exists :supersede
				 :if-does-not-exist :create :element-type 'base-char)
	      (format t "~%;;; Creating alias ~A -> ~A, ~A" alias name filename)
	      (format out "(defparameter ext::~A (ext::make-encoding 'ext::~A))" alias name))))

(copy-encoding-file "ext:encodings;tools.lisp" "build:encodings;tools.lisp")
(copy-encoding-file (merge-pathnames "ISO-2022-JP" +encodings-root+)
                    "build:encodings;ISO-2022-JP")
(copy-encoding-file (merge-pathnames "ISO-2022-JP-1" +encodings-root+)
                    "build:encodings;ISO-2022-JP-1")
