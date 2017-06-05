;;; mel-b-ccl.el --- Base64 encoder/decoder using CCL.

;; Copyright (C) 1998,1999,2000 Free Software Foundation, Inc.

;; Author: Tanaka Akira <akr@m17n.org>
;; Created: 1998/9/17
;; Keywords: MIME, Base64

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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'ccl)
(require 'pccl)
(require 'mime-def)


;;; @ constants
;;;

(eval-when-compile

(defconst mel-ccl-4-table
  '(  0   1   2   3))

(defconst mel-ccl-16-table
  '(  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15))

(defconst mel-ccl-64-table
  '(  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
     16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
     32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
     48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63))

(defconst mel-ccl-256-table
  '(  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
     16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
     32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
     48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
     64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
     80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
     96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
    112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
    128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
    144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
    160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
    176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191
    192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207
    208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223
    224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
    240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255))

(defconst mel-ccl-256-to-64-table
  '(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil  62 nil nil nil  63
     52  53  54  55  56  57  58  59  60  61 nil nil nil   t nil nil
    nil   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
     15  16  17  18  19  20  21  22  23  24  25 nil nil nil nil nil
    nil  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
     41  42  43  44  45  46  47  48  49  50  51 nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))

(defconst mel-ccl-64-to-256-table
  (mapcar
   'char-int
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
abcdefghijklmnopqrstuvwxyz\
0123456789\
+/"))

)


;;; @ CCL programs
;;;

(eval-when-compile

(defun mel-ccl-decode-b-bit-ex (v)
  (logior
   (lsh (logand v (lsh 255 16)) -16)
   (logand v (lsh 255 8))
   (lsh (logand v 255) 16)))

)

(eval-when-compile

(defconst mel-ccl-decode-b-0-table
  (vconcat
   (mapcar
    (lambda (v)
      (if (integerp v)
	  (mel-ccl-decode-b-bit-ex (lsh v 18))
	(lsh 1 24)))
    mel-ccl-256-to-64-table)))

(defconst mel-ccl-decode-b-1-table
  (vconcat
   (mapcar
    (lambda (v)
      (if (integerp v)
	  (mel-ccl-decode-b-bit-ex (lsh v 12))
	(lsh 1 25)))
    mel-ccl-256-to-64-table)))

(defconst mel-ccl-decode-b-2-table
  (vconcat
   (mapcar
    (lambda (v)
      (if (integerp v)
	  (mel-ccl-decode-b-bit-ex (lsh v 6))
	(lsh 1 26)))
    mel-ccl-256-to-64-table)))

(defconst mel-ccl-decode-b-3-table
  (vconcat
   (mapcar
    (lambda (v)
      (if (integerp v)
	  (mel-ccl-decode-b-bit-ex v)
	(lsh 1 27)))
    mel-ccl-256-to-64-table)))

)

(check-broken-facility ccl-cascading-read)

(if-broken ccl-cascading-read
    (define-ccl-program mel-ccl-decode-b
      `(1
	(loop
	 (loop
	  (read-branch
	   r1
	   ,@(mapcar
	      (lambda (v)
		(cond
		 ((or (eq v nil) (eq v t)) '(repeat))
		 (t `((r0 = ,(lsh v 2)) (break)))))
	      mel-ccl-256-to-64-table)))
	 (loop
	  (read-branch
	   r1
	   ,@(mapcar
	      (lambda (v)
		(cond
		 ((or (eq v nil) (eq v t)) '(repeat))
		 ((= (lsh v -4) 0) `((write r0) (r0 = ,(lsh (logand v 15) 4)) (break)))
		 (t `((r0 |= ,(lsh v -4)) (write r0) (r0 = ,(lsh (logand v 15) 4)) (break)))))
	      mel-ccl-256-to-64-table)))
	 (loop
	  (read-branch
	   r1
	   ,@(mapcar
	      (lambda (v)
		(cond
		 ((eq v nil) '(repeat))
		 ((eq v t) '(end))
		 ((= (lsh v -2) 0) `((write r0) (r0 = ,(lsh (logand v 3) 6)) (break)))
		 (t `((r0 |= ,(lsh v -2)) (write r0) (r0 = ,(lsh (logand v 3) 6)) (break)))))
	      mel-ccl-256-to-64-table)))
	 (loop
	  (read-branch
	   r1
	   ,@(mapcar
	      (lambda (v)
		(cond
		 ((eq v nil) '(repeat))
		 ((eq v t) '(end))
		 (t `((r0 |= ,v) (write r0) (break)))))
	      mel-ccl-256-to-64-table)))
	 (repeat))))
  (define-ccl-program mel-ccl-decode-b
    `(1
      (loop
       (read r0 r1 r2 r3)
       (r4 = r0 ,mel-ccl-decode-b-0-table)
       (r5 = r1 ,mel-ccl-decode-b-1-table)
       (r4 |= r5)
       (r5 = r2 ,mel-ccl-decode-b-2-table)
       (r4 |= r5)
       (r5 = r3 ,mel-ccl-decode-b-3-table)
       (r4 |= r5)
       (if (r4 & ,(lognot (1- (lsh 1 24))))
	   ((loop
	     (if (r4 & ,(lsh 1 24))
		 ((r0 = r1) (r1 = r2) (r2 = r3) (read r3)
		  (r4 >>= 1) (r4 &= ,(logior (lsh 7 24)))
		  (r5 = r3 ,mel-ccl-decode-b-3-table)
		  (r4 |= r5)
		  (repeat))
	       (break)))
	    (loop
	     (if (r4 & ,(lsh 1 25))
		 ((r1 = r2) (r2 = r3) (read r3)
		  (r4 >>= 1) (r4 &= ,(logior (lsh 7 24)))
		  (r5 = r3 ,mel-ccl-decode-b-3-table)
		  (r4 |= r5)
		  (repeat))
	       (break)))
	    (loop
	     (if (r2 != ?=)
		 (if (r4 & ,(lsh 1 26))
		     ((r2 = r3) (read r3)
		      (r4 >>= 1) (r4 &= ,(logior (lsh 7 24)))
		      (r5 = r3 ,mel-ccl-decode-b-3-table)
		      (r4 |= r5)
		      (repeat))
		   ((r6 = 0)
		    (break)))
	       ((r6 = 1)
		(break))))
	    (loop
	     (if (r3 != ?=)
		 (if (r4 & ,(lsh 1 27))
		     ((read r3)
		      (r4 = r3 ,mel-ccl-decode-b-3-table)
		      (repeat))
		   (break))
	       ((r6 |= 2)
		(break))))
	    (r4 = r0 ,mel-ccl-decode-b-0-table)
	    (r5 = r1 ,mel-ccl-decode-b-1-table)
	    (r4 |= r5)
	    (branch
	     r6
	     ;; BBBB
	     ((r5 = r2 ,mel-ccl-decode-b-2-table)
	      (r4 |= r5)
	      (r5 = r3 ,mel-ccl-decode-b-3-table)
	      (r4 |= r5)
	      (r4 >8= 0)
	      (write r7)
	      (r4 >8= 0)
	      (write r7)
	      (write-repeat r4))
	     ;; error: BB=B 
	     ((write (r4 & 255))
	      (end))
	     ;; BBB=
	     ((r5 = r2 ,mel-ccl-decode-b-2-table)
	      (r4 |= r5)
	      (r4 >8= 0)
	      (write r7)
	      (write (r4 & 255))
	      (end)			; Excessive (end) is workaround for XEmacs 21.0.
					; Without this, "AAA=" is converted to "^@^@^@".
	      (end))
	     ;; BB==
	     ((write (r4 & 255))
	      (end))))
	 ((r4 >8= 0)
	  (write r7)
	  (r4 >8= 0)
	  (write r7)
	  (write-repeat r4))))))
  )

(eval-when-compile

;; Generated CCL program works not properly on 20.2 because CCL_EOF_BLOCK
;; is not executed.
(defun mel-ccl-encode-base64-generic
  (&optional quantums-per-line output-crlf terminate-with-newline)
  `(2
    ((r3 = 0)
     (r2 = 0)
     (read r1)
     (loop
      (branch
       r1
       ,@(mapcar
          (lambda (r1)
            `((write ,(nth (lsh r1 -2) mel-ccl-64-to-256-table))
              (r0 = ,(logand r1 3))))
          mel-ccl-256-table))
      (r2 = 1)
      (read-branch
       r1
       ,@(mapcar
          (lambda (r1)
            `((write r0 ,(vconcat
                          (mapcar
                           (lambda (r0)
                             (nth (logior (lsh r0 4)
                                          (lsh r1 -4))
                                  mel-ccl-64-to-256-table))
                           mel-ccl-4-table)))
              (r0 = ,(logand r1 15))))
          mel-ccl-256-table))
      (r2 = 2)
      (read-branch
       r1
       ,@(mapcar
          (lambda (r1)
            `((write r0 ,(vconcat
                          (mapcar
                           (lambda (r0)
                             (nth (logior (lsh r0 2)
                                          (lsh r1 -6))
                                  mel-ccl-64-to-256-table))
                           mel-ccl-16-table)))))
          mel-ccl-256-table))
      (r1 &= 63)
      (write r1 ,(vconcat
                  (mapcar
                   (lambda (r1)
                     (nth r1 mel-ccl-64-to-256-table))
                   mel-ccl-64-table)))
      (r3 += 1)
      (r2 = 0)
      (read r1)
      ,@(when quantums-per-line
	  `((if (r3 == ,quantums-per-line)
		((write ,(if output-crlf "\r\n" "\n"))
		 (r3 = 0)))))
      (repeat)))
    (branch
     r2
     ,(if terminate-with-newline
	  `(if (r3 > 0) (write ,(if output-crlf "\r\n" "\n")))
	`(r0 = 0))
     ((write r0 ,(vconcat
                  (mapcar
                   (lambda (r0)
                     (nth (lsh r0 4) mel-ccl-64-to-256-table))
                   mel-ccl-4-table)))
      (write ,(if terminate-with-newline
		  (if output-crlf "==\r\n" "==\n")
		"==")))
     ((write r0 ,(vconcat
                  (mapcar
                   (lambda (r0)
                     (nth (lsh r0 2) mel-ccl-64-to-256-table))
                   mel-ccl-16-table)))
      (write ,(if terminate-with-newline
		  (if output-crlf "=\r\n" "=\n")
		"="))))
    ))
)

(define-ccl-program mel-ccl-encode-b
  (mel-ccl-encode-base64-generic))

;; 19 * 4 = 76
(define-ccl-program mel-ccl-encode-base64-crlf-crlf
  (mel-ccl-encode-base64-generic 19 t))

(define-ccl-program mel-ccl-encode-base64-crlf-lf
  (mel-ccl-encode-base64-generic 19 nil))


;;; @ coding system
;;;

(make-ccl-coding-system
 'mel-ccl-b-rev ?B "MIME B-encoding (reversed)"
 'mel-ccl-encode-b 'mel-ccl-decode-b)

(make-ccl-coding-system
 'mel-ccl-base64-crlf-rev
 ?B "MIME Base64-encoding (reversed)"
 'mel-ccl-encode-base64-crlf-crlf
 'mel-ccl-decode-b)

(make-ccl-coding-system
 'mel-ccl-base64-lf-rev
 ?B "MIME Base64-encoding (LF encoding) (reversed)"
 'mel-ccl-encode-base64-crlf-lf
 'mel-ccl-decode-b)


;;; @ B
;;;

(check-broken-facility ccl-execute-eof-block-on-decoding-some)

(unless-broken ccl-execute-eof-block-on-decoding-some

  (defun base64-ccl-encode-string (string &optional no-line-break)
    "Encode STRING with base64 encoding."
    (if no-line-break
	(decode-coding-string string 'mel-ccl-b-rev)
      (decode-coding-string string 'mel-ccl-base64-lf-rev)))
  (defalias-maybe 'base64-encode-string 'base64-ccl-encode-string)

  (defun base64-ccl-encode-region (start end &optional no-line-break)
    "Encode region from START to END with base64 encoding."
    (interactive "*r")
    (if no-line-break
	(decode-coding-region start end 'mel-ccl-b-rev)
      (decode-coding-region start end 'mel-ccl-base64-lf-rev)))
  (defalias-maybe 'base64-encode-region 'base64-ccl-encode-region)

  (defun base64-ccl-insert-encoded-file (filename)
    "Encode contents of file FILENAME to base64, and insert the result."
    (interactive "*fInsert encoded file: ")
    (insert
     (decode-coding-string
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(insert-file-contents-as-binary filename)
	(buffer-string))
      'mel-ccl-base64-lf-rev)))

  (mel-define-method-function (mime-encode-string string (nil "base64"))
			      'base64-ccl-encode-string)
  (mel-define-method-function (mime-encode-region start end (nil "base64"))
			      'base64-ccl-encode-region)
  (mel-define-method-function
   (mime-insert-encoded-file filename (nil "base64"))
   'base64-ccl-insert-encoded-file)

  (mel-define-method-function (encoded-text-encode-string string (nil "B"))
			      'base64-ccl-encode-string)
  )

(defun base64-ccl-decode-string (string)
  "Decode base64 encoded STRING"
  (encode-coding-string string 'mel-ccl-b-rev))
(defalias-maybe 'base64-decode-string 'base64-ccl-decode-string)

(defun base64-ccl-decode-region (start end)
  "Decode base64 encoded the region from START to END."
  (interactive "*r")
  (encode-coding-region start end 'mel-ccl-b-rev))
(defalias-maybe 'base64-decode-region 'base64-ccl-decode-region)

(defun base64-ccl-write-decoded-region (start end filename)
  "Decode the region from START to END and write out to FILENAME."
  (interactive "*r\nFWrite decoded region to file: ")
  (let ((coding-system-for-write 'mel-ccl-b-rev)
	jka-compr-compression-info-list jam-zcat-filename-list)
    (write-region start end filename)))

(mel-define-method-function (mime-decode-string string (nil "base64"))
			    'base64-ccl-decode-string)
(mel-define-method-function (mime-decode-region start end (nil "base64"))
			    'base64-ccl-decode-region)
(mel-define-method-function
 (mime-write-decoded-region start end filename (nil "base64"))
 'base64-ccl-write-decoded-region)

(mel-define-method encoded-text-decode-string (string (nil "B"))
  (if (string-match (eval-when-compile
		      (concat "\\`" B-encoded-text-regexp "\\'"))
		    string)
      (base64-ccl-decode-string string)
    (error "Invalid encoded-text %s" string)))


;;; @ end
;;;

(provide 'mel-b-ccl)

;;; mel-b-ccl.el ends here.
