;;; mel-q-ccl.el --- Quoted-Printable encoder/decoder using CCL.

;; Copyright (C) 1998,1999 Tanaka Akira

;; Author: Tanaka Akira <akr@jaist.ac.jp>
;; Created: 1998/9/17
;; Keywords: MIME, Quoted-Printable, Q-encoding

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

(defconst mel-ccl-16-table
  '(  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15))

(defconst mel-ccl-28-table
  '(  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
     16  17  18  19  20  21  22  23  24  25  26  27))

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

(defconst mel-ccl-256-to-16-table
  '(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      0   1   2   3   4   5   6   7   8   9 nil nil nil nil nil nil
    nil  10  11  12  13  14  15 nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))

(defconst mel-ccl-16-to-256-table
  (mapcar 'char-int "0123456789ABCDEF"))

(defconst mel-ccl-high-table
  (vconcat
   (mapcar
    (lambda (v) (nth (lsh v -4) mel-ccl-16-to-256-table))
    mel-ccl-256-table)))

(defconst mel-ccl-low-table
  (vconcat
   (mapcar
    (lambda (v) (nth (logand v 15) mel-ccl-16-to-256-table))
    mel-ccl-256-table)))

(defconst mel-ccl-u-raw
  (mapcar
   'char-int
   "0123456789\
ABCDEFGHIJKLMNOPQRSTUVWXYZ\
abcdefghijklmnopqrstuvwxyz\
!@#$%&'()*+,-./:;<>@[\\]^`{|}~"))

(defconst mel-ccl-c-raw
  (mapcar
   'char-int
   "0123456789\
ABCDEFGHIJKLMNOPQRSTUVWXYZ\
abcdefghijklmnopqrstuvwxyz\
!@#$%&'*+,-./:;<>@[]^`{|}~"))

(defconst mel-ccl-p-raw
  (mapcar
   'char-int
   "0123456789\
ABCDEFGHIJKLMNOPQRSTUVWXYZ\
abcdefghijklmnopqrstuvwxyz\
!*+-/"))

(defconst mel-ccl-qp-table
  [enc enc enc enc enc enc enc enc enc wsp lf  enc enc cr  enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   wsp raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw
   raw raw raw raw raw raw raw raw raw raw raw raw raw enc raw raw
   raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw
   raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw
   raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw
   raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc])

)


;;; @ CCL programs
;;;

;;; Q

(define-ccl-program mel-ccl-decode-q
  `(1
    ((loop
      (read-branch
       r0
       ,@(mapcar
          (lambda (r0)
            (cond
             ((= r0 (char-int ?_))
              `(write-repeat ? ))
             ((= r0 (char-int ?=))
              `((loop
                 (read-branch
                  r1
                  ,@(mapcar
                     (lambda (v)
                       (if (integerp v)
                           `((r0 = ,v) (break))
                         '(repeat)))
                     mel-ccl-256-to-16-table)))
                (loop
                 (read-branch
                  r1
                  ,@(mapcar
                     (lambda (v)
                       (if (integerp v)
                           `((write r0 ,(vconcat
                                         (mapcar
                                          (lambda (r0)
                                            (logior (lsh r0 4) v))
                                          mel-ccl-16-table)))
                             (break))
                         '(repeat)))
                     mel-ccl-256-to-16-table)))
                (repeat)))
             (t
              `(write-repeat ,r0))))
          mel-ccl-256-table))))))

(eval-when-compile

(defun mel-ccl-encode-q-generic (raw)
  `(3
    (loop
     (loop
      (read-branch
       r0
       ,@(mapcar
          (lambda (r0)
            (cond
             ((= r0 32) `(write-repeat ?_))
             ((member r0 raw) `(write-repeat ,r0))
             (t '(break))))
          mel-ccl-256-table)))
     (write ?=)
     (write r0 ,mel-ccl-high-table)
     (write r0 ,mel-ccl-low-table)
     (repeat))))

;; On xemacs, generated program counts iso-8859-1 8bit character as 6bytes.
(defun mel-ccl-count-q-length (raw)
  `(0
    ((r0 = 0)
     (loop
      (read-branch
       r1
       ,@(mapcar
	  (lambda (r1)
	    (if (or (= r1 32) (member r1 raw))
		'((r0 += 1) (repeat))
	      '((r0 += 3) (repeat))))
	  mel-ccl-256-table))))))

)

(define-ccl-program mel-ccl-encode-uq
  (mel-ccl-encode-q-generic mel-ccl-u-raw))
(define-ccl-program mel-ccl-encode-cq
  (mel-ccl-encode-q-generic mel-ccl-c-raw))
(define-ccl-program mel-ccl-encode-pq
  (mel-ccl-encode-q-generic mel-ccl-p-raw))

(define-ccl-program mel-ccl-count-uq
  (mel-ccl-count-q-length mel-ccl-u-raw))
(define-ccl-program mel-ccl-count-cq
  (mel-ccl-count-q-length mel-ccl-c-raw))
(define-ccl-program mel-ccl-count-pq
  (mel-ccl-count-q-length mel-ccl-p-raw))

;; Quoted-Printable

(eval-when-compile

(defvar eof-block-branches)
(defvar eof-block-reg)
(defun mel-ccl-set-eof-block (branch)
  (let ((p (assoc branch eof-block-branches)))
    (unless p
      (setq p (cons branch (length eof-block-branches))
	    eof-block-branches (cons p eof-block-branches)))
    `(,eof-block-reg = ,(cdr p))))

)

(eval-when-compile

(defun mel-ccl-try-to-read-crlf (input-crlf reg
					    succ
					    cr-eof cr-fail
					    lf-eof lf-fail
					    crlf-eof crlf-fail)
  (if input-crlf
      `(,(mel-ccl-set-eof-block cr-eof)
	(read-if (,reg == ?\r)
	  (,(mel-ccl-set-eof-block lf-eof)
	   (read-if (,reg == ?\n)
	     ,succ
	     ,lf-fail))
	  ,cr-fail))
    `(,(mel-ccl-set-eof-block crlf-eof)
      (read-if (,reg == ?\n)
	,succ
	,crlf-fail))))

)

(eval-when-compile

;; Generated CCL program works not properly on 20.2 because CCL_EOF_BLOCK
;; is not executed.
(defun mel-ccl-encode-quoted-printable-generic (input-crlf output-crlf)
  (let ((hard (if output-crlf "\r\n" "\n"))
	(soft (if output-crlf "=\r\n" "=\n"))
	(eof-block-branches nil)
	(eof-block-reg 'r4)
	(after-wsp 'r5)
	(column 'r6)
	(type 'r3)
	(current 'r0)
	(type-raw 0)
	(type-enc 1)
	(type-wsp 2)
	(type-brk 3)
	)
    `(4
      ((,column = 0)
       (,after-wsp = 0)
       ,(mel-ccl-set-eof-block '(end))
       (read r0)
       (loop	; invariant: column <= 75
	(loop
	 (loop
	  (branch
	   r0
	   ,@(mapcar
	      (lambda (r0)
		(let ((tmp (aref mel-ccl-qp-table r0)))
		  (cond
		   ((eq r0 (char-int ?F))
		    `(if (,column == 0)
			 (,(mel-ccl-set-eof-block '((write "F") (end)))
			  (read-if (r0 == ?r)
			    (,(mel-ccl-set-eof-block '((write "Fr") (end)))
			     (read-if (r0 == ?o)
			       (,(mel-ccl-set-eof-block '((write "Fro") (end)))
				(read-if (r0 == ?m)
				  (,(mel-ccl-set-eof-block '((write "From") (end)))
				   (read-if (r0 == ? )
				     ((,column = 7)
				      (,after-wsp = 1)
				      ,(mel-ccl-set-eof-block '((write "From=20") (end)))
				      (read r0)
				      (write-repeat "=46rom "))
				     ((,column = 4)
				      (write-repeat "From"))))
				  ((,column = 3)
				   (write-repeat "Fro"))))
			       ((,column = 2)
				(write-repeat "Fr"))))
			    ((,column = 1)
			     (write-repeat "F"))))
		       ((,type = ,type-raw) (break)) ; RAW
		       ))
		   ((eq r0 (char-int ?.))
		    `(if (,column == 0)
			 ,(mel-ccl-try-to-read-crlf
			    input-crlf 'r0
			    ;; "." CR LF (input-crlf: t)
			    ;; "." LF (input-crlf: nil)
			    `((write ,(concat "=2E" hard))
			      ,(mel-ccl-set-eof-block '(end))
			      (read r0)
			      (repeat))
			    ;; "." <EOF>
			    '((write ".") (end))
			    ;; "." noCR (input-crlf: t)
			    `((,column = 1)
			      (write-repeat "."))
			    ;; "." CR <EOF> (input-crlf: t)
			    '((write ".=0D") (end))
			    ;; "." CR noLF (input-crlf: t)
			    `((,column = 4)
			      (write-repeat ".=0D"))
			    ;; "." <EOF> (input-crlf: nil)
			    '((write ".") (end))
			    ;; "." noLF (input-crlf: nil)
			    `((,column = 1)
			      (write-repeat ".")))
		       ((,type = ,type-raw) (break)) ; RAW
		       ))
		   ((eq tmp 'raw) `((,type = ,type-raw) (break)))
		   ((eq tmp 'enc) `((,type = ,type-enc) (break)))
		   ((eq tmp 'wsp) `((,type = ,type-wsp) (break)))
		   ((eq tmp 'cr) `((,type = ,(if input-crlf type-brk type-enc))
				   (break)))
		   ((eq tmp 'lf) `((,type = ,(if input-crlf type-enc type-brk))
				   (break)))
		   )))
	      mel-ccl-256-table)))
	 ;; r0:type{raw,enc,wsp,brk}
	 (branch
	  ,type
	  ;; r0:type-raw
	  (if (,column < 75)
	      ((,column += 1)
	       (,after-wsp = 0)
	       ,(mel-ccl-set-eof-block '(end))
	       (write-read-repeat r0))
	    ((r1 = (r0 + 0))
	     (,after-wsp = 0)
	     ,@(mel-ccl-try-to-read-crlf
		input-crlf 'r0
		`((,column = 0)
		  (write r1)
		  ,(mel-ccl-set-eof-block `((write ,hard) (end)))
		  (read r0)
		  (write-repeat ,hard))
		'((write r1) (end))
		`((,column = 1)
		  (write ,soft) (write-repeat r1))
		`((write ,soft) (write r1) (write "=0D") (end))
		`((,column = 4)
		  (write ,soft) (write r1) (write-repeat "=0D"))
		'((write r1) (end))
		`((,column = 1)
		  (write ,soft) (write-repeat r1)))))
	  ;; r0:type-enc
	  ((,after-wsp = 0)
	   (if (,column < 73)
	       ((,column += 3)
		(write "=")
		(write r0 ,mel-ccl-high-table)
		,(mel-ccl-set-eof-block '(end))
		(write-read-repeat r0 ,mel-ccl-low-table))
	     (if (,column < 74)
		 ((r1 = (r0 + 0))
		  (,after-wsp = 0)
		  ,@(mel-ccl-try-to-read-crlf
		     input-crlf 'r0
		     `((,column = 0)
		       (write "=")
		       (write r1 ,mel-ccl-high-table)
		       (write r1 ,mel-ccl-low-table)
		       (write ,hard)
		       ,(mel-ccl-set-eof-block '(end))
		       (read r0)
		       (repeat))
		     `((write "=")
		       (write r1 ,mel-ccl-high-table)
		       (write r1 ,mel-ccl-low-table)
		       (end))
		     `((,column = 3)
		       (write ,(concat soft "="))
		       (write r1 ,mel-ccl-high-table)
		       (write r1 ,mel-ccl-low-table)
		       (repeat))
		     `((write ,(concat soft "="))
		       (write r1 ,mel-ccl-high-table)
		       (write r1 ,mel-ccl-low-table)
		       (write "=0D")
		       (end))
		     `((,column = 6)
		       (write ,(concat soft "="))
		       (write r1 ,mel-ccl-high-table)
		       (write r1 ,mel-ccl-low-table)
		       (write-repeat "=0D"))
		     `((write "=")
		       (write r1 ,mel-ccl-high-table)
		       (write r1 ,mel-ccl-low-table)
		       (end))
		     `((,column = 3)
		       (write ,(concat soft "="))
		       (write r1 ,mel-ccl-high-table)
		       (write r1 ,mel-ccl-low-table)
		       (repeat))))
	       ((,column = 3)
		(write ,(concat soft "="))
		(write r0 ,mel-ccl-high-table)
		,(mel-ccl-set-eof-block '(end))
		(write-read-repeat r0 ,mel-ccl-low-table)))))
	  ;; r0:type-wsp
	  (if (,column < 73)
	      ((r1 = (r0 + 0))
	       ,@(mel-ccl-try-to-read-crlf
		  input-crlf 'r0
		  `((,column = 0)
		    (,after-wsp = 0)
		    (write "=")
		    (write r1 ,mel-ccl-high-table)
		    (write r1 ,mel-ccl-low-table)
		    (write ,hard)
		    ,(mel-ccl-set-eof-block `(end))
		    (read r0)
		    (repeat))
		  `((write "=")
		    (write r1 ,mel-ccl-high-table)
		    (write r1 ,mel-ccl-low-table)
		    (end))
		  `((,column += 1)
		    (,after-wsp = 1)
		    (write-repeat r1))
		  `((write r1)
		    (write "=0D")
		    (end))
		  `((,column += 4)
		    (,after-wsp = 0)
		    (write r1)
		    (write-repeat "=0D"))
		  `((write "=")
		    (write r1 ,mel-ccl-high-table)
		    (write r1 ,mel-ccl-low-table)
		    (end))
		  `((,column += 1)
		    (,after-wsp = 1)
		    (write-repeat r1))))
	    (if (,column < 74)
		((r1 = (r0 + 0))
		 ,@(mel-ccl-try-to-read-crlf
		    input-crlf 'r0
		    `((,column = 0)
		      (,after-wsp = 0)
		      (write "=")
		      (write r1 ,mel-ccl-high-table)
		      (write r1 ,mel-ccl-low-table)
		      (write ,hard)
		      ,(mel-ccl-set-eof-block `(end))
		      (read r0)
		      (repeat))
		    `((write "=")
		      (write r1 ,mel-ccl-high-table)
		      (write r1 ,mel-ccl-low-table)
		      (end))
		    `((,column += 1)
		      (,after-wsp = 1)
		      (write-repeat r1))
		    `((write r1)
		      (write ,(concat soft "=0D"))
		      (end))
		    `((,column = 3)
		      (,after-wsp = 0)
		      (write r1)
		      (write-repeat ,(concat soft "=0D")))
		    `((write "=")
		      (write r1 ,mel-ccl-high-table)
		      (write r1 ,mel-ccl-low-table)
		      (end))
		    `((,column += 1)
		      (,after-wsp = 1)
		      (write-repeat r1))))
	      (if (,column < 75)
		  ((,column += 1)
		   (,after-wsp = 1)
		   ,(mel-ccl-set-eof-block `((write ,soft) (end)))
		   (write-read-repeat r0))
		((write ,soft)
		 (,column = 0)
		 (,after-wsp = 0)
		 (repeat)))))
	  ;; r0:type-brk
	  ,(if input-crlf
	       ;; r0{CR}:type-brk
	       `((if ((,column > 73) & ,after-wsp)
		     ((,column = 0)
		      (,after-wsp = 0)
		      (write ,soft)))
		 ,(mel-ccl-set-eof-block `((if (,column > 73) (write ,soft))
					   (write "=0D") (end)))
		 (read-if (r0 == ?\n)
		   (if ,after-wsp
		       ((,after-wsp = 0)
			(,column = 0)
			(write ,(concat soft hard))
			,(mel-ccl-set-eof-block '(end))
			(read r0)
			(repeat))
		     ((,after-wsp = 0)
		      (,column = 0)
		      (write ,hard)
		      ,(mel-ccl-set-eof-block '(end))
		      (read r0)
		      (repeat)))
		   (if (,column < 73)
		       ((,after-wsp = 0)
			(,column += 3)
			(write-repeat "=0D"))
		     (if (,column < 74)
			 (if (r0 == ?\r)
			     ((,after-wsp = 0)
			      ,(mel-ccl-set-eof-block
				`((write ,(concat soft "=0D=0D")) (end)))
			      (read-if (r0 == ?\n)
				((,column = 0)
				 ,(mel-ccl-set-eof-block
				   `((write ,(concat "=0D" hard)) (end)))
				 (read r0)
				 (write-repeat ,(concat "=0D" hard)))
				((,column = 6)
				 (write-repeat ,(concat soft "=0D=0D")))))
			   ((,after-wsp = 0)
			    (,column = 3)
			    (write-repeat ,(concat soft "=0D"))))
		       ((,after-wsp = 0)
			(,column = 3)
			(write-repeat ,(concat soft "=0D")))))))
	     ;; r0{LF}:type-brk
	     `(if ,after-wsp
		  ;; WSP ; r0{LF}:type-brk
		  ((,after-wsp = 0)
		   (,column = 0)
		   (write ,(concat soft (if output-crlf "\r" "")))
		   ,(mel-ccl-set-eof-block `(end))
		   (write-read-repeat r0))
		;; noWSP ; r0{LF}:type-brk
		((,after-wsp = 0)
		 (,column = 0)
		 ,@(if output-crlf '((write ?\r)) '())
		 ,(mel-ccl-set-eof-block `(end))
		 (write-read-repeat r0)))
	     )))))
      (branch
       ,eof-block-reg
       ,@(reverse (mapcar 'car eof-block-branches))))))

(defun mel-ccl-decode-quoted-printable-generic (input-crlf output-crlf)
  `(1
    ((read r0)
     (loop
      (branch
       r0
       ,@(mapcar
          (lambda (r0)
            (let ((tmp (aref mel-ccl-qp-table r0)))
              (cond
               ((eq tmp 'raw) `(write-read-repeat r0))
               ((eq tmp 'wsp) (if (eq r0 (char-int ? ))
                                  `(r1 = 1)
                                `(r1 = 0)))
               ((eq tmp 'cr)
                (if input-crlf
                    ;; r0='\r'
                    `((read r0)
                      ;; '\r' r0
                      (if (r0 == ?\n)
                          ;; '\r' r0='\n'
                          ;; hard line break found.
                          ,(if output-crlf
                               '((write ?\r)
                                 (write-read-repeat r0))
                             '(write-read-repeat r0))
                        ;; '\r' r0:[^\n]
                        ;; invalid control character (bare CR) found.
                        ;; -> ignore it and rescan from r0.
                        (repeat)))
                  ;; r0='\r'
                  ;; invalid character (bare CR) found.
                  ;; -> ignore.
                  `((read r0)
                    (repeat))))
               ((eq tmp 'lf)
                (if input-crlf
                    ;; r0='\n'
                    ;; invalid character (bare LF) found.
                    ;; -> ignore.
                    `((read r0)
                      (repeat))
                  ;; r0='\r\n'
                  ;; hard line break found.
                  (if output-crlf
                      '((write ?\r)
                        (write-read-repeat r0))
                    '(write-read-repeat r0))))
               ((eq r0 (char-int ?=))
                ;; r0='='
                `((read r0)
                  ;; '=' r0
                  (r1 = (r0 == ?\t))
                  (if ((r0 == ? ) | r1)
                      ;; '=' r0:[\t ]
                      ;; Skip transport-padding.
                      ;; It should check CR LF after
                      ;; transport-padding.
                      (loop
                       (read-if (r0 == ?\t)
                                (repeat)
                                (if (r0 == ? )
                                    (repeat)
                                  (break)))))
                  ;; '=' [\t ]* r0:[^\t ]
                  (branch
                   r0
                   ,@(mapcar
                      (lambda (r0)
                        (cond
                         ((eq r0 (char-int ?\r))
                          (if input-crlf
                              ;; '=' [\t ]* r0='\r'
                              `((read r0)
                                ;; '=' [\t ]* '\r' r0
                                (if (r0 == ?\n)
                                    ;; '=' [\t ]* '\r' r0='\n'
                                    ;; soft line break found.
                                    ((read r0)
                                     (repeat))
                                  ;; '=' [\t ]* '\r' r0:[^\n]
                                  ;; invalid input ->
                                  ;; output "=" and rescan from r0.
                                  ((write "=")
                                   (repeat))))
                            ;; '=' [\t ]* r0='\r'
                            ;; invalid input (bare CR found) -> 
                            ;; output "=" and rescan from next.
                            `((write ?=)
                              (read r0)
                              (repeat))))
                         ((eq r0 (char-int ?\n))
                          (if input-crlf
                              ;; '=' [\t ]* r0='\n'
                              ;; invalid input (bare LF found) -> 
                              ;; output "=" and rescan from next.
                              `((write ?=)
                                (read r0)
                                (repeat))
                            ;; '=' [\t ]* r0='\r\n'
                            ;; soft line break found.
                            `((read r0)
                              (repeat))))
                         ((setq tmp (nth r0 mel-ccl-256-to-16-table))
                          ;; '=' [\t ]* r0:[0-9A-F]
                          ;; upper nibble of hexadecimal digit found.
                          `((r1 = (r0 + 0))
			    (r0 = ,tmp)))
                         (t
                          ;; '=' [\t ]* r0:[^\r0-9A-F]
                          ;; invalid input ->
                          ;; output "=" and rescan from r0.
                          `((write ?=)
                            (repeat)))))
                      mel-ccl-256-table))
                  ;; '=' [\t ]* r1:r0:[0-9A-F]
                  (read-branch
                   r2
                   ,@(mapcar
                      (lambda (r2)
                        (if (setq tmp (nth r2 mel-ccl-256-to-16-table))
                            ;; '=' [\t ]* r1:r0:[0-9A-F] r2:[0-9A-F]
                            `(write-read-repeat
                              r0
                              ,(vconcat
                                (mapcar
                                 (lambda (r0)
                                   (logior (lsh r0 4) tmp))
                                 mel-ccl-16-table)))
                          ;; '=' [\t ]* r1:r0:[0-9A-F] r2:[^0-9A-F]
                          ;; invalid input
                          `(r3 = 0)	; nop
                          ))
                      mel-ccl-256-table))
                  ;; '=' [\t ]* r1:r0:[0-9A-F] r2:[^0-9A-F]
                  ;; invalid input ->
                  ;; output "=" with hex digit and rescan from r2.
                  (write ?=)
                  (r0 = (r2 + 0))
                  (write-repeat r1)))
               (t
                ;; r0:[^\t\r -~]
                ;; invalid character found.
                ;; -> ignore.
                `((read r0)
                  (repeat))))))
          mel-ccl-256-table))
      ;; r1[0]:[\t ]
      (loop
       ,@(apply
	  'append
	  (mapcar
	   (lambda (regnum)
	     (let ((reg (aref [r1 r2 r3 r4 r5] regnum)))
	       (apply
		'append
		(mapcar
		 (lambda (bit)
		   (if (= bit 0)
		       (if (= regnum 0)
			   nil
			 `((read r0)
			   (if (r0 == ?\t)
			       (,reg = 0)
			     (if (r0 == ?\ )
				 (,reg = 1)
			       ((r6 = ,(+ (* regnum 28) bit))
				(break))))))
		     `((read r0)
		       (if (r0 == ?\ )
			   (,reg |= ,(lsh 1 bit))
			 (if (r0 != ?\t)
			     ((r6 = ,(+ (* regnum 28) bit))
			      (break)))))))
		 mel-ccl-28-table))))
	   '(0 1 2 3 4)))
       ;; white space buffer exhaust.
       ;; error: line length limit (76bytes) violation.
       ;; -> ignore these white spaces.
       (repeat))
      ,(if input-crlf
           `(if (r0 == ?\r)
                ((read r0)
                 (if (r0 == ?\n)
                     ;; trailing white spaces found.
                     ;; -> ignore these white spacs.
                     ((write ,(if output-crlf "\r\n" "\n"))
                      (read r0)
                      (repeat))
                   ;; [\t ]* \r r0:[^\n]
                   ;; error: bare CR found.
                   ;; -> output white spaces and ignore bare CR.
                   ))
              ;; [\t ]* r0:[^\r]
              ;; middle white spaces found.
              )
         `(if (r0 == ?\n)
              ;; trailing white spaces found.
              ;; -> ignore these white spacs.
              ((write ,(if output-crlf "\r\n" "\n"))
               (read r0)
               (repeat))
            ;; [\t ]* r0:[^\n]
            ;; middle white spaces found.
            ))
      ,@(apply
	 'append
	 (mapcar
	  (lambda (regnum)
	    (let ((reg (aref [r1 r2 r3 r4 r5] regnum)))
	      (apply
	       'append
	       (mapcar
		(lambda (bit)
		  `((if (,reg & ,(lsh 1 bit))
			(write ?\ )
		      (write ?\t))
		    (if (r6 == ,(+ (* regnum 28) bit 1))
			(repeat))))
		mel-ccl-28-table))))
	  '(0 1 2 3 4)))
      (repeat)
      ))))

)

(define-ccl-program mel-ccl-encode-quoted-printable-crlf-crlf
  (mel-ccl-encode-quoted-printable-generic t t))

(define-ccl-program mel-ccl-encode-quoted-printable-crlf-lf
  (mel-ccl-encode-quoted-printable-generic t nil))

(define-ccl-program mel-ccl-encode-quoted-printable-lf-crlf
  (mel-ccl-encode-quoted-printable-generic nil t))

(define-ccl-program mel-ccl-encode-quoted-printable-lf-lf
  (mel-ccl-encode-quoted-printable-generic nil nil))

(define-ccl-program mel-ccl-decode-quoted-printable-crlf-crlf
  (mel-ccl-decode-quoted-printable-generic t t))

(define-ccl-program mel-ccl-decode-quoted-printable-crlf-lf
  (mel-ccl-decode-quoted-printable-generic t nil))

(define-ccl-program mel-ccl-decode-quoted-printable-lf-crlf
  (mel-ccl-decode-quoted-printable-generic nil t))

(define-ccl-program mel-ccl-decode-quoted-printable-lf-lf
  (mel-ccl-decode-quoted-printable-generic nil nil))


;;; @ coding system
;;;

(make-ccl-coding-system
 'mel-ccl-uq-rev ?Q "MIME Q-encoding in unstructured field (reversed)"
 'mel-ccl-encode-uq 'mel-ccl-decode-q)

(make-ccl-coding-system
 'mel-ccl-cq-rev ?Q "MIME Q-encoding in comment (reversed)"
 'mel-ccl-encode-cq 'mel-ccl-decode-q)

(make-ccl-coding-system
 'mel-ccl-pq-rev ?Q "MIME Q-encoding in phrase (reversed)"
 'mel-ccl-encode-pq 'mel-ccl-decode-q)

(make-ccl-coding-system
 'mel-ccl-quoted-printable-crlf-crlf-rev
 ?Q "MIME Quoted-Printable-encoding (reversed)"
 'mel-ccl-encode-quoted-printable-crlf-crlf
 'mel-ccl-decode-quoted-printable-crlf-crlf)

(make-ccl-coding-system
 'mel-ccl-quoted-printable-lf-crlf-rev
 ?Q "MIME Quoted-Printable-encoding (LF encoding) (reversed)"
 'mel-ccl-encode-quoted-printable-crlf-lf
 'mel-ccl-decode-quoted-printable-lf-crlf)

(make-ccl-coding-system
 'mel-ccl-quoted-printable-crlf-lf-rev
 ?Q "MIME Quoted-Printable-encoding (LF internal) (reversed)"
 'mel-ccl-encode-quoted-printable-lf-crlf
 'mel-ccl-decode-quoted-printable-crlf-lf)

(make-ccl-coding-system
 'mel-ccl-quoted-printable-lf-lf-rev
 ?Q "MIME Quoted-Printable-encoding (LF encoding) (LF internal) (reversed)"
 'mel-ccl-encode-quoted-printable-lf-lf
 'mel-ccl-decode-quoted-printable-lf-lf)


;;; @ quoted-printable
;;;

(check-broken-facility ccl-execute-eof-block-on-decoding-some)

(unless-broken ccl-execute-eof-block-on-decoding-some

  (defun quoted-printable-ccl-encode-string (string)
    "Encode STRING with quoted-printable encoding."
    (decode-coding-string
     string
     'mel-ccl-quoted-printable-lf-lf-rev))

  (defun quoted-printable-ccl-encode-region (start end)
    "Encode the region from START to END with quoted-printable encoding."
    (interactive "*r")
    (decode-coding-region start end 'mel-ccl-quoted-printable-lf-lf-rev))

  (defun quoted-printable-ccl-insert-encoded-file (filename)
    "Encode contents of the file named as FILENAME, and insert it."
    (interactive "*fInsert encoded file: ")
    (insert
     (decode-coding-string
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(insert-file-contents-as-binary filename)
	(buffer-string))
      'mel-ccl-quoted-printable-lf-lf-rev)))

  (mel-define-method-function
   (mime-encode-string string (nil "quoted-printable"))
   'quoted-printable-ccl-encode-string)
  (mel-define-method-function
   (mime-encode-region start end (nil "quoted-printable"))
   'quoted-printable-ccl-encode-region)
  (mel-define-method-function
   (mime-insert-encoded-file filename (nil "quoted-printable"))
   'quoted-printable-ccl-insert-encoded-file)
  )

(defun quoted-printable-ccl-decode-string (string)
  "Decode quoted-printable encoded STRING."
  (encode-coding-string
   string
   'mel-ccl-quoted-printable-lf-lf-rev))

(defun quoted-printable-ccl-decode-region (start end)
  "Decode the region from START to END with quoted-printable
encoding."
  (interactive "*r")
  (encode-coding-region start end 'mel-ccl-quoted-printable-lf-lf-rev))

(defun quoted-printable-ccl-write-decoded-region (start end filename)
  "Decode quoted-printable encoded current region and write out to FILENAME."
  (interactive "*r\nFWrite decoded region to file: ")
  (let ((coding-system-for-write 'mel-ccl-quoted-printable-lf-lf-rev)
	jka-compr-compression-info-list jam-zcat-filename-list)
    (write-region start end filename)))

(mel-define-method-function
 (mime-decode-string string (nil "quoted-printable"))
 'quoted-printable-ccl-decode-string)
(mel-define-method-function
 (mime-decode-region start end (nil "quoted-printable"))
 'quoted-printable-ccl-decode-region)
(mel-define-method-function
 (mime-write-decoded-region start end filename (nil "quoted-printable"))
 'quoted-printable-ccl-write-decoded-region)


;;; @ Q
;;;

(defun q-encoding-ccl-encode-string (string &optional mode)
  "Encode STRING to Q-encoding of encoded-word, and return the result.
MODE allows `text', `comment', `phrase' or nil.  Default value is
`phrase'."
  (decode-coding-string
   string
   (cond
    ((eq mode 'text) 'mel-ccl-uq-rev)
    ((eq mode 'comment) 'mel-ccl-cq-rev)
    (t 'mel-ccl-pq-rev))))

(defun q-encoding-ccl-decode-string (string)
  "Decode Q encoded STRING and return the result."
  (encode-coding-string
   string
   'mel-ccl-uq-rev))

(unless (featurep 'xemacs)
  (defun q-encoding-ccl-encoded-length (string &optional mode)
    (let ((status [nil nil nil nil nil nil nil nil nil]))
      (fillarray status nil)		; XXX: Is this necessary?
      (ccl-execute-on-string
       (cond
	((eq mode 'text) 'mel-ccl-count-uq)
	((eq mode 'comment) 'mel-ccl-count-cq)
	(t 'mel-ccl-count-pq))
       status
       string)
      (aref status 0)))
  )

(mel-define-method-function (encoded-text-encode-string string (nil "Q"))
			    'q-encoding-ccl-encode-string)

(mel-define-method encoded-text-decode-string (string (nil "Q"))
  (if (string-match (eval-when-compile
		      (concat "\\`" Q-encoded-text-regexp "\\'"))
		    string)
      (q-encoding-ccl-decode-string string)
    (error "Invalid encoded-text %s" string)))


;;; @ end
;;;

(provide 'mel-q-ccl)

;;; mel-q-ccl.el ends here.
