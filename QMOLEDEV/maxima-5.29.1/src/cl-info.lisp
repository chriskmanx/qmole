(in-package :cl-info)

(defvar *info-section-hashtable* (make-hash-table :test 'equal))
(defvar *info-deffn-defvr-hashtable* (make-hash-table :test 'equal))

(defvar *prompt-prefix* "")
(defvar *prompt-suffix* "")

(defun print-prompt (prompt-count)
  (format t "~&~a~a~a"
	  *prompt-prefix*
	  (if (zerop prompt-count)
	      (intl:gettext "Enter space-separated numbers, `all' or `none': ")
	      (intl:gettext "Still waiting: "))
	  *prompt-suffix*))

(defvar +select-by-keyword-alist+
  '((noop "") (all "a" "al" "all") (none "n" "no" "non" "none")))

(defun parse-user-choice (nitems)
  (loop
   with line = (read-line) and nth and pos = 0
   while (multiple-value-setq (nth pos)
	   (parse-integer line :start pos :junk-allowed t))
   if (or (minusp nth) (>= nth nitems))
   do (format *debug-io* (intl:gettext "~&Discarding invalid number ~d.") nth)
   else collect nth into list
   finally
   (let ((keyword
	  (car (rassoc
		(string-right-trim
		 '(#\space #\tab #\newline #\;) (subseq line pos))
		+select-by-keyword-alist+
		:test #'(lambda (item list)
			  (member item list :test #'string-equal))))))
     (unless keyword
       (setq keyword 'noop)
       (format *debug-io* (intl:gettext "~&Ignoring trailing garbage in input.")))
     (return (cons keyword list)))))

(defun select-info-items (selection items)
  (case (pop selection)
    (noop (loop
	   for i in selection
	   collect (nth i items)))
    (all items)
    (none 'none)))

; ------------------------------------------------------------------
; STUFF ABOVE SALVAGED FROM PREVIOUS INCARNATION OF SRC/CL-INFO.LISP
; STUFF BELOW IS NEW, BASED ON LOOKUP TABLE BUILT AHEAD OF TIME
; ------------------------------------------------------------------

; ------------------ search help topics ------------------

(defun autoload-maxima-index ()
  ;; Autoload the index, but make sure we use a sensible *read-base*.
  ;; See bug 1951964.  GCL doesn't seem to have
  ;; with-standard-io-syntax.  Is just binding *read-base* enough?  Is
  ;; with-standard-io-syntax too much for what we want?
  #-gcl
  (with-standard-io-syntax
    (maxima::mfuncall 'cause-maxima-index-to-load))
  #+gcl
  (let ((*read-base* 10.))
    (maxima::mfuncall 'cause-maxima-index-to-load)))

(defun info-exact (x)
  (autoload-maxima-index)
  (let ((exact-matches (exact-topic-match x)))
    (if (null exact-matches)
      (progn
        (format t (intl:gettext "  No exact match found for topic `~a'.~%  Try `?? ~a' (inexact match) instead.~%~%") x x)
        nil)
      (progn
        (display-items exact-matches)
        (if (some-inexact x (inexact-topic-match x))
          (format t "  There are also some inexact matches for `~a'.~%  Try `?? ~a' to see them.~%~%" x x))
        t))))

(defun some-inexact (x inexact-matches)
  (some #'(lambda (y) (not (equal y x))) (mapcar #'car inexact-matches)))

(defun exact-topic-match (topic)
  (let*
    ((topic (regex-sanitize topic))
     (regex1 (concatenate 'string "^" topic "$"))
     (regex2 (concatenate 'string "^" topic " *<[0-9]+>$")))
    (append
      (find-regex-matches regex1 *info-section-hashtable*)
      (find-regex-matches regex2 *info-section-hashtable*)
      (find-regex-matches regex1 *info-deffn-defvr-hashtable*)
      (find-regex-matches regex2 *info-deffn-defvr-hashtable*))))

(defun info-inexact (x)
  (autoload-maxima-index)
  (let (wanted tem)
    (setf tem (inexact-topic-match x))
    (when tem (display-items tem))
    (not (null tem))))

(defun display-items (items)
  (let ((nitems (length items)))

    (loop for i from 0 for item in items do
      (when (> nitems 1)
        (let ((heading-title (nth 3 (cdr item))))
          (format t "~% ~d: ~a~@[  (~a)~]"
                  i
                  (car item)
                  heading-title))))

    (setq wanted
          (if (> nitems 1)
          (loop
           for prompt-count from 0
           thereis (progn
                 (finish-output *debug-io*)
                 (print-prompt prompt-count)
                 (force-output)
                 (clear-input)
                 (select-info-items
                  (parse-user-choice nitems) items)))
          items))
    (clear-input)
    (finish-output *debug-io*)
    (when (consp wanted)
      (format t "~%")
      (loop for item in wanted
        do (format t "~A~%~%" (read-info-text item))))))

(defun inexact-topic-match (topic)
  (setq topic (regex-sanitize topic))
  (append
    (find-regex-matches topic *info-section-hashtable*)
    (find-regex-matches topic *info-deffn-defvr-hashtable*)))

(defun regex-sanitize (s)
  "Precede any regex special characters with a backslash."
  (let
    ((L (coerce maxima-nregex::*regex-special-chars* 'list)))

    ; WORK AROUND NREGEX STRANGENESS: CARET (^) IS NOT ON LIST *REGEX-SPECIAL-CHARS*
    ; INSTEAD OF CHANGING NREGEX (WITH POTENTIAL FOR INTRODUCING SUBTLE BUGS)
    ; JUST APPEND CARET TO LIST HERE
    (setq L (cons #\^ L))

    (coerce (apply #'append
                   (mapcar #'(lambda (c) (if (member c L :test #'eq)
					     `(#\\ ,c) `(,c))) (coerce s 'list)))
            'string)))

(defun find-regex-matches (regex-string hashtable)
  (let*
    ((regex (maxima-nregex::regex-compile regex-string :case-sensitive nil))
     (regex-fcn (coerce regex 'function))
     (regex-matches nil))
    (maphash
      #'(lambda (key value)
          (if (funcall regex-fcn key)
            (setq regex-matches (cons `(,key . ,value) regex-matches))
            nil))
      hashtable)
    (stable-sort regex-matches #'string-lessp :key #'car)))

(defun read-info-text (x)
  (declare (special maxima::*maxima-infodir* maxima::*maxima-lang-subdir*))
  (let* ((value (cdr x))
	 (filename (car value))
	 (byte-offset (cadr value))
	 (byte-count (caddr value))
	 (text (make-string byte-count))
	 (subdir-bit
	  (if (null maxima::*maxima-lang-subdir*)
	      ""
	      (concatenate 'string "/" maxima::*maxima-lang-subdir*)))
	 (path+filename (concatenate 'string maxima::*maxima-infodir* subdir-bit "/" filename)))
    (with-open-file (in path+filename :direction :input)
      (file-position in byte-offset)
      #+gcl (gcl-read-sequence text in :start 0 :end byte-count)
      #-gcl (read-sequence text in :start 0 :end byte-count))
    text))

#+gcl
(defun gcl-read-sequence (s in &key (start 0) (end nil))
  (dotimes (i (- end start))
    (setf (aref s i) (read-char in))))

; --------------- build help topic indices ---------------

(defun load-info-hashtables ()
  (declare (special *info-section-pairs* *info-deffn-defvr-pairs*))
  (if (and (zerop (length *info-section-pairs*)) 
           (zerop (length *info-deffn-defvr-pairs*)))
    (format t (intl:gettext "warning: empty documentation index; ? and ?? won't work!~%")))
  ; (format t "HEY, I'M LOADING THE INFO HASHTABLES NOW~%")
  (mapc
    #'(lambda (x) (setf (gethash (car x) *info-section-hashtable*) (cdr x)))
    *info-section-pairs*)
  (mapc
    #'(lambda (x) (setf (gethash (car x) *info-deffn-defvr-hashtable*) (cdr x)))
    *info-deffn-defvr-pairs*))
