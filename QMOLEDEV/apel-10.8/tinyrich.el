;;;
;;; $Id: tinyrich.el,v 5.0 1995/09/20 14:45:56 morioka Exp $
;;;
;;;          by MORIOKA Tomohiko  <morioka@jaist.ac.jp>
;;; modified by YAMATE Keiichirou <ics9118@sem1.info.osaka-cu.ac.jp>
;;;

(defvar mime-viewer/face-list-for-text/enriched
  (cond ((and (>= emacs-major-version 19) window-system)
	 '(bold italic fixed underline)
	 )
	((and (boundp 'NEMACS) NEMACS)
	 '("bold" "italic" "underline")
	 )))

(defun enriched-decode (beg end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (while (re-search-forward "[ \t]*\\(\n+\\)[ \t]*" nil t)
	(let ((str (buffer-substring (match-beginning 1)
				     (match-end 1))))
	  (if (string= str "\n")
	      (replace-match " ")
	    (replace-match (substring str 1))
	    )))
      (goto-char beg)
      (let (cmd sym str (fb (point)) fe b e)
	(while (re-search-forward "<\\(<\\|[^<>\n\r \t]+>\\)" nil t)
	  (setq b (match-beginning 0))
	  (setq cmd (buffer-substring b (match-end 0)))
	  (if (string= cmd "<<")
	      (replace-match "<")
	    (replace-match "")
	    (setq cmd (downcase (substring cmd 1 (- (length cmd) 1))))
	    )
	  (setq sym (intern cmd))
	  (cond ((eq sym 'param)
		 (setq b (point))
		 (save-excursion
		   (save-restriction
		     (if (search-forward "</param>" nil t)
			 (progn
			   (replace-match "")
			   (setq e (point))
			   )
		       (setq e end)
		       )))
		 (delete-region b e)
		 )
		((memq sym mime-viewer/face-list-for-text/enriched)
		 (setq b (point))
		 (save-excursion
		   (save-restriction
		     (if (re-search-forward (concat "</" cmd ">") nil t)
			 (progn
			   (replace-match "")
			   (setq e (point))
			   )
		       (setq e end)
		       )))
		 (tm:set-face-region b e sym)
		 )))
	(goto-char (point-max))
	(if (not (eq (preceding-char) ?\n))
	    (insert "\n")
	  )
	))))


;;; @ text/richtext <-> text/enriched converter
;;;

(defun richtext-to-enriched-region (beg end)
  "Convert the region of text/richtext style to text/enriched style."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (b e i)
	(while (re-search-forward "[ \t]*<comment>" nil t)
	  (setq b (match-beginning 0))
	  (delete-region b
			 (if (re-search-forward "</comment>[ \t]*" nil t)
			     (match-end 0)
			   (point-max)
			   ))
	  )
	(goto-char (point-min))
	(while (re-search-forward "\n\n+" nil t)
	  (replace-match "\n")
	  )
	(goto-char (point-min))
	(while (re-search-forward "[ \t\n]*<nl>[ \t\n]*" nil t)
	  (setq b (match-beginning 0))
	  (setq e (match-end 0))
	  (setq i 1)
	  (while (looking-at "[ \t\n]*<nl>[ \t\n]*")
	    (setq e (match-end 0))
	    (setq i (1+ i))
	    (goto-char e)
	    )
	  (delete-region b e)
	  (while (>= i 0)
	    (insert "\n")
	    (setq i (1- i))
	    ))
	(goto-char (point-min))
	(while (search-forward "<lt>" nil t)
	  (replace-match "<<")
	  )
	))))

(defun enriched-to-richtext-region (beg end)
  "Convert the region of text/enriched style to text/richtext style."
  (save-excursion
    (save-restriction
      (goto-char beg)
      (and (search-forward "text/enriched")
	   (replace-match "text/richtext"))
      (search-forward "\n\n")
      (narrow-to-region (match-end 0) end)
      (let (str n)
	(goto-char (point-min))
	(while (re-search-forward "\n\n+" nil t)
	  (setq str (buffer-substring (match-beginning 0)
				      (match-end 0)))
	  (setq n (1- (length str)))
	  (setq str "")
	  (while (> n 0)
	    (setq str (concat str "<nl>\n"))
	    (setq n (1- n))
	    )
	  (replace-match str)
	  )
	(goto-char (point-min))
	(while (search-forward "<<" nil t)
	  (replace-match "<lt>")
	  )
	))))


;;; @ encoder and decoder
;;;

(defun richtext-decode (beg end)
  (save-restriction
    (narrow-to-region beg end)
    (richtext-to-enriched-region beg (point-max))
    (enriched-decode beg (point-max))
    ))

;; (defun richtext-encode (beg end)
;;   (save-restriction
;;     (narrow-to-region beg end)
;;     (enriched-encode beg (point-max))
;;     (enriched-to-richtext-region beg (point-max))
;;     ))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'tinyrich) (require 'apel-ver))

;; tinyrich.el ends here.
