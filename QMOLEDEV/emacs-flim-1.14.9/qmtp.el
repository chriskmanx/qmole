;;; qmtp.el --- basic functions to send mail with QMTP server

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: QMTP, qmail

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


;;; Commentary:

;; Installation:

;; To send mail using QMTP instead of SMTP, do

;; (fset 'smtp-send-buffer 'qmtp-send-buffer)

;;; Code:

(require 'custom)
(require 'mel) ; binary-funcall

(defgroup qmtp nil
  "QMTP protocol for sending mail."
  :group 'mail)

(defcustom qmtp-default-server nil
  "Specify default QMTP server."
  :type '(choice (const nil) string)
  :group 'qmtp)

(defvar qmtp-server qmtp-default-server
  "The name of the host running QMTP server.
It can also be a function
called from `qmtp-via-qmtp' with arguments SENDER and RECIPIENTS.")

(defcustom qmtp-service "qmtp"
  "QMTP service port number.  \"qmtp\" or 209."
  :type '(choice (integer :tag "209" 209)
                 (string :tag "qmtp" "qmtp"))
  :group 'qmtp)

(defcustom qmtp-timeout 30
  "Timeout for each QMTP session."
  :type 'integer
  :group 'qmtp)

;;;###autoload
(defvar qmtp-open-connection-function (function open-network-stream))

(defvar qmtp-error-response-alist
  '((?Z "Temporary failure")
    (?D "Permanent failure")))

(defvar qmtp-read-point nil)

(defun qmtp-encode-netstring-string (string)
  (format "%d:%s," (length string) string))

(defun qmtp-send-package (process sender recipients buffer)
  (with-temp-buffer
    (buffer-disable-undo)
    (erase-buffer)
    (set-buffer-multibyte nil)
    (insert
     (format "%d:\n"
	     (with-current-buffer buffer
	       (1+ (point-max));; for the "\n"
	       )))
    (insert-buffer-substring buffer)
    (insert
     "\n,"
     (qmtp-encode-netstring-string sender)
     (qmtp-encode-netstring-string
      (mapconcat #'qmtp-encode-netstring-string
		 recipients "")))
    (process-send-region process (point-min)(point-max)))
  (goto-char qmtp-read-point)
  (while (and (memq (process-status process) '(open run))
	      (not (re-search-forward "^[0-9]+:" nil 'noerror)))
    (unless (accept-process-output process qmtp-timeout)
      (error "timeout expired: %d" qmtp-timeout))
    (goto-char qmtp-read-point))
  (let ((response (char-after (match-end 0))))
    (unless (eq response ?K)
      (error (nth 1 (assq response qmtp-error-response-alist))))
    (setq recipients (cdr recipients))
    (beginning-of-line 2)
    (setq qmtp-read-point (point))))

;;;###autoload
(defun qmtp-via-qmtp (sender recipients buffer)
  (condition-case nil
      (progn
	(qmtp-send-buffer sender recipients buffer)
	t)
    (error)))

(make-obsolete 'qmtp-via-qmtp "It's old API.")

;;;###autoload
(defun qmtp-send-buffer (sender recipients buffer)
  (save-excursion
    (set-buffer
     (get-buffer-create
      (format "*trace of QMTP session to %s*" qmtp-server)))
    (buffer-disable-undo)
    (erase-buffer)
    (make-local-variable 'qmtp-read-point)
    (setq qmtp-read-point (point-min))
    (let (process)
      (unwind-protect
	  (progn
	    (setq process
		  (binary-funcall qmtp-open-connection-function
				  "QMTP" (current-buffer)
				  qmtp-server qmtp-service))
	    (qmtp-send-package process sender recipients buffer))
	(when (and process
		   (memq (process-status process) '(open run)))
	  ;; QUIT
	  (process-send-eof process)
	  (delete-process process))))))

(provide 'qmtp)

;;; qmtp.el ends here
