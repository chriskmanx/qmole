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

;; ocp-wizard additional conventions + server startup

;;   Functions owz-string-command and checked-string-command inplement
;;   an additional ocp-wizard specific convention for encoding failure
;;   (exception OwzFailure) and unexpected errors. The former raises
;;   the lisp exception owz-failure and owz-error; the latter reports
;;   them in the minibuffer.

;; - Callbacks are just lisp forms that are simply evaluated. Therefore,
;;   implementing the request processing just amounts to binding all
;;   symbols which may appear in the received forms. Failure is encoded
;;   by the two special messages "CALLBACK_READ_ERROR\n" and
;;   "ERROR_IN_CALLBACK\n<error contents as a form>\n".

(defconst callback-read-error "CALLBACK_READ_ERROR"
  "represents an error when reading a callback")

(defconst error-in-callback "ERROR_IN_CALLBACK"
  "represents an error when executing the callback")

(put 'owz-failure
     'error-conditions
     '(error owz-failure))
(put 'owz-failure 'error-message "OCP Wizard command failed")

(put 'owz-error
     'error-conditions
     '(error owz-error))
(put 'owz-error 'error-message "OCP Wizard unexpected error")

(defun owz-string-command (c)
  (let ((res (ocp-rpc-string-command c)))
    (if (and
         (>= (length res) 6)
         (string= (substring res 0 6) "Error\n"))
        (signal 'owz-error (cons (substring res 6) nil))
      (if (and
           (>= (length res) 7)
           (string= (substring res 0 7) "Failed\n"))
          (signal 'owz-failure (cons (substring res 7) nil))
        res))
    ))

(defun checked-string-command (c)
  (condition-case cond
      (owz-string-command c)
    (owz-failure
     (progn (message "Command failed: %s" (cadr cond)) nil))
    (owz-error
     (progn (message "%s" (cadr cond)) nil))
    ))

;; Provides the callback handler required by OCP RPC
(defun ocp-rpc-process-callback (connection-buffer)
  (condition-case nil
      (let ((callback (read connection-buffer)))
        ;;(message "received command %s" callback)
        (condition-case e
            (let ((res
                   (with-local-quit
                     (prin1-to-string (eval callback)))))
              (if (not quit-flag)
                  res
                (setq quit-flag nil)
                (concat error-in-callback "\n" "Quit")))
          (error
           (concat error-in-callback "\n"
                   (prin1-to-string e)))
          ))
    (error callback-read-error))
  )

(defvar typerex-server-port 0
  "port number for the ocp-wizard server")

(defcustom ocp-server-command "ocp-wizard"
  "command to run the TypeRex server ; may be any shell command"
  :group 'typerex-misc :type '(string))

(defcustom ocp-debug nil "whether TypeRex should run in debug mode"
  :group 'typerex-misc :type '(string))

(defcustom ocp-dont-catch-errors nil "fail-fast mode"
  :group 'typerex-misc :type '(boolean))

(defcustom ocp-profile nil "a command name to be profiled"
  :group 'typerex-misc :type '(string))

(defvar ocp-wizard-server-process nil "the ocp-wizard server process")

(defvar typerex-server-version nil "version of ocp-wizard server")

(defun ocp-config ()
  (let ((config (read (checked-string-command  "version"))))
    (setq typerex-server-version (cdr (assoc 'version config)))
    (setq typerex-library-path (cdr(assoc 'ocamllib config)))
    config))

(defun ocp-restart-server ()
  "(Re-)Start the TypeRex server, killing any existing one"
  (interactive)
  (if
      (and ocp-wizard-server-process
           (eq (process-status ocp-wizard-server-process) 'run))
      (kill-process ocp-wizard-server-process))
  (mapc
   (lambda (b) (with-current-buffer b (reset-tokenization)))
   (buffer-list))
    (setq typerex-server-port (ocp-rpc-start-server))
    (message "Listening on port %d" typerex-server-port)
    (message "Starting TypeRex server '%s'" ocp-server-command)
    (let ((line
           (concat
            (if typerex-library-path (concat"OCAMLLIB=" typerex-library-path " ") "")
            ocp-server-command
            (if (eq ocp-debug t) " -debug all"
              (if (eq ocp-debug nil) ""
                (concat " -debug " ocp-debug)))
            (if ocp-dont-catch-errors " -dont-catch-errors" "")
            (if ocp-profile (concat " -profile " ocp-profile) "")
            (if (or (eq ocp-theme nil) (member ocp-theme '("tuareg" "caml")))
                ""
              (concat " -coloring-theme " ocp-theme))
            " -backtrace"
            " emacs-server " (int-to-string typerex-server-port))))
      (message "Running: %s" line)
      (setq ocp-wizard-server-process
            (start-process-shell-command
             ;; The output should be empty: logging is in ~/.ocp-wizard-log
             "typerex-server" "*typerex-server-stdout-stderr*" line)))
    (set-process-query-on-exit-flag ocp-wizard-server-process nil)
    (message "Waiting for TypeRex server to connect")
    (let ((waited-time 0))
      (while (and (< waited-time 1.5) (eq ocp-rpc-connection nil))
        (sleep-for 0.01)
        (setq waited-time (+ waited-time 0.01)))
      (if (eq ocp-rpc-connection nil)
          (message "Timeout while waiting for TypeRex server!!!")
        (message "Connection established with TypeRex server")
        (message "Checking version")
        (ocp-config)
        (if (string= typerex-version typerex-server-version)
            (message "TypeRex-%s started" typerex-version)
          (message "Warning! TypeRex server version mismatch: %s <> %s"
                   typerex-server-version typerex-version))
        )))
