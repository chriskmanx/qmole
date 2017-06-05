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

;; Emacs lisp implementation of our RPC protocol.

;; Usage:

;; - Connect to the listening server using by evaluating
;;     (start-connection port-number)
;;   or setup a server with
;;     (start-rpc-server)
;;   The protocol is the one implemented by class
;;     Server.tagged_connection

;; - Then use
;;     (ocp-rpc-string-command command)
;;   which behaves as method send_tagged_command, to execute requests.

;; - Callbacks are processed by calling the function
;;   ocp-rpc-process-callback on the string representing the
;;   callback. The ocp-rpc-process-callback function is responsible
;;   for error encoding.

;; - Accepting requests is not implemented, since we don't use it in
;;   TypeRex because Emacs initiates the requests.

(require 'tq)

(defconst ocp-rpc-end-of-message "END_OF_MESSAGE"
  "mark for the client to detect the end of the answer")

(defconst ocp-rpc-request-start "REQUEST_START"
  "mark for the client to detect the beginning of a request")

(defconst ocp-rpc-request-start-length (+ 1 (length ocp-rpc-request-start)))

(defvar ocp-rpc-connection nil
  "connection process, (used, e.g. for sending data)")

(defvar ocp-rpc-queue nil
  "transaction queue on ocp-rpc-connection")

(defvar ocp-rpc-connection-server nil
  "server process")

(defun ocp-rpc-make-transaction (c)
  "send a message and get the expected reply. Both messages can
be either requests or answers"
;;  (message "sending message %s" c)
  (let ((reply (make-vector 1 nil)))
    (tq-enqueue
     ocp-rpc-queue
     (concat c "\n" ocp-rpc-end-of-message "\n")
     (concat "\n" ocp-rpc-end-of-message "\n")
     reply
     (lambda (reply answer)
       (setq answer (substring answer 0 (- (+ 2 (length ocp-rpc-end-of-message)))))
;;       (message "received message %s" answer)
       (aset reply 0 answer))
     t)
    (let ((quit-inhibited inhibit-quit))
      (setq inhibit-quit t)
      (while
          (and (eq (aref reply 0) nil)
               (eq (process-status ocp-rpc-connection) 'open))
        (accept-process-output ocp-rpc-connection 1 0))
      (unless (eq (process-status ocp-rpc-connection) 'open)
        (signal 'error '("Error: connection closed")))
      (setq inhibit-quit quit-inhibited)
      (aref reply 0)))
  )

(defun ocp-rpc-get-answer (c)
  "send a message and get a result"
  (let ((answer (ocp-rpc-make-transaction c)))
    (if (and (>= (length answer) ocp-rpc-request-start-length)
         (string=
         (substring answer 0 ocp-rpc-request-start-length)
         (concat ocp-rpc-request-start "\n")))
        (let ((callback (substring answer (length ocp-rpc-request-start))))
;;          (message "received callback %s" callback)
          (let ((result (ocp-rpc-process-callback callback)))
;;            (message "sending result %s" result)
            (ocp-rpc-get-answer result)))
      answer)))

(defun ocp-rpc-string-command (c)
  "send a command and get the expected result"
  (ocp-rpc-get-answer (concat ocp-rpc-request-start "\n" c)))

(defun ocp-rpc-start-connection (port)
  "start connection by listening on specified port"
  (setq max-lisp-eval-depth 10000)
  (setq ocp-rpc-connection
        (open-network-stream "ocp-wizard-client" nil 'local port))
  (set-process-filter ocp-rpc-connection 'ocp-rpc-connection-filter)
  (set-process-query-on-exit-flag ocp-rpc-connection nil)
  )

(defun ocp-rpc-connection-sentinel (process event)
  (if (string= (substring event 0 4) "open")
      (progn
;;        (message "connected")
        (setq ocp-rpc-connection process)
        (setq ocp-rpc-queue (tq-create ocp-rpc-connection))
        (set-process-query-on-exit-flag ocp-rpc-connection nil))
    ))

(defun ocp-rpc-start-server ()
  "start a server connection and return the listening port"
  (setq max-lisp-eval-depth 10000)
  (setq ocp-rpc-connection nil)
  (setq ocp-rpc-connection-server
        (make-network-process
         :name "ocp-connection"
         :server 0 :host "127.0.0.1" :family 'ipv4 :service t :reuseaddr))
  (set-process-sentinel ocp-rpc-connection-server 'ocp-rpc-connection-sentinel)
  (set-process-query-on-exit-flag ocp-rpc-connection-server nil)
  (let ((internal
         ;; try to be compatible with older Emacs
         (if (coding-system-p 'emacs-internal) 'emacs-internal 'emacs-mule)))
    (set-process-coding-system ocp-rpc-connection-server internal internal))
  (process-contact ocp-rpc-connection-server :service)
  )
