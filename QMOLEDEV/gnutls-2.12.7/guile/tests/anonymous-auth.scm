;;; GnuTLS --- Guile bindings for GnuTLS.
;;; Copyright (C) 2007, 2010, 2011 Free Software Foundation, Inc.
;;;
;;; GnuTLS is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; GnuTLS is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with GnuTLS; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

;;; Written by Ludovic Courtès <ludo@chbouib.org>.


;;;
;;; Test session establishment using anonymous authentication.  Exercise the
;;; record layer low-level API.
;;;

(use-modules (gnutls)
             (gnutls build tests)
             (srfi srfi-4))


;; TLS session settings.
(define %protos  (list protocol/tls-1.0))
(define %certs   '())
(define %ciphers (list cipher/null cipher/arcfour cipher/aes-128-cbc
                       cipher/aes-256-cbc))
(define %kx      (list kx/anon-dh))
(define %macs    (list mac/sha1 mac/rmd160 mac/md5))

;; Message sent by the client.
(define %message (apply u8vector (iota 256)))

(define (import-something import-proc file fmt)
  (let* ((path (search-path %load-path file))
         (size (stat:size (stat path)))
         (raw  (make-u8vector size)))
    (uniform-vector-read! raw (open-input-file path))
    (import-proc raw fmt)))

(define (import-dh-params file)
  (import-something pkcs3-import-dh-parameters file
                    x509-certificate-format/pem))

;; Debugging.
;; (set-log-level! 100)
;; (set-log-procedure! (lambda (level str)
;;                       (format #t "[~a|~a] ~a" (getpid) level str)))

(run-test
    (lambda ()
      (let ((socket-pair (socketpair PF_UNIX SOCK_STREAM 0))
            (pid         (primitive-fork)))
        (if (= 0 pid)

            (let ((client (make-session connection-end/client)))
              ;; client-side (child process)
              (set-session-default-priority! client)
              (set-session-certificate-type-priority! client %certs)
              (set-session-kx-priority! client %kx)
              (set-session-protocol-priority! client %protos)
              (set-session-cipher-priority! client %ciphers)
              (set-session-mac-priority! client %macs)

              (set-session-transport-fd! client (fileno (car socket-pair)))
              (set-session-credentials! client (make-anonymous-client-credentials))
              (set-session-dh-prime-bits! client 1024)

              (handshake client)
              (record-send client %message)
              (bye client close-request/rdwr)

              (primitive-exit))

            (let ((server (make-session connection-end/server)))
              ;; server-side
              (set-session-default-priority! server)
              (set-session-certificate-type-priority! server %certs)
              (set-session-kx-priority! server %kx)
              (set-session-protocol-priority! server %protos)
              (set-session-cipher-priority! server %ciphers)
              (set-session-mac-priority! server %macs)

              (set-session-transport-fd! server (fileno (cdr socket-pair)))
              (let ((cred (make-anonymous-server-credentials))
                    (dh-params (import-dh-params "dh-parameters.pem")))
                ;; Note: DH parameter generation can take some time.
                (set-anonymous-server-dh-parameters! cred dh-params)
                (set-session-credentials! server cred))
              (set-session-dh-prime-bits! server 1024)

              (handshake server)
              (let* ((buf (make-u8vector (u8vector-length %message)))
                     (amount (record-receive! server buf)))
                (bye server close-request/rdwr)
                (and (= amount (u8vector-length %message))
                     (equal? buf %message))))))))

;;; arch-tag: 8c98de24-0a53-4290-974e-4b071ad162a0
