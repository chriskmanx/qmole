;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Multithreading API regression tests

(in-package :cl-test)

;;; Date: 04/09/2009
;;; From: Matthew Mondor
;;; Fixed: 05/09/2009 (Juanjo)
;;; Description:
;;;
;;;	When a WITH-LOCK is interrupted, it is not able to release
;;;	the resulting lock and an error is signaled.
;;;

(deftest mp-0001-with-lock
    (progn
      (defparameter *mp-0001-with-lock-a* t)
      (defparameter *mp-0001-with-lock-b* (mp:make-lock))
      (mp:with-lock (*mp-0001-with-lock-b*)
        (let ((background-process
               (mp:process-run-function
                'mp-0001-with-lock
                (coerce '(lambda ()
                          (handler-case
                              (progn
                                (setf *mp-0001-with-lock-a* 1)
                                (mp:with-lock (*mp-0001-with-lock-b*)
                                  (setf *mp-0001-with-lock-a* 1)))
                            (error (c)
                              (princ c)(terpri)
                              (setf *mp-0001-with-lock-a* c)))
                          (setf *mp-0001-with-lock-a* 2))
                        'function))))
          ;; The background process should not be able to get
          ;; the lock, and will simply wait. Now we interrupt it
          ;; and the process should gracefully quit, without
          ;; signalling any serious condition
          (and (sleep 1)
               (mp:process-kill background-process)
               (progn (sleep 1)
                      (not (mp:process-active-p background-process)))
               (eq *mp-0001-with-lock-a* 1)))))
  t)