;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafeEmail - FFI bindings to libproven email validation
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-email)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (email-valid?))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function binding
(define ffi-email-is-valid
  (pointer->procedure '* (dynamic-func "proven_email_is_valid" libproven)
                      (list '* size_t)))

;;; Helper: convert string to (pointer, length)
(define (string->ffi-args str)
  (let* ((bv (string->utf8 str))
         (len (bytevector-length bv))
         (ptr (bytevector->pointer bv)))
    (values ptr len)))

;;; Helper: parse BoolResult struct { i32 status, bool value }
(define (parse-bool-result ptr)
  (let* ((bv (pointer->bytevector ptr 8))
         (status (bytevector-s32-native-ref bv 0))
         (value (bytevector-s32-native-ref bv 4)))
    (and (= status 0) (not (= value 0)))))

;;; Check if string is a valid email address (delegates to Idris 2)
(define (email-valid? email)
  (call-with-values (lambda () (string->ffi-args email))
    (lambda (ptr len)
      (parse-bool-result (ffi-email-is-valid ptr len)))))
