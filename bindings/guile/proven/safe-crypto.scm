;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafeCrypto - FFI bindings to libproven cryptographic operations
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-crypto)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (constant-time-equal?
            random-bytes))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function bindings

(define ffi-constant-time-eq
  (pointer->procedure '* (dynamic-func "proven_crypto_constant_time_eq" libproven)
                      (list '* size_t '* size_t)))

(define ffi-random-bytes
  (pointer->procedure int32 (dynamic-func "proven_crypto_random_bytes" libproven)
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

;;; Constant-time string comparison (delegates to Idris 2)
(define (constant-time-equal? str1 str2)
  (let-values (((ptr1 len1) (string->ffi-args str1))
               ((ptr2 len2) (string->ffi-args str2)))
    (parse-bool-result (ffi-constant-time-eq ptr1 len1 ptr2 len2))))

;;; Generate cryptographically secure random bytes (delegates to Idris 2)
;;; Returns a bytevector of the requested length
(define (random-bytes len)
  (let* ((bv (make-bytevector len 0))
         (ptr (bytevector->pointer bv))
         (status (ffi-random-bytes ptr len)))
    (if (= status 0)
        bv
        (make-bytevector 0))))
