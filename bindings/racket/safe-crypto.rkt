#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeCrypto - FFI bindings to libproven cryptographic operations
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide constant-time-equal? random-bytes)

(define libproven (ffi-lib "libproven"))

(define-cstruct _BoolResult
  ([status _int32]
   [value  _int32]))

(define ffi-constant-time-eq
  (get-ffi-obj "proven_crypto_constant_time_eq" libproven
               (_fun _pointer _size _pointer _size -> _BoolResult)))

(define ffi-random-bytes
  (get-ffi-obj "proven_crypto_random_bytes" libproven
               (_fun _pointer _size -> _int32)))

;; Constant-time comparison (delegates to Idris 2)
(define (constant-time-equal? a b)
  (define ba (string->bytes/utf-8 a))
  (define bb (string->bytes/utf-8 b))
  (define result (ffi-constant-time-eq ba (bytes-length ba) bb (bytes-length bb)))
  (and (= (BoolResult-status result) 0)
       (not (= (BoolResult-value result) 0))))

;; Generate secure random bytes (delegates to Idris 2)
(define (random-bytes len)
  (define buf (make-bytes len 0))
  (define status (ffi-random-bytes buf len))
  (if (= status 0) buf (make-bytes 0)))
