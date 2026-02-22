#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeEmail - FFI bindings to libproven email validation
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide valid-email?)

(define libproven (ffi-lib "libproven"))

(define-cstruct _BoolResult
  ([status _int32]
   [value  _int32]))

(define ffi-email-is-valid
  (get-ffi-obj "proven_email_is_valid" libproven
               (_fun _pointer _size -> _BoolResult)))

;; Check if email is valid (delegates to Idris 2)
(define (valid-email? email)
  (define bstr (string->bytes/utf-8 email))
  (define result (ffi-email-is-valid bstr (bytes-length bstr)))
  (and (= (BoolResult-status result) 0)
       (not (= (BoolResult-value result) 0))))
