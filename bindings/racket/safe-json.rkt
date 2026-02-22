#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeJson - FFI bindings to libproven JSON validation
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide json-valid? json-type-of)

(define libproven (ffi-lib "libproven"))

(define ffi-json-is-valid
  (get-ffi-obj "proven_json_is_valid" libproven
               (_fun _string/utf-8 -> _pointer)))

(define ffi-json-get-type
  (get-ffi-obj "proven_json_get_type" libproven
               (_fun _string/utf-8 -> _string/utf-8)))

;; Check if JSON string is valid (delegates to Idris 2)
(define (json-valid? json-string)
  (not (ptr-equal? (ffi-json-is-valid json-string) #f)))

;; Get type of JSON value (delegates to Idris 2)
(define (json-type-of json-string)
  (ffi-json-get-type json-string))
