#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafePhone - FFI bindings to libproven phone number operations
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide phone-parse phone-format-e164)

(define libproven (ffi-lib "libproven"))

(define-cstruct _StringResult
  ([status _int32]
   [value  _pointer]
   [length _size]))

(define ffi-phone-parse
  (get-ffi-obj "proven_phone_parse" libproven
               (_fun _pointer _size -> _pointer)))

(define ffi-phone-format-e164
  (get-ffi-obj "proven_phone_format_e164" libproven
               (_fun _uint16 _uint64 -> _StringResult)))

(define ffi-free-string
  (get-ffi-obj "proven_free_string" libproven (_fun _pointer -> _void)))

;; Parse phone number (delegates to Idris 2)
(define (phone-parse phone-string)
  (define bstr (string->bytes/utf-8 phone-string))
  (ffi-phone-parse bstr (bytes-length bstr)))

;; Format phone as E.164 (delegates to Idris 2)
(define (phone-format-e164 country-code national-number)
  (define sr (ffi-phone-format-e164 country-code national-number))
  (if (= (StringResult-status sr) 0)
      (let ([str (cast (StringResult-value sr) _pointer _string/utf-8)])
        (ffi-free-string (StringResult-value sr))
        str)
      #f))
