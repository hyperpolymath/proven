#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeCurrency - FFI bindings to libproven currency operations
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide currency-parse currency-format)

(define libproven (ffi-lib "libproven"))

(define-cstruct _StringResult
  ([status _int32]
   [value  _pointer]
   [length _size]))

(define ffi-currency-parse
  (get-ffi-obj "proven_currency_parse" libproven
               (_fun _pointer _size -> _pointer)))

(define ffi-currency-format
  (get-ffi-obj "proven_currency_format" libproven
               (_fun _int64 _pointer _uint8 -> _StringResult)))

(define ffi-free-string
  (get-ffi-obj "proven_free_string" libproven (_fun _pointer -> _void)))

;; Parse currency string (delegates to Idris 2)
(define (currency-parse currency-string)
  (define bstr (string->bytes/utf-8 currency-string))
  (ffi-currency-parse bstr (bytes-length bstr)))

;; Format currency (delegates to Idris 2)
;; amount-minor: amount in smallest units
;; code: 3-letter ISO 4217 code
;; decimal-places: number of decimal places
(define (currency-format amount-minor code decimal-places)
  (define code-bytes (make-bytes 3 0))
  (define code-utf8 (string->bytes/utf-8 code))
  (bytes-copy! code-bytes 0 code-utf8 0 (min 3 (bytes-length code-utf8)))
  (define sr (ffi-currency-format amount-minor code-bytes decimal-places))
  (if (= (StringResult-status sr) 0)
      (let ([str (cast (StringResult-value sr) _pointer _string/utf-8)])
        (ffi-free-string (StringResult-value sr))
        str)
      #f))
