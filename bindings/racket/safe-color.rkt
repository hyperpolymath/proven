#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeColor - FFI bindings to libproven color operations
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide parse-hex-color rgb-to-hex)

(define libproven (ffi-lib "libproven"))

(define-cstruct _StringResult
  ([status _int32]
   [value  _pointer]
   [length _size]))

(define ffi-color-parse-hex
  (get-ffi-obj "proven_color_parse_hex" libproven
               (_fun _pointer _size -> _pointer)))

(define ffi-color-to-hex
  (get-ffi-obj "proven_color_to_hex" libproven
               (_fun _pointer -> _StringResult)))

(define ffi-free-string
  (get-ffi-obj "proven_free_string" libproven (_fun _pointer -> _void)))

;; Parse hex color string (delegates to Idris 2)
(define (parse-hex-color hex-string)
  (define bstr (string->bytes/utf-8 hex-string))
  (ffi-color-parse-hex bstr (bytes-length bstr)))

;; Convert RGB to hex string (delegates to Idris 2)
(define (rgb-to-hex rgb-ptr)
  (define sr (ffi-color-to-hex rgb-ptr))
  (if (= (StringResult-status sr) 0)
      (let ([str (cast (StringResult-value sr) _pointer _string/utf-8)])
        (ffi-free-string (StringResult-value sr))
        str)
      #f))
