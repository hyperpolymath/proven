#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeString - FFI bindings to libproven string escaping
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide escape-html escape-sql escape-js is-valid-utf8?)

;; Load libproven
(define libproven (ffi-lib "libproven"))

;; StringResult C struct: { i32 status, pointer value, size_t length }
(define-cstruct _StringResult
  ([status _int32]
   [value  _pointer]
   [length _size]))

;; BoolResult C struct: { i32 status, i32 value }
(define-cstruct _BoolResult
  ([status _int32]
   [value  _int32]))

;; Free string allocated by libproven
(define ffi-free-string
  (get-ffi-obj "proven_free_string" libproven (_fun _pointer -> _void)))

;; FFI bindings
(define ffi-escape-html
  (get-ffi-obj "proven_string_escape_html" libproven (_fun _pointer _size -> _StringResult)))
(define ffi-escape-sql
  (get-ffi-obj "proven_string_escape_sql" libproven (_fun _pointer _size -> _StringResult)))
(define ffi-escape-js
  (get-ffi-obj "proven_string_escape_js" libproven (_fun _pointer _size -> _StringResult)))
(define ffi-is-valid-utf8
  (get-ffi-obj "proven_string_is_valid_utf8" libproven (_fun _pointer _size -> _BoolResult)))

;; Helper: call string FFI with Scheme string input
(define (call-string-ffi ffi-fn input)
  (define bstr (string->bytes/utf-8 input))
  (define len (bytes-length bstr))
  (define result (ffi-fn bstr len))
  (if (= (StringResult-status result) 0)
      (let ([str (cast (StringResult-value result) _pointer _string/utf-8)])
        (ffi-free-string (StringResult-value result))
        str)
      ""))

;; Escape HTML special characters (delegates to Idris 2)
(define (escape-html input) (call-string-ffi ffi-escape-html input))

;; Escape SQL single quotes (delegates to Idris 2)
(define (escape-sql input) (call-string-ffi ffi-escape-sql input))

;; Escape JavaScript special characters (delegates to Idris 2)
(define (escape-js input) (call-string-ffi ffi-escape-js input))

;; Check if string is valid UTF-8 (delegates to Idris 2)
(define (is-valid-utf8? input)
  (define bstr (string->bytes/utf-8 input))
  (define result (ffi-is-valid-utf8 bstr (bytes-length bstr)))
  (and (= (BoolResult-status result) 0)
       (not (= (BoolResult-value result) 0))))
