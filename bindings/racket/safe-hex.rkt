#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeHex - FFI bindings to libproven hex encoding/decoding
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide hex-encode hex-encode-upper hex-decode)

(define libproven (ffi-lib "libproven"))

(define-cstruct _StringResult
  ([status _int32]
   [value  _pointer]
   [length _size]))

(define ffi-hex-encode
  (get-ffi-obj "proven_hex_encode" libproven
               (_fun _pointer _size _bool -> _StringResult)))

(define ffi-hex-decode
  (get-ffi-obj "proven_hex_decode" libproven
               (_fun _pointer _size -> _StringResult)))

(define ffi-free-string
  (get-ffi-obj "proven_free_string" libproven (_fun _pointer -> _void)))

;; Helper: parse StringResult to string
(define (string-result->string sr)
  (if (= (StringResult-status sr) 0)
      (let ([str (cast (StringResult-value sr) _pointer _string/utf-8)])
        (ffi-free-string (StringResult-value sr))
        str)
      #f))

;; Encode bytes to lowercase hex (delegates to Idris 2)
(define (hex-encode bstr)
  (string-result->string (ffi-hex-encode bstr (bytes-length bstr) #f)))

;; Encode bytes to uppercase hex (delegates to Idris 2)
(define (hex-encode-upper bstr)
  (string-result->string (ffi-hex-encode bstr (bytes-length bstr) #t)))

;; Decode hex string to bytes (delegates to Idris 2)
(define (hex-decode hex-string)
  (define bstr (string->bytes/utf-8 hex-string))
  (define sr (ffi-hex-decode bstr (bytes-length bstr)))
  (if (= (StringResult-status sr) 0)
      (let* ([len (StringResult-length sr)]
             [result (make-bytes len)])
        (memcpy result (StringResult-value sr) len)
        (ffi-free-string (StringResult-value sr))
        result)
      #f))
