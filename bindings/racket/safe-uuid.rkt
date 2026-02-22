#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeUUID - FFI bindings to libproven UUID operations
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide generate-uuid-v4 parse-uuid uuid->string uuid-nil? uuid-version)

(define libproven (ffi-lib "libproven"))

(define-cstruct _StringResult
  ([status _int32]
   [value  _pointer]
   [length _size]))

(define ffi-uuid-v4
  (get-ffi-obj "proven_uuid_v4" libproven (_fun -> _pointer)))
(define ffi-uuid-parse
  (get-ffi-obj "proven_uuid_parse" libproven (_fun _pointer _size -> _pointer)))
(define ffi-uuid-to-string
  (get-ffi-obj "proven_uuid_to_string" libproven (_fun _pointer -> _StringResult)))
(define ffi-uuid-is-nil
  (get-ffi-obj "proven_uuid_is_nil" libproven (_fun _pointer -> _bool)))
(define ffi-uuid-version
  (get-ffi-obj "proven_uuid_version" libproven (_fun _pointer -> _uint8)))
(define ffi-free-string
  (get-ffi-obj "proven_free_string" libproven (_fun _pointer -> _void)))

;; Generate UUID v4 (delegates to Idris 2)
(define (generate-uuid-v4)
  (define result-ptr (ffi-uuid-v4))
  ;; Extract 16-byte UUID from result
  (define uuid-bytes (make-bytes 16))
  (memcpy uuid-bytes (ptr-add result-ptr 4) 16)
  uuid-bytes)

;; Parse UUID string (delegates to Idris 2)
(define (parse-uuid uuid-string)
  (define bstr (string->bytes/utf-8 uuid-string))
  (define result-ptr (ffi-uuid-parse bstr (bytes-length bstr)))
  (define uuid-bytes (make-bytes 16))
  (memcpy uuid-bytes (ptr-add result-ptr 4) 16)
  uuid-bytes)

;; Convert UUID bytes to string (delegates to Idris 2)
(define (uuid->string uuid-bytes)
  (define sr (ffi-uuid-to-string uuid-bytes))
  (if (= (StringResult-status sr) 0)
      (let ([str (cast (StringResult-value sr) _pointer _string/utf-8)])
        (ffi-free-string (StringResult-value sr))
        str)
      #f))

;; Check if UUID is nil (delegates to Idris 2)
(define (uuid-nil? uuid-bytes)
  (ffi-uuid-is-nil uuid-bytes))

;; Get UUID version (delegates to Idris 2)
(define (uuid-version uuid-bytes)
  (ffi-uuid-version uuid-bytes))
