#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeVersion - FFI bindings to libproven semantic versioning
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide version-parse version-compare)

(define libproven (ffi-lib "libproven"))

(define ffi-version-parse
  (get-ffi-obj "proven_version_parse" libproven
               (_fun _pointer _size -> _pointer)))

(define ffi-version-compare
  (get-ffi-obj "proven_version_compare" libproven
               (_fun _pointer _pointer -> _int32)))

;; Parse semantic version string (delegates to Idris 2)
(define (version-parse version-string)
  (define bstr (string->bytes/utf-8 version-string))
  (ffi-version-parse bstr (bytes-length bstr)))

;; Compare two semantic versions (delegates to Idris 2)
;; Returns -1, 0, or 1
(define (version-compare v1-ptr v2-ptr)
  (ffi-version-compare v1-ptr v2-ptr))
