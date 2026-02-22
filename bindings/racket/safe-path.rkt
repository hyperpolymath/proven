#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafePath - FFI bindings to libproven path validation
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide has-traversal? sanitize-filename)

(define libproven (ffi-lib "libproven"))

(define ffi-path-has-traversal
  (get-ffi-obj "proven_path_has_traversal" libproven
               (_fun _string/utf-8 -> _pointer)))

(define ffi-path-sanitize-filename
  (get-ffi-obj "proven_path_sanitize_filename" libproven
               (_fun _string/utf-8 -> _string/utf-8)))

;; Check for path traversal (delegates to Idris 2)
(define (has-traversal? path)
  (define result (ffi-path-has-traversal path))
  (not (ptr-equal? result #f)))

;; Sanitize filename (delegates to Idris 2)
(define (sanitize-filename filename)
  (ffi-path-sanitize-filename filename))
