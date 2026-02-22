#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeUrl - FFI bindings to libproven URL validation and parsing
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide url-valid? url-scheme url-host url-port url-path url-query url-fragment)

(define libproven (ffi-lib "libproven"))

(define ffi-url-is-valid
  (get-ffi-obj "proven_url_is_valid" libproven
               (_fun _string/utf-8 -> _pointer)))
(define ffi-url-scheme
  (get-ffi-obj "proven_url_scheme" libproven
               (_fun _string/utf-8 -> _string/utf-8)))
(define ffi-url-host
  (get-ffi-obj "proven_url_host" libproven
               (_fun _string/utf-8 -> _string/utf-8)))
(define ffi-url-port
  (get-ffi-obj "proven_url_port" libproven
               (_fun _string/utf-8 -> _string/utf-8)))
(define ffi-url-path
  (get-ffi-obj "proven_url_path" libproven
               (_fun _string/utf-8 -> _string/utf-8)))
(define ffi-url-query
  (get-ffi-obj "proven_url_query" libproven
               (_fun _string/utf-8 -> _string/utf-8)))
(define ffi-url-fragment
  (get-ffi-obj "proven_url_fragment" libproven
               (_fun _string/utf-8 -> _string/utf-8)))

;; URL validation and component extraction (all delegate to Idris 2)
(define (url-valid? url-string)
  (not (ptr-equal? (ffi-url-is-valid url-string) #f)))
(define (url-scheme url-string) (ffi-url-scheme url-string))
(define (url-host url-string) (ffi-url-host url-string))
(define (url-port url-string) (ffi-url-port url-string))
(define (url-path url-string) (ffi-url-path url-string))
(define (url-query url-string) (ffi-url-query url-string))
(define (url-fragment url-string) (ffi-url-fragment url-string))
