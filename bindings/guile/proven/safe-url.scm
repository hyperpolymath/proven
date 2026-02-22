;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafeUrl - FFI bindings to libproven URL validation and parsing
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-url)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (url-valid?
            url-scheme
            url-host
            url-port
            url-path
            url-query
            url-fragment))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function bindings (IdrisValue-based API)

(define ffi-url-is-valid
  (pointer->procedure '* (dynamic-func "proven_url_is_valid" libproven)
                      (list '*)))

(define ffi-url-scheme
  (pointer->procedure '* (dynamic-func "proven_url_scheme" libproven)
                      (list '*)))

(define ffi-url-host
  (pointer->procedure '* (dynamic-func "proven_url_host" libproven)
                      (list '*)))

(define ffi-url-port
  (pointer->procedure '* (dynamic-func "proven_url_port" libproven)
                      (list '*)))

(define ffi-url-path
  (pointer->procedure '* (dynamic-func "proven_url_path" libproven)
                      (list '*)))

(define ffi-url-query
  (pointer->procedure '* (dynamic-func "proven_url_query" libproven)
                      (list '*)))

(define ffi-url-fragment
  (pointer->procedure '* (dynamic-func "proven_url_fragment" libproven)
                      (list '*)))

;;; Helper: parse IdrisValue as boolean
(define (parse-idris-bool ptr)
  (let* ((bv (pointer->bytevector ptr (sizeof '*)))
         (val (bytevector-uint-native-ref bv 0 (sizeof '*))))
    (not (= val 0))))

;;; Helper: parse IdrisValue as string (null-terminated C string)
(define (parse-idris-string ptr)
  (pointer->string ptr))

;;; Check if URL string is valid (delegates to Idris 2)
(define (url-valid? url-string)
  (parse-idris-bool
   (ffi-url-is-valid (string->pointer url-string))))

;;; Get URL scheme (delegates to Idris 2)
(define (url-scheme url-string)
  (parse-idris-string
   (ffi-url-scheme (string->pointer url-string))))

;;; Get URL host (delegates to Idris 2)
(define (url-host url-string)
  (parse-idris-string
   (ffi-url-host (string->pointer url-string))))

;;; Get URL port (delegates to Idris 2)
(define (url-port url-string)
  (parse-idris-string
   (ffi-url-port (string->pointer url-string))))

;;; Get URL path (delegates to Idris 2)
(define (url-path url-string)
  (parse-idris-string
   (ffi-url-path (string->pointer url-string))))

;;; Get URL query (delegates to Idris 2)
(define (url-query url-string)
  (parse-idris-string
   (ffi-url-query (string->pointer url-string))))

;;; Get URL fragment (delegates to Idris 2)
(define (url-fragment url-string)
  (parse-idris-string
   (ffi-url-fragment (string->pointer url-string))))
