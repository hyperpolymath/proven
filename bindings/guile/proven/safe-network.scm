;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafeNetwork - FFI bindings to libproven network validation
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-network)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (ipv4-parse
            ipv4-private?
            ipv4-loopback?))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function bindings (IdrisValue-based API)

(define ffi-network-parse-ipv4
  (pointer->procedure '* (dynamic-func "proven_network_parse_ipv4" libproven)
                      (list '*)))

(define ffi-network-ipv4-is-private
  (pointer->procedure '* (dynamic-func "proven_network_ipv4_is_private" libproven)
                      (list '*)))

(define ffi-network-ipv4-is-loopback
  (pointer->procedure '* (dynamic-func "proven_network_ipv4_is_loopback" libproven)
                      (list '*)))

;;; Helper: parse IdrisValue as boolean
(define (parse-idris-bool ptr)
  (let* ((bv (pointer->bytevector ptr (sizeof '*)))
         (val (bytevector-uint-native-ref bv 0 (sizeof '*))))
    (not (= val 0))))

;;; Parse IPv4 address string (delegates to Idris 2)
;;; Returns parsed result or #f on invalid input
(define (ipv4-parse ip-string)
  (ffi-network-parse-ipv4 (string->pointer ip-string)))

;;; Check if IPv4 address is in private range (delegates to Idris 2)
(define (ipv4-private? ip-string)
  (parse-idris-bool
   (ffi-network-ipv4-is-private (string->pointer ip-string))))

;;; Check if IPv4 address is loopback (delegates to Idris 2)
(define (ipv4-loopback? ip-string)
  (parse-idris-bool
   (ffi-network-ipv4-is-loopback (string->pointer ip-string))))
