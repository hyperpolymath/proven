#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeNetwork - FFI bindings to libproven network validation
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide ipv4-parse ipv4-private? ipv4-loopback?)

(define libproven (ffi-lib "libproven"))

(define ffi-network-parse-ipv4
  (get-ffi-obj "proven_network_parse_ipv4" libproven
               (_fun _string/utf-8 -> _pointer)))

(define ffi-network-ipv4-is-private
  (get-ffi-obj "proven_network_ipv4_is_private" libproven
               (_fun _string/utf-8 -> _pointer)))

(define ffi-network-ipv4-is-loopback
  (get-ffi-obj "proven_network_ipv4_is_loopback" libproven
               (_fun _string/utf-8 -> _pointer)))

;; Parse IPv4 address (delegates to Idris 2)
(define (ipv4-parse ip-string)
  (ffi-network-parse-ipv4 ip-string))

;; Check if IPv4 is private (delegates to Idris 2)
(define (ipv4-private? ip-string)
  (not (ptr-equal? (ffi-network-ipv4-is-private ip-string) #f)))

;; Check if IPv4 is loopback (delegates to Idris 2)
(define (ipv4-loopback? ip-string)
  (not (ptr-equal? (ffi-network-ipv4-is-loopback ip-string) #f)))
