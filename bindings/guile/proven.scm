;;; SPDX-License-Identifier: PMPL-1.0
;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;
;;; Proven - Code that cannot crash
;;;
;;; A verified safety library for Guile Scheme providing:
;;; - SafeMath: Arithmetic without overflow/underflow/division-by-zero
;;; - SafeString: XSS/SQL injection prevention
;;; - SafePath: Path operations without traversal attacks
;;; - SafeEmail: RFC-compliant email validation
;;; - SafeNetwork: IP/CIDR/port validation
;;; - SafeUuid: UUID validation and parsing
;;; - SafeHex: Hexadecimal encoding and validation

(define-module (proven)
  #:use-module (proven safe-math)
  #:use-module (proven safe-string)
  #:use-module (proven safe-path)
  #:use-module (proven safe-email)
  #:use-module (proven safe-network)
  #:use-module (proven safe-uuid)
  #:use-module (proven safe-hex)
  #:re-export (;; SafeMath
               MAX-INT64
               MIN-INT64
               safe-add
               safe-sub
               safe-mul
               safe-div
               safe-mod
               safe-abs
               safe-negate
               clamp
               in-range?

               ;; SafeString
               escape-html
               escape-sql
               escape-js
               escape-shell
               sanitize
               slugify
               truncate-string

               ;; SafePath
               path-has-traversal?
               path-is-absolute?
               path-is-relative?
               sanitize-filename
               path-validate
               path-join-safe
               path-extension
               path-basename
               path-dirname

               ;; SafeEmail
               email-valid?
               email-parse
               email-normalize
               email-get-domain
               email-get-local

               ;; SafeNetwork
               ipv4-valid?
               ipv6-valid?
               ip-valid?
               port-valid?
               hostname-valid?
               cidr-valid?
               cidr-parse
               ip-private?
               ip-loopback?

               ;; SafeUuid
               UUID-NIL
               uuid-valid?
               uuid-valid-v4?
               uuid-nil?
               uuid-version
               uuid-normalize
               uuid-parse
               uuid-to-hex
               uuid-from-hex

               ;; SafeHex
               hex-valid?
               hex-valid-bytes?
               hex-normalize
               hex-equals?
               hex-byte-length
               hex-is-md5?
               hex-is-sha1?
               hex-is-sha256?
               hex-is-sha384?
               hex-is-sha512?
               hex-pad-left))

(define proven-version "1.0.0")
