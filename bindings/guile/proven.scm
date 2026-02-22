;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; Proven - FFI bindings to libproven (Idris 2 verified safety library)
;;;
;;; All computation is performed in Idris 2 via the Zig FFI layer.
;;; This module re-exports all submodule bindings.

(define-module (proven)
  #:use-module (proven safe-math)
  #:use-module (proven safe-string)
  #:use-module (proven safe-path)
  #:use-module (proven safe-email)
  #:use-module (proven safe-network)
  #:use-module (proven safe-uuid)
  #:use-module (proven safe-hex)
  #:use-module (proven safe-crypto)
  #:use-module (proven safe-currency)
  #:use-module (proven safe-datetime)
  #:use-module (proven safe-json)
  #:use-module (proven safe-phone)
  #:use-module (proven safe-url)
  #:re-export (;; SafeMath
               safe-add
               safe-sub
               safe-mul
               safe-div
               safe-mod
               safe-abs
               safe-negate
               clamp
               in-range?
               MAX-INT64
               MIN-INT64

               ;; SafeString
               escape-html
               escape-sql
               escape-js
               is-valid-utf8?

               ;; SafePath
               path-has-traversal?
               sanitize-filename

               ;; SafeEmail
               email-valid?

               ;; SafeNetwork
               ipv4-parse
               ipv4-private?
               ipv4-loopback?

               ;; SafeUuid
               uuid-v4
               uuid-parse
               uuid->string
               uuid-nil?
               uuid-version

               ;; SafeHex
               hex-encode
               hex-decode

               ;; SafeCrypto
               constant-time-equal?
               random-bytes

               ;; SafeCurrency
               currency-parse
               currency-format

               ;; SafeDatetime
               datetime-parse
               datetime-format-iso8601
               leap-year?
               days-in-month

               ;; SafeJson
               json-valid?
               json-type-of

               ;; SafePhone
               phone-parse
               phone-format-e164

               ;; SafeUrl
               url-valid?
               url-scheme
               url-host
               url-port
               url-path
               url-query
               url-fragment))

(define proven-version "1.0.0")
