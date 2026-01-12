;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.core
  "Proven - A safety library for Clojure.

  Provides safe, validated operations for common programming tasks:
  - proven.safe-math: Overflow-checked arithmetic
  - proven.safe-string: XSS prevention and string sanitization
  - proven.safe-path: Directory traversal protection
  - proven.safe-email: Email validation
  - proven.safe-network: IP address validation
  - proven.safe-crypto: Cryptographic operations
  - proven.safe-uuid: UUID generation and validation
  - proven.safe-currency: Type-safe monetary values
  - proven.safe-phone: Phone number validation (E.164)
  - proven.safe-hex: Hexadecimal encoding/decoding"
  (:require [proven.safe-math :as math]
            [proven.safe-string :as string]
            [proven.safe-path :as path]
            [proven.safe-email :as email]
            [proven.safe-network :as network]
            [proven.safe-crypto :as crypto]
            [proven.safe-uuid :as uuid]
            [proven.safe-currency :as currency]
            [proven.safe-phone :as phone]
            [proven.safe-hex :as hex]))

;; Re-export commonly used functions for convenience

(def add math/add)
(def sub math/sub)
(def mul math/mul)
(def div math/div)

(def escape-html string/escape-html)
(def escape-sql string/escape-sql)
(def escape-js string/escape-js)

(def has-traversal? path/has-traversal?)
(def sanitize-filename path/sanitize-filename)

(def valid-email? email/valid?)
(def parse-email email/parse)

(def parse-ipv4 network/parse-ipv4)
(def valid-ipv4? network/valid-ipv4?)

(def sha256 crypto/sha256)
(def generate-token crypto/generate-token)
(def constant-time-equals crypto/constant-time-equals)

;; UUID
(def parse-uuid uuid/parse)
(def valid-uuid? uuid/valid?)
(def generate-uuid uuid/generate-v4)

;; Currency
(def parse-currency-code currency/parse-code)
(def valid-currency-code? currency/valid-code?)
(def money-from-major currency/from-major)
(def money-from-minor currency/from-minor)
(def format-money currency/format-money)

;; Phone
(def parse-phone phone/parse)
(def valid-phone? phone/valid?)
(def format-e164 phone/format-e164)

;; Hex
(def hex-encode hex/encode)
(def hex-decode hex/decode)
(def hex-valid? hex/valid?)
(def hex-constant-time-equal hex/constant-time-equal)
