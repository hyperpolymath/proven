;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.core
  "Proven - A safety library for Clojure.

  Provides safe, validated operations for common programming tasks:
  - proven.safe-math: Overflow-checked arithmetic
  - proven.safe-string: XSS prevention and string sanitization
  - proven.safe-path: Directory traversal protection
  - proven.safe-email: Email validation
  - proven.safe-network: IP address validation
  - proven.safe-crypto: Cryptographic operations"
  (:require [proven.safe-math :as math]
            [proven.safe-string :as string]
            [proven.safe-path :as path]
            [proven.safe-email :as email]
            [proven.safe-network :as network]
            [proven.safe-crypto :as crypto]))

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
