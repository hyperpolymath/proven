;; SPDX-License-Identifier: PMPL-1.0
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.core
  "Proven - A comprehensive safety library for Clojure (38 modules).

  Module Categories:

  Core (11):
  - proven.safe-math: Overflow-checked arithmetic
  - proven.safe-string: XSS prevention and string sanitization
  - proven.safe-path: Directory traversal protection
  - proven.safe-email: Email validation
  - proven.safe-url: URL parsing and validation
  - proven.safe-network: IP address validation
  - proven.safe-crypto: Cryptographic operations
  - proven.safe-uuid: UUID generation and validation
  - proven.safe-currency: Type-safe monetary values
  - proven.safe-phone: Phone number validation (E.164)
  - proven.safe-hex: Hexadecimal encoding/decoding

  Data (7):
  - proven.safe-json: Safe JSON parsing
  - proven.safe-datetime: Date/time operations
  - proven.safe-float: Floating-point safety
  - proven.safe-version: Semantic versioning
  - proven.safe-color: Color manipulation and WCAG contrast
  - proven.safe-angle: Angle conversions and trigonometry
  - proven.safe-unit: Physical unit conversions

  Data Structures (5):
  - proven.safe-buffer: Bounded buffers
  - proven.safe-queue: Priority queues
  - proven.safe-bloom: Bloom filters
  - proven.safe-lru: LRU caches
  - proven.safe-graph: Graph operations with cycle detection

  Resilience (4):
  - proven.safe-rate-limiter: Rate limiting
  - proven.safe-circuit-breaker: Circuit breaker pattern
  - proven.safe-retry: Retry with exponential backoff
  - proven.safe-monotonic: Monotonic counters

  State (2):
  - proven.safe-state-machine: State machine with validated transitions
  - proven.safe-calculator: Safe expression evaluation

  Algorithm (4):
  - proven.safe-geo: Geographic coordinates and distance
  - proven.safe-probability: Probability operations
  - proven.safe-checksum: CRC, hash, and Luhn checksums
  - proven.safe-tensor: Vector and matrix operations

  Security (2):
  - proven.safe-password: Password validation and strength
  - proven.safe-ml: Machine learning primitives

  HTTP (3):
  - proven.safe-header: HTTP header validation
  - proven.safe-cookie: Cookie security
  - proven.safe-content-type: MIME type handling"
  (:require
   ;; Core (11)
   [proven.safe-math :as math]
   [proven.safe-string :as string]
   [proven.safe-path :as path]
   [proven.safe-email :as email]
   [proven.safe-url :as url]
   [proven.safe-network :as network]
   [proven.safe-crypto :as crypto]
   [proven.safe-uuid :as uuid]
   [proven.safe-currency :as currency]
   [proven.safe-phone :as phone]
   [proven.safe-hex :as hex]
   ;; Data (7)
   [proven.safe-json :as json]
   [proven.safe-datetime :as datetime]
   [proven.safe-float :as float]
   [proven.safe-version :as version]
   [proven.safe-color :as color]
   [proven.safe-angle :as angle]
   [proven.safe-unit :as unit]
   ;; Data Structures (5)
   [proven.safe-buffer :as buffer]
   [proven.safe-queue :as queue]
   [proven.safe-bloom :as bloom]
   [proven.safe-lru :as lru]
   [proven.safe-graph :as graph]
   ;; Resilience (4)
   [proven.safe-rate-limiter :as rate-limiter]
   [proven.safe-circuit-breaker :as circuit-breaker]
   [proven.safe-retry :as retry]
   [proven.safe-monotonic :as monotonic]
   ;; State (2)
   [proven.safe-state-machine :as state-machine]
   [proven.safe-calculator :as calculator]
   ;; Algorithm (4)
   [proven.safe-geo :as geo]
   [proven.safe-probability :as probability]
   [proven.safe-checksum :as checksum]
   [proven.safe-tensor :as tensor]
   ;; Security (2)
   [proven.safe-password :as password]
   [proven.safe-ml :as ml]
   ;; HTTP (3)
   [proven.safe-header :as header]
   [proven.safe-cookie :as cookie]
   [proven.safe-content-type :as content-type]))

;; Library metadata
(def version "0.4.0")
(def module-count 38)

;; Re-export commonly used functions for convenience

;; Math
(def add math/add)
(def sub math/sub)
(def mul math/mul)
(def div math/div)

;; String
(def escape-html string/escape-html)
(def escape-sql string/escape-sql)
(def escape-js string/escape-js)

;; Path
(def has-traversal? path/has-traversal?)
(def sanitize-filename path/sanitize-filename)

;; Email
(def valid-email? email/valid?)
(def parse-email email/parse)

;; URL
(def parse-url url/parse)
(def valid-url? url/valid?)

;; Network
(def parse-ipv4 network/parse-ipv4)
(def valid-ipv4? network/valid-ipv4?)

;; Crypto
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

;; JSON
(def parse-json json/parse)
(def json-valid? json/valid?)

;; DateTime
(def parse-iso8601 datetime/parse-iso8601)
(def format-iso8601 datetime/format-iso8601)

;; Float
(def safe-div-float float/safe-div)
(def finite? float/finite?)

;; Version
(def parse-version version/parse)
(def version-compare version/compare-versions)

;; Color
(def parse-hex-color color/parse-hex)
(def contrast-ratio color/contrast-ratio)

;; Geo
(def haversine-distance geo/haversine-distance)
(def valid-coordinate? geo/valid-coordinate?)

;; Graph
(def has-cycle? graph/has-cycle?)
(def topological-sort graph/topological-sort)

;; Circuit Breaker
(def create-circuit-breaker circuit-breaker/create)

;; Rate Limiter
(def create-rate-limiter rate-limiter/create)

;; State Machine
(def create-state-machine state-machine/create)

;; Password
(def password-strength password/strength)
(def validate-password password/validate)

;; Checksum
(def crc32 checksum/crc32)
(def luhn-check checksum/luhn-check)
