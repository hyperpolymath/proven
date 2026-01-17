;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven - Package definition
;;;; Version: 0.4.0
;;;; Module Count: 38

(defpackage #:proven
  (:use #:cl)
  (:export
   ;; Version information
   #:*version*
   #:*module-count*

   ;;; ========================================
   ;;; CORE MODULES (11)
   ;;; ========================================

   ;; SafeMath
   #:safe-result
   #:safe-result-value
   #:safe-result-ok-p
   #:make-safe-result
   #:safe-add
   #:safe-sub
   #:safe-mul
   #:safe-div
   #:safe-mod
   #:safe-abs
   #:safe-negate
   #:clamp
   #:in-range-p
   #:+max-int64+
   #:+min-int64+

   ;; SafeString
   #:escape-html
   #:escape-sql
   #:escape-js
   #:sanitize-default
   #:url-encode
   #:slugify

   ;; SafePath
   #:path-result
   #:path-result-path
   #:path-result-error
   #:path-result-ok-p
   #:make-path-result
   #:has-traversal-p
   #:sanitize-filename
   #:safe-path-join

   ;; SafeEmail
   #:email-result
   #:email-result-local-part
   #:email-result-domain
   #:email-result-error
   #:email-result-ok-p
   #:make-email-result
   #:valid-email-p
   #:parse-email
   #:disposable-email-p
   #:normalize-email

   ;; SafeURL
   #:url
   #:url-scheme
   #:url-host
   #:url-port
   #:url-path
   #:url-query
   #:url-fragment
   #:url-userinfo
   #:url-result
   #:url-result-url
   #:url-result-error
   #:url-result-ok-p
   #:parse-url
   #:format-url
   #:url-valid-p
   #:url-http-p
   #:url-https-p
   #:url-secure-p
   #:normalize-url
   #:url-encode-component
   #:url-decode-component
   #:parse-query-string
   #:format-query-string
   #:join-url-paths
   #:+default-ports+

   ;; SafeNetwork
   #:ipv4-address
   #:ipv4-address-octets
   #:ipv4-address-valid-p
   #:make-ipv4-address
   #:parse-ipv4
   #:format-ipv4
   #:loopback-p
   #:private-ip-p
   #:reserved-ip-p
   #:public-ip-p
   #:classify-ip
   #:+ip-class-invalid+
   #:+ip-class-loopback+
   #:+ip-class-private+
   #:+ip-class-reserved+
   #:+ip-class-public+
   #:valid-port-p
   #:privileged-port-p

   ;; SafeCrypto
   #:constant-time-equal-p
   #:simple-hash
   #:bytes-to-hex
   #:generate-token
   #:random-int
   #:secure-wipe

   ;; SafeUUID
   #:uuid
   #:uuid-bytes
   #:uuid-version
   #:uuid-variant
   #:uuid-result
   #:uuid-result-uuid
   #:uuid-result-error
   #:uuid-result-ok-p
   #:parse-uuid
   #:format-uuid
   #:uuid-valid-p
   #:uuid-nil-p
   #:uuid-equal-p
   #:generate-uuid-v4
   #:make-nil-uuid
   #:+nil-uuid-bytes+

   ;; SafeCurrency
   #:currency-code-p
   #:currency
   #:currency-code
   #:currency-minor-units
   #:currency-name
   #:currency-symbol
   #:money
   #:money-amount
   #:money-currency
   #:money-result
   #:money-result-value
   #:money-result-error
   #:money-result-ok-p
   #:money-add
   #:money-subtract
   #:money-multiply
   #:money-divide
   #:money-negate
   #:money-abs
   #:money-compare
   #:money-zero-p
   #:money-positive-p
   #:money-negative-p
   #:format-money
   #:parse-money
   #:make-money
   #:make-money-minor
   #:get-currency
   #:register-currency
   #:list-currencies
   #:*currencies*

   ;; SafePhone
   #:country-code-valid-p
   #:country-phone-info
   #:cpi-country-code
   #:cpi-calling-code
   #:cpi-name
   #:cpi-min-length
   #:cpi-max-length
   #:phone-number
   #:phone-country
   #:phone-national-number
   #:phone-extension
   #:phone-raw-input
   #:phone-result
   #:phone-result-phone
   #:phone-result-error
   #:phone-result-ok-p
   #:parse-phone
   #:format-phone
   #:phone-valid-p
   #:phone-equal-p
   #:valid-phone-p
   #:normalize-phone
   #:list-countries
   #:register-country
   #:get-country-info
   #:*country-phones*

   ;; SafeHex
   #:hex-result
   #:hex-result-value
   #:hex-result-error
   #:hex-result-ok-p
   #:hex-encode
   #:hex-decode
   #:hex-valid-p
   #:hex-constant-time-equal-p
   #:bytes-to-hex-lower
   #:bytes-to-hex-upper
   #:hex-to-bytes
   #:hex-to-string
   #:string-to-hex
   #:hex-format-bytes
   #:hex-dump
   #:hex-xor
   #:hex-and
   #:hex-or
   #:+hex-chars-lower+
   #:+hex-chars-upper+

   ;;; ========================================
   ;;; DATA MODULES (7)
   ;;; ========================================

   ;; SafeJSON
   #:json-value
   #:json-object
   #:json-array
   #:json-string
   #:json-number
   #:json-boolean
   #:json-null
   #:json-result
   #:json-result-value
   #:json-result-error
   #:json-result-ok-p
   #:parse-json
   #:format-json
   #:json-get
   #:json-get-path
   #:json-type
   #:json-null-p
   #:json-escape-string
   #:json-pretty-print

   ;; SafeDatetime
   #:datetime
   #:datetime-year
   #:datetime-month
   #:datetime-day
   #:datetime-hour
   #:datetime-minute
   #:datetime-second
   #:datetime-millisecond
   #:datetime-timezone
   #:datetime-result
   #:datetime-result-datetime
   #:datetime-result-error
   #:datetime-result-ok-p
   #:parse-datetime
   #:parse-iso8601
   #:format-datetime
   #:format-iso8601
   #:format-rfc2822
   #:datetime-valid-p
   #:datetime-add
   #:datetime-subtract
   #:datetime-diff
   #:datetime-compare
   #:leap-year-p
   #:days-in-month
   #:day-of-week
   #:day-of-year
   #:week-of-year
   #:unix-timestamp
   #:from-unix-timestamp
   #:+datetime-epoch+

   ;; SafeFloat
   #:+float-epsilon+
   #:safe-float-div
   #:safe-float-ln
   #:safe-float-log10
   #:safe-float-sqrt
   #:safe-float-pow
   #:safe-float-exp
   #:float-magnitude
   #:float-normalize
   #:float-finite-p
   #:float-safe-divisor-p
   #:float-clamp
   #:float-reciprocal
   #:float-mean
   #:float-variance
   #:float-std-dev

   ;; SafeVersion
   #:version
   #:version-major
   #:version-minor
   #:version-patch
   #:version-prerelease
   #:version-build-metadata
   #:version-result
   #:version-result-version
   #:version-result-error
   #:version-result-ok-p
   #:parse-version
   #:format-version
   #:version-prerelease-p
   #:version-stable-p
   #:version-bump-major
   #:version-bump-minor
   #:version-bump-patch
   #:version-compare
   #:version-satisfies-p

   ;; SafeColor
   #:rgb
   #:rgb-r
   #:rgb-g
   #:rgb-b
   #:rgba
   #:rgba-r
   #:rgba-g
   #:rgba-b
   #:rgba-a
   #:rgb-from-hex
   #:rgb-to-hex
   #:rgb-luminance
   #:contrast-ratio
   #:meets-wcag-aa-p
   #:meets-wcag-aaa-p
   #:blend-colors
   #:+rgb-black+
   #:+rgb-white+
   #:+rgb-red+
   #:+rgb-green+
   #:+rgb-blue+

   ;; SafeAngle
   #:degrees
   #:degrees-value
   #:radians
   #:radians-value
   #:degrees-to-radians
   #:radians-to-degrees
   #:normalize-degrees
   #:normalize-radians
   #:degrees-sin
   #:degrees-cos
   #:degrees-tan
   #:radians-sin
   #:radians-cos
   #:radians-tan
   #:angle-diff-degrees
   #:lerp-angle-degrees
   #:+pi+
   #:+two-pi+

   ;; SafeUnit
   #:length-unit
   #:mass-unit
   #:temperature-unit
   #:time-unit
   #:data-unit
   #:convert-length
   #:convert-mass
   #:convert-temperature
   #:convert-time
   #:convert-data
   #:+length-meters+
   #:+length-kilometers+
   #:+length-centimeters+
   #:+length-millimeters+
   #:+length-miles+
   #:+length-yards+
   #:+length-feet+
   #:+length-inches+
   #:+mass-kilograms+
   #:+mass-grams+
   #:+mass-milligrams+
   #:+mass-pounds+
   #:+mass-ounces+
   #:+mass-stones+
   #:+temp-celsius+
   #:+temp-fahrenheit+
   #:+temp-kelvin+
   #:+time-seconds+
   #:+time-milliseconds+
   #:+time-microseconds+
   #:+time-nanoseconds+
   #:+time-minutes+
   #:+time-hours+
   #:+time-days+
   #:+time-weeks+
   #:+data-bytes+
   #:+data-kilobytes+
   #:+data-megabytes+
   #:+data-gigabytes+
   #:+data-terabytes+
   #:+data-kibibytes+
   #:+data-mebibytes+
   #:+data-gibibytes+
   #:+data-tebibytes+

   ;;; ========================================
   ;;; DATA STRUCTURES MODULES (5)
   ;;; ========================================

   ;; SafeBuffer
   #:safe-buffer
   #:make-safe-buffer
   #:buffer-capacity
   #:buffer-length
   #:buffer-remaining
   #:buffer-empty-p
   #:buffer-full-p
   #:buffer-clear
   #:buffer-write-byte
   #:buffer-write-bytes
   #:buffer-read-byte
   #:buffer-read-bytes
   #:buffer-peek-byte
   #:buffer-to-bytes
   #:buffer-from-bytes

   ;; SafeQueue
   #:bounded-queue
   #:make-bounded-queue
   #:queue-capacity
   #:queue-length
   #:queue-empty-p
   #:queue-full-p
   #:queue-push
   #:queue-pop
   #:queue-peek
   #:queue-clear
   #:priority-queue
   #:make-priority-queue
   #:pqueue-push
   #:pqueue-pop
   #:pqueue-peek
   #:pqueue-empty-p

   ;; SafeBloom
   #:bloom-filter
   #:make-bloom-filter
   #:bloom-add
   #:bloom-contains-p
   #:bloom-false-positive-rate
   #:bloom-clear
   #:bloom-count
   #:bloom-merge

   ;; SafeLRU
   #:lru-cache
   #:make-lru-cache
   #:lru-capacity
   #:lru-length
   #:lru-get
   #:lru-put
   #:lru-contains-p
   #:lru-remove
   #:lru-clear
   #:lru-keys
   #:lru-values

   ;; SafeGraph
   #:graph
   #:make-graph
   #:graph-directed-p
   #:graph-add-node
   #:graph-add-edge
   #:graph-remove-node
   #:graph-remove-edge
   #:graph-has-node-p
   #:graph-has-edge-p
   #:graph-neighbors
   #:graph-nodes
   #:graph-edges
   #:graph-node-count
   #:graph-edge-count
   #:graph-bfs
   #:graph-dfs
   #:graph-shortest-path
   #:graph-has-cycle-p
   #:graph-topological-sort

   ;;; ========================================
   ;;; RESILIENCE MODULES (4)
   ;;; ========================================

   ;; SafeRateLimiter
   #:token-bucket
   #:make-token-bucket
   #:token-bucket-try-acquire
   #:token-bucket-available
   #:token-bucket-refill
   #:sliding-window
   #:make-sliding-window
   #:sliding-window-try-request
   #:sliding-window-request-count
   #:fixed-window
   #:make-fixed-window
   #:fixed-window-try-request
   #:fixed-window-reset

   ;; SafeCircuitBreaker
   #:circuit-state
   #:+circuit-closed+
   #:+circuit-open+
   #:+circuit-half-open+
   #:circuit-config
   #:make-circuit-config
   #:circuit-breaker
   #:make-circuit-breaker
   #:circuit-breaker-state
   #:circuit-breaker-call
   #:circuit-breaker-record-success
   #:circuit-breaker-record-failure
   #:circuit-breaker-reset

   ;; SafeRetry
   #:retry-config
   #:make-retry-config
   #:retry-config-max-attempts
   #:retry-config-initial-delay
   #:retry-config-max-delay
   #:retry-config-multiplier
   #:retry-config-jitter
   #:retry-state
   #:make-retry-state
   #:retry-should-retry-p
   #:retry-next-delay
   #:retry-record-attempt
   #:exponential-backoff
   #:with-retry

   ;; SafeMonotonic
   #:monotonic-counter
   #:make-monotonic-counter
   #:monotonic-counter-get
   #:monotonic-counter-next
   #:monotonic-counter-advance
   #:monotonic-counter-try-set
   #:monotonic-counter-ensure-at-least
   #:high-water-mark
   #:make-high-water-mark
   #:high-water-mark-get
   #:high-water-mark-update
   #:sequence-generator
   #:make-sequence-generator
   #:sequence-generator-next-id
   #:sequence-generator-next-id-padded
   #:epoch-generator
   #:make-epoch-generator
   #:epoch-generator-next-id

   ;;; ========================================
   ;;; STATE MODULES (2)
   ;;; ========================================

   ;; SafeStateMachine
   #:state-machine
   #:make-state-machine
   #:state-machine-current
   #:state-machine-can-transition-p
   #:state-machine-valid-transitions
   #:state-machine-transition
   #:state-machine-force-transition
   #:state-machine-history
   #:state-machine-clear-history
   #:state-machine-reset
   #:state-machine-add-transition
   #:state-machine-add-transitions
   #:state-machine-builder
   #:make-state-machine-builder
   #:builder-initial
   #:builder-transition
   #:builder-transitions
   #:builder-build

   ;; SafeCalculator
   #:safe-calculator
   #:make-safe-calculator
   #:calculator-value
   #:calculator-history
   #:calculator-clear
   #:calculator-add
   #:calculator-subtract
   #:calculator-multiply
   #:calculator-divide
   #:calculator-negate
   #:calculator-abs
   #:calculator-set
   #:calculator-undo

   ;;; ========================================
   ;;; ALGORITHM MODULES (4)
   ;;; ========================================

   ;; SafeGeo
   #:coordinate
   #:make-coordinate
   #:coordinate-lat
   #:coordinate-lon
   #:coordinate-lat-rad
   #:coordinate-lon-rad
   #:coordinate-northern-p
   #:coordinate-eastern-p
   #:haversine-distance
   #:distance-km
   #:distance-mi
   #:bearing
   #:destination-point
   #:bounding-box
   #:make-bounding-box
   #:bounding-box-contains-p
   #:bounding-box-center
   #:+earth-radius-km+
   #:+earth-radius-mi+

   ;; SafeProbability
   #:probability
   #:make-probability
   #:probability-value
   #:probability-complement
   #:probability-and
   #:probability-or
   #:probability-given
   #:probability-to-percent
   #:probability-from-percent
   #:probability-to-odds
   #:probability-from-odds
   #:+probability-zero+
   #:+probability-one+
   #:+probability-half+
   #:expected-value
   #:normalize-weights
   #:entropy
   #:binomial-probability

   ;; SafeChecksum
   #:crc32
   #:adler32
   #:fletcher16
   #:fnv1a-64
   #:fnv1a-32
   #:djb2
   #:sdbm
   #:luhn-check-p
   #:luhn-digit
   #:verify-crc32

   ;; SafeTensor
   #:tensor
   #:make-tensor
   #:tensor-data
   #:tensor-shape
   #:tensor-rank
   #:tensor-size
   #:tensor-get
   #:tensor-set
   #:tensor-add
   #:tensor-subtract
   #:tensor-hadamard
   #:tensor-dot
   #:tensor-mat-vec
   #:tensor-transpose
   #:tensor-reshape
   #:tensor-argmax
   #:tensor-sum
   #:tensor-mean
   #:tensor-zeros
   #:tensor-ones
   #:tensor-fill

   ;;; ========================================
   ;;; SECURITY MODULES (2)
   ;;; ========================================

   ;; SafePassword
   #:password-strength
   #:+strength-very-weak+
   #:+strength-weak+
   #:+strength-fair+
   #:+strength-strong+
   #:+strength-very-strong+
   #:password-policy
   #:make-password-policy
   #:policy-min-length
   #:policy-require-uppercase
   #:policy-require-lowercase
   #:policy-require-digit
   #:policy-require-special
   #:policy-max-length
   #:+policy-nist+
   #:+policy-pci-dss+
   #:+policy-hipaa+
   #:validate-password
   #:password-strength-score
   #:password-meets-policy-p
   #:password-feedback

   ;; SafeML
   #:softmax
   #:cross-entropy
   #:mse
   #:mae
   #:relu
   #:sigmoid
   #:tanh-activation
   #:gelu
   #:batch-normalize
   #:accuracy
   #:f1-score
   #:precision-score
   #:recall-score

   ;;; ========================================
   ;;; HTTP MODULES (3)
   ;;; ========================================

   ;; SafeHeader
   #:http-header
   #:make-http-header
   #:header-name
   #:header-value
   #:header-result
   #:header-result-header
   #:header-result-error
   #:header-result-ok-p
   #:parse-header
   #:format-header
   #:header-valid-p
   #:header-has-crlf-p
   #:header-dangerous-p
   #:normalize-header-name
   #:build-content-security-policy
   #:build-strict-transport-security
   #:+dangerous-headers+

   ;; SafeCookie
   #:cookie
   #:make-cookie
   #:cookie-name
   #:cookie-value
   #:cookie-domain
   #:cookie-path
   #:cookie-expires
   #:cookie-max-age
   #:cookie-secure
   #:cookie-http-only
   #:cookie-same-site
   #:same-site
   #:+same-site-strict+
   #:+same-site-lax+
   #:+same-site-none+
   #:cookie-result
   #:cookie-result-cookie
   #:cookie-result-error
   #:cookie-result-ok-p
   #:parse-cookie
   #:format-cookie
   #:format-set-cookie
   #:cookie-valid-p
   #:cookie-secure-p
   #:cookie-prefix-valid-p

   ;; SafeContentType
   #:media-type
   #:make-media-type
   #:media-type-type
   #:media-type-subtype
   #:media-type-suffix
   #:media-type-parameters
   #:content-type
   #:make-content-type
   #:content-type-media
   #:content-type-charset
   #:content-type-boundary
   #:content-type-result
   #:content-type-result-content-type
   #:content-type-result-error
   #:content-type-result-ok-p
   #:parse-content-type
   #:format-content-type
   #:content-type-json-p
   #:content-type-html-p
   #:content-type-xml-p
   #:content-type-text-p
   #:content-type-binary-p
   #:sniff-content-type
   #:sniffable-type-p
   #:+charset-utf8+
   #:+charset-ascii+
   #:+charset-iso-8859-1+))

(in-package #:proven)

;;; Version constants
(defparameter *version* "0.4.0"
  "Current version of the Proven library.")

(defparameter *module-count* 38
  "Total number of modules in the Proven library.")
