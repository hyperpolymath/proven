;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; Proven - Package definition
;;;; Thin CFFI wrapper; all computation delegates to libproven (Idris 2 + Zig).

(defpackage #:proven
  (:use #:cl #:cffi)
  (:export
   ;; Library lifecycle
   #:init
   #:deinit
   #:initialized-p
   #:ffi-abi-version
   #:free-proven-string

   ;; Version
   #:version-major
   #:version-minor
   #:version-patch
   #:module-count

   ;; SafeMath (8 functions)
   #:math-div
   #:math-mod
   #:math-add-checked
   #:math-sub-checked
   #:math-mul-checked
   #:math-abs-safe
   #:math-clamp
   #:math-pow-checked

   ;; SafeString (4 functions)
   #:string-is-valid-utf8
   #:string-escape-sql
   #:string-escape-html
   #:string-escape-js

   ;; SafePath (2 functions)
   #:path-has-traversal
   #:path-sanitize-filename

   ;; SafeEmail (1 function)
   #:email-is-valid

   ;; SafeNetwork (3 functions)
   #:network-parse-ipv4
   #:network-ipv4-is-private
   #:network-ipv4-is-loopback

   ;; SafeCrypto (2 functions)
   #:crypto-constant-time-eq
   #:crypto-random-bytes

   ;; SafeUUID (5 functions)
   #:uuid-v4
   #:uuid-to-string
   #:uuid-parse
   #:uuid-is-nil
   #:uuid-version

   ;; SafeCurrency (2 functions)
   #:currency-parse
   #:currency-format

   ;; SafePhone (2 functions)
   #:phone-parse
   #:phone-format-e164

   ;; SafeHex (2 functions)
   #:hex-encode
   #:hex-decode

   ;; SafeURL (1 function)
   #:url-parse

   ;; SafeJson (2 functions)
   #:json-is-valid
   #:json-get-type

   ;; SafeDatetime (4 functions)
   #:datetime-parse
   #:datetime-format-iso8601
   #:datetime-is-leap-year
   #:datetime-days-in-month

   ;; SafeFloat (5 functions)
   #:float-div
   #:float-is-finite
   #:float-is-nan
   #:float-sqrt
   #:float-ln

   ;; SafeVersion (2 functions)
   #:version-parse
   #:version-compare

   ;; SafeColor (3 functions)
   #:color-parse-hex
   #:color-rgb-to-hsl
   #:color-to-hex

   ;; SafeAngle (4 functions)
   #:angle-deg-to-rad
   #:angle-rad-to-deg
   #:angle-normalize-degrees
   #:angle-normalize-radians

   ;; SafeUnit (2 functions)
   #:unit-convert-length
   #:unit-convert-temp

   ;; SafeBuffer (4 functions)
   #:buffer-create
   #:buffer-append
   #:buffer-get
   #:buffer-free

   ;; SafeQueue (5 functions)
   #:queue-create
   #:queue-push
   #:queue-pop
   #:queue-size
   #:queue-free

   ;; SafeBloom (4 functions)
   #:bloom-create
   #:bloom-add
   #:bloom-contains
   #:bloom-free

   ;; SafeLRU (4 functions)
   #:lru-create
   #:lru-get
   #:lru-put
   #:lru-free

   ;; SafeGraph (4 functions)
   #:graph-create
   #:graph-add-edge
   #:graph-has-edge
   #:graph-free

   ;; SafeRateLimiter (3 functions)
   #:rate-limiter-create
   #:rate-limiter-try-acquire
   #:rate-limiter-free

   ;; SafeCircuitBreaker (6 functions)
   #:circuit-breaker-create
   #:circuit-breaker-allow
   #:circuit-breaker-success
   #:circuit-breaker-failure
   #:circuit-breaker-state
   #:circuit-breaker-free

   ;; SafeRetry (2 functions)
   #:retry-delay
   #:retry-should-retry

   ;; SafeMonotonic (3 functions)
   #:monotonic-create
   #:monotonic-next
   #:monotonic-free

   ;; SafeStateMachine (5 functions)
   #:state-machine-create
   #:state-machine-allow
   #:state-machine-transition
   #:state-machine-state
   #:state-machine-free

   ;; SafeCalculator (1 function)
   #:calculator-eval

   ;; SafeGeo (3 functions)
   #:geo-validate
   #:geo-distance
   #:geo-in-bounds

   ;; SafeProbability (4 functions)
   #:probability-create
   #:probability-and
   #:probability-or-exclusive
   #:probability-not

   ;; SafeChecksum (2 functions)
   #:checksum-crc32
   #:checksum-verify-crc32

   ;; SafeTensor (5 functions)
   #:tensor-create
   #:tensor-set
   #:tensor-get
   #:tensor-matmul
   #:tensor-free

   ;; SafePassword (2 functions)
   #:password-validate
   #:password-is-common

   ;; SafeML (5 functions)
   #:ml-softmax
   #:ml-sigmoid
   #:ml-relu
   #:ml-leaky-relu
   #:ml-clamp

   ;; SafeHeader (6 functions)
   #:header-has-crlf
   #:header-is-valid-name
   #:header-is-dangerous
   #:header-render
   #:header-build-csp
   #:header-build-hsts

   ;; SafeCookie (6 functions)
   #:cookie-has-injection
   #:cookie-validate-name
   #:cookie-validate-value
   #:cookie-get-prefix
   #:cookie-build-set-cookie
   #:cookie-build-delete

   ;; SafeContentType (5 functions)
   #:content-type-parse
   #:content-type-can-sniff-dangerous
   #:content-type-render
   #:content-type-is-json
   #:content-type-is-xml

   ;; SafeHTTP (3 functions)
   #:http-url-encode
   #:http-url-decode
   #:http-parse-www-authenticate

   ;; SafeRegistry (3 functions)
   #:registry-parse
   #:registry-to-string
   #:registry-has-registry

   ;; SafeDigest (3 functions)
   #:digest-parse
   #:digest-verify
   #:digest-to-string))

(in-package #:proven)
