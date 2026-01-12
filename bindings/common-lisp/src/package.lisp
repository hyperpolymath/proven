;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven - Package definition

(defpackage #:proven
  (:use #:cl)
  (:export
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
   #:+hex-chars-upper+))

(in-package #:proven)
