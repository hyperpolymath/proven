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
   #:secure-wipe))

(in-package #:proven)
