;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeCrypto - JNA wrapper for proven_crypto_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.crypto
  "Safe cryptographic operations via libproven JNA FFI.

  Provides constant-time comparison and secure random byte generation."
  (:require [proven.native :as n])
  (:import [com.sun.jna Memory]))

(defn constant-time-eq?
  "Constant-time byte comparison (timing attack prevention).
  Returns boolean or nil on error."
  [^bytes a ^bytes b]
  (let [mem-a (n/to-native-bytes a)
        mem-b (n/to-native-bytes b)]
    (when (and mem-a mem-b)
      (n/call-bool-result "proven_crypto_constant_time_eq"
                          mem-a (long (alength a))
                          mem-b (long (alength b))))))

(defn random-bytes
  "Generate cryptographically secure random bytes.
  Returns a byte array of the specified length, or nil on error."
  [^long length]
  (when (pos? length)
    (let [mem (Memory. length)
          status (n/call-int "proven_crypto_random_bytes" mem (long length))]
      (when (zero? status)
        (.getByteArray mem 0 (int length))))))
