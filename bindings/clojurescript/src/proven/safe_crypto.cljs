;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafeCrypto - Cryptographic primitives.
;;
;; All functions delegate to libproven via JS interop (transitive FFI).
;; Returns nil on error. NEVER reimplements logic.

(ns proven.safe-crypto
  "Safe cryptographic operations: constant-time comparison, secure random.
   All computation delegates to the Idris 2 core via FFI."
  (:require [proven.ffi :as ffi]))


(defn constant-time-eq?
  "Constant-time byte comparison (timing-attack safe).
  Returns true/false, or nil on error.

  Example:
    (constant-time-eq? \"secret\" \"secret\")  ;=> true
    (constant-time-eq? \"aaa\" \"bbb\")        ;=> false"
  [a b]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_crypto_constant_time_eq
                   lib a (count a) b (count b))]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn random-bytes
  "Generate n cryptographically secure random bytes.
  Returns a Uint8Array, or nil on error.

  Example:
    (random-bytes 32)  ;=> Uint8Array of 32 random bytes"
  [n]
  (when (pos? n)
    (when-let [lib (ffi/get-lib)]
      (let [buf (js/Uint8Array. n)
            status (.proven_crypto_random_bytes lib buf n)]
        (when (ffi/ok? status)
          buf)))))
