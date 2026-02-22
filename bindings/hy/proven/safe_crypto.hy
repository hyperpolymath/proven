; SPDX-License-Identifier: PMPL-1.0-or-later
; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafeCrypto - Cryptographic primitives.
;;
;; All functions delegate to libproven via FFI.
;; Returns None on error. NEVER reimplements logic.

(import ctypes)
(import proven.ffi [get-lib ok? PROVEN-OK])


(defn constant-time-eq [a b]
  "Constant-time byte comparison (timing-attack safe).
  Arguments can be bytes or str. Returns True/False, or None on error.

  Example:
    (constant-time-eq b\"secret\" b\"secret\")  ; => True
    (constant-time-eq b\"aaa\" b\"bbb\")        ; => False
  "
  (setv ba (if (isinstance a str) (.encode a "utf-8") a))
  (setv bb (if (isinstance b str) (.encode b "utf-8") b))
  (setv result (.proven_crypto_constant_time_eq (get-lib) ba (len ba) bb (len bb)))
  (when (ok? result.status)
    (return result.value))
  None)


(defn random-bytes [n]
  "Generate n cryptographically secure random bytes.
  Returns bytes object, or None on error.

  Example:
    (random-bytes 32)  ; => b\"\\x4a\\x...\" (32 random bytes)
  "
  (when (<= n 0)
    (return None))
  (setv buf (ctypes.create_string_buffer n))
  (setv status (.proven_crypto_random_bytes (get-lib) buf n))
  (when (= status PROVEN-OK)
    (return (. buf raw)))
  None)
