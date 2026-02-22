;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven/safe-crypto.fnl - Safe cryptographic primitive wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via LuaJIT FFI.
;; Do NOT reimplement any cryptographic logic in Fennel.

(local {: lib : ffi : bool-result : int-result : string-result} (require :proven.ffi))

;; ============================================================================
;; Safe cryptographic operations
;; ============================================================================

(fn constant-time-eq [a b]
  "Constant-time byte comparison (timing-attack safe). Returns nil on error."
  (bool-result (lib.proven_crypto_constant_time_eq a (length a) b (length b))))

(fn random-bytes [n]
  "Generate n cryptographically secure random bytes. Returns string or nil."
  (let [buf (ffi.new "char[?]" n)
        status (lib.proven_crypto_random_bytes buf n)]
    (when (= status 0)
      (ffi.string buf n))))

;; ============================================================================
;; Hex encoding/decoding
;; ============================================================================

(fn hex-encode [data ?uppercase]
  "Encode bytes to hex string. Returns nil on error."
  (string-result (lib.proven_hex_encode data (length data) (or ?uppercase false))))

(fn hex-decode [hex-string]
  "Decode hex string to bytes. Returns nil on error."
  (let [r (lib.proven_hex_decode hex-string (length hex-string))]
    (when (and (= r.status 0) (not= r.data nil))
      (let [s (ffi.string r.data r.length)]
        (lib.proven_hex_free (ffi.new "HexDecodeResult[1]" [r]))
        s))))

;; ============================================================================
;; Checksum
;; ============================================================================

(fn crc32 [s]
  "Calculate CRC32 checksum. Returns nil on error."
  (int-result (lib.proven_checksum_crc32 s (length s))))

(fn verify-crc32 [s expected]
  "Verify CRC32 checksum matches expected value. Returns nil on error."
  (bool-result (lib.proven_checksum_verify_crc32 s (length s) expected)))

;; ============================================================================
;; Export
;; ============================================================================

{:constant_time_eq constant-time-eq
 :random_bytes     random-bytes
 :hex_encode       hex-encode
 :hex_decode       hex-decode
 : crc32
 :verify_crc32     verify-crc32}
