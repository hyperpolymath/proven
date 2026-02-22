# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# proven/safe-crypto.janet - Safe cryptographic primitive wrappers for libproven.
# All computation delegates to the Idris 2 verified core via Janet FFI.
# Do NOT reimplement any cryptographic logic in Janet.

(import ./ffi :prefix "ffi/")

## ============================================================================
## Safe cryptographic operations
## ============================================================================

(defn constant-time-eq
  "Constant-time byte comparison (timing-attack safe).
  Returns true if equal, nil on error."
  [a b]
  (ffi/extract-bool-result
    (ffi/proven-crypto-constant-time-eq a (length a) b (length b))))

(defn random-bytes
  "Generate n cryptographically secure random bytes.
  Returns a buffer, or nil on error."
  [n]
  (let [buf (buffer/new n)]
    # Fill the buffer to n bytes so FFI has space to write
    (buffer/push-byte buf ;(seq [_ :range [0 n]] 0))
    (let [status (ffi/proven-crypto-random-bytes buf n)]
      (when (= status 0)
        buf))))

## ============================================================================
## Hex encoding/decoding
## ============================================================================

(defn hex-encode
  "Encode bytes to hex string. Returns nil on error."
  [data &opt uppercase]
  (default uppercase false)
  (ffi/extract-string-result (ffi/proven-hex-encode data (length data) uppercase)))

(defn hex-decode
  "Decode hex string to bytes. Returns nil on error."
  [hex-string]
  (let [r (ffi/proven-hex-decode hex-string (length hex-string))]
    (when (and r (= (get r 0) ffi/proven-ok) (not (nil? (get r 1))))
      (let [ptr (get r 1)
            len (get r 2)
            s (ffi/read :string ptr len)]
        # Note: proven_hex_free would need to be called on the result
        s))))

## ============================================================================
## Checksum
## ============================================================================

(defn crc32
  "Calculate CRC32 checksum. Returns nil on error."
  [data]
  (ffi/extract-int-result (ffi/proven-checksum-crc32 data (length data))))

(defn verify-crc32
  "Verify CRC32 checksum matches expected value. Returns nil on error."
  [data expected]
  (ffi/extract-bool-result (ffi/proven-checksum-verify-crc32 data (length data) expected)))
