;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven-safe-crypto.el - Safe cryptographic primitive wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via FFI.
;; Do NOT reimplement any cryptographic logic in Emacs Lisp.

;;; Commentary:
;;
;; Provides constant-time comparison (timing-attack safe), cryptographically
;; secure random byte generation, hex encoding/decoding, and CRC32 checksums.
;; Every function returns nil on error instead of signaling.

;;; Code:

(require 'proven-ffi)

;;; ============================================================================
;;; Safe cryptographic operations
;;; ============================================================================

(defun proven-safe-crypto-constant-time-eq (a b)
  "Compare strings A and B in constant time (timing-attack safe).
Returns non-nil if equal, nil if not equal or on error.
Calls proven_crypto_constant_time_eq via FFI."
  (condition-case nil
      (proven-ffi--extract-bool-result (proven--ffi-crypto-constant-time-eq a b))
    (error nil)))

(defun proven-safe-crypto-random-bytes (n)
  "Generate N cryptographically secure random bytes.
Returns a unibyte string of length N, or nil on error.
Calls proven_crypto_random_bytes via FFI."
  (condition-case nil
      (proven--ffi-crypto-random-bytes n)
    (error nil)))

;;; ============================================================================
;;; Hex encoding/decoding
;;; ============================================================================

(defun proven-safe-crypto-hex-encode (data &optional uppercase)
  "Encode DATA (a string) as hexadecimal.
If UPPERCASE is non-nil, use uppercase hex digits.
Returns the hex string, or nil on error.
Calls proven_hex_encode via FFI."
  (condition-case nil
      (proven-ffi--extract-string-result
       (proven--ffi-hex-encode data (if uppercase t nil)))
    (error nil)))

(defun proven-safe-crypto-hex-decode (hex-string)
  "Decode HEX-STRING from hexadecimal to raw bytes.
Returns a unibyte string, or nil on error.
Calls proven_hex_decode via FFI."
  (condition-case nil
      (proven--ffi-hex-decode hex-string)
    (error nil)))

;;; ============================================================================
;;; Checksum
;;; ============================================================================

(defun proven-safe-crypto-crc32 (data)
  "Calculate CRC32 checksum of DATA string.
Returns the checksum as an integer, or nil on error.
Calls proven_checksum_crc32 via FFI."
  (condition-case nil
      (proven-ffi--extract-int-result (proven--ffi-checksum-crc32 data))
    (error nil)))

(defun proven-safe-crypto-verify-crc32 (data expected)
  "Verify CRC32 checksum of DATA matches EXPECTED integer.
Returns non-nil if match, nil otherwise.
Calls proven_checksum_verify_crc32 via FFI."
  (condition-case nil
      (proven-ffi--extract-bool-result
       (proven--ffi-checksum-verify-crc32 data expected))
    (error nil)))

(provide 'proven-safe-crypto)

;;; proven-safe-crypto.el ends here
