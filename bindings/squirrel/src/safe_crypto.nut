// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCrypto - Cryptographic primitives for Squirrel.
//
// All operations delegate to libproven via the native "proven" table.
// Returns null on error.

/**
 * SafeCrypto - Timing-safe comparison and secure random generation.
 *
 * @example
 *   local crypto = SafeCrypto();
 *   local eq = crypto.constant_time_eq("secret", "secret");  // true
 *   local hex = crypto.random_hex(16);  // 32-char hex string
 */
class SafeCrypto {
    /**
     * Constant-time byte comparison (timing-attack safe).
     * @param {string} a - First buffer.
     * @param {string} b - Second buffer.
     * @return {bool|null} true if equal, false if not, null on error.
     */
    function constant_time_eq(a, b) {
        return proven.constant_time_eq(a, b);
    }

    /**
     * Generate cryptographically secure random bytes as hex.
     * @param {integer} nbytes - Number of random bytes (1-1024).
     * @return {string|null} Hex-encoded random bytes, or null on error.
     */
    function random_hex(nbytes) {
        return proven.random_hex(nbytes);
    }

    /**
     * Hex-encode a string.
     * @param {string} data - Data to encode.
     * @return {string|null} Hex string, or null on error.
     */
    function hex_encode(data) {
        return proven.hex_encode(data);
    }

    /**
     * Hex-decode a string.
     * @param {string} hex_str - Hex string to decode.
     * @return {string|null} Decoded bytes, or null on error.
     */
    function hex_decode(hex_str) {
        return proven.hex_decode(hex_str);
    }

    /**
     * Compute CRC32 checksum of a string.
     * @param {string} data - Data to checksum.
     * @return {integer|null} CRC32 value, or null on error.
     */
    function crc32(data) {
        return proven.crc32(data);
    }
}
