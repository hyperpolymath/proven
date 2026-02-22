// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCrypto.vala -- Cryptographic primitives and hex encoding.
 *
 * Every method delegates to libproven via C extern calls declared in
 * lib_proven.vapi.  No cryptographic logic is reimplemented here.
 */
namespace Proven {

    /**
     * Safe cryptographic operations.
     *
     * Provides constant-time comparison (timing-attack safe), secure
     * random byte generation, and hexadecimal encoding/decoding.
     */
    public class SafeCrypto : GLib.Object {

        /**
         * Constant-time byte comparison (timing-attack resistant).
         *
         * Returns false if the buffers differ in length, without leaking
         * timing information about content.
         *
         * @param a First buffer.
         * @param b Second buffer.
         * @return true if equal, false otherwise; null on internal error.
         */
        public static bool? constant_time_eq (uint8[] a, uint8[] b) {
            LibProven.BoolResult r = LibProven.crypto_constant_time_eq (a, b);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }

        /**
         * Fill a buffer with cryptographically secure random bytes.
         *
         * @param length Number of random bytes to generate.
         * @return Array of random bytes, or null on failure.
         */
        public static uint8[]? random_bytes (size_t length) {
            uint8[] buffer = new uint8[length];
            int32 status = LibProven.crypto_random_bytes (buffer);
            if (status != 0) {
                return null;
            }
            return buffer;
        }

        /**
         * Encode raw bytes as a hexadecimal string.
         *
         * @param data  Bytes to encode.
         * @param upper If true, use uppercase hex digits (A-F).
         * @return Hex string, or null on error.
         */
        public static string? hex_encode (uint8[] data, bool upper = false) {
            LibProven.StringResult r = LibProven.hex_encode (data, upper);
            if (r.status != 0) {
                return null;
            }
            string result = ((string) r.value).substring (0, (long) r.length);
            LibProven.free_string (r.value);
            return result;
        }

        /**
         * Decode a hexadecimal string to raw bytes.
         *
         * @param hex Hex string to decode.
         * @return Decoded bytes, or null on error (e.g. odd length, invalid chars).
         */
        public static uint8[]? hex_decode (string hex) {
            unowned uint8[] data = (uint8[]) hex.data;
            LibProven.HexDecodeResult r = LibProven.hex_decode (data);
            if (r.status != 0) {
                return null;
            }
            uint8[] result = new uint8[r.length];
            GLib.Memory.copy (result, r.data, r.length);
            LibProven.hex_free (&r);
            return result;
        }

        /**
         * Compute the CRC32 checksum of a byte buffer.
         *
         * @param data Input bytes.
         * @return CRC32 value, or null on error.
         */
        public static int64? crc32 (uint8[] data) {
            LibProven.IntResult r = LibProven.checksum_crc32 (data);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }

        /**
         * Verify a CRC32 checksum against an expected value.
         *
         * @param data     Input bytes.
         * @param expected Expected CRC32 value.
         * @return true if the checksum matches; null on error.
         */
        public static bool? verify_crc32 (uint8[] data, uint32 expected) {
            LibProven.BoolResult r = LibProven.checksum_verify_crc32 (data, expected);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }
    }
}
