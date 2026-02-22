// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeCrypto.cs - Cryptographic primitives (constant-time comparison, random bytes, SHA-256).
//
// Thin P/Invoke wrapper over libproven. ALL computation is performed in
// verified Idris 2 code via the Zig FFI bridge. No logic is reimplemented here.

namespace Proven
{
    /// <summary>
    /// Safe cryptographic operations backed by formally verified Idris 2 code.
    /// Provides constant-time comparison, cryptographic random byte generation,
    /// hex encoding, and CRC32 checksums. All methods delegate to the libproven FFI.
    /// </summary>
    public static class SafeCrypto
    {
        /// <summary>
        /// Compare two byte arrays in constant time (timing-attack safe).
        /// Returns false if lengths differ. Delegates to
        /// proven_crypto_constant_time_eq via FFI.
        /// </summary>
        /// <param name="a">First byte array.</param>
        /// <param name="b">Second byte array.</param>
        /// <returns>true if equal, false if different, null on FFI error.</returns>
        public static bool? ConstantTimeEquals(byte[] a, byte[] b)
        {
            return MarshalHelpers.BoolResultToNullable(
                LibProven.proven_crypto_constant_time_eq(
                    a, (nuint)a.Length,
                    b, (nuint)b.Length));
        }

        /// <summary>
        /// Generate cryptographically secure random bytes.
        /// Delegates to proven_crypto_random_bytes via FFI.
        /// </summary>
        /// <param name="length">Number of random bytes to generate.</param>
        /// <returns>Array of random bytes, or null on error.</returns>
        public static byte[]? RandomBytes(int length)
        {
            if (length < 0)
            {
                return null;
            }

            byte[] buffer = new byte[length];
            int status = LibProven.proven_crypto_random_bytes(buffer, (nuint)length);

            if (status == 0)
            {
                return buffer;
            }

            return null;
        }

        /// <summary>
        /// Encode bytes to a lowercase hexadecimal string.
        /// Delegates to proven_hex_encode via FFI.
        /// </summary>
        /// <param name="data">Byte data to encode.</param>
        /// <returns>The hex-encoded string, or null on error.</returns>
        public static string? HexEncode(byte[] data)
        {
            return MarshalHelpers.StringResultToManaged(
                LibProven.proven_hex_encode(data, (nuint)data.Length, false));
        }

        /// <summary>
        /// Calculate CRC32 checksum of byte data.
        /// Delegates to proven_checksum_crc32 via FFI.
        /// </summary>
        /// <param name="data">Byte data to checksum.</param>
        /// <returns>The CRC32 value as a long, or null on error.</returns>
        public static long? Crc32(byte[] data)
        {
            return MarshalHelpers.IntResultToNullable(
                LibProven.proven_checksum_crc32(data, (nuint)data.Length));
        }

        /// <summary>
        /// Verify that a CRC32 checksum matches the expected value.
        /// Delegates to proven_checksum_verify_crc32 via FFI.
        /// </summary>
        /// <param name="data">Byte data to verify.</param>
        /// <param name="expected">Expected CRC32 value.</param>
        /// <returns>true if CRC32 matches, false if not, null on FFI error.</returns>
        public static bool? VerifyCrc32(byte[] data, uint expected)
        {
            return MarshalHelpers.BoolResultToNullable(
                LibProven.proven_checksum_verify_crc32(data, (nuint)data.Length, expected));
        }
    }
}
