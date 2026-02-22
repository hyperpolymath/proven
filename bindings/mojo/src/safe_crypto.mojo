# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeCrypto -- Timing-safe cryptographic primitives.

Provides constant-time byte comparison (resistant to timing attacks) and
cryptographically secure random byte generation. All computation is delegated
to the formally verified Idris 2 core via C FFI. No logic is reimplemented.

Functions:
    constant_time_eq -- Constant-time byte comparison (timing-attack safe)
    random_bytes     -- Fill a buffer with cryptographically secure random bytes
    checksum_crc32   -- Calculate CRC32 checksum
    verify_crc32     -- Verify CRC32 matches expected value
"""

from memory import UnsafePointer

from .lib_proven import (
    BoolResult,
    IntResult,
    proven_crypto_constant_time_eq,
    proven_crypto_random_bytes,
    proven_checksum_crc32,
    proven_checksum_verify_crc32,
    proven_hex_encode,
    proven_free_string,
    string_result_to_string,
    PROVEN_OK,
)


fn _str_to_ptr(s: String) -> (UnsafePointer[UInt8], Int):
    """Extract a raw byte pointer and length from a Mojo String."""
    var byte_slice = s.as_bytes()
    var length = len(byte_slice)
    var ptr = byte_slice.unsafe_ptr()
    return (ptr, length)


fn constant_time_eq(a: String, b: String) -> Bool:
    """Compare two byte strings in constant time (timing-attack resistant).

    Returns True if the byte contents are identical, False otherwise.
    Returns False if the lengths differ (without leaking which bytes differ).
    On error, returns False as a safe default.
    All computation performed in formally verified Idris 2 code.
    """
    var pair_a = _str_to_ptr(a)
    var pair_b = _str_to_ptr(b)
    var result = proven_crypto_constant_time_eq(
        pair_a[0], pair_a[1], pair_b[0], pair_b[1]
    )
    if result.succeeded():
        return result.value
    return False


fn random_bytes(count: Int) -> Optional[List[UInt8]]:
    """Generate cryptographically secure random bytes.

    Args:
        count: Number of random bytes to generate. Must be > 0.

    Returns a List[UInt8] of `count` random bytes, or None on error.
    All computation performed in formally verified Idris 2 code.
    """
    if count <= 0:
        return None

    var buf = List[UInt8](capacity=count)
    # Pre-fill list so the memory is allocated
    for _ in range(count):
        buf.append(0)

    var status = proven_crypto_random_bytes(buf.unsafe_ptr(), count)
    if status != PROVEN_OK:
        return None

    return buf


fn random_bytes_hex(count: Int, uppercase: Bool = False) -> Optional[String]:
    """Generate random bytes and return them as a hex-encoded string.

    Args:
        count: Number of random bytes to generate (hex output is 2x this).
        uppercase: If True, use uppercase hex digits.

    Returns None on error.
    All computation performed in formally verified Idris 2 code.
    """
    if count <= 0:
        return None

    var buf = List[UInt8](capacity=count)
    for _ in range(count):
        buf.append(0)

    var status = proven_crypto_random_bytes(buf.unsafe_ptr(), count)
    if status != PROVEN_OK:
        return None

    var hex_result = proven_hex_encode(buf.unsafe_ptr(), count, uppercase)
    return string_result_to_string(hex_result)


fn checksum_crc32(data: String) -> Optional[Int64]:
    """Calculate CRC32 checksum of data.

    Returns the checksum value, or None on error.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(data)
    var result = proven_checksum_crc32(pair[0], pair[1])
    if result.succeeded():
        return result.value
    return None


fn verify_crc32(data: String, expected: UInt32) -> Bool:
    """Verify CRC32 matches an expected value.

    Returns True if the checksum matches, False otherwise.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(data)
    var result = proven_checksum_verify_crc32(pair[0], pair[1], expected)
    if result.succeeded():
        return result.value
    return False
