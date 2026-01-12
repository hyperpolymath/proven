# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeCrypto - Cryptographic operations that cannot crash.

Provides timing-safe comparisons and secure random bytes.
"""

from typing import Optional

from .core import ProvenStatus, ProvenError, get_lib, check_status


class SafeCrypto:
    """Safe cryptographic operations with proven correctness guarantees."""

    @staticmethod
    def constant_time_eq(a: bytes, b: bytes) -> bool:
        """
        Compare two byte sequences in constant time.

        Prevents timing attacks by ensuring comparison takes the same
        time regardless of where differences occur.

        Args:
            a: First byte sequence
            b: Second byte sequence

        Returns:
            True if equal, False otherwise

        Example:
            >>> SafeCrypto.constant_time_eq(b"secret", b"secret")
            True
            >>> SafeCrypto.constant_time_eq(b"secret", b"public")
            False
        """
        lib = get_lib()
        result = lib.proven_crypto_constant_time_eq(a, len(a), b, len(b))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    @staticmethod
    def secure_compare(a: str, b: str) -> bool:
        """
        Compare two strings in constant time.

        Convenience wrapper for constant_time_eq with string encoding.

        Args:
            a: First string
            b: Second string

        Returns:
            True if equal, False otherwise
        """
        return SafeCrypto.constant_time_eq(a.encode("utf-8"), b.encode("utf-8"))

    @staticmethod
    def random_bytes(length: int) -> Optional[bytes]:
        """
        Generate cryptographically secure random bytes.

        Uses the OS's secure random source (e.g., /dev/urandom).

        Args:
            length: Number of bytes to generate

        Returns:
            Random bytes, or None on error

        Example:
            >>> key = SafeCrypto.random_bytes(32)
            >>> len(key)
            32
        """
        if length <= 0:
            return None
        if length > 1024 * 1024:  # 1MB limit
            return None

        lib = get_lib()
        buffer = (ctypes.c_char * length)()
        status = lib.proven_crypto_random_bytes(buffer, length)
        if status != ProvenStatus.OK:
            return None
        return bytes(buffer)

    @staticmethod
    def random_hex(length: int) -> Optional[str]:
        """
        Generate a random hex string.

        Args:
            length: Number of hex characters (must be even)

        Returns:
            Random hex string, or None on error

        Example:
            >>> token = SafeCrypto.random_hex(32)
            >>> len(token)
            32
        """
        if length <= 0 or length % 2 != 0:
            return None

        random_bytes = SafeCrypto.random_bytes(length // 2)
        if random_bytes is None:
            return None
        return random_bytes.hex()


# Import ctypes for random_bytes buffer allocation
import ctypes
