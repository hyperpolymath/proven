# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeChecksum - Various checksum and hash functions.

Provides CRC-32, Adler-32, FNV, Luhn, and other checksum algorithms.
"""

from typing import Union


def crc32(data: Union[str, bytes]) -> int:
    """
    Calculate CRC-32 checksum.

    Args:
        data: Data to checksum

    Returns:
        32-bit CRC value

    Example:
        >>> hex(crc32("hello"))
        '0x3610a686'
    """
    if isinstance(data, str):
        data = data.encode("utf-8")

    # CRC-32 polynomial (IEEE 802.3)
    crc = 0xFFFFFFFF
    polynomial = 0xEDB88320

    for byte in data:
        crc ^= byte
        for _ in range(8):
            if crc & 1:
                crc = (crc >> 1) ^ polynomial
            else:
                crc >>= 1

    return crc ^ 0xFFFFFFFF


def adler32(data: Union[str, bytes]) -> int:
    """
    Calculate Adler-32 checksum.

    Args:
        data: Data to checksum

    Returns:
        32-bit Adler checksum

    Example:
        >>> hex(adler32("hello"))
        '0x62c0215'
    """
    if isinstance(data, str):
        data = data.encode("utf-8")

    MOD_ADLER = 65521
    a = 1
    b = 0

    for byte in data:
        a = (a + byte) % MOD_ADLER
        b = (b + a) % MOD_ADLER

    return (b << 16) | a


def fletcher16(data: Union[str, bytes]) -> int:
    """
    Calculate Fletcher-16 checksum.

    Args:
        data: Data to checksum

    Returns:
        16-bit Fletcher checksum
    """
    if isinstance(data, str):
        data = data.encode("utf-8")

    sum1 = 0
    sum2 = 0

    for byte in data:
        sum1 = (sum1 + byte) % 255
        sum2 = (sum2 + sum1) % 255

    return (sum2 << 8) | sum1


def fnv1a_64(data: Union[str, bytes]) -> int:
    """
    Calculate FNV-1a 64-bit hash.

    Args:
        data: Data to hash

    Returns:
        64-bit FNV-1a hash

    Example:
        >>> hex(fnv1a_64("hello"))
        '0xa430d84680aabd0b'
    """
    if isinstance(data, str):
        data = data.encode("utf-8")

    FNV_OFFSET = 0xcbf29ce484222325
    FNV_PRIME = 0x100000001b3

    hash_value = FNV_OFFSET
    for byte in data:
        hash_value ^= byte
        hash_value = (hash_value * FNV_PRIME) & 0xFFFFFFFFFFFFFFFF

    return hash_value


def fnv1a_32(data: Union[str, bytes]) -> int:
    """
    Calculate FNV-1a 32-bit hash.

    Args:
        data: Data to hash

    Returns:
        32-bit FNV-1a hash
    """
    if isinstance(data, str):
        data = data.encode("utf-8")

    FNV_OFFSET = 0x811c9dc5
    FNV_PRIME = 0x01000193

    hash_value = FNV_OFFSET
    for byte in data:
        hash_value ^= byte
        hash_value = (hash_value * FNV_PRIME) & 0xFFFFFFFF

    return hash_value


def djb2(data: Union[str, bytes]) -> int:
    """
    Calculate DJB2 hash.

    Args:
        data: Data to hash

    Returns:
        DJB2 hash value
    """
    if isinstance(data, str):
        data = data.encode("utf-8")

    hash_value = 5381
    for byte in data:
        hash_value = ((hash_value << 5) + hash_value + byte) & 0xFFFFFFFF

    return hash_value


def sdbm(data: Union[str, bytes]) -> int:
    """
    Calculate SDBM hash.

    Args:
        data: Data to hash

    Returns:
        SDBM hash value
    """
    if isinstance(data, str):
        data = data.encode("utf-8")

    hash_value = 0
    for byte in data:
        hash_value = byte + (hash_value << 6) + (hash_value << 16) - hash_value
        hash_value &= 0xFFFFFFFF

    return hash_value


def luhn_check(digits: str) -> bool:
    """
    Validate a number using Luhn algorithm.

    Used for credit card validation, IMEI, etc.

    Args:
        digits: String of digits to validate

    Returns:
        True if valid Luhn checksum

    Example:
        >>> luhn_check("79927398713")  # Valid
        True
        >>> luhn_check("79927398710")  # Invalid
        False
    """
    if not digits or not digits.isdigit():
        return False

    total = 0
    is_second = False

    # Process from right to left
    for i in range(len(digits) - 1, -1, -1):
        d = int(digits[i])

        if is_second:
            d *= 2
            if d > 9:
                d -= 9

        total += d
        is_second = not is_second

    return total % 10 == 0


def luhn_generate_check_digit(digits: str) -> str:
    """
    Generate Luhn check digit for a number.

    Args:
        digits: String of digits (without check digit)

    Returns:
        The check digit to append

    Example:
        >>> luhn_generate_check_digit("7992739871")
        '3'
    """
    if not digits or not digits.isdigit():
        return ""

    # Try each digit 0-9
    for check in range(10):
        if luhn_check(digits + str(check)):
            return str(check)

    return ""


def xor_checksum(data: Union[str, bytes]) -> int:
    """
    Calculate XOR checksum (single byte).

    Args:
        data: Data to checksum

    Returns:
        8-bit XOR checksum
    """
    if isinstance(data, str):
        data = data.encode("utf-8")

    result = 0
    for byte in data:
        result ^= byte
    return result


class SafeChecksum:
    """Safe checksum utilities."""

    crc32 = staticmethod(crc32)
    adler32 = staticmethod(adler32)
    fletcher16 = staticmethod(fletcher16)
    fnv1a_64 = staticmethod(fnv1a_64)
    fnv1a_32 = staticmethod(fnv1a_32)
    djb2 = staticmethod(djb2)
    sdbm = staticmethod(sdbm)
    luhn_check = staticmethod(luhn_check)
    luhn_generate_check_digit = staticmethod(luhn_generate_check_digit)
    xor_checksum = staticmethod(xor_checksum)
