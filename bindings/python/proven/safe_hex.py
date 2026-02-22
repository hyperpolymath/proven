# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeHex - Hexadecimal operations that cannot crash.

Provides safe hex encoding, decoding, and formatting without exceptions.
All encoding/decoding operations delegate to the Idris core via FFI.
"""

from __future__ import annotations

import ctypes
from dataclasses import dataclass
from enum import Enum
from typing import Optional, Union, Iterator

from .core import ProvenStatus, ProvenError, get_lib, check_status


class HexCase(Enum):
    """Hex output case."""
    LOWER = "lower"
    UPPER = "upper"


class HexFormat(Enum):
    """Hex output format styles."""
    PLAIN = "plain"           # "deadbeef"
    PREFIXED = "prefixed"     # "0xdeadbeef"
    SPACED = "spaced"         # "de ad be ef"
    COLON = "colon"           # "de:ad:be:ef"
    GROUPED = "grouped"       # "dead beef"


@dataclass(frozen=True)
class SafeHex:
    """
    A hexadecimal-encoded byte sequence with safety guarantees.

    Encoding and decoding are delegated to the Idris core via FFI.

    Attributes:
        data: The underlying bytes
    """

    data: bytes

    @classmethod
    def from_bytes(cls, data: bytes) -> SafeHex:
        """Create SafeHex from bytes."""
        return cls(data=data)

    @classmethod
    def decode(cls, hex_string: str) -> Optional[SafeHex]:
        """
        Decode a hex string to SafeHex via FFI.

        Args:
            hex_string: The hex string to decode

        Returns:
            SafeHex if valid, None otherwise
        """
        if not hex_string:
            return cls(data=b"")

        lib = get_lib()
        encoded = hex_string.encode("utf-8")
        result = lib.proven_hex_decode(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return None
        if result.data is None:
            return cls(data=b"")
        decoded = result.data[:result.length]
        lib.proven_free_string(result.data)
        return cls(data=bytes(decoded))

    @classmethod
    def from_int(cls, value: int, byte_length: Optional[int] = None,
                 signed: bool = False) -> Optional[SafeHex]:
        """
        Create SafeHex from an integer.

        Args:
            value: The integer to encode
            byte_length: Number of bytes (calculated if None)
            signed: Whether to use signed encoding

        Returns:
            SafeHex if successful, None if value doesn't fit
        """
        try:
            if byte_length is None:
                if value == 0:
                    byte_length = 1
                elif signed:
                    byte_length = (value.bit_length() + 8) // 8
                else:
                    byte_length = (value.bit_length() + 7) // 8

            data = value.to_bytes(byte_length, byteorder="big", signed=signed)
            return cls(data=data)
        except (OverflowError, ValueError):
            return None

    def encode(self, case: HexCase = HexCase.LOWER) -> str:
        """
        Encode to plain hex string via FFI.

        Args:
            case: Output case (LOWER or UPPER)

        Returns:
            Hex string representation
        """
        lib = get_lib()
        upper = case == HexCase.UPPER
        result = lib.proven_hex_encode(self.data, len(self.data), upper)
        if result.status != ProvenStatus.OK or result.value is None:
            # Fallback should not happen for valid data
            return self.data.hex()
        output = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return output

    def format(
        self,
        style: HexFormat = HexFormat.PLAIN,
        case: HexCase = HexCase.LOWER,
        group_size: int = 2
    ) -> str:
        """
        Format hex string with various styles.

        Args:
            style: Output format style
            case: Output case
            group_size: Bytes per group (for GROUPED style)

        Returns:
            Formatted hex string
        """
        hex_str = self.encode(case)

        if style == HexFormat.PLAIN:
            return hex_str
        elif style == HexFormat.PREFIXED:
            prefix = "0x" if case == HexCase.LOWER else "0X"
            return f"{prefix}{hex_str}"
        elif style == HexFormat.SPACED:
            pairs = [hex_str[i:i+2] for i in range(0, len(hex_str), 2)]
            return " ".join(pairs)
        elif style == HexFormat.COLON:
            pairs = [hex_str[i:i+2] for i in range(0, len(hex_str), 2)]
            return ":".join(pairs)
        elif style == HexFormat.GROUPED:
            chunk_chars = group_size * 2
            groups = [hex_str[i:i+chunk_chars] for i in range(0, len(hex_str), chunk_chars)]
            return " ".join(groups)

        return hex_str

    def to_int(self, signed: bool = False) -> int:
        """Convert to integer."""
        return int.from_bytes(self.data, byteorder="big", signed=signed)

    def to_bytes(self) -> bytes:
        """Get the underlying bytes."""
        return self.data

    @property
    def length(self) -> int:
        """Get the length in bytes."""
        return len(self.data)

    @property
    def hex_length(self) -> int:
        """Get the length of the hex string (2 chars per byte)."""
        return len(self.data) * 2

    def slice(self, start: int, end: Optional[int] = None) -> SafeHex:
        """Slice the hex data."""
        if end is None:
            return SafeHex(data=self.data[start:])
        return SafeHex(data=self.data[start:end])

    def concat(self, other: SafeHex) -> SafeHex:
        """Concatenate with another SafeHex."""
        return SafeHex(data=self.data + other.data)

    def xor(self, other: SafeHex) -> Optional[SafeHex]:
        """XOR with another SafeHex (must be same length)."""
        if len(self.data) != len(other.data):
            return None
        result = bytes(a ^ b for a, b in zip(self.data, other.data))
        return SafeHex(data=result)

    def reverse(self) -> SafeHex:
        """Reverse the byte order."""
        return SafeHex(data=self.data[::-1])

    def pad_left(self, total_length: int, pad_byte: int = 0) -> SafeHex:
        """Pad on the left (big-endian padding)."""
        if len(self.data) >= total_length:
            return self
        pad_count = total_length - len(self.data)
        padding = bytes([pad_byte & 0xFF]) * pad_count
        return SafeHex(data=padding + self.data)

    def pad_right(self, total_length: int, pad_byte: int = 0) -> SafeHex:
        """Pad on the right (little-endian padding)."""
        if len(self.data) >= total_length:
            return self
        pad_count = total_length - len(self.data)
        padding = bytes([pad_byte & 0xFF]) * pad_count
        return SafeHex(data=self.data + padding)

    def chunks(self, chunk_size: int) -> Iterator[SafeHex]:
        """Iterate over chunks of the data."""
        for i in range(0, len(self.data), chunk_size):
            yield SafeHex(data=self.data[i:i+chunk_size])

    def starts_with(self, prefix: Union[SafeHex, bytes, str]) -> bool:
        """Check if data starts with prefix."""
        if isinstance(prefix, str):
            decoded = SafeHex.decode(prefix)
            if decoded is None:
                return False
            prefix_bytes = decoded.data
        elif isinstance(prefix, SafeHex):
            prefix_bytes = prefix.data
        else:
            prefix_bytes = prefix
        return self.data.startswith(prefix_bytes)

    def ends_with(self, suffix: Union[SafeHex, bytes, str]) -> bool:
        """Check if data ends with suffix."""
        if isinstance(suffix, str):
            decoded = SafeHex.decode(suffix)
            if decoded is None:
                return False
            suffix_bytes = decoded.data
        elif isinstance(suffix, SafeHex):
            suffix_bytes = suffix.data
        else:
            suffix_bytes = suffix
        return self.data.endswith(suffix_bytes)

    def __str__(self) -> str:
        return self.encode()

    def __repr__(self) -> str:
        return f"SafeHex(data={self.data!r})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, SafeHex):
            return self.data == other.data
        if isinstance(other, bytes):
            return self.data == other
        return NotImplemented

    def __hash__(self) -> int:
        return hash(self.data)

    def __len__(self) -> int:
        return len(self.data)

    def __bool__(self) -> bool:
        return len(self.data) > 0

    def __add__(self, other: SafeHex) -> SafeHex:
        return self.concat(other)

    def __xor__(self, other: SafeHex) -> SafeHex:
        result = self.xor(other)
        if result is None:
            raise ValueError(
                f"Cannot XOR: lengths differ ({len(self.data)} vs {len(other.data)})"
            )
        return result

    def __getitem__(self, key: Union[int, slice]) -> Union[int, SafeHex]:
        if isinstance(key, slice):
            return SafeHex(data=self.data[key])
        return self.data[key]

    def __iter__(self) -> Iterator[int]:
        return iter(self.data)

    def __contains__(self, item: Union[int, bytes, SafeHex]) -> bool:
        if isinstance(item, int):
            return item in self.data
        if isinstance(item, SafeHex):
            return item.data in self.data
        return item in self.data
