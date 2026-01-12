# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeHex - Hexadecimal operations that cannot crash.

Provides safe hex encoding, decoding, and formatting without exceptions.
All operations return None on failure instead of raising.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Optional, Union, Iterator


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

    Provides safe encoding, decoding, and formatting operations that
    never raise exceptions. Invalid operations return None.

    Attributes:
        data: The underlying bytes

    Example:
        >>> hex_val = SafeHex.from_bytes(b"\\xde\\xad\\xbe\\xef")
        >>> hex_val.encode()
        'deadbeef'
        >>> SafeHex.decode("deadbeef")
        SafeHex(data=b'\\xde\\xad\\xbe\\xef')
    """

    data: bytes

    @classmethod
    def from_bytes(cls, data: bytes) -> SafeHex:
        """
        Create SafeHex from bytes.

        Args:
            data: The bytes to wrap

        Returns:
            SafeHex instance

        Example:
            >>> SafeHex.from_bytes(b"hello")
            SafeHex(data=b'hello')
        """
        return cls(data=data)

    @classmethod
    def decode(cls, hex_string: str) -> Optional[SafeHex]:
        """
        Decode a hex string to SafeHex.

        Accepts formats:
        - Plain: "deadbeef"
        - With prefix: "0x..." or "0X..."
        - With spaces: "de ad be ef"
        - With colons: "de:ad:be:ef"
        - With hyphens: "de-ad-be-ef"

        Args:
            hex_string: The hex string to decode

        Returns:
            SafeHex if valid, None otherwise

        Example:
            >>> SafeHex.decode("0xDEADBEEF")
            SafeHex(data=b'\\xde\\xad\\xbe\\xef')
            >>> SafeHex.decode("not-hex")
            None
        """
        if not hex_string:
            return cls(data=b"")

        # Normalize: remove prefix, separators, and whitespace
        normalized = hex_string.strip()

        # Remove 0x prefix
        if normalized.lower().startswith("0x"):
            normalized = normalized[2:]

        # Remove common separators
        normalized = normalized.replace(" ", "").replace(":", "").replace("-", "")

        # Validate length (must be even)
        if len(normalized) % 2 != 0:
            return None

        # Validate and decode
        try:
            decoded = bytes.fromhex(normalized)
            return cls(data=decoded)
        except ValueError:
            return None

    @classmethod
    def from_int(cls, value: int, byte_length: Optional[int] = None, signed: bool = False) -> Optional[SafeHex]:
        """
        Create SafeHex from an integer.

        Args:
            value: The integer to encode
            byte_length: Number of bytes (calculated if None)
            signed: Whether to use signed encoding

        Returns:
            SafeHex if successful, None if value doesn't fit

        Example:
            >>> SafeHex.from_int(255)
            SafeHex(data=b'\\xff')
            >>> SafeHex.from_int(256, byte_length=2)
            SafeHex(data=b'\\x01\\x00')
        """
        try:
            if byte_length is None:
                # Calculate minimum bytes needed
                if value == 0:
                    byte_length = 1
                elif signed:
                    # For signed, need to account for sign bit
                    byte_length = (value.bit_length() + 8) // 8
                else:
                    byte_length = (value.bit_length() + 7) // 8

            data = value.to_bytes(byte_length, byteorder="big", signed=signed)
            return cls(data=data)
        except (OverflowError, ValueError):
            return None

    def encode(self, case: HexCase = HexCase.LOWER) -> str:
        """
        Encode to plain hex string.

        Args:
            case: Output case (LOWER or UPPER)

        Returns:
            Hex string representation

        Example:
            >>> SafeHex.from_bytes(b"\\xde\\xad").encode()
            'dead'
            >>> SafeHex.from_bytes(b"\\xde\\xad").encode(HexCase.UPPER)
            'DEAD'
        """
        hex_str = self.data.hex()
        if case == HexCase.UPPER:
            return hex_str.upper()
        return hex_str

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

        Example:
            >>> hex_val = SafeHex.from_bytes(b"\\xde\\xad\\xbe\\xef")
            >>> hex_val.format(HexFormat.PREFIXED)
            '0xdeadbeef'
            >>> hex_val.format(HexFormat.COLON, HexCase.UPPER)
            'DE:AD:BE:EF'
        """
        hex_str = self.encode(case)

        if style == HexFormat.PLAIN:
            return hex_str

        elif style == HexFormat.PREFIXED:
            prefix = "0x" if case == HexCase.LOWER else "0X"
            return f"{prefix}{hex_str}"

        elif style == HexFormat.SPACED:
            # Group into byte pairs with spaces
            pairs = [hex_str[i:i+2] for i in range(0, len(hex_str), 2)]
            return " ".join(pairs)

        elif style == HexFormat.COLON:
            # Group into byte pairs with colons
            pairs = [hex_str[i:i+2] for i in range(0, len(hex_str), 2)]
            return ":".join(pairs)

        elif style == HexFormat.GROUPED:
            # Group into chunks with spaces
            chunk_chars = group_size * 2
            groups = [hex_str[i:i+chunk_chars] for i in range(0, len(hex_str), chunk_chars)]
            return " ".join(groups)

        return hex_str

    def to_int(self, signed: bool = False) -> int:
        """
        Convert to integer.

        Args:
            signed: Whether to interpret as signed integer

        Returns:
            Integer value

        Example:
            >>> SafeHex.from_bytes(b"\\xff").to_int()
            255
            >>> SafeHex.from_bytes(b"\\xff").to_int(signed=True)
            -1
        """
        return int.from_bytes(self.data, byteorder="big", signed=signed)

    def to_bytes(self) -> bytes:
        """
        Get the underlying bytes.

        Returns:
            The bytes data
        """
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
        """
        Slice the hex data.

        Args:
            start: Start byte index
            end: End byte index (exclusive)

        Returns:
            New SafeHex with sliced data

        Example:
            >>> SafeHex.decode("deadbeef").slice(1, 3)
            SafeHex(data=b'\\xad\\xbe')
        """
        if end is None:
            return SafeHex(data=self.data[start:])
        return SafeHex(data=self.data[start:end])

    def concat(self, other: SafeHex) -> SafeHex:
        """
        Concatenate with another SafeHex.

        Args:
            other: SafeHex to append

        Returns:
            New SafeHex with concatenated data

        Example:
            >>> a = SafeHex.decode("dead")
            >>> b = SafeHex.decode("beef")
            >>> a.concat(b)
            SafeHex(data=b'\\xde\\xad\\xbe\\xef')
        """
        return SafeHex(data=self.data + other.data)

    def xor(self, other: SafeHex) -> Optional[SafeHex]:
        """
        XOR with another SafeHex.

        Args:
            other: SafeHex to XOR with (must be same length)

        Returns:
            New SafeHex with XORed data, or None if lengths differ

        Example:
            >>> a = SafeHex.decode("ff00")
            >>> b = SafeHex.decode("0f0f")
            >>> a.xor(b).encode()
            'f00f'
        """
        if len(self.data) != len(other.data):
            return None

        result = bytes(a ^ b for a, b in zip(self.data, other.data))
        return SafeHex(data=result)

    def reverse(self) -> SafeHex:
        """
        Reverse the byte order.

        Returns:
            New SafeHex with reversed bytes

        Example:
            >>> SafeHex.decode("deadbeef").reverse().encode()
            'efbeadde'
        """
        return SafeHex(data=self.data[::-1])

    def pad_left(self, total_length: int, pad_byte: int = 0) -> SafeHex:
        """
        Pad on the left (big-endian padding).

        Args:
            total_length: Desired total length in bytes
            pad_byte: Byte value to pad with (0-255)

        Returns:
            Padded SafeHex (or original if already >= total_length)

        Example:
            >>> SafeHex.decode("ff").pad_left(4).encode()
            '000000ff'
        """
        if len(self.data) >= total_length:
            return self

        pad_count = total_length - len(self.data)
        padding = bytes([pad_byte & 0xFF]) * pad_count
        return SafeHex(data=padding + self.data)

    def pad_right(self, total_length: int, pad_byte: int = 0) -> SafeHex:
        """
        Pad on the right (little-endian padding).

        Args:
            total_length: Desired total length in bytes
            pad_byte: Byte value to pad with (0-255)

        Returns:
            Padded SafeHex (or original if already >= total_length)

        Example:
            >>> SafeHex.decode("ff").pad_right(4).encode()
            'ff000000'
        """
        if len(self.data) >= total_length:
            return self

        pad_count = total_length - len(self.data)
        padding = bytes([pad_byte & 0xFF]) * pad_count
        return SafeHex(data=self.data + padding)

    def chunks(self, chunk_size: int) -> Iterator[SafeHex]:
        """
        Iterate over chunks of the data.

        Args:
            chunk_size: Size of each chunk in bytes

        Yields:
            SafeHex for each chunk

        Example:
            >>> list(SafeHex.decode("deadbeef").chunks(2))
            [SafeHex(data=b'\\xde\\xad'), SafeHex(data=b'\\xbe\\xef')]
        """
        for i in range(0, len(self.data), chunk_size):
            yield SafeHex(data=self.data[i:i+chunk_size])

    def starts_with(self, prefix: Union[SafeHex, bytes, str]) -> bool:
        """
        Check if data starts with prefix.

        Args:
            prefix: Prefix to check (SafeHex, bytes, or hex string)

        Returns:
            True if data starts with prefix
        """
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
        """
        Check if data ends with suffix.

        Args:
            suffix: Suffix to check (SafeHex, bytes, or hex string)

        Returns:
            True if data ends with suffix
        """
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
        """Return plain lowercase hex string."""
        return self.encode()

    def __repr__(self) -> str:
        """Return detailed representation."""
        return f"SafeHex(data={self.data!r})"

    def __eq__(self, other: object) -> bool:
        """Check equality."""
        if isinstance(other, SafeHex):
            return self.data == other.data
        if isinstance(other, bytes):
            return self.data == other
        return NotImplemented

    def __hash__(self) -> int:
        """Return hash for use in sets and dicts."""
        return hash(self.data)

    def __len__(self) -> int:
        """Return length in bytes."""
        return len(self.data)

    def __bool__(self) -> bool:
        """Return True if non-empty."""
        return len(self.data) > 0

    def __add__(self, other: SafeHex) -> SafeHex:
        """Concatenate using + operator."""
        return self.concat(other)

    def __xor__(self, other: SafeHex) -> SafeHex:
        """XOR using ^ operator (raises if lengths differ)."""
        result = self.xor(other)
        if result is None:
            raise ValueError(f"Cannot XOR: lengths differ ({len(self.data)} vs {len(other.data)})")
        return result

    def __getitem__(self, key: Union[int, slice]) -> Union[int, SafeHex]:
        """
        Get byte by index or slice.

        Args:
            key: Index or slice

        Returns:
            Byte value (int) for index, SafeHex for slice
        """
        if isinstance(key, slice):
            return SafeHex(data=self.data[key])
        return self.data[key]

    def __iter__(self) -> Iterator[int]:
        """Iterate over bytes."""
        return iter(self.data)

    def __contains__(self, item: Union[int, bytes, SafeHex]) -> bool:
        """Check if byte or sequence is contained."""
        if isinstance(item, int):
            return item in self.data
        if isinstance(item, SafeHex):
            return item.data in self.data
        return item in self.data
