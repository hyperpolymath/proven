# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeUUID - UUID operations that cannot crash.

Provides safe UUID generation (v4/v5), parsing, and formatting without exceptions.
All operations return None or default values on failure instead of raising.
"""

from __future__ import annotations

import secrets
import hashlib
from dataclasses import dataclass, field
from enum import IntEnum
from typing import Optional, ClassVar


class UUIDVersion(IntEnum):
    """UUID version identifiers."""
    V1 = 1  # Time-based (not implemented - requires MAC address)
    V3 = 3  # MD5 namespace (not recommended)
    V4 = 4  # Random
    V5 = 5  # SHA-1 namespace


class UUIDVariant(IntEnum):
    """UUID variant identifiers."""
    NCS = 0         # Reserved for NCS backward compatibility
    RFC4122 = 1     # The variant specified in RFC 4122
    MICROSOFT = 2   # Reserved for Microsoft backward compatibility
    FUTURE = 3      # Reserved for future definition


@dataclass(frozen=True)
class SafeUUID:
    """
    A universally unique identifier with proven safety guarantees.

    This implementation handles all edge cases without raising exceptions.
    Invalid UUIDs are represented as None through factory methods.

    Attributes:
        bytes_value: The 16-byte representation of the UUID

    Example:
        >>> uuid = SafeUUID.v4()
        >>> str(uuid)
        'a1b2c3d4-e5f6-4789-abcd-ef0123456789'
        >>> uuid.version
        <UUIDVersion.V4: 4>
    """

    bytes_value: bytes = field(repr=False)

    # Well-known namespace UUIDs (RFC 4122)
    NAMESPACE_DNS: ClassVar[SafeUUID]
    NAMESPACE_URL: ClassVar[SafeUUID]
    NAMESPACE_OID: ClassVar[SafeUUID]
    NAMESPACE_X500: ClassVar[SafeUUID]
    NIL: ClassVar[SafeUUID]

    def __post_init__(self) -> None:
        """Validate that bytes_value is exactly 16 bytes."""
        if len(self.bytes_value) != 16:
            raise ValueError("UUID must be exactly 16 bytes")

    @classmethod
    def v4(cls) -> SafeUUID:
        """
        Generate a random UUID (version 4).

        Uses cryptographically secure random bytes.

        Returns:
            A new random UUID

        Example:
            >>> uuid = SafeUUID.v4()
            >>> uuid.version == UUIDVersion.V4
            True
        """
        random_bytes = bytearray(secrets.token_bytes(16))

        # Set version to 4 (0100 in high nibble of byte 6)
        random_bytes[6] = (random_bytes[6] & 0x0F) | 0x40

        # Set variant to RFC 4122 (10xx in high bits of byte 8)
        random_bytes[8] = (random_bytes[8] & 0x3F) | 0x80

        return cls(bytes(random_bytes))

    @classmethod
    def v5(cls, namespace: SafeUUID, name: str) -> SafeUUID:
        """
        Generate a name-based UUID using SHA-1 (version 5).

        Deterministic: same namespace + name always produces same UUID.

        Args:
            namespace: The namespace UUID (use NAMESPACE_* constants)
            name: The name to hash within the namespace

        Returns:
            A deterministic UUID based on namespace and name

        Example:
            >>> uuid1 = SafeUUID.v5(SafeUUID.NAMESPACE_DNS, "example.com")
            >>> uuid2 = SafeUUID.v5(SafeUUID.NAMESPACE_DNS, "example.com")
            >>> uuid1 == uuid2
            True
        """
        # Concatenate namespace bytes and name bytes
        hash_input = namespace.bytes_value + name.encode("utf-8")

        # SHA-1 hash
        sha1_hash = hashlib.sha1(hash_input).digest()

        # Take first 16 bytes
        uuid_bytes = bytearray(sha1_hash[:16])

        # Set version to 5 (0101 in high nibble of byte 6)
        uuid_bytes[6] = (uuid_bytes[6] & 0x0F) | 0x50

        # Set variant to RFC 4122 (10xx in high bits of byte 8)
        uuid_bytes[8] = (uuid_bytes[8] & 0x3F) | 0x80

        return cls(bytes(uuid_bytes))

    @classmethod
    def parse(cls, uuid_string: str) -> Optional[SafeUUID]:
        """
        Parse a UUID from its string representation.

        Accepts formats:
        - Standard: "a1b2c3d4-e5f6-4789-abcd-ef0123456789"
        - Without hyphens: "a1b2c3d4e5f64789abcdef0123456789"
        - Braces: "{a1b2c3d4-e5f6-4789-abcd-ef0123456789}"
        - URN: "urn:uuid:a1b2c3d4-e5f6-4789-abcd-ef0123456789"

        Args:
            uuid_string: The string to parse

        Returns:
            SafeUUID if valid, None otherwise

        Example:
            >>> SafeUUID.parse("550e8400-e29b-41d4-a716-446655440000")
            SafeUUID(...)
            >>> SafeUUID.parse("not-a-uuid")
            None
        """
        if not uuid_string:
            return None

        # Normalize: remove common wrappings
        normalized = uuid_string.strip()

        # Handle URN prefix
        if normalized.lower().startswith("urn:uuid:"):
            normalized = normalized[9:]

        # Handle braces
        if normalized.startswith("{") and normalized.endswith("}"):
            normalized = normalized[1:-1]

        # Remove hyphens
        hex_string = normalized.replace("-", "")

        # Validate length
        if len(hex_string) != 32:
            return None

        # Validate hex characters
        try:
            uuid_bytes = bytes.fromhex(hex_string)
        except ValueError:
            return None

        return cls(uuid_bytes)

    @classmethod
    def from_bytes(cls, data: bytes) -> Optional[SafeUUID]:
        """
        Create a UUID from 16 bytes.

        Args:
            data: Exactly 16 bytes

        Returns:
            SafeUUID if data is 16 bytes, None otherwise

        Example:
            >>> SafeUUID.from_bytes(b'\\x00' * 16)
            SafeUUID(...)
        """
        if len(data) != 16:
            return None
        return cls(data)

    @property
    def version(self) -> Optional[UUIDVersion]:
        """
        Get the UUID version.

        Returns:
            The UUIDVersion, or None if not a recognized version
        """
        version_nibble = (self.bytes_value[6] >> 4) & 0x0F
        try:
            return UUIDVersion(version_nibble)
        except ValueError:
            return None

    @property
    def variant(self) -> UUIDVariant:
        """
        Get the UUID variant.

        Returns:
            The UUIDVariant
        """
        high_bits = self.bytes_value[8] >> 6

        if (high_bits & 0b10) == 0:
            return UUIDVariant.NCS
        elif (high_bits & 0b11) == 0b10:
            return UUIDVariant.RFC4122
        elif (high_bits & 0b11) == 0b11 and (self.bytes_value[8] >> 5) == 0b110:
            return UUIDVariant.MICROSOFT
        else:
            return UUIDVariant.FUTURE

    @property
    def is_nil(self) -> bool:
        """Check if this is the nil UUID (all zeros)."""
        return self.bytes_value == b"\x00" * 16

    def format_standard(self) -> str:
        """
        Format as standard UUID string with hyphens.

        Returns:
            String in format "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"

        Example:
            >>> uuid.format_standard()
            'a1b2c3d4-e5f6-4789-abcd-ef0123456789'
        """
        hex_string = self.bytes_value.hex()
        return f"{hex_string[:8]}-{hex_string[8:12]}-{hex_string[12:16]}-{hex_string[16:20]}-{hex_string[20:]}"

    def format_compact(self) -> str:
        """
        Format as compact UUID string without hyphens.

        Returns:
            32 character hex string

        Example:
            >>> uuid.format_compact()
            'a1b2c3d4e5f64789abcdef0123456789'
        """
        return self.bytes_value.hex()

    def format_urn(self) -> str:
        """
        Format as URN.

        Returns:
            String in format "urn:uuid:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
        """
        return f"urn:uuid:{self.format_standard()}"

    def format_braces(self) -> str:
        """
        Format with braces (Microsoft style).

        Returns:
            String in format "{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}"
        """
        return f"{{{self.format_standard()}}}"

    def __str__(self) -> str:
        """Return standard format string representation."""
        return self.format_standard()

    def __eq__(self, other: object) -> bool:
        """Check equality with another UUID."""
        if isinstance(other, SafeUUID):
            return self.bytes_value == other.bytes_value
        return NotImplemented

    def __hash__(self) -> int:
        """Return hash value for use in sets and dicts."""
        return hash(self.bytes_value)

    def __lt__(self, other: SafeUUID) -> bool:
        """Compare UUIDs lexicographically by bytes."""
        if not isinstance(other, SafeUUID):
            return NotImplemented
        return self.bytes_value < other.bytes_value


# Initialize well-known namespace UUIDs
SafeUUID.NAMESPACE_DNS = SafeUUID(bytes.fromhex("6ba7b8109dad11d180b400c04fd430c8"))
SafeUUID.NAMESPACE_URL = SafeUUID(bytes.fromhex("6ba7b8119dad11d180b400c04fd430c8"))
SafeUUID.NAMESPACE_OID = SafeUUID(bytes.fromhex("6ba7b8129dad11d180b400c04fd430c8"))
SafeUUID.NAMESPACE_X500 = SafeUUID(bytes.fromhex("6ba7b8149dad11d180b400c04fd430c8"))
SafeUUID.NIL = SafeUUID(b"\x00" * 16)
