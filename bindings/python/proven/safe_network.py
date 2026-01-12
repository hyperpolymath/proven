# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeNetwork - Network operations that cannot crash.

Provides safe IP address parsing and validation.
"""

from dataclasses import dataclass
from typing import Optional, Tuple

from .core import ProvenStatus, ProvenError, get_lib, check_status, IPv4Address


@dataclass
class IPv4:
    """An IPv4 address."""
    octets: Tuple[int, int, int, int]

    def __str__(self) -> str:
        """Format as dotted decimal."""
        return f"{self.octets[0]}.{self.octets[1]}.{self.octets[2]}.{self.octets[3]}"

    def is_private(self) -> bool:
        """Check if this is a private (RFC 1918) address."""
        return SafeNetwork.is_private(str(self))

    def is_loopback(self) -> bool:
        """Check if this is a loopback address."""
        return SafeNetwork.is_loopback(str(self))


class SafeNetwork:
    """Safe network operations with proven correctness guarantees."""

    @staticmethod
    def parse_ipv4(address: str) -> Optional[IPv4]:
        """
        Parse an IPv4 address.

        Args:
            address: The IP address string (e.g., "192.168.1.1")

        Returns:
            IPv4 object, or None if invalid

        Example:
            >>> ip = SafeNetwork.parse_ipv4("192.168.1.1")
            >>> ip.octets
            (192, 168, 1, 1)
        """
        lib = get_lib()
        encoded = address.encode("utf-8")
        result = lib.proven_network_parse_ipv4(encoded, len(encoded))

        if result.status != ProvenStatus.OK:
            return None

        octets = tuple(result.address.octets)
        return IPv4(octets=octets)

    @staticmethod
    def is_valid_ipv4(address: str) -> bool:
        """
        Check if a string is a valid IPv4 address.

        Args:
            address: The string to check

        Returns:
            True if valid IPv4, False otherwise
        """
        return SafeNetwork.parse_ipv4(address) is not None

    @staticmethod
    def is_private(address: str) -> bool:
        """
        Check if an IPv4 address is in a private range.

        Private ranges (RFC 1918):
        - 10.0.0.0/8
        - 172.16.0.0/12
        - 192.168.0.0/16

        Args:
            address: The IP address string

        Returns:
            True if private, False otherwise (including if invalid)

        Example:
            >>> SafeNetwork.is_private("192.168.1.1")
            True
            >>> SafeNetwork.is_private("8.8.8.8")
            False
        """
        lib = get_lib()
        encoded = address.encode("utf-8")
        result = lib.proven_network_parse_ipv4(encoded, len(encoded))

        if result.status != ProvenStatus.OK:
            return False

        return lib.proven_network_ipv4_is_private(result.address)

    @staticmethod
    def is_loopback(address: str) -> bool:
        """
        Check if an IPv4 address is a loopback address.

        Loopback range: 127.0.0.0/8

        Args:
            address: The IP address string

        Returns:
            True if loopback, False otherwise (including if invalid)

        Example:
            >>> SafeNetwork.is_loopback("127.0.0.1")
            True
            >>> SafeNetwork.is_loopback("192.168.1.1")
            False
        """
        lib = get_lib()
        encoded = address.encode("utf-8")
        result = lib.proven_network_parse_ipv4(encoded, len(encoded))

        if result.status != ProvenStatus.OK:
            return False

        return lib.proven_network_ipv4_is_loopback(result.address)

    @staticmethod
    def is_public(address: str) -> bool:
        """
        Check if an IPv4 address is a public (routable) address.

        Args:
            address: The IP address string

        Returns:
            True if public, False otherwise
        """
        if not SafeNetwork.is_valid_ipv4(address):
            return False
        return not SafeNetwork.is_private(address) and not SafeNetwork.is_loopback(address)
