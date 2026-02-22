# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeNetwork -- IPv4 address parsing and classification.

Parses IPv4 address strings and classifies them as private (RFC 1918) or
loopback (127.0.0.0/8). All computation is delegated to the formally verified
Idris 2 core via C FFI. No logic is reimplemented here.

Functions:
    parse_ipv4     -- Parse an IPv4 address string
    ipv4_is_private  -- Check if address is RFC 1918 private
    ipv4_is_loopback -- Check if address is loopback (127.x.x.x)
"""

from memory import UnsafePointer

from .lib_proven import (
    IPv4Address,
    IPv4Result,
    proven_network_parse_ipv4,
    proven_network_ipv4_is_private,
    proven_network_ipv4_is_loopback,
    PROVEN_OK,
)


fn _str_to_ptr(s: String) -> (UnsafePointer[UInt8], Int):
    """Extract a raw byte pointer and length from a Mojo String."""
    var byte_slice = s.as_bytes()
    var length = len(byte_slice)
    var ptr = byte_slice.unsafe_ptr()
    return (ptr, length)


fn parse_ipv4(address: String) -> Optional[IPv4Address]:
    """Parse an IPv4 address string (e.g. '192.168.1.1').

    Returns None if the string is not a valid IPv4 address.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(address)
    var result = proven_network_parse_ipv4(pair[0], pair[1])
    if result.succeeded():
        return result.address
    return None


fn ipv4_is_private(addr: IPv4Address) -> Bool:
    """Check if an IPv4 address is private (RFC 1918).

    Private ranges: 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16.
    All computation performed in formally verified Idris 2 code.
    """
    return proven_network_ipv4_is_private(addr)


fn ipv4_is_loopback(addr: IPv4Address) -> Bool:
    """Check if an IPv4 address is loopback (127.0.0.0/8).

    All computation performed in formally verified Idris 2 code.
    """
    return proven_network_ipv4_is_loopback(addr)


fn ipv4_to_string(addr: IPv4Address) -> String:
    """Format an IPv4Address as a dotted-decimal string.

    This is a trivial formatting helper that does not involve libproven
    computation. It exists for convenience when printing/logging addresses
    obtained from parse_ipv4.
    """
    return (
        str(Int(addr.o0))
        + "."
        + str(Int(addr.o1))
        + "."
        + str(Int(addr.o2))
        + "."
        + str(Int(addr.o3))
    )
