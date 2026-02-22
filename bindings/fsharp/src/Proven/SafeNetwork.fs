// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe network validation and operations via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeNetwork =

    /// Parse IPv4 address string (e.g., "192.168.1.1").
    let parseIPv4 (address: string) : IPv4Address option =
        let bytes = toUtf8 address
        let result = FFI.proven_network_parse_ipv4(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result.Address
        else None

    /// Check if IPv4 address is private (RFC 1918).
    let isPrivate (addr: IPv4Address) : bool =
        FFI.proven_network_ipv4_is_private(addr)

    /// Check if IPv4 address is loopback (127.0.0.0/8).
    let isLoopback (addr: IPv4Address) : bool =
        FFI.proven_network_ipv4_is_loopback(addr)
