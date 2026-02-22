// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeNetwork - Network operations that cannot crash.
////
//// Thin FFI wrapper over libproven proven_network_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// Parsed IPv4 address octets.
pub type Ipv4 {
  Ipv4(a: Int, b: Int, c: Int, d: Int)
}

/// Parse an IPv4 address string (e.g., "192.168.1.1").
/// Returns the four octets on success.
@external(erlang, "proven_nif", "network_parse_ipv4")
pub fn parse_ipv4(address: String) -> Result(Ipv4, String)

/// Check if an IPv4 address is in a private range (RFC 1918).
@external(erlang, "proven_nif", "network_ipv4_is_private")
pub fn ipv4_is_private(a: Int, b: Int, c: Int, d: Int) -> Bool

/// Check if an IPv4 address is a loopback address (127.0.0.0/8).
@external(erlang, "proven_nif", "network_ipv4_is_loopback")
pub fn ipv4_is_loopback(a: Int, b: Int, c: Int, d: Int) -> Bool
