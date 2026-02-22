// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe network address operations via libproven FFI.
//!
//! Provides IPv4 address parsing and classification.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Parsed IPv4 address.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ipv4Addr {
    /// Four octets of the IPv4 address.
    pub octets: [u8; 4],
}

impl std::fmt::Display for Ipv4Addr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}.{}.{}.{}",
            self.octets[0], self.octets[1], self.octets[2], self.octets[3]
        )
    }
}

/// Safe network address operations.
pub struct SafeNetwork;

impl SafeNetwork {
    /// Parse an IPv4 address string.
    ///
    /// Returns `Err` if the string is not a valid dotted-decimal IPv4 address.
    pub fn parse_ipv4(input: &str) -> Result<Ipv4Addr> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer within the given length.
        let result = unsafe {
            ffi::proven_network_parse_ipv4(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;
        Ok(Ipv4Addr {
            octets: result.address.octets,
        })
    }

    /// Check if an IPv4 address is in a private range (RFC 1918).
    ///
    /// Private ranges: 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16.
    pub fn ipv4_is_private(addr: &Ipv4Addr) -> bool {
        let ffi_addr = ffi::IPv4Address {
            octets: addr.octets,
        };
        // SAFETY: proven_network_ipv4_is_private takes a value-type struct
        // with no pointers; always safe to call.
        unsafe { ffi::proven_network_ipv4_is_private(ffi_addr) }
    }

    /// Check if an IPv4 address is a loopback address (127.0.0.0/8).
    pub fn ipv4_is_loopback(addr: &Ipv4Addr) -> bool {
        let ffi_addr = ffi::IPv4Address {
            octets: addr.octets,
        };
        // SAFETY: proven_network_ipv4_is_loopback takes a value-type struct
        // with no pointers; always safe to call.
        unsafe { ffi::proven_network_ipv4_is_loopback(ffi_addr) }
    }
}
