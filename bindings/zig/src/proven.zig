// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Proven - Safety-first utility functions with formal verification guarantees.
//!
//! Provides safe operations for:
//! - Math: Division, modulo, checked arithmetic with overflow detection
//! - Strings: HTML/SQL/JS escaping, safe operations
//! - Paths: Traversal detection and filename sanitization
//! - Email: Validation, parsing, normalization
//! - Network: IPv4 parsing, private/loopback/public classification
//! - Crypto: Constant-time comparison, secure zeroing
//! - UUID: RFC 4122 UUID generation and validation
//! - Currency: Type-safe monetary values with ISO 4217 codes
//! - Phone: E.164 phone number parsing and formatting
//! - Hex: Hexadecimal encoding and decoding

const std = @import("std");

pub const safe_math = @import("safe_math.zig");
pub const safe_string = @import("safe_string.zig");
pub const safe_path = @import("safe_path.zig");
pub const safe_email = @import("safe_email.zig");
pub const safe_network = @import("safe_network.zig");
pub const safe_crypto = @import("safe_crypto.zig");
pub const safe_uuid = @import("safe_uuid.zig");
pub const safe_currency = @import("safe_currency.zig");
pub const safe_phone = @import("safe_phone.zig");
pub const safe_hex = @import("safe_hex.zig");

test {
    std.testing.refAllDecls(@This());
}
