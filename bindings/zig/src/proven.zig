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

const std = @import("std");

pub const safe_math = @import("safe_math.zig");
pub const safe_string = @import("safe_string.zig");
pub const safe_path = @import("safe_path.zig");
pub const safe_email = @import("safe_email.zig");
pub const safe_network = @import("safe_network.zig");
pub const safe_crypto = @import("safe_crypto.zig");

test {
    std.testing.refAllDecls(@This());
}
