// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// Proven - Code that cannot crash
////
//// Thin FFI wrapper over libproven, a formally verified safety library.
//// All computation happens in the Idris2 core via Zig FFI and Erlang NIFs.
////
//// Modules:
//// - proven/math: Safe arithmetic (division, overflow detection)
//// - proven/string_ops: Safe string escaping (SQL, HTML, JS)
//// - proven/path: Path sanitization and traversal prevention
//// - proven/email: RFC 5321 email validation
//// - proven/network: IPv4 parsing and classification
//// - proven/url: RFC 3986 URL parsing
//// - proven/crypto: Timing-safe comparison, CSPRNG
//// - proven/json: JSON validation and type detection
//// - proven/datetime: ISO 8601 date/time parsing
//// - proven/float_ops: Safe floating-point arithmetic
//// - proven/version: Semantic version parsing and comparison
//// - proven/color: Color parsing and conversion
//// - proven/angle: Angle conversion and normalization
//// - proven/unit: Physical unit conversion
//// - proven/uuid: RFC 4122 UUID generation and parsing
//// - proven/hex: Hex encoding/decoding with timing-safe comparison
//// - proven/currency: ISO 4217 currency operations
//// - proven/phone: E.164 phone number validation

pub const version = "1.0.0"

/// Initialize the Proven runtime. Call before using any other functions.
@external(erlang, "proven_nif", "init")
pub fn init() -> Result(Nil, String)

/// Cleanup the Proven runtime. Call when done.
@external(erlang, "proven_nif", "deinit")
pub fn deinit() -> Nil

/// Check if the Proven runtime is initialized.
@external(erlang, "proven_nif", "is_initialized")
pub fn is_initialized() -> Bool

/// Get the FFI ABI version for compatibility checking.
@external(erlang, "proven_nif", "ffi_abi_version")
pub fn ffi_abi_version() -> Int
