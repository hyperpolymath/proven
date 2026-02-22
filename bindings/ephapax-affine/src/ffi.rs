// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Raw FFI declarations for libproven (affine variant).
//!
//! This module declares the C ABI interface to libproven. All computation
//! is performed in Idris 2 via the Zig FFI bridge. These are the low-level,
//! unsafe bindings used internally by the safe affine wrapper types.
//!
//! ## ABI Result Types
//!
//! The C ABI uses the following result structs:
//!
//! - [`IntResult`]: `{ i32 status, i64 value }`
//! - [`BoolResult`]: `{ i32 status, i32 value }`
//! - [`StringResult`]: `{ i32 status, *const u8 ptr, usize len }`
//! - [`FloatResult`]: `{ i32 status, f64 value }`
//!
//! Status code 0 indicates success; negative values indicate errors.

#![allow(non_camel_case_types)]

// ============================================================================
// Status codes (matches ProvenStatus enum in Zig)
// ============================================================================

/// Operation succeeded.
pub const STATUS_OK: i32 = 0;
/// Null pointer passed to FFI function.
pub const STATUS_ERR_NULL_POINTER: i32 = -1;
/// Invalid argument provided.
pub const STATUS_ERR_INVALID_ARGUMENT: i32 = -2;
/// Integer overflow detected.
pub const STATUS_ERR_OVERFLOW: i32 = -3;
/// Integer underflow detected.
pub const STATUS_ERR_UNDERFLOW: i32 = -4;
/// Division by zero attempted.
pub const STATUS_ERR_DIVISION_BY_ZERO: i32 = -5;
/// Parse failure.
pub const STATUS_ERR_PARSE_FAILURE: i32 = -6;
/// Validation failed.
pub const STATUS_ERR_VALIDATION_FAILED: i32 = -7;
/// Access out of bounds.
pub const STATUS_ERR_OUT_OF_BOUNDS: i32 = -8;
/// Encoding error.
pub const STATUS_ERR_ENCODING_ERROR: i32 = -9;
/// Memory allocation failed.
pub const STATUS_ERR_ALLOCATION_FAILED: i32 = -10;
/// Function not yet implemented.
pub const STATUS_ERR_NOT_IMPLEMENTED: i32 = -99;

// ============================================================================
// Result structures (matches C ABI extern structs)
// ============================================================================

/// Result for integer operations.
///
/// Layout: `{ i32 status, i64 value }`
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct IntResult {
    /// Status code (0 = success, negative = error).
    pub status: i32,
    /// The computed value (only valid when status == 0).
    pub value: i64,
}

/// Result for boolean operations.
///
/// Layout: `{ i32 status, i32 value }`
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct BoolResult {
    /// Status code (0 = success, negative = error).
    pub status: i32,
    /// The boolean result (only valid when status == 0).
    pub value: i32,
}

/// Result for string operations.
///
/// Layout: `{ i32 status, *const u8 ptr, usize len }`
///
/// When status == 0, `ptr` points to a UTF-8 encoded string of `len` bytes
/// allocated by libproven. The caller must free this via [`proven_free_string`].
#[repr(C)]
#[derive(Debug)]
pub struct StringResult {
    /// Status code (0 = success, negative = error).
    pub status: i32,
    /// Pointer to the allocated string data.
    pub ptr: *mut u8,
    /// Length of the string in bytes.
    pub len: usize,
}

/// Result for floating-point operations.
///
/// Layout: `{ i32 status, f64 value }`
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FloatResult {
    /// Status code (0 = success, negative = error).
    pub status: i32,
    /// The computed value (only valid when status == 0).
    pub value: f64,
}

/// Opaque handle to a bounded buffer managed by libproven.
#[repr(C)]
pub struct ProvenBuffer {
    _private: [u8; 0],
}

/// Result for buffer creation.
#[repr(C)]
#[derive(Debug)]
pub struct BufferResult {
    /// Status code (0 = success, negative = error).
    pub status: i32,
    /// Pointer to the allocated buffer (only valid when status == 0).
    pub buffer: *mut ProvenBuffer,
}

// ============================================================================
// Extern function declarations
// ============================================================================

#[link(name = "proven")]
extern "C" {
    // ========================================================================
    // Lifecycle
    // ========================================================================

    /// Initialize the Proven runtime (Idris 2 backend).
    ///
    /// Must be called before any other proven function. Idempotent.
    pub fn proven_init() -> i32;

    /// Deinitialize the Proven runtime. Idempotent.
    pub fn proven_deinit();

    /// Check whether the runtime has been initialized.
    pub fn proven_is_initialized() -> bool;

    // ========================================================================
    // Memory management
    // ========================================================================

    /// Free a string allocated by libproven.
    ///
    /// # Safety
    ///
    /// `ptr` must have been allocated by a libproven string-returning function
    /// and must not have been freed already.
    pub fn proven_free_string(ptr: *mut u8);

    // ========================================================================
    // Safe math (integer arithmetic)
    // ========================================================================

    /// Checked addition: returns overflow error if result exceeds i64 range.
    pub fn proven_math_add_checked(a: i64, b: i64) -> IntResult;

    /// Checked subtraction: returns underflow error if result goes below i64 range.
    pub fn proven_math_sub_checked(a: i64, b: i64) -> IntResult;

    /// Checked multiplication: returns overflow error if result overflows.
    pub fn proven_math_mul_checked(a: i64, b: i64) -> IntResult;

    /// Safe division: returns division-by-zero error if denominator is 0.
    pub fn proven_math_div(numerator: i64, denominator: i64) -> IntResult;

    /// Safe modulo: returns division-by-zero error if denominator is 0.
    pub fn proven_math_mod(numerator: i64, denominator: i64) -> IntResult;

    /// Safe absolute value: returns overflow error for i64::MIN.
    pub fn proven_math_abs_safe(n: i64) -> IntResult;

    /// Safe negation: returns overflow error for i64::MIN.
    pub fn proven_math_negate_safe(n: i64) -> IntResult;

    /// Clamp value to [lo, hi]. Always succeeds.
    pub fn proven_math_clamp(lo: i64, hi: i64, value: i64) -> i64;

    /// Checked exponentiation.
    pub fn proven_math_pow_checked(base: i64, exp: u32) -> IntResult;

    // ========================================================================
    // Safe float (floating-point arithmetic)
    // ========================================================================

    /// Safe floating-point addition (NaN/Infinity prevention).
    pub fn proven_float_add(a: f64, b: f64) -> FloatResult;

    /// Safe floating-point multiplication (NaN/Infinity prevention).
    pub fn proven_float_mul(a: f64, b: f64) -> FloatResult;

    // ========================================================================
    // Safe string operations
    // ========================================================================

    /// Validate UTF-8 encoding of a byte slice.
    pub fn proven_string_is_valid_utf8(ptr: *const u8, len: usize) -> BoolResult;

    /// Sanitize a string (strip control characters, normalize whitespace).
    ///
    /// Caller must free the returned string via [`proven_free_string`].
    pub fn proven_sanitize_string(ptr: *const u8, len: usize) -> StringResult;

    /// Escape a string for safe SQL interpolation.
    ///
    /// Caller must free the returned string via [`proven_free_string`].
    pub fn proven_string_escape_sql(ptr: *const u8, len: usize) -> StringResult;

    /// Escape a string for safe HTML rendering.
    ///
    /// Caller must free the returned string via [`proven_free_string`].
    pub fn proven_string_escape_html(ptr: *const u8, len: usize) -> StringResult;

    /// Escape a string for safe JavaScript string literals.
    ///
    /// Caller must free the returned string via [`proven_free_string`].
    pub fn proven_string_escape_js(ptr: *const u8, len: usize) -> StringResult;

    // ========================================================================
    // Validation functions
    // ========================================================================

    /// Validate an email address (RFC 5321/5322).
    pub fn proven_validate_email(ptr: *const u8, len: usize) -> BoolResult;

    /// Validate a URL (RFC 3986).
    pub fn proven_validate_url(ptr: *const u8, len: usize) -> BoolResult;

    /// Validate an IPv4 address.
    pub fn proven_validate_ipv4(ptr: *const u8, len: usize) -> BoolResult;

    /// Validate a filesystem path (no traversal attacks).
    pub fn proven_validate_path(ptr: *const u8, len: usize) -> BoolResult;

    /// Validate a JSON document.
    pub fn proven_json_validate(ptr: *const u8, len: usize) -> BoolResult;

    // ========================================================================
    // Cryptographic operations
    // ========================================================================

    /// SHA-256 hash of input bytes.
    ///
    /// Caller must free the returned string via [`proven_free_string`].
    pub fn proven_hash_sha256(ptr: *const u8, len: usize) -> StringResult;

    /// Hex-encode a byte slice.
    ///
    /// Caller must free the returned string via [`proven_free_string`].
    pub fn proven_hex_encode(ptr: *const u8, len: usize) -> StringResult;

    /// Hex-decode a hex string into raw bytes.
    ///
    /// Caller must free the returned data via [`proven_free_string`].
    pub fn proven_hex_decode(ptr: *const u8, len: usize) -> StringResult;

    /// Constant-time byte comparison (timing-attack resistant).
    pub fn proven_crypto_constant_time_eq(
        a: *const u8,
        a_len: usize,
        b: *const u8,
        b_len: usize,
    ) -> BoolResult;

    /// Fill buffer with cryptographically secure random bytes.
    pub fn proven_crypto_random_bytes(buf: *mut u8, len: usize) -> i32;

    // ========================================================================
    // Buffer operations
    // ========================================================================

    /// Create a new bounded buffer with the given capacity.
    pub fn proven_buffer_create(capacity: usize) -> BufferResult;

    /// Append data to a bounded buffer.
    pub fn proven_buffer_append(
        buffer: *mut ProvenBuffer,
        data: *const u8,
        len: usize,
    ) -> i32;

    /// Get the contents of a bounded buffer.
    pub fn proven_buffer_get(
        buffer: *mut ProvenBuffer,
        out_ptr: *mut *const u8,
        out_len: *mut usize,
    ) -> i32;

    /// Free a bounded buffer.
    pub fn proven_buffer_free(buffer: *mut ProvenBuffer);
}
