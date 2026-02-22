// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// FFI declarations for libproven C ABI.
///
/// All extern functions map directly to the Zig FFI layer, which in turn
/// calls the formally verified Idris 2 implementation. No logic is
/// reimplemented here. Eclexia's Rust interop layer bridges to C ABI.

// ---------------------------------------------------------------------------
// Status codes (matches ProvenStatus enum in Zig)
// ---------------------------------------------------------------------------

/// Status codes returned by libproven functions.
const STATUS_OK: Int = 0
const STATUS_ERR_NULL_POINTER: Int = -1
const STATUS_ERR_INVALID_ARGUMENT: Int = -2
const STATUS_ERR_OVERFLOW: Int = -3
const STATUS_ERR_UNDERFLOW: Int = -4
const STATUS_ERR_DIVISION_BY_ZERO: Int = -5
const STATUS_ERR_PARSE_FAILURE: Int = -6
const STATUS_ERR_VALIDATION_FAILED: Int = -7
const STATUS_ERR_OUT_OF_BOUNDS: Int = -8
const STATUS_ERR_ENCODING_ERROR: Int = -9
const STATUS_ERR_ALLOCATION_FAILED: Int = -10
const STATUS_ERR_NOT_IMPLEMENTED: Int = -99

// ---------------------------------------------------------------------------
// Result structures (repr(C) compatible)
// ---------------------------------------------------------------------------

/// Result for integer operations.
@repr("C")
type IntResult = { status: Int, value: Int }

/// Result for boolean operations.
@repr("C")
type BoolResult = { status: Int, value: Int }

/// Result for string operations. Caller must free ptr via proven_free_string.
@repr("C")
type StringResult = { status: Int, ptr: RawPtr, len: Int }

/// Result for floating-point operations.
@repr("C")
type FloatResult = { status: Int, value: Float }

// ---------------------------------------------------------------------------
// Lifecycle
// ---------------------------------------------------------------------------

extern "C" fn proven_init() -> Int
extern "C" fn proven_deinit()
extern "C" fn proven_is_initialized() -> Bool

// ---------------------------------------------------------------------------
// Memory management
// ---------------------------------------------------------------------------

extern "C" fn proven_free_string(ptr: RawPtr)

// ---------------------------------------------------------------------------
// SafeMath
// ---------------------------------------------------------------------------

extern "C" fn proven_math_add_checked(a: Int, b: Int) -> IntResult
extern "C" fn proven_math_sub_checked(a: Int, b: Int) -> IntResult
extern "C" fn proven_math_mul_checked(a: Int, b: Int) -> IntResult
extern "C" fn proven_math_div(numerator: Int, denominator: Int) -> IntResult
extern "C" fn proven_math_mod(numerator: Int, denominator: Int) -> IntResult
extern "C" fn proven_math_abs_safe(n: Int) -> IntResult
extern "C" fn proven_math_clamp(lo: Int, hi: Int, value: Int) -> Int
extern "C" fn proven_math_pow_checked(base: Int, exp: Int) -> IntResult

// ---------------------------------------------------------------------------
// SafeFloat
// ---------------------------------------------------------------------------

extern "C" fn proven_float_div(a: Float, b: Float) -> FloatResult
extern "C" fn proven_float_sqrt(x: Float) -> FloatResult
extern "C" fn proven_float_ln(x: Float) -> FloatResult
extern "C" fn proven_float_is_finite(x: Float) -> Bool
extern "C" fn proven_float_is_nan(x: Float) -> Bool

// ---------------------------------------------------------------------------
// SafeString
// ---------------------------------------------------------------------------

extern "C" fn proven_string_is_valid_utf8(ptr: RawPtr, len: Int) -> BoolResult
extern "C" fn proven_string_escape_sql(ptr: RawPtr, len: Int) -> StringResult
extern "C" fn proven_string_escape_html(ptr: RawPtr, len: Int) -> StringResult
extern "C" fn proven_string_escape_js(ptr: RawPtr, len: Int) -> StringResult

// ---------------------------------------------------------------------------
// SafePath
// ---------------------------------------------------------------------------

extern "C" fn proven_path_has_traversal(ptr: RawPtr, len: Int) -> BoolResult
extern "C" fn proven_path_sanitize_filename(ptr: RawPtr, len: Int) -> StringResult

// ---------------------------------------------------------------------------
// SafeEmail
// ---------------------------------------------------------------------------

extern "C" fn proven_email_is_valid(ptr: RawPtr, len: Int) -> BoolResult

// ---------------------------------------------------------------------------
// SafeUrl
// ---------------------------------------------------------------------------

extern "C" fn proven_url_parse(ptr: RawPtr, len: Int) -> IntResult

// ---------------------------------------------------------------------------
// SafeCrypto
// ---------------------------------------------------------------------------

extern "C" fn proven_crypto_constant_time_eq(
    ptr1: RawPtr, len1: Int,
    ptr2: RawPtr, len2: Int
) -> BoolResult

extern "C" fn proven_crypto_random_bytes(ptr: RawPtr, len: Int) -> Int

// ---------------------------------------------------------------------------
// SafeHex
// ---------------------------------------------------------------------------

extern "C" fn proven_hex_encode(ptr: RawPtr, len: Int, uppercase: Bool) -> StringResult
extern "C" fn proven_hex_decode(ptr: RawPtr, len: Int) -> StringResult

// ---------------------------------------------------------------------------
// SafeJson
// ---------------------------------------------------------------------------

extern "C" fn proven_json_is_valid(ptr: RawPtr, len: Int) -> BoolResult
extern "C" fn proven_json_get_type(ptr: RawPtr, len: Int) -> Int

// ---------------------------------------------------------------------------
// SafeCurrency
// ---------------------------------------------------------------------------

extern "C" fn proven_currency_parse(ptr: RawPtr, len: Int) -> IntResult
extern "C" fn proven_currency_format(amount_minor: Int, code_ptr: RawPtr, decimal_places: Int) -> StringResult
