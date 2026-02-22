// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// lib_proven.carbon - C import declarations for libproven FFI.
//
// This file declares all external C functions and types from libproven.
// ALL computation is performed in Idris 2 via the Zig FFI bridge.
// Carbon code MUST NOT reimplement any logic; it only marshals data.

library "proven";

package Proven api;

import Cpp library "proven";

// ============================================================================
// Status Codes
// ============================================================================

// Status codes returned by libproven operations.
// Zero indicates success; negative values indicate specific error conditions.
let STATUS_OK: i32 = 0;
let STATUS_ERR_NULL_POINTER: i32 = -1;
let STATUS_ERR_INVALID_ARGUMENT: i32 = -2;
let STATUS_ERR_OVERFLOW: i32 = -3;
let STATUS_ERR_UNDERFLOW: i32 = -4;
let STATUS_ERR_DIVISION_BY_ZERO: i32 = -5;
let STATUS_ERR_PARSE_FAILURE: i32 = -6;
let STATUS_ERR_VALIDATION_FAILED: i32 = -7;
let STATUS_ERR_OUT_OF_BOUNDS: i32 = -8;
let STATUS_ERR_ENCODING_ERROR: i32 = -9;
let STATUS_ERR_ALLOCATION_FAILED: i32 = -10;
let STATUS_ERR_NOT_IMPLEMENTED: i32 = -99;

// ============================================================================
// Result Structures (C ABI compatible)
// ============================================================================

// Result for integer operations from libproven.
class IntResult {
  var status: i32;
  var value: i64;
}

// Result for boolean operations from libproven.
class BoolResult {
  var status: i32;
  var value: i32;
}

// Result for string operations from libproven.
// Caller must free value using proven_free_string().
class StringResult {
  var status: i32;
  var ptr: Cpp.Ptr(i8);
  var len: i32;
}

// Result for floating-point operations from libproven.
class FloatResult {
  var status: i32;
  var value: f64;
}

// ============================================================================
// JSON Type Enum
// ============================================================================

// JSON value types returned by proven_json_get_type.
let JSON_NULL: i32 = 0;
let JSON_BOOL: i32 = 1;
let JSON_NUMBER: i32 = 2;
let JSON_STRING: i32 = 3;
let JSON_ARRAY: i32 = 4;
let JSON_OBJECT: i32 = 5;
let JSON_INVALID: i32 = -1;

// ============================================================================
// Extern C Function Declarations - Lifecycle
// ============================================================================

extern "C" fn proven_init() -> i32;
extern "C" fn proven_deinit() -> ();
extern "C" fn proven_is_initialized() -> bool;

// ============================================================================
// Extern C Function Declarations - Memory
// ============================================================================

extern "C" fn proven_free_string(ptr: Cpp.Ptr(i8)) -> ();

// ============================================================================
// Extern C Function Declarations - SafeMath
// ============================================================================

extern "C" fn proven_math_add_checked(a: i64, b: i64) -> IntResult;
extern "C" fn proven_math_sub_checked(a: i64, b: i64) -> IntResult;
extern "C" fn proven_math_mul_checked(a: i64, b: i64) -> IntResult;
extern "C" fn proven_math_div(numerator: i64, denominator: i64) -> IntResult;
extern "C" fn proven_math_mod(numerator: i64, denominator: i64) -> IntResult;
extern "C" fn proven_math_abs_safe(n: i64) -> IntResult;
extern "C" fn proven_math_clamp(lo: i64, hi: i64, value: i64) -> i64;
extern "C" fn proven_math_pow_checked(base: i64, exp: u32) -> IntResult;

// ============================================================================
// Extern C Function Declarations - SafeString
// ============================================================================

extern "C" fn proven_string_is_valid_utf8(ptr: Cpp.Ptr(u8), len: i64) -> BoolResult;
extern "C" fn proven_string_escape_sql(ptr: Cpp.Ptr(u8), len: i64) -> StringResult;
extern "C" fn proven_string_escape_html(ptr: Cpp.Ptr(u8), len: i64) -> StringResult;
extern "C" fn proven_string_escape_js(ptr: Cpp.Ptr(u8), len: i64) -> StringResult;

// ============================================================================
// Extern C Function Declarations - SafePath
// ============================================================================

extern "C" fn proven_path_has_traversal(ptr: Cpp.Ptr(u8), len: i64) -> BoolResult;
extern "C" fn proven_path_sanitize_filename(ptr: Cpp.Ptr(u8), len: i64) -> StringResult;

// ============================================================================
// Extern C Function Declarations - SafeEmail
// ============================================================================

extern "C" fn proven_email_is_valid(ptr: Cpp.Ptr(u8), len: i64) -> BoolResult;

// ============================================================================
// Extern C Function Declarations - SafeUrl
// ============================================================================

extern "C" fn proven_url_parse(ptr: Cpp.Ptr(u8), len: i64) -> Cpp.Ptr(());
extern "C" fn proven_url_free(components: Cpp.Ptr(())) -> ();

// ============================================================================
// Extern C Function Declarations - SafeCrypto
// ============================================================================

extern "C" fn proven_crypto_constant_time_eq(
    ptr1: Cpp.Ptr(u8), len1: i64,
    ptr2: Cpp.Ptr(u8), len2: i64
) -> BoolResult;
extern "C" fn proven_crypto_random_bytes(ptr: Cpp.Ptr(u8), len: i64) -> i32;

// ============================================================================
// Extern C Function Declarations - SafeJson
// ============================================================================

extern "C" fn proven_json_is_valid(ptr: Cpp.Ptr(u8), len: i64) -> BoolResult;
extern "C" fn proven_json_get_type(ptr: Cpp.Ptr(u8), len: i64) -> i32;

// ============================================================================
// Extern C Function Declarations - SafeFloat
// ============================================================================

extern "C" fn proven_float_div(a: f64, b: f64) -> FloatResult;
extern "C" fn proven_float_is_finite(x: f64) -> bool;
extern "C" fn proven_float_is_nan(x: f64) -> bool;
extern "C" fn proven_float_sqrt(x: f64) -> FloatResult;
extern "C" fn proven_float_ln(x: f64) -> FloatResult;

// ============================================================================
// Extern C Function Declarations - SafeHex
// ============================================================================

extern "C" fn proven_hex_encode(ptr: Cpp.Ptr(u8), len: i64, uppercase: bool) -> StringResult;

// ============================================================================
// Extern C Function Declarations - SafeDateTime
// ============================================================================

extern "C" fn proven_datetime_is_leap_year(year: i32) -> bool;
extern "C" fn proven_datetime_days_in_month(year: i32, month: u8) -> u8;

// ============================================================================
// Extern C Function Declarations - SafeColor
// ============================================================================

extern "C" fn proven_color_parse_hex(ptr: Cpp.Ptr(u8), len: i64) -> Cpp.Ptr(());
extern "C" fn proven_color_to_hex(r: u8, g: u8, b: u8) -> StringResult;

// ============================================================================
// Extern C Function Declarations - SafeChecksum
// ============================================================================

extern "C" fn proven_checksum_crc32(ptr: Cpp.Ptr(u8), len: i64) -> IntResult;
extern "C" fn proven_checksum_verify_crc32(ptr: Cpp.Ptr(u8), len: i64, expected: u32) -> BoolResult;

// ============================================================================
// Version Information
// ============================================================================

extern "C" fn proven_ffi_abi_version() -> u32;
extern "C" fn proven_version_major() -> u32;
extern "C" fn proven_version_minor() -> u32;
extern "C" fn proven_version_patch() -> u32;
extern "C" fn proven_module_count() -> u32;

// ============================================================================
// Helper: Check if a result indicates success
// ============================================================================

fn Succeeded(status: i32) -> bool {
  return status == STATUS_OK;
}

// Helper: Check if a result indicates failure.
fn Failed(status: i32) -> bool {
  return status != STATUS_OK;
}
