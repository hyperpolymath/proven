// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// LibProven - Low-level FFI declarations for libproven C ABI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This file declares FFI types and primitives; it does NOT reimplement
// any logic.

use "lib:proven"

// =========================================================================
// Status codes
// =========================================================================

primitive ProvenOk                  fun apply(): I32 =>  0
primitive ProvenErrNullPointer      fun apply(): I32 => -1
primitive ProvenErrInvalidArgument  fun apply(): I32 => -2
primitive ProvenErrOverflow         fun apply(): I32 => -3
primitive ProvenErrUnderflow        fun apply(): I32 => -4
primitive ProvenErrDivisionByZero   fun apply(): I32 => -5
primitive ProvenErrParseFailure     fun apply(): I32 => -6
primitive ProvenErrValidationFailed fun apply(): I32 => -7
primitive ProvenErrOutOfBounds      fun apply(): I32 => -8
primitive ProvenErrEncodingError    fun apply(): I32 => -9
primitive ProvenErrAllocationFailed fun apply(): I32 => -10
primitive ProvenErrNotImplemented   fun apply(): I32 => -99

// =========================================================================
// Result structures (C ABI layout)
// =========================================================================

struct IntResult
  """Result for integer operations from libproven."""
  var status: I32 = 0
  var value: I64 = 0

struct BoolResult
  """Result for boolean operations from libproven."""
  var status: I32 = 0
  var value: Bool = false

struct StringResult
  """
  Result for string operations from libproven.
  Caller must free value with proven_free_string().
  """
  var status: I32 = 0
  var value: Pointer[U8] tag = Pointer[U8]
  var length: USize = 0

struct FloatResult
  """Result for floating-point operations from libproven."""
  var status: I32 = 0
  var value: F64 = 0.0

struct IPv4Address
  """IPv4 address (4 octets)."""
  var o1: U8 = 0
  var o2: U8 = 0
  var o3: U8 = 0
  var o4: U8 = 0

struct IPv4Result
  """Result for IPv4 parsing."""
  var status: I32 = 0
  var address: IPv4Address = IPv4Address

// =========================================================================
// FFI declarations
// =========================================================================

primitive _LibProven
  """
  Raw FFI function declarations for libproven.

  All functions delegate to the Idris 2 core via the Zig FFI bridge.
  No logic is reimplemented.
  """

  // -- Lifecycle ----------------------------------------------------------

  fun init(): I32 =>
    @proven_init[I32]()

  fun deinit(): None =>
    @proven_deinit[None]()

  fun is_initialized(): Bool =>
    @proven_is_initialized[Bool]()

  fun ffi_abi_version(): U32 =>
    @proven_ffi_abi_version[U32]()

  fun version_major(): U32 =>
    @proven_version_major[U32]()

  fun version_minor(): U32 =>
    @proven_version_minor[U32]()

  fun version_patch(): U32 =>
    @proven_version_patch[U32]()

  fun module_count(): U32 =>
    @proven_module_count[U32]()

  // -- Memory management --------------------------------------------------

  fun free_string(ptr: Pointer[U8] tag): None =>
    @proven_free_string[None](ptr)

  // -- SafeMath -----------------------------------------------------------

  fun math_add_checked(a: I64, b: I64): IntResult =>
    @proven_math_add_checked[IntResult](a, b)

  fun math_sub_checked(a: I64, b: I64): IntResult =>
    @proven_math_sub_checked[IntResult](a, b)

  fun math_mul_checked(a: I64, b: I64): IntResult =>
    @proven_math_mul_checked[IntResult](a, b)

  fun math_div(a: I64, b: I64): IntResult =>
    @proven_math_div[IntResult](a, b)

  fun math_mod(a: I64, b: I64): IntResult =>
    @proven_math_mod[IntResult](a, b)

  fun math_abs_safe(n: I64): IntResult =>
    @proven_math_abs_safe[IntResult](n)

  fun math_clamp(lo: I64, hi: I64, value: I64): I64 =>
    @proven_math_clamp[I64](lo, hi, value)

  fun math_pow_checked(base: I64, exp: U32): IntResult =>
    @proven_math_pow_checked[IntResult](base, exp)

  // -- SafeString ---------------------------------------------------------

  fun string_is_valid_utf8(ptr: Pointer[U8] tag, len: USize): BoolResult =>
    @proven_string_is_valid_utf8[BoolResult](ptr, len)

  fun string_escape_sql(ptr: Pointer[U8] tag, len: USize): StringResult =>
    @proven_string_escape_sql[StringResult](ptr, len)

  fun string_escape_html(ptr: Pointer[U8] tag, len: USize): StringResult =>
    @proven_string_escape_html[StringResult](ptr, len)

  fun string_escape_js(ptr: Pointer[U8] tag, len: USize): StringResult =>
    @proven_string_escape_js[StringResult](ptr, len)

  // -- SafePath -----------------------------------------------------------

  fun path_has_traversal(ptr: Pointer[U8] tag, len: USize): BoolResult =>
    @proven_path_has_traversal[BoolResult](ptr, len)

  fun path_sanitize_filename(ptr: Pointer[U8] tag, len: USize): StringResult =>
    @proven_path_sanitize_filename[StringResult](ptr, len)

  // -- SafeEmail ----------------------------------------------------------

  fun email_is_valid(ptr: Pointer[U8] tag, len: USize): BoolResult =>
    @proven_email_is_valid[BoolResult](ptr, len)

  // -- SafeUrl ------------------------------------------------------------

  fun http_url_encode(ptr: Pointer[U8] tag, len: USize): StringResult =>
    @proven_http_url_encode[StringResult](ptr, len)

  fun http_url_decode(ptr: Pointer[U8] tag, len: USize): StringResult =>
    @proven_http_url_decode[StringResult](ptr, len)

  // -- SafeNetwork --------------------------------------------------------

  fun network_parse_ipv4(ptr: Pointer[U8] tag, len: USize): IPv4Result =>
    @proven_network_parse_ipv4[IPv4Result](ptr, len)

  fun network_ipv4_is_private(addr: IPv4Address): Bool =>
    @proven_network_ipv4_is_private[Bool](addr)

  fun network_ipv4_is_loopback(addr: IPv4Address): Bool =>
    @proven_network_ipv4_is_loopback[Bool](addr)

  // -- SafeCrypto ---------------------------------------------------------

  fun crypto_constant_time_eq(
    ptr1: Pointer[U8] tag, len1: USize,
    ptr2: Pointer[U8] tag, len2: USize
  ): BoolResult =>
    @proven_crypto_constant_time_eq[BoolResult](ptr1, len1, ptr2, len2)

  fun crypto_random_bytes(ptr: Pointer[U8], len: USize): I32 =>
    @proven_crypto_random_bytes[I32](ptr, len)

  // -- SafeJson -----------------------------------------------------------

  fun json_is_valid(ptr: Pointer[U8] tag, len: USize): BoolResult =>
    @proven_json_is_valid[BoolResult](ptr, len)

  fun json_get_type(ptr: Pointer[U8] tag, len: USize): I32 =>
    @proven_json_get_type[I32](ptr, len)

  // -- SafeDateTime -------------------------------------------------------

  fun datetime_is_leap_year(year: I32): Bool =>
    @proven_datetime_is_leap_year[Bool](year)

  fun datetime_days_in_month(year: I32, month: U8): U8 =>
    @proven_datetime_days_in_month[U8](year, month)

  // -- SafeFloat ----------------------------------------------------------

  fun float_div(a: F64, b: F64): FloatResult =>
    @proven_float_div[FloatResult](a, b)

  fun float_is_finite(x: F64): Bool =>
    @proven_float_is_finite[Bool](x)

  fun float_is_nan(x: F64): Bool =>
    @proven_float_is_nan[Bool](x)

  fun float_sqrt(x: F64): FloatResult =>
    @proven_float_sqrt[FloatResult](x)

  fun float_ln(x: F64): FloatResult =>
    @proven_float_ln[FloatResult](x)

  // -- SafeHex ------------------------------------------------------------

  fun hex_encode(ptr: Pointer[U8] tag, len: USize, uppercase: Bool): StringResult =>
    @proven_hex_encode[StringResult](ptr, len, uppercase)

  // -- SafeAngle ----------------------------------------------------------

  fun angle_deg_to_rad(degrees: F64): F64 =>
    @proven_angle_deg_to_rad[F64](degrees)

  fun angle_rad_to_deg(radians: F64): F64 =>
    @proven_angle_rad_to_deg[F64](radians)

  fun angle_normalize_degrees(degrees: F64): F64 =>
    @proven_angle_normalize_degrees[F64](degrees)

  fun angle_normalize_radians(radians: F64): F64 =>
    @proven_angle_normalize_radians[F64](radians)

  // -- SafeCalculator -----------------------------------------------------

  fun calculator_eval(ptr: Pointer[U8] tag, len: USize): FloatResult =>
    @proven_calculator_eval[FloatResult](ptr, len)

  // -- Helpers ------------------------------------------------------------

  fun _extract_string(r: StringResult): (String | None) =>
    """Extract a Pony String from a StringResult, freeing C memory."""
    if r.status != 0 then
      return None
    end
    if r.length == 0 then
      return ""
    end
    let s = String.from_cpointer(r.value, r.length, r.length)
    // Note: from_cpointer copies the data, so we free the original.
    free_string(r.value)
    s
