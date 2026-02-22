// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// LibProven - Low-level extern declarations for libproven C ABI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This module declares FFI types and functions; it does NOT reimplement
// any logic.

module LibProven {

  // Link against libproven shared or static library.
  require "proven.h";
  use CTypes;

  // ========================================================================
  // Status codes
  // ========================================================================

  /** Status codes returned by Proven operations. */
  enum ProvenStatus : int(32) {
    OK                   =   0,
    ErrNullPointer       =  -1,
    ErrInvalidArgument   =  -2,
    ErrOverflow          =  -3,
    ErrUnderflow         =  -4,
    ErrDivisionByZero    =  -5,
    ErrParseFailure      =  -6,
    ErrValidationFailed  =  -7,
    ErrOutOfBounds       =  -8,
    ErrEncodingError     =  -9,
    ErrAllocationFailed  = -10,
    ErrNotImplemented    = -99
  }

  // ========================================================================
  // Result types
  // ========================================================================

  /** Result for integer operations. */
  extern "ProvenIntResult" record IntResult {
    var status : int(32);
    var value  : int(64);
  }

  /** Result for boolean operations. */
  extern "ProvenBoolResult" record BoolResult {
    var status : int(32);
    var value  : bool;
  }

  /**
   * Result for string operations.
   * Caller must free ``value`` with ``provenFreeString()``.
   */
  extern "ProvenStringResult" record StringResult {
    var status : int(32);
    var value  : c_ptrConst(c_char);
    var length : c_size_t;
  }

  /** Result for floating-point operations. */
  extern "ProvenFloatResult" record FloatResult {
    var status : int(32);
    var value  : real(64);
  }

  /** IPv4 address structure. */
  extern "ProvenIPv4Address" record IPv4Address {
    var octets : c_array(uint(8), 4);
  }

  /** IPv4 parse result. */
  extern "ProvenIPv4Result" record IPv4Result {
    var status  : int(32);
    var address : IPv4Address;
  }

  /** JSON value type. */
  extern "ProvenJsonType" type JsonType = int(32);

  // JSON type constants.
  param JSON_NULL    : int(32) =  0;
  param JSON_BOOL    : int(32) =  1;
  param JSON_NUMBER  : int(32) =  2;
  param JSON_STRING  : int(32) =  3;
  param JSON_ARRAY   : int(32) =  4;
  param JSON_OBJECT  : int(32) =  5;
  param JSON_INVALID : int(32) = -1;

  /** DateTime components. */
  extern "ProvenDateTime" record DateTime {
    var year              : int(32);
    var month             : uint(8);
    var day               : uint(8);
    var hour              : uint(8);
    var minute            : uint(8);
    var second            : uint(8);
    var nanosecond        : uint(32);
    var tz_offset_minutes : int(16);
  }

  /** DateTime result. */
  extern "ProvenDateTimeResult" record DateTimeResult {
    var status   : int(32);
    var datetime : DateTime;
  }

  // ========================================================================
  // Lifecycle functions
  // ========================================================================

  extern "proven_init"           proc provenInit(): int(32);
  extern "proven_deinit"         proc provenDeinit(): void;
  extern "proven_is_initialized" proc provenIsInitialized(): bool;
  extern "proven_ffi_abi_version" proc provenFfiAbiVersion(): uint(32);
  extern "proven_version_major"  proc provenVersionMajor(): uint(32);
  extern "proven_version_minor"  proc provenVersionMinor(): uint(32);
  extern "proven_version_patch"  proc provenVersionPatch(): uint(32);
  extern "proven_module_count"   proc provenModuleCount(): uint(32);

  // ========================================================================
  // Memory management
  // ========================================================================

  extern "proven_free_string" proc provenFreeString(ptr: c_ptrConst(c_char)): void;

  // ========================================================================
  // SafeMath
  // ========================================================================

  extern "proven_math_add_checked" proc provenMathAddChecked(a: int(64), b: int(64)): IntResult;
  extern "proven_math_sub_checked" proc provenMathSubChecked(a: int(64), b: int(64)): IntResult;
  extern "proven_math_mul_checked" proc provenMathMulChecked(a: int(64), b: int(64)): IntResult;
  extern "proven_math_div"         proc provenMathDiv(a: int(64), b: int(64)): IntResult;
  extern "proven_math_mod"         proc provenMathMod(a: int(64), b: int(64)): IntResult;
  extern "proven_math_abs_safe"    proc provenMathAbsSafe(n: int(64)): IntResult;
  extern "proven_math_clamp"       proc provenMathClamp(lo: int(64), hi: int(64), value: int(64)): int(64);
  extern "proven_math_pow_checked" proc provenMathPowChecked(base: int(64), exp: uint(32)): IntResult;

  // ========================================================================
  // SafeString
  // ========================================================================

  extern "proven_string_is_valid_utf8" proc provenStringIsValidUtf8(ptr: c_ptrConst(uint(8)), len: c_size_t): BoolResult;
  extern "proven_string_escape_sql"    proc provenStringEscapeSql(ptr: c_ptrConst(uint(8)), len: c_size_t): StringResult;
  extern "proven_string_escape_html"   proc provenStringEscapeHtml(ptr: c_ptrConst(uint(8)), len: c_size_t): StringResult;
  extern "proven_string_escape_js"     proc provenStringEscapeJs(ptr: c_ptrConst(uint(8)), len: c_size_t): StringResult;

  // ========================================================================
  // SafePath
  // ========================================================================

  extern "proven_path_has_traversal"       proc provenPathHasTraversal(ptr: c_ptrConst(uint(8)), len: c_size_t): BoolResult;
  extern "proven_path_sanitize_filename"   proc provenPathSanitizeFilename(ptr: c_ptrConst(uint(8)), len: c_size_t): StringResult;

  // ========================================================================
  // SafeEmail
  // ========================================================================

  extern "proven_email_is_valid" proc provenEmailIsValid(ptr: c_ptrConst(uint(8)), len: c_size_t): BoolResult;

  // ========================================================================
  // SafeUrl
  // ========================================================================

  extern "proven_http_url_encode" proc provenHttpUrlEncode(ptr: c_ptrConst(uint(8)), len: c_size_t): StringResult;
  extern "proven_http_url_decode" proc provenHttpUrlDecode(ptr: c_ptrConst(uint(8)), len: c_size_t): StringResult;

  // ========================================================================
  // SafeNetwork
  // ========================================================================

  extern "proven_network_parse_ipv4"      proc provenNetworkParseIpv4(ptr: c_ptrConst(uint(8)), len: c_size_t): IPv4Result;
  extern "proven_network_ipv4_is_private"  proc provenNetworkIpv4IsPrivate(addr: IPv4Address): bool;
  extern "proven_network_ipv4_is_loopback" proc provenNetworkIpv4IsLoopback(addr: IPv4Address): bool;

  // ========================================================================
  // SafeCrypto
  // ========================================================================

  extern "proven_crypto_constant_time_eq" proc provenCryptoConstantTimeEq(
      ptr1: c_ptrConst(uint(8)), len1: c_size_t,
      ptr2: c_ptrConst(uint(8)), len2: c_size_t
  ): BoolResult;
  extern "proven_crypto_random_bytes" proc provenCryptoRandomBytes(
      ptr: c_ptr(uint(8)), len: c_size_t
  ): int(32);

  // ========================================================================
  // SafeJson
  // ========================================================================

  extern "proven_json_is_valid" proc provenJsonIsValid(ptr: c_ptrConst(uint(8)), len: c_size_t): BoolResult;
  extern "proven_json_get_type" proc provenJsonGetType(ptr: c_ptrConst(uint(8)), len: c_size_t): int(32);

  // ========================================================================
  // SafeDateTime
  // ========================================================================

  extern "proven_datetime_parse"           proc provenDatetimeParse(ptr: c_ptrConst(uint(8)), len: c_size_t): DateTimeResult;
  extern "proven_datetime_format_iso8601"  proc provenDatetimeFormatIso8601(dt: DateTime): StringResult;
  extern "proven_datetime_is_leap_year"    proc provenDatetimeIsLeapYear(year: int(32)): bool;
  extern "proven_datetime_days_in_month"   proc provenDatetimeDaysInMonth(year: int(32), month: uint(8)): uint(8);

  // ========================================================================
  // SafeFloat
  // ========================================================================

  extern "proven_float_div"       proc provenFloatDiv(a: real(64), b: real(64)): FloatResult;
  extern "proven_float_is_finite" proc provenFloatIsFinite(x: real(64)): bool;
  extern "proven_float_is_nan"    proc provenFloatIsNan(x: real(64)): bool;
  extern "proven_float_sqrt"      proc provenFloatSqrt(x: real(64)): FloatResult;
  extern "proven_float_ln"        proc provenFloatLn(x: real(64)): FloatResult;

  // ========================================================================
  // SafeHex
  // ========================================================================

  extern "proven_hex_encode" proc provenHexEncode(
      ptr: c_ptrConst(uint(8)), len: c_size_t, uppercase: bool
  ): StringResult;

  // ========================================================================
  // SafeColor
  // ========================================================================

  extern "proven_color_to_hex" proc provenColorToHex(rgb: c_array(uint(8), 3)): StringResult;

  // ========================================================================
  // SafeAngle
  // ========================================================================

  extern "proven_angle_deg_to_rad"          proc provenAngleDegToRad(degrees: real(64)): real(64);
  extern "proven_angle_rad_to_deg"          proc provenAngleRadToDeg(radians: real(64)): real(64);
  extern "proven_angle_normalize_degrees"   proc provenAngleNormalizeDegrees(degrees: real(64)): real(64);
  extern "proven_angle_normalize_radians"   proc provenAngleNormalizeRadians(radians: real(64)): real(64);

  // ========================================================================
  // SafeCalculator
  // ========================================================================

  extern "proven_calculator_eval" proc provenCalculatorEval(
      ptr: c_ptrConst(uint(8)), len: c_size_t
  ): FloatResult;

  // ========================================================================
  // Helper: convert Chapel string to C pointer + length.
  // ========================================================================

  /** Convert a Chapel string to (c_ptr(uint(8)), c_size_t) pair for FFI. */
  inline proc toCBytes(s: string): (c_ptrConst(uint(8)), c_size_t) {
    return (s.c_str(): c_ptrConst(uint(8)), s.numBytes: c_size_t);
  }

  /** Check if a result status indicates success. */
  inline proc isOk(status: int(32)): bool {
    return status == 0;
  }

  /** Extract a Chapel string from a StringResult and free the C memory. */
  proc extractString(ref r: StringResult): string {
    if !isOk(r.status) || r.value == nil then return "";
    var s = string.createCopyingBuffer(r.value: c_ptrConst(c_char), r.length: int);
    provenFreeString(r.value);
    return s;
  }

}
