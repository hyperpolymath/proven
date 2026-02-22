// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// LibProven - External C function declarations for libproven.
//
// This unit mirrors the C types and function signatures from the proven.h
// header. All types use packed records for C ABI compatibility. All functions
// use cdecl calling convention and link against the 'proven' shared library.
//
// Users should prefer the high-level Safe* wrapper units over calling these
// FFI primitives directly. All computation is performed in formally verified
// Idris 2 code; this unit is a thin, type-safe declaration layer.
//
// Compatible with Free Pascal (FPC) and Delphi.

unit LibProven;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
  {$PACKRECORDS C}
{$ENDIF}

interface

uses
  SysUtils;

// ============================================================================
// Status Codes (mirrors ProvenStatus enum)
// ============================================================================

const
  PROVEN_OK                    =   0;
  PROVEN_ERR_NULL_POINTER      =  -1;
  PROVEN_ERR_INVALID_ARGUMENT  =  -2;
  PROVEN_ERR_OVERFLOW          =  -3;
  PROVEN_ERR_UNDERFLOW         =  -4;
  PROVEN_ERR_DIVISION_BY_ZERO  =  -5;
  PROVEN_ERR_PARSE_FAILURE     =  -6;
  PROVEN_ERR_VALIDATION_FAILED =  -7;
  PROVEN_ERR_OUT_OF_BOUNDS     =  -8;
  PROVEN_ERR_ENCODING_ERROR    =  -9;
  PROVEN_ERR_ALLOCATION_FAILED = -10;
  PROVEN_ERR_NOT_IMPLEMENTED   = -99;

// ============================================================================
// JSON Type Constants
// ============================================================================

const
  PROVEN_JSON_NULL    =  0;
  PROVEN_JSON_BOOL    =  1;
  PROVEN_JSON_NUMBER  =  2;
  PROVEN_JSON_STRING  =  3;
  PROVEN_JSON_ARRAY   =  4;
  PROVEN_JSON_OBJECT  =  5;
  PROVEN_JSON_INVALID = -1;

// ============================================================================
// Core Result Types
// ============================================================================

type
  // Result for integer operations.
  TProvenIntResult = packed record
    Status: Int32;
    Value: Int64;
  end;

  // Result for boolean operations.
  TProvenBoolResult = packed record
    Status: Int32;
    Value: ByteBool;
  end;

  // Result for string operations.
  // Caller must free Value using proven_free_string().
  TProvenStringResult = packed record
    Status: Int32;
    Value: PAnsiChar;
    Length: NativeUInt;
  end;

  // Result for floating-point operations.
  TProvenFloatResult = packed record
    Status: Int32;
    Value: Double;
  end;

// ============================================================================
// Network Types
// ============================================================================

type
  // IPv4 address structure (4 octets).
  TProvenIPv4Address = packed record
    Octets: array[0..3] of Byte;
  end;

  // Result for IPv4 parsing.
  TProvenIPv4Result = packed record
    Status: Int32;
    Address: TProvenIPv4Address;
  end;

// ============================================================================
// URL Types
// ============================================================================

type
  // URL components returned by proven_url_parse.
  TProvenUrlComponents = packed record
    Scheme: PAnsiChar;
    SchemeLen: NativeUInt;
    Host: PAnsiChar;
    HostLen: NativeUInt;
    Port: UInt16;
    HasPort: ByteBool;
    Path: PAnsiChar;
    PathLen: NativeUInt;
    Query: PAnsiChar;
    QueryLen: NativeUInt;
    Fragment: PAnsiChar;
    FragmentLen: NativeUInt;
  end;

  PProvenUrlComponents = ^TProvenUrlComponents;

  // Result for URL parsing.
  TProvenUrlResult = packed record
    Status: Int32;
    Components: TProvenUrlComponents;
  end;

// ============================================================================
// DateTime Types
// ============================================================================

type
  // DateTime components (ISO 8601).
  TProvenDateTime = packed record
    Year: Int32;
    Month: Byte;            // 1-12
    Day: Byte;              // 1-31
    Hour: Byte;             // 0-23
    Minute: Byte;           // 0-59
    Second: Byte;           // 0-59
    Nanosecond: UInt32;
    TzOffsetMinutes: Int16; // 0 for UTC, negative for west of UTC
  end;

  // Result for DateTime parsing.
  TProvenDateTimeResult = packed record
    Status: Int32;
    DateTime: TProvenDateTime;
  end;

// ============================================================================
// Version Types
// ============================================================================

type
  // Semantic version structure.
  TProvenSemanticVersion = packed record
    Major: UInt32;
    Minor: UInt32;
    Patch: UInt32;
    PrereleaseLen: NativeUInt;
    Prerelease: PAnsiChar;  // May be nil
  end;

  PProvenSemanticVersion = ^TProvenSemanticVersion;

  // Result for version parsing.
  TProvenVersionResult = packed record
    Status: Int32;
    Version: TProvenSemanticVersion;
  end;

// ============================================================================
// Hex Types
// ============================================================================

type
  // Hex decode result with byte data.
  TProvenHexDecodeResult = packed record
    Status: Int32;
    Data: PByte;
    Length: NativeUInt;
  end;

  PProvenHexDecodeResult = ^TProvenHexDecodeResult;

// ============================================================================
// Color Types
// ============================================================================

type
  // RGB color (8-bit per channel).
  TProvenRGBColor = packed record
    R: Byte;
    G: Byte;
    B: Byte;
  end;

  // HSL color.
  TProvenHSLColor = packed record
    H: Double;  // Hue: 0-360 degrees
    S: Double;  // Saturation: 0-1
    L: Double;  // Lightness: 0-1
  end;

  // Color parse result.
  TProvenColorResult = packed record
    Status: Int32;
    Color: TProvenRGBColor;
  end;

// ============================================================================
// Runtime Management (3 functions)
// ============================================================================

// Initialize the Proven runtime (includes Idris 2 runtime).
// Must be called before any other Proven function. Safe to call multiple times.
function proven_init: Int32; cdecl; external 'proven';

// Cleanup the Proven runtime.
procedure proven_deinit; cdecl; external 'proven';

// Check if the runtime is initialized.
function proven_is_initialized: ByteBool; cdecl; external 'proven';

// ============================================================================
// Version Information (4 functions)
// ============================================================================

// Get FFI ABI version for compatibility checking.
function proven_ffi_abi_version: UInt32; cdecl; external 'proven';

// Get major version number.
function proven_version_major: UInt32; cdecl; external 'proven';

// Get minor version number.
function proven_version_minor: UInt32; cdecl; external 'proven';

// Get patch version number.
function proven_version_patch: UInt32; cdecl; external 'proven';

// Get total module count.
function proven_module_count: UInt32; cdecl; external 'proven';

// ============================================================================
// Memory Management (4 functions)
// ============================================================================

// Free a string allocated by Proven functions.
procedure proven_free_string(Ptr: PAnsiChar); cdecl; external 'proven';

// Free URL components allocated by proven_url_parse.
procedure proven_url_free(Components: PProvenUrlComponents); cdecl; external 'proven';

// Free version result resources (prerelease string).
procedure proven_version_free(Version: PProvenSemanticVersion); cdecl; external 'proven';

// Free hex decode result allocated by proven_hex_decode.
procedure proven_hex_free(Result: PProvenHexDecodeResult); cdecl; external 'proven';

// ============================================================================
// SafeMath (8 functions)
// ============================================================================

// Safe integer division. Returns ERR_DIVISION_BY_ZERO if Denominator is 0.
function proven_math_div(Numerator, Denominator: Int64): TProvenIntResult;
  cdecl; external 'proven';

// Safe modulo. Returns ERR_DIVISION_BY_ZERO if Denominator is 0.
function proven_math_mod(Numerator, Denominator: Int64): TProvenIntResult;
  cdecl; external 'proven';

// Checked addition with overflow detection.
function proven_math_add_checked(A, B: Int64): TProvenIntResult;
  cdecl; external 'proven';

// Checked subtraction with underflow detection.
function proven_math_sub_checked(A, B: Int64): TProvenIntResult;
  cdecl; external 'proven';

// Checked multiplication with overflow detection.
function proven_math_mul_checked(A, B: Int64): TProvenIntResult;
  cdecl; external 'proven';

// Safe absolute value. Returns ERR_OVERFLOW for Int64.MinValue.
function proven_math_abs_safe(N: Int64): TProvenIntResult;
  cdecl; external 'proven';

// Clamp value to [Lo, Hi] range.
function proven_math_clamp(Lo, Hi, Value: Int64): Int64;
  cdecl; external 'proven';

// Integer exponentiation with overflow checking.
function proven_math_pow_checked(Base: Int64; Exp: UInt32): TProvenIntResult;
  cdecl; external 'proven';

// ============================================================================
// SafeString (4 functions)
// ============================================================================

// Check if bytes are valid UTF-8.
function proven_string_is_valid_utf8(Ptr: PByte; Len: NativeUInt): TProvenBoolResult;
  cdecl; external 'proven';

// Escape string for SQL (single quotes). Caller must free result.
function proven_string_escape_sql(Ptr: PByte; Len: NativeUInt): TProvenStringResult;
  cdecl; external 'proven';

// Escape string for HTML (prevents XSS). Caller must free result.
function proven_string_escape_html(Ptr: PByte; Len: NativeUInt): TProvenStringResult;
  cdecl; external 'proven';

// Escape string for JavaScript string literals. Caller must free result.
function proven_string_escape_js(Ptr: PByte; Len: NativeUInt): TProvenStringResult;
  cdecl; external 'proven';

// ============================================================================
// SafePath (2 functions)
// ============================================================================

// Check if path contains directory traversal sequences ("..").
// True = traversal detected.
function proven_path_has_traversal(Ptr: PByte; Len: NativeUInt): TProvenBoolResult;
  cdecl; external 'proven';

// Sanitize a filename by removing dangerous characters. Caller must free result.
function proven_path_sanitize_filename(Ptr: PByte; Len: NativeUInt): TProvenStringResult;
  cdecl; external 'proven';

// ============================================================================
// SafeEmail (1 function)
// ============================================================================

// Validate email address (RFC 5321 simplified).
function proven_email_is_valid(Ptr: PByte; Len: NativeUInt): TProvenBoolResult;
  cdecl; external 'proven';

// ============================================================================
// SafeNetwork (3 functions)
// ============================================================================

// Parse IPv4 address string (e.g., "192.168.1.1").
function proven_network_parse_ipv4(Ptr: PByte; Len: NativeUInt): TProvenIPv4Result;
  cdecl; external 'proven';

// Check if IPv4 address is private (RFC 1918).
function proven_network_ipv4_is_private(Addr: TProvenIPv4Address): ByteBool;
  cdecl; external 'proven';

// Check if IPv4 address is loopback (127.0.0.0/8).
function proven_network_ipv4_is_loopback(Addr: TProvenIPv4Address): ByteBool;
  cdecl; external 'proven';

// ============================================================================
// SafeUrl (1 function, plus proven_url_free above)
// ============================================================================

// Parse a URL into components. Caller must free with proven_url_free.
function proven_url_parse(Ptr: PByte; Len: NativeUInt): TProvenUrlResult;
  cdecl; external 'proven';

// ============================================================================
// SafeCrypto (2 functions)
// ============================================================================

// Constant-time byte comparison (timing-attack safe).
function proven_crypto_constant_time_eq(
  Ptr1: PByte; Len1: NativeUInt;
  Ptr2: PByte; Len2: NativeUInt
): TProvenBoolResult; cdecl; external 'proven';

// Fill buffer with cryptographically secure random bytes.
function proven_crypto_random_bytes(Ptr: PByte; Len: NativeUInt): Int32;
  cdecl; external 'proven';

// ============================================================================
// SafeFloat (5 functions)
// ============================================================================

// Safe floating-point division.
function proven_float_div(A, B: Double): TProvenFloatResult;
  cdecl; external 'proven';

// Check if float is finite (not NaN or Inf).
function proven_float_is_finite(X: Double): ByteBool;
  cdecl; external 'proven';

// Check if float is NaN.
function proven_float_is_nan(X: Double): ByteBool;
  cdecl; external 'proven';

// Safe square root.
function proven_float_sqrt(X: Double): TProvenFloatResult;
  cdecl; external 'proven';

// Safe natural logarithm.
function proven_float_ln(X: Double): TProvenFloatResult;
  cdecl; external 'proven';

// ============================================================================
// SafeJson (2 functions)
// ============================================================================

// Check if string is valid JSON.
function proven_json_is_valid(Ptr: PByte; Len: NativeUInt): TProvenBoolResult;
  cdecl; external 'proven';

// Get JSON value type at root level.
function proven_json_get_type(Ptr: PByte; Len: NativeUInt): Int32;
  cdecl; external 'proven';

// ============================================================================
// SafeDateTime (4 functions)
// ============================================================================

// Parse ISO 8601 date string.
function proven_datetime_parse(Ptr: PByte; Len: NativeUInt): TProvenDateTimeResult;
  cdecl; external 'proven';

// Format DateTime as ISO 8601 string. Caller must free result.
function proven_datetime_format_iso8601(DT: TProvenDateTime): TProvenStringResult;
  cdecl; external 'proven';

// Check if year is a leap year.
function proven_datetime_is_leap_year(Year: Int32): ByteBool;
  cdecl; external 'proven';

// Get number of days in a month. Returns 0 if invalid.
function proven_datetime_days_in_month(Year: Int32; Month: Byte): Byte;
  cdecl; external 'proven';

// ============================================================================
// SafeVersion (2 functions, plus proven_version_free above)
// ============================================================================

// Parse semantic version string (e.g., "1.2.3-alpha").
function proven_version_parse(Ptr: PByte; Len: NativeUInt): TProvenVersionResult;
  cdecl; external 'proven';

// Compare two semantic versions. Negative if A < B, 0 if equal, positive if A > B.
function proven_version_compare(A, B: TProvenSemanticVersion): Int32;
  cdecl; external 'proven';

// ============================================================================
// SafeHex (2 functions, plus proven_hex_free above)
// ============================================================================

// Encode bytes to hex string. Caller must free result.
function proven_hex_encode(Ptr: PByte; Len: NativeUInt; Uppercase: ByteBool): TProvenStringResult;
  cdecl; external 'proven';

// Decode hex string to bytes. Caller must free with proven_hex_free.
function proven_hex_decode(Ptr: PByte; Len: NativeUInt): TProvenHexDecodeResult;
  cdecl; external 'proven';

// ============================================================================
// SafeChecksum (2 functions)
// ============================================================================

// Calculate CRC32 checksum.
function proven_checksum_crc32(Ptr: PByte; Len: NativeUInt): TProvenIntResult;
  cdecl; external 'proven';

// Verify CRC32 matches expected value.
function proven_checksum_verify_crc32(Ptr: PByte; Len: NativeUInt; Expected: UInt32): TProvenBoolResult;
  cdecl; external 'proven';

// ============================================================================
// SafeColor (3 functions)
// ============================================================================

// Parse hex color string (#RRGGBB or #RGB).
function proven_color_parse_hex(Ptr: PByte; Len: NativeUInt): TProvenColorResult;
  cdecl; external 'proven';

// Convert RGB to HSL.
function proven_color_rgb_to_hsl(RGB: TProvenRGBColor): TProvenHSLColor;
  cdecl; external 'proven';

// Format RGB as hex string ("#rrggbb"). Caller must free result.
function proven_color_to_hex(RGB: TProvenRGBColor): TProvenStringResult;
  cdecl; external 'proven';

// ============================================================================
// SafeAngle (4 functions)
// ============================================================================

// Convert degrees to radians.
function proven_angle_deg_to_rad(Degrees: Double): Double;
  cdecl; external 'proven';

// Convert radians to degrees.
function proven_angle_rad_to_deg(Radians: Double): Double;
  cdecl; external 'proven';

// Normalize angle to [0, 360) degrees.
function proven_angle_normalize_degrees(Degrees: Double): Double;
  cdecl; external 'proven';

// Normalize angle to [0, 2*pi) radians.
function proven_angle_normalize_radians(Radians: Double): Double;
  cdecl; external 'proven';

// ============================================================================
// SafeCalculator (1 function)
// ============================================================================

// Evaluate an arithmetic expression safely.
function proven_calculator_eval(Ptr: PByte; Len: NativeUInt): TProvenFloatResult;
  cdecl; external 'proven';

// ============================================================================
// SafeProbability (4 functions)
// ============================================================================

// Create probability value (clamped to [0, 1]).
function proven_probability_create(Value: Double): Double;
  cdecl; external 'proven';

// Multiply probabilities (independent events).
function proven_probability_and(A, B: Double): Double;
  cdecl; external 'proven';

// Add probabilities (mutually exclusive events, clamped to 1.0).
function proven_probability_or_exclusive(A, B: Double): Double;
  cdecl; external 'proven';

// Complement probability (1 - P).
function proven_probability_not(P: Double): Double;
  cdecl; external 'proven';

implementation

end.
