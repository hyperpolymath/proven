/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Proven.FFI - Raw C FFI declarations for libproven

This module declares all `@[extern]` bindings to the libproven C ABI. No logic
is reimplemented here; every function delegates to the formally verified Idris 2
core via the Zig FFI bridge.

## Memory management

- `StringResult` values contain a C-allocated string that must be freed with
  `provenFreeString`. The high-level wrappers in sibling modules handle this
  automatically via `IO` and `ByteArray` marshaling.
- Integer, boolean, and float results do not require freeing.
- Opaque pointer types (buffer, queue, etc.) have matching `*Free` calls.

## Status codes

All fallible operations return a result structure whose first field is
`Int32` status:

| Value | Meaning                   |
|-------|---------------------------|
|   0   | OK                        |
|  -1   | Null pointer              |
|  -2   | Invalid argument          |
|  -3   | Overflow                  |
|  -4   | Underflow                 |
|  -5   | Division by zero          |
|  -6   | Parse failure             |
|  -7   | Validation failed         |
|  -8   | Out of bounds             |
|  -9   | Encoding error            |
| -10   | Allocation failed         |
| -99   | Not implemented           |
-/

namespace Proven.FFI

-- ============================================================================
-- Status codes
-- ============================================================================

/-- Status codes returned by libproven operations. -/
inductive ProvenStatus where
  | ok
  | errNullPointer
  | errInvalidArgument
  | errOverflow
  | errUnderflow
  | errDivisionByZero
  | errParseFailure
  | errValidationFailed
  | errOutOfBounds
  | errEncodingError
  | errAllocationFailed
  | errNotImplemented
  | errUnknown (code : Int32)
  deriving Repr, BEq, Inhabited

/-- Convert a raw `Int32` status code to `ProvenStatus`. -/
def ProvenStatus.ofInt32 (n : Int32) : ProvenStatus :=
  if n == 0 then .ok
  else if n == -1 then .errNullPointer
  else if n == -2 then .errInvalidArgument
  else if n == -3 then .errOverflow
  else if n == -4 then .errUnderflow
  else if n == -5 then .errDivisionByZero
  else if n == -6 then .errParseFailure
  else if n == -7 then .errValidationFailed
  else if n == -8 then .errOutOfBounds
  else if n == -9 then .errEncodingError
  else if n == -10 then .errAllocationFailed
  else if n == -99 then .errNotImplemented
  else .errUnknown n

/-- Human-readable description of a status code. -/
def ProvenStatus.message : ProvenStatus -> String
  | .ok => "OK"
  | .errNullPointer => "Null pointer"
  | .errInvalidArgument => "Invalid argument"
  | .errOverflow => "Arithmetic overflow"
  | .errUnderflow => "Arithmetic underflow"
  | .errDivisionByZero => "Division by zero"
  | .errParseFailure => "Parse failure"
  | .errValidationFailed => "Validation failed"
  | .errOutOfBounds => "Out of bounds"
  | .errEncodingError => "Encoding error"
  | .errAllocationFailed => "Allocation failed"
  | .errNotImplemented => "Not implemented"
  | .errUnknown code => s!"Unknown error (code {code})"

instance : ToString ProvenStatus := ⟨ProvenStatus.message⟩

-- ============================================================================
-- Result structures matching C ABI
-- ============================================================================

/-- Corresponds to C `ProvenIntResult { int32_t status; int64_t value; }`. -/
structure IntResult where
  status : Int32
  value  : Int64
  deriving Repr, Inhabited

/-- Corresponds to C `ProvenBoolResult { int32_t status; bool value; }`. -/
structure BoolResult where
  status : Int32
  value  : Bool
  deriving Repr, Inhabited

/-- Corresponds to C `ProvenFloatResult { int32_t status; double value; }`. -/
structure FloatResult where
  status : Int32
  value  : Float
  deriving Repr, Inhabited

/-- Opaque pointer to a C-allocated string with length.
    Must be freed via `provenFreeString`. -/
structure StringResultRaw where
  status : Int32
  ptr    : USize  -- char* as opaque pointer
  len    : USize
  deriving Repr, Inhabited

/-- IPv4 address as four octets. -/
structure IPv4Address where
  a : UInt8
  b : UInt8
  c : UInt8
  d : UInt8
  deriving Repr, BEq, Inhabited

/-- Result for IPv4 parsing. -/
structure IPv4Result where
  status  : Int32
  address : IPv4Address
  deriving Repr, Inhabited

/-- DateTime components matching C `ProvenDateTime`. -/
structure DateTime where
  year             : Int32
  month            : UInt8
  day              : UInt8
  hour             : UInt8
  minute           : UInt8
  second           : UInt8
  nanosecond       : UInt32
  tzOffsetMinutes  : Int16
  deriving Repr, BEq, Inhabited

/-- Result for DateTime parsing. -/
structure DateTimeResult where
  status   : Int32
  datetime : DateTime
  deriving Repr, Inhabited

-- ============================================================================
-- Lifecycle FFI declarations
-- ============================================================================

/-- Initialize the Proven runtime. Must be called before any other function.
    Returns 0 on success. Safe to call multiple times. -/
@[extern "proven_init"]
opaque provenInit : IO Int32

/-- Shut down the Proven runtime. All allocated resources should be freed
    before calling this. -/
@[extern "proven_deinit"]
opaque provenDeinit : IO Unit

/-- Check whether the Proven runtime is currently initialized. -/
@[extern "proven_is_initialized"]
opaque provenIsInitialized : IO Bool

-- ============================================================================
-- Version information FFI declarations
-- ============================================================================

/-- Get the FFI ABI version for compatibility checking. -/
@[extern "proven_ffi_abi_version"]
opaque provenFfiAbiVersion : IO UInt32

/-- Get library major version number. -/
@[extern "proven_version_major"]
opaque provenVersionMajor : IO UInt32

/-- Get library minor version number. -/
@[extern "proven_version_minor"]
opaque provenVersionMinor : IO UInt32

/-- Get library patch version number. -/
@[extern "proven_version_patch"]
opaque provenVersionPatch : IO UInt32

/-- Get total module count in the library. -/
@[extern "proven_module_count"]
opaque provenModuleCount : IO UInt32

-- ============================================================================
-- Memory management FFI declarations
-- ============================================================================

/-- Free a C-allocated string returned by libproven. -/
@[extern "proven_free_string"]
opaque provenFreeString (ptr : USize) : IO Unit

-- ============================================================================
-- SafeMath FFI declarations
-- ============================================================================

/-- Checked addition with overflow detection. -/
@[extern "proven_math_add_checked"]
opaque provenMathAddChecked (a : Int64) (b : Int64) : IO IntResult

/-- Checked subtraction with underflow detection. -/
@[extern "proven_math_sub_checked"]
opaque provenMathSubChecked (a : Int64) (b : Int64) : IO IntResult

/-- Checked multiplication with overflow detection. -/
@[extern "proven_math_mul_checked"]
opaque provenMathMulChecked (a : Int64) (b : Int64) : IO IntResult

/-- Safe integer division. Returns error on division by zero or
    `Int64.minValue / -1` overflow. -/
@[extern "proven_math_div"]
opaque provenMathDiv (numerator : Int64) (denominator : Int64) : IO IntResult

/-- Safe modulo operation. Returns error on division by zero. -/
@[extern "proven_math_mod"]
opaque provenMathMod (numerator : Int64) (denominator : Int64) : IO IntResult

/-- Safe absolute value. Returns error for `Int64.minValue`. -/
@[extern "proven_math_abs_safe"]
opaque provenMathAbsSafe (n : Int64) : IO IntResult

/-- Clamp value to `[lo, hi]` range. -/
@[extern "proven_math_clamp"]
opaque provenMathClamp (lo : Int64) (hi : Int64) (value : Int64) : IO Int64

/-- Checked exponentiation with overflow detection. -/
@[extern "proven_math_pow_checked"]
opaque provenMathPowChecked (base : Int64) (exp : UInt32) : IO IntResult

-- ============================================================================
-- SafeString FFI declarations
-- ============================================================================

/-- Check if byte data is valid UTF-8. -/
@[extern "lean_proven_string_is_valid_utf8"]
opaque provenStringIsValidUtf8 (data : @& ByteArray) : IO BoolResult

/-- Escape string for SQL (single quotes). Caller must free result. -/
@[extern "lean_proven_string_escape_sql"]
opaque provenStringEscapeSql (data : @& ByteArray) : IO StringResultRaw

/-- Escape string for HTML (prevents XSS). Caller must free result. -/
@[extern "lean_proven_string_escape_html"]
opaque provenStringEscapeHtml (data : @& ByteArray) : IO StringResultRaw

/-- Escape string for JavaScript string literals. Caller must free result. -/
@[extern "lean_proven_string_escape_js"]
opaque provenStringEscapeJs (data : @& ByteArray) : IO StringResultRaw

-- ============================================================================
-- SafePath FFI declarations
-- ============================================================================

/-- Check if path contains directory traversal sequences (`..`).
    Returns `true` if traversal detected. -/
@[extern "lean_proven_path_has_traversal"]
opaque provenPathHasTraversal (data : @& ByteArray) : IO BoolResult

/-- Sanitize a filename by removing dangerous characters.
    Caller must free result. -/
@[extern "lean_proven_path_sanitize_filename"]
opaque provenPathSanitizeFilename (data : @& ByteArray) : IO StringResultRaw

-- ============================================================================
-- SafeEmail FFI declarations
-- ============================================================================

/-- Validate email address (RFC 5321 simplified). -/
@[extern "lean_proven_email_is_valid"]
opaque provenEmailIsValid (data : @& ByteArray) : IO BoolResult

-- ============================================================================
-- SafeUrl FFI declarations
-- ============================================================================

/-- URL components returned by URL parsing. All string fields are C-allocated
    and must be freed together via `provenUrlFree`. Represented as raw fields
    for FFI compatibility. -/
structure UrlComponentsRaw where
  schemePtr    : USize
  schemeLen    : USize
  hostPtr      : USize
  hostLen      : USize
  port         : UInt16
  hasPort      : Bool
  pathPtr      : USize
  pathLen      : USize
  queryPtr     : USize
  queryLen     : USize
  fragmentPtr  : USize
  fragmentLen  : USize
  deriving Repr, Inhabited

/-- Raw result for URL parsing. -/
structure UrlResultRaw where
  status     : Int32
  components : UrlComponentsRaw
  deriving Repr, Inhabited

/-- Parse a URL into components. Caller must free with `provenUrlFree`. -/
@[extern "lean_proven_url_parse"]
opaque provenUrlParse (data : @& ByteArray) : IO UrlResultRaw

/-- Free URL components allocated by `provenUrlParse`. -/
@[extern "lean_proven_url_free"]
opaque provenUrlFree (components : UrlComponentsRaw) : IO Unit

-- ============================================================================
-- SafeNetwork FFI declarations
-- ============================================================================

/-- Parse an IPv4 address string (e.g., "192.168.1.1"). -/
@[extern "lean_proven_network_parse_ipv4"]
opaque provenNetworkParseIpv4 (data : @& ByteArray) : IO IPv4Result

/-- Check if IPv4 address is private (RFC 1918). -/
@[extern "lean_proven_network_ipv4_is_private"]
opaque provenNetworkIpv4IsPrivate (addr : IPv4Address) : IO Bool

/-- Check if IPv4 address is loopback (127.0.0.0/8). -/
@[extern "lean_proven_network_ipv4_is_loopback"]
opaque provenNetworkIpv4IsLoopback (addr : IPv4Address) : IO Bool

-- ============================================================================
-- SafeCrypto FFI declarations
-- ============================================================================

/-- Constant-time byte comparison (timing-attack safe). -/
@[extern "lean_proven_crypto_constant_time_eq"]
opaque provenCryptoConstantTimeEq (a : @& ByteArray) (b : @& ByteArray) : IO BoolResult

/-- Fill a `ByteArray` with cryptographically secure random bytes. -/
@[extern "lean_proven_crypto_random_bytes"]
opaque provenCryptoRandomBytes (len : USize) : IO ByteArray

-- ============================================================================
-- SafeJson FFI declarations
-- ============================================================================

/-- Check if string is valid JSON. -/
@[extern "lean_proven_json_is_valid"]
opaque provenJsonIsValid (data : @& ByteArray) : IO BoolResult

/-- JSON value type enumeration. -/
inductive JsonType where
  | null
  | bool
  | number
  | string
  | array
  | object
  | invalid
  deriving Repr, BEq, Inhabited

/-- Convert raw `Int32` to `JsonType`. -/
def JsonType.ofInt32 (n : Int32) : JsonType :=
  if n == 0 then .null
  else if n == 1 then .bool
  else if n == 2 then .number
  else if n == 3 then .string
  else if n == 4 then .array
  else if n == 5 then .object
  else .invalid

/-- Get JSON value type at root level. -/
@[extern "lean_proven_json_get_type"]
opaque provenJsonGetType (data : @& ByteArray) : IO Int32

-- ============================================================================
-- SafeDateTime FFI declarations
-- ============================================================================

/-- Parse ISO 8601 date string. -/
@[extern "lean_proven_datetime_parse"]
opaque provenDatetimeParse (data : @& ByteArray) : IO DateTimeResult

/-- Format DateTime as ISO 8601 string. Caller must free result. -/
@[extern "lean_proven_datetime_format_iso8601"]
opaque provenDatetimeFormatIso8601 (dt : DateTime) : IO StringResultRaw

/-- Check if year is a leap year. -/
@[extern "proven_datetime_is_leap_year"]
opaque provenDatetimeIsLeapYear (year : Int32) : IO Bool

/-- Get number of days in a month. Returns 0 if invalid month. -/
@[extern "proven_datetime_days_in_month"]
opaque provenDatetimeDaysInMonth (year : Int32) (month : UInt8) : IO UInt8

-- ============================================================================
-- SafeFloat FFI declarations
-- ============================================================================

/-- Safe floating-point division. -/
@[extern "proven_float_div"]
opaque provenFloatDiv (a : Float) (b : Float) : IO FloatResult

/-- Check if float is finite (not NaN or Inf). -/
@[extern "proven_float_is_finite"]
opaque provenFloatIsFinite (x : Float) : IO Bool

/-- Check if float is NaN. -/
@[extern "proven_float_is_nan"]
opaque provenFloatIsNan (x : Float) : IO Bool

/-- Safe square root. Returns error if x is negative or NaN. -/
@[extern "proven_float_sqrt"]
opaque provenFloatSqrt (x : Float) : IO FloatResult

/-- Safe natural logarithm. Returns error if x <= 0 or NaN. -/
@[extern "proven_float_ln"]
opaque provenFloatLn (x : Float) : IO FloatResult

-- ============================================================================
-- SafeHex FFI declarations
-- ============================================================================

/-- Encode bytes to hex string. -/
@[extern "lean_proven_hex_encode"]
opaque provenHexEncode (data : @& ByteArray) (uppercase : Bool) : IO StringResultRaw

/-- Decode hex string to bytes. -/
@[extern "lean_proven_hex_decode"]
opaque provenHexDecode (data : @& ByteArray) : IO StringResultRaw

-- ============================================================================
-- SafeChecksum FFI declarations
-- ============================================================================

/-- Calculate CRC32 checksum. -/
@[extern "lean_proven_checksum_crc32"]
opaque provenChecksumCrc32 (data : @& ByteArray) : IO IntResult

/-- Verify CRC32 matches expected value. -/
@[extern "lean_proven_checksum_verify_crc32"]
opaque provenChecksumVerifyCrc32 (data : @& ByteArray) (expected : UInt32) : IO BoolResult

-- ============================================================================
-- Helper: marshal StringResultRaw to Lean String
-- ============================================================================

/-- Copy bytes from a C pointer into a `ByteArray`, then free the C string.
    This is the canonical way to convert `StringResultRaw` to a Lean `String`.
    Returns `none` if the status indicates an error. -/
@[extern "lean_proven_marshal_string_result"]
opaque marshalStringResult (raw : StringResultRaw) : IO (Option String)

end Proven.FFI
