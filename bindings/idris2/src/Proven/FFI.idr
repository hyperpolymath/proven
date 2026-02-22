-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Raw %foreign "C" declarations for libproven.
|||
||| This module declares every C function exported by the precompiled
||| libproven shared library. Each declaration uses the
||| `%foreign "C:symbol_name,libproven"` syntax so the Idris 2 RefC
||| codegen links against the shared object at compile time.
|||
||| These are low-level, effectful primitives. For safe, total wrappers
||| that return `Maybe` types, use the individual `Proven.Safe*` modules.
|||
||| String-accepting C functions (which take `const uint8_t* ptr, size_t len`)
||| are declared here with `(String, Int)` parameters. The RefC backend
||| marshals `String` as `const char*` and `Int` as a machine word, which
||| is ABI-compatible with the C signatures.
|||
||| Memory management rules:
|||   - String results from C must be freed with `prim__proven_free_string`
|||   - Integer, boolean, and float results need no freeing
|||   - Opaque pointers (URL components, etc.) have matching free functions
module Proven.FFI

import System.FFI

%default total

-- ============================================================================
-- Status code constants
-- ============================================================================

||| Success status code (PROVEN_OK = 0).
public export
statusOK : Int
statusOK = 0

||| Null pointer error (PROVEN_ERR_NULL_POINTER = -1).
public export
statusErrNullPointer : Int
statusErrNullPointer = -1

||| Invalid argument error (PROVEN_ERR_INVALID_ARGUMENT = -2).
public export
statusErrInvalidArgument : Int
statusErrInvalidArgument = -2

||| Integer overflow error (PROVEN_ERR_OVERFLOW = -3).
public export
statusErrOverflow : Int
statusErrOverflow = -3

||| Integer underflow error (PROVEN_ERR_UNDERFLOW = -4).
public export
statusErrUnderflow : Int
statusErrUnderflow = -4

||| Division by zero error (PROVEN_ERR_DIVISION_BY_ZERO = -5).
public export
statusErrDivisionByZero : Int
statusErrDivisionByZero = -5

||| Parse failure error (PROVEN_ERR_PARSE_FAILURE = -6).
public export
statusErrParseFailure : Int
statusErrParseFailure = -6

||| Validation failed error (PROVEN_ERR_VALIDATION_FAILED = -7).
public export
statusErrValidationFailed : Int
statusErrValidationFailed = -7

||| Index out of bounds error (PROVEN_ERR_OUT_OF_BOUNDS = -8).
public export
statusErrOutOfBounds : Int
statusErrOutOfBounds = -8

||| Encoding error (PROVEN_ERR_ENCODING_ERROR = -9).
public export
statusErrEncodingError : Int
statusErrEncodingError = -9

||| Allocation failure error (PROVEN_ERR_ALLOCATION_FAILED = -10).
public export
statusErrAllocationFailed : Int
statusErrAllocationFailed = -10

||| Not implemented error (PROVEN_ERR_NOT_IMPLEMENTED = -99).
public export
statusErrNotImplemented : Int
statusErrNotImplemented = -99

-- ============================================================================
-- ProvenError: A typed representation of libproven error codes
-- ============================================================================

||| Error codes returned by libproven functions, represented as an
||| algebraic data type for pattern matching.
public export
data ProvenError
  = NullPointer
  | InvalidArgument
  | Overflow
  | Underflow
  | DivisionByZero
  | ParseFailure
  | ValidationFailed
  | OutOfBounds
  | EncodingError
  | AllocationFailed
  | NotImplemented
  | UnknownError Int

||| Display a human-readable description of a ProvenError.
public export
Show ProvenError where
  show NullPointer      = "ProvenError: null pointer"
  show InvalidArgument  = "ProvenError: invalid argument"
  show Overflow         = "ProvenError: integer overflow"
  show Underflow        = "ProvenError: integer underflow"
  show DivisionByZero   = "ProvenError: division by zero"
  show ParseFailure     = "ProvenError: parse failure"
  show ValidationFailed = "ProvenError: validation failed"
  show OutOfBounds      = "ProvenError: index out of bounds"
  show EncodingError    = "ProvenError: encoding error"
  show AllocationFailed = "ProvenError: allocation failed"
  show NotImplemented   = "ProvenError: not implemented"
  show (UnknownError n) = "ProvenError: unknown error code " ++ show n

||| Convert an integer status code to a ProvenError.
||| Returns `Nothing` for statusOK (0), `Just err` for non-zero codes.
public export
statusToError : Int -> Maybe ProvenError
statusToError 0    = Nothing
statusToError (-1) = Just NullPointer
statusToError (-2) = Just InvalidArgument
statusToError (-3) = Just Overflow
statusToError (-4) = Just Underflow
statusToError (-5) = Just DivisionByZero
statusToError (-6) = Just ParseFailure
statusToError (-7) = Just ValidationFailed
statusToError (-8) = Just OutOfBounds
statusToError (-9) = Just EncodingError
statusToError (-10) = Just AllocationFailed
statusToError (-99) = Just NotImplemented
statusToError n    = Just (UnknownError n)

||| Convert a status code to `Either ProvenError ()`.
||| Useful for operations that return only a status with no payload.
public export
statusToEither : Int -> Either ProvenError ()
statusToEither s = case statusToError s of
  Nothing  => Right ()
  Just err => Left err

||| Check whether a status code indicates success.
public export
isOK : Int -> Bool
isOK 0 = True
isOK _ = False

-- ============================================================================
-- JSON value types (for SafeJson)
-- ============================================================================

||| JSON value types as returned by `proven_json_get_type`.
public export
data JsonType
  = JsonNull
  | JsonBool
  | JsonNumber
  | JsonString
  | JsonArray
  | JsonObject
  | JsonInvalid

||| Display a JSON type.
public export
Show JsonType where
  show JsonNull    = "null"
  show JsonBool    = "boolean"
  show JsonNumber  = "number"
  show JsonString  = "string"
  show JsonArray   = "array"
  show JsonObject  = "object"
  show JsonInvalid = "invalid"

||| Convert the integer type code from C to a JsonType.
public export
intToJsonType : Int -> JsonType
intToJsonType 0    = JsonNull
intToJsonType 1    = JsonBool
intToJsonType 2    = JsonNumber
intToJsonType 3    = JsonString
intToJsonType 4    = JsonArray
intToJsonType 5    = JsonObject
intToJsonType _    = JsonInvalid

-- ============================================================================
-- Lifecycle
-- ============================================================================

||| Initialise the proven runtime (includes Idris 2 runtime).
||| Must be called before any other proven function. Safe to call multiple times.
||| Returns the status code: 0 on success, negative on error.
%foreign "C:proven_init,libproven"
export
prim__proven_init : PrimIO Int

||| Shut down the proven runtime.
||| Call when done using proven functions. All allocated resources should
||| be freed before calling this.
%foreign "C:proven_deinit,libproven"
export
prim__proven_deinit : PrimIO ()

||| Check whether the runtime is initialised.
||| Returns non-zero (true) if initialised, zero (false) otherwise.
%foreign "C:proven_is_initialized,libproven"
export
prim__proven_is_initialized : PrimIO Int

-- ============================================================================
-- Version
-- ============================================================================

||| Get the FFI ABI version for compatibility checking.
%foreign "C:proven_ffi_abi_version,libproven"
export
prim__proven_ffi_abi_version : PrimIO Int

||| Get the major version number.
%foreign "C:proven_version_major,libproven"
export
prim__proven_version_major : PrimIO Int

||| Get the minor version number.
%foreign "C:proven_version_minor,libproven"
export
prim__proven_version_minor : PrimIO Int

||| Get the patch version number.
%foreign "C:proven_version_patch,libproven"
export
prim__proven_version_patch : PrimIO Int

||| Get the total module count in libproven.
%foreign "C:proven_module_count,libproven"
export
prim__proven_module_count : PrimIO Int

-- ============================================================================
-- Memory management
-- ============================================================================

||| Free a string allocated by a proven function.
||| Must be called for every successful string result from the C layer.
||| Passing a null/invalid pointer is safe (no-op).
%foreign "C:proven_free_string,libproven"
export
prim__proven_free_string : AnyPtr -> PrimIO ()

-- ============================================================================
-- SafeMath (8 functions)
-- ============================================================================

||| Safe integer division. Returns status and quotient.
||| Status is PROVEN_ERR_DIVISION_BY_ZERO if denominator is 0,
||| PROVEN_ERR_OVERFLOW for INT64_MIN / -1.
%foreign "C:proven_math_div,libproven"
export
prim__proven_math_div : Int64 -> Int64 -> PrimIO (Int, Int64)

||| Safe modulo. Returns status and remainder.
%foreign "C:proven_math_mod,libproven"
export
prim__proven_math_mod : Int64 -> Int64 -> PrimIO (Int, Int64)

||| Checked addition with overflow detection.
%foreign "C:proven_math_add_checked,libproven"
export
prim__proven_math_add_checked : Int64 -> Int64 -> PrimIO (Int, Int64)

||| Checked subtraction with underflow detection.
%foreign "C:proven_math_sub_checked,libproven"
export
prim__proven_math_sub_checked : Int64 -> Int64 -> PrimIO (Int, Int64)

||| Checked multiplication with overflow detection.
%foreign "C:proven_math_mul_checked,libproven"
export
prim__proven_math_mul_checked : Int64 -> Int64 -> PrimIO (Int, Int64)

||| Safe absolute value. PROVEN_ERR_OVERFLOW for INT64_MIN.
%foreign "C:proven_math_abs_safe,libproven"
export
prim__proven_math_abs_safe : Int64 -> PrimIO (Int, Int64)

||| Clamp value to [lo, hi] range. Pure function, cannot fail.
%foreign "C:proven_math_clamp,libproven"
export
prim__proven_math_clamp : Int64 -> Int64 -> Int64 -> Int64

||| Integer exponentiation with overflow checking.
||| Exponent is passed as a 32-bit unsigned integer.
%foreign "C:proven_math_pow_checked,libproven"
export
prim__proven_math_pow_checked : Int64 -> Int -> PrimIO (Int, Int64)

-- ============================================================================
-- SafeString (4 functions)
-- ============================================================================

||| Check if bytes are valid UTF-8.
||| Takes the string data as a String (char*) and its length.
%foreign "C:proven_string_is_valid_utf8,libproven"
export
prim__proven_string_is_valid_utf8 : String -> Int -> PrimIO (Int, Int)

||| Escape string for SQL. Returns (status, result_ptr, result_len).
||| Caller must free result_ptr via prim__proven_free_string.
%foreign "C:proven_string_escape_sql,libproven"
export
prim__proven_string_escape_sql : String -> Int -> PrimIO (Int, AnyPtr, Int)

||| Escape string for HTML. Returns (status, result_ptr, result_len).
||| Caller must free result_ptr via prim__proven_free_string.
%foreign "C:proven_string_escape_html,libproven"
export
prim__proven_string_escape_html : String -> Int -> PrimIO (Int, AnyPtr, Int)

||| Escape string for JavaScript. Returns (status, result_ptr, result_len).
||| Caller must free result_ptr via prim__proven_free_string.
%foreign "C:proven_string_escape_js,libproven"
export
prim__proven_string_escape_js : String -> Int -> PrimIO (Int, AnyPtr, Int)

-- ============================================================================
-- SafePath (2 functions)
-- ============================================================================

||| Check if path contains directory traversal sequences ("..").
||| Returns (status, bool_value) where bool_value is non-zero if traversal found.
%foreign "C:proven_path_has_traversal,libproven"
export
prim__proven_path_has_traversal : String -> Int -> PrimIO (Int, Int)

||| Sanitise a filename by removing dangerous characters.
||| Returns (status, result_ptr, result_len). Caller must free result_ptr.
%foreign "C:proven_path_sanitize_filename,libproven"
export
prim__proven_path_sanitize_filename : String -> Int -> PrimIO (Int, AnyPtr, Int)

-- ============================================================================
-- SafeEmail (1 function)
-- ============================================================================

||| Validate email address (RFC 5321 simplified).
||| Returns (status, bool_value) where bool_value is non-zero if valid.
%foreign "C:proven_email_is_valid,libproven"
export
prim__proven_email_is_valid : String -> Int -> PrimIO (Int, Int)

-- ============================================================================
-- SafeNetwork (3 functions)
-- ============================================================================

||| Parse IPv4 address string.
||| Returns an opaque struct pointer. Use the SafeNetwork wrapper to
||| extract octets and status.
%foreign "C:proven_network_parse_ipv4,libproven"
export
prim__proven_network_parse_ipv4 : String -> Int -> PrimIO AnyPtr

||| Check if IPv4 address is private (RFC 1918).
||| Takes the four octets packed as a struct (passed via AnyPtr).
%foreign "C:proven_network_ipv4_is_private,libproven"
export
prim__proven_network_ipv4_is_private : AnyPtr -> PrimIO Int

||| Check if IPv4 address is loopback (127.0.0.0/8).
%foreign "C:proven_network_ipv4_is_loopback,libproven"
export
prim__proven_network_ipv4_is_loopback : AnyPtr -> PrimIO Int

-- ============================================================================
-- SafeCrypto (2 functions)
-- ============================================================================

||| Constant-time byte comparison (timing-attack safe).
||| Returns (status, bool_value).
%foreign "C:proven_crypto_constant_time_eq,libproven"
export
prim__proven_crypto_constant_time_eq : String -> Int -> String -> Int -> PrimIO (Int, Int)

||| Fill buffer with cryptographically secure random bytes.
||| Returns a status code.
%foreign "C:proven_crypto_random_bytes,libproven"
export
prim__proven_crypto_random_bytes : AnyPtr -> Int -> PrimIO Int

-- ============================================================================
-- SafeUrl (2 functions)
-- ============================================================================

||| Parse a URL into components.
||| Returns an opaque pointer to a ProvenUrlResult struct.
||| Use the SafeUrl wrapper module to extract components.
||| Caller must free via prim__proven_url_free.
%foreign "C:proven_url_parse,libproven"
export
prim__proven_url_parse : String -> Int -> PrimIO AnyPtr

||| Free URL components allocated by proven_url_parse.
%foreign "C:proven_url_free,libproven"
export
prim__proven_url_free : AnyPtr -> PrimIO ()

-- ============================================================================
-- SafeJson (2 functions)
-- ============================================================================

||| Check if string is valid JSON.
||| Returns (status, bool_value) where bool_value is non-zero if valid.
%foreign "C:proven_json_is_valid,libproven"
export
prim__proven_json_is_valid : String -> Int -> PrimIO (Int, Int)

||| Get JSON value type at root level.
||| Returns: 0=null, 1=bool, 2=number, 3=string, 4=array, 5=object, -1=invalid.
%foreign "C:proven_json_get_type,libproven"
export
prim__proven_json_get_type : String -> Int -> PrimIO Int

-- ============================================================================
-- SafeDateTime (4 functions)
-- ============================================================================

||| Parse ISO 8601 date string.
||| Returns an opaque pointer to a ProvenDateTimeResult struct.
||| Use the SafeDateTime wrapper module to extract components.
%foreign "C:proven_datetime_parse,libproven"
export
prim__proven_datetime_parse : String -> Int -> PrimIO AnyPtr

||| Format DateTime as ISO 8601 string.
||| Takes a pointer to a ProvenDateTime struct.
||| Returns (status, result_ptr, result_len). Caller must free result_ptr.
%foreign "C:proven_datetime_format_iso8601,libproven"
export
prim__proven_datetime_format_iso8601 : AnyPtr -> PrimIO (Int, AnyPtr, Int)

||| Check if year is a leap year. Pure function.
%foreign "C:proven_datetime_is_leap_year,libproven"
export
prim__proven_datetime_is_leap_year : Int -> Int

||| Get number of days in a month. Returns 0 if invalid month. Pure function.
%foreign "C:proven_datetime_days_in_month,libproven"
export
prim__proven_datetime_days_in_month : Int -> Int -> Int

-- ============================================================================
-- SafeFloat (5 functions)
-- ============================================================================

||| Safe floating-point division.
||| Returns (status, value). PROVEN_ERR_DIVISION_BY_ZERO if b is 0.
%foreign "C:proven_float_div,libproven"
export
prim__proven_float_div : Double -> Double -> PrimIO (Int, Double)

||| Check if float is finite (not NaN or Inf). Pure function.
||| Returns non-zero if finite.
%foreign "C:proven_float_is_finite,libproven"
export
prim__proven_float_is_finite : Double -> Int

||| Check if float is NaN. Pure function.
||| Returns non-zero if NaN.
%foreign "C:proven_float_is_nan,libproven"
export
prim__proven_float_is_nan : Double -> Int

||| Safe square root.
||| Returns (status, value). PROVEN_ERR_INVALID_ARGUMENT if x < 0.
%foreign "C:proven_float_sqrt,libproven"
export
prim__proven_float_sqrt : Double -> PrimIO (Int, Double)

||| Safe natural logarithm.
||| Returns (status, value). PROVEN_ERR_INVALID_ARGUMENT if x <= 0.
%foreign "C:proven_float_ln,libproven"
export
prim__proven_float_ln : Double -> PrimIO (Int, Double)

-- ============================================================================
-- SafeHex (3 functions)
-- ============================================================================

||| Encode bytes to hex string.
||| Third parameter: non-zero for uppercase hex digits.
||| Returns (status, result_ptr, result_len). Caller must free result_ptr.
%foreign "C:proven_hex_encode,libproven"
export
prim__proven_hex_encode : String -> Int -> Int -> PrimIO (Int, AnyPtr, Int)

||| Decode hex string to bytes.
||| Returns an opaque pointer to a ProvenHexDecodeResult struct.
||| Caller must free via prim__proven_hex_free.
%foreign "C:proven_hex_decode,libproven"
export
prim__proven_hex_decode : String -> Int -> PrimIO AnyPtr

||| Free hex decode result.
%foreign "C:proven_hex_free,libproven"
export
prim__proven_hex_free : AnyPtr -> PrimIO ()

-- ============================================================================
-- SafeColor (3 functions)
-- ============================================================================

||| Parse hex color string (#RRGGBB or #RGB).
||| Returns an opaque pointer containing (status, r, g, b).
%foreign "C:proven_color_parse_hex,libproven"
export
prim__proven_color_parse_hex : String -> Int -> PrimIO AnyPtr

||| Format RGB as hex string ("#rrggbb", lowercase).
||| Returns (status, result_ptr, result_len). Caller must free result_ptr.
%foreign "C:proven_color_to_hex,libproven"
export
prim__proven_color_to_hex : Int -> Int -> Int -> PrimIO (Int, AnyPtr, Int)

||| Convert RGB to HSL. Returns an opaque pointer to ProvenHSLColor.
%foreign "C:proven_color_rgb_to_hsl,libproven"
export
prim__proven_color_rgb_to_hsl : Int -> Int -> Int -> PrimIO AnyPtr

-- ============================================================================
-- SafeChecksum (2 functions)
-- ============================================================================

||| Calculate CRC32 checksum.
||| Returns (status, crc32_value).
%foreign "C:proven_checksum_crc32,libproven"
export
prim__proven_checksum_crc32 : String -> Int -> PrimIO (Int, Int64)

||| Verify CRC32 matches expected value.
||| Returns (status, bool_value) where bool_value is non-zero if match.
%foreign "C:proven_checksum_verify_crc32,libproven"
export
prim__proven_checksum_verify_crc32 : String -> Int -> Int -> PrimIO (Int, Int)

-- ============================================================================
-- SafeVersion (3 functions)
-- ============================================================================

||| Parse semantic version string.
||| Returns an opaque pointer to a ProvenVersionResult struct.
||| Caller must free via prim__proven_version_free.
%foreign "C:proven_version_parse,libproven"
export
prim__proven_version_parse : String -> Int -> PrimIO AnyPtr

||| Compare two semantic versions.
||| Takes two opaque version struct pointers.
||| Returns negative if a < b, 0 if equal, positive if a > b.
%foreign "C:proven_version_compare,libproven"
export
prim__proven_version_compare : AnyPtr -> AnyPtr -> Int

||| Free version result resources (prerelease string).
%foreign "C:proven_version_free,libproven"
export
prim__proven_version_free : AnyPtr -> PrimIO ()

-- ============================================================================
-- SafeGeo (3 functions)
-- ============================================================================

||| Validate and normalise geographic coordinate.
||| Returns (status, lat, lon).
%foreign "C:proven_geo_validate,libproven"
export
prim__proven_geo_validate : Double -> Double -> PrimIO (Int, Double, Double)

||| Calculate distance between two points (Haversine formula).
||| Takes (lat1, lon1, lat2, lon2) flattened from the two coord structs.
||| Returns (status, distance_in_metres).
%foreign "C:proven_geo_distance,libproven"
export
prim__proven_geo_distance : Double -> Double -> Double -> Double -> PrimIO (Int, Double)

||| Check if coordinate is inside a bounding box.
||| Takes (coord_lat, coord_lon, min_lat, max_lat, min_lon, max_lon).
||| Returns non-zero if in bounds.
%foreign "C:proven_geo_in_bounds,libproven"
export
prim__proven_geo_in_bounds : Double -> Double -> Double -> Double -> Double -> Double -> Int

-- ============================================================================
-- SafeProbability (4 pure functions)
-- ============================================================================

||| Create probability value clamped to [0, 1]. Pure.
%foreign "C:proven_probability_create,libproven"
export
prim__proven_probability_create : Double -> Double

||| Multiply probabilities (independent events: P(A and B)). Pure.
%foreign "C:proven_probability_and,libproven"
export
prim__proven_probability_and : Double -> Double -> Double

||| Add probabilities (mutually exclusive events: P(A or B)). Pure.
%foreign "C:proven_probability_or_exclusive,libproven"
export
prim__proven_probability_or_exclusive : Double -> Double -> Double

||| Complement probability: P(not A) = 1 - P(A). Pure.
%foreign "C:proven_probability_not,libproven"
export
prim__proven_probability_not : Double -> Double

-- ============================================================================
-- SafeAngle (4 pure functions)
-- ============================================================================

||| Convert degrees to radians. Pure.
%foreign "C:proven_angle_deg_to_rad,libproven"
export
prim__proven_angle_deg_to_rad : Double -> Double

||| Convert radians to degrees. Pure.
%foreign "C:proven_angle_rad_to_deg,libproven"
export
prim__proven_angle_rad_to_deg : Double -> Double

||| Normalise angle to [0, 360) degrees. Pure.
%foreign "C:proven_angle_normalize_degrees,libproven"
export
prim__proven_angle_normalize_degrees : Double -> Double

||| Normalise angle to [0, 2*pi) radians. Pure.
%foreign "C:proven_angle_normalize_radians,libproven"
export
prim__proven_angle_normalize_radians : Double -> Double

-- ============================================================================
-- SafeCalculator (1 function)
-- ============================================================================

||| Evaluate arithmetic expression safely.
||| Supports +, -, *, /, parentheses, negative and decimal numbers.
||| Returns (status, result_value).
%foreign "C:proven_calculator_eval,libproven"
export
prim__proven_calculator_eval : String -> Int -> PrimIO (Int, Double)

-- ============================================================================
-- SafePassword (2 functions)
-- ============================================================================

||| Validate password strength.
||| Returns an opaque pointer to a ProvenPasswordResult struct.
%foreign "C:proven_password_validate,libproven"
export
prim__proven_password_validate : String -> Int -> PrimIO AnyPtr

||| Check if password is in common passwords list.
||| Returns non-zero if the password is common.
%foreign "C:proven_password_is_common,libproven"
export
prim__proven_password_is_common : String -> Int -> PrimIO Int

-- ============================================================================
-- SafeUnit (2 functions)
-- ============================================================================

||| Convert length between units.
||| Unit codes: 0=meters, 1=km, 2=cm, 3=mm, 4=feet, 5=inches, 6=miles, 7=yards.
||| Returns (status, converted_value).
%foreign "C:proven_unit_convert_length,libproven"
export
prim__proven_unit_convert_length : Double -> Int -> Int -> PrimIO (Int, Double)

||| Convert temperature between units.
||| Unit codes: 0=Celsius, 1=Fahrenheit, 2=Kelvin.
||| Returns (status, converted_value).
%foreign "C:proven_unit_convert_temp,libproven"
export
prim__proven_unit_convert_temp : Double -> Int -> Int -> PrimIO (Int, Double)

-- ============================================================================
-- SafeML (4 pure functions)
-- ============================================================================

||| Sigmoid function: 1 / (1 + exp(-x)). Pure.
%foreign "C:proven_ml_sigmoid,libproven"
export
prim__proven_ml_sigmoid : Double -> Double

||| ReLU function: max(0, x). Pure.
%foreign "C:proven_ml_relu,libproven"
export
prim__proven_ml_relu : Double -> Double

||| Leaky ReLU: x >= 0 ? x : alpha * x. Pure.
%foreign "C:proven_ml_leaky_relu,libproven"
export
prim__proven_ml_leaky_relu : Double -> Double -> Double

||| Clamp value to [min_val, max_val]. Pure.
%foreign "C:proven_ml_clamp,libproven"
export
prim__proven_ml_clamp : Double -> Double -> Double -> Double

-- ============================================================================
-- SafeHeader (3 functions)
-- ============================================================================

||| Check for CRLF injection in header value.
||| Returns (status, bool_value) where bool_value is non-zero if CRLF found.
%foreign "C:proven_header_has_crlf,libproven"
export
prim__proven_header_has_crlf : String -> Int -> PrimIO (Int, Int)

||| Check if header name is a valid token per RFC 7230.
||| Returns (status, bool_value).
%foreign "C:proven_header_is_valid_name,libproven"
export
prim__proven_header_is_valid_name : String -> Int -> PrimIO (Int, Int)

||| Check if header name is in the dangerous headers list.
||| Returns (status, bool_value).
%foreign "C:proven_header_is_dangerous,libproven"
export
prim__proven_header_is_dangerous : String -> Int -> PrimIO (Int, Int)

-- ============================================================================
-- SafeCookie (3 validation functions)
-- ============================================================================

||| Check for cookie injection characters (semicolon, CR, LF).
||| Returns (status, bool_value).
%foreign "C:proven_cookie_has_injection,libproven"
export
prim__proven_cookie_has_injection : String -> Int -> PrimIO (Int, Int)

||| Validate cookie name.
||| Returns (status, bool_value).
%foreign "C:proven_cookie_validate_name,libproven"
export
prim__proven_cookie_validate_name : String -> Int -> PrimIO (Int, Int)

||| Validate cookie value.
||| Returns (status, bool_value).
%foreign "C:proven_cookie_validate_value,libproven"
export
prim__proven_cookie_validate_value : String -> Int -> PrimIO (Int, Int)

-- ============================================================================
-- SafeHttp (2 functions)
-- ============================================================================

||| URL-encode a string (RFC 3986 percent encoding).
||| Returns (status, result_ptr, result_len). Caller must free result_ptr.
%foreign "C:proven_http_url_encode,libproven"
export
prim__proven_http_url_encode : String -> Int -> PrimIO (Int, AnyPtr, Int)

||| URL-decode a percent-encoded string.
||| Returns (status, result_ptr, result_len). Caller must free result_ptr.
%foreign "C:proven_http_url_decode,libproven"
export
prim__proven_http_url_decode : String -> Int -> PrimIO (Int, AnyPtr, Int)

-- ============================================================================
-- SafePhone (1 function)
-- ============================================================================

||| Parse phone number. Returns opaque pointer to ProvenPhoneResult.
%foreign "C:proven_phone_parse,libproven"
export
prim__proven_phone_parse : String -> Int -> PrimIO AnyPtr
