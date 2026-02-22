{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

{-
  ffi.orc - FFI declarations for libproven via JNA (Java Native Access)

  Orc is a concurrent programming language for orchestration. It calls Java
  classes via `import site`. To reach libproven, we use JNA which provides
  Java->C ABI bridging without writing JNI glue code.

  Architecture:
    Orc (.orc)
      | import site -> Java class
      v
    JNA (com.sun.jna)
      | JNA -> C ABI call
      v
    libproven.so (Zig FFI layer)
      | Zig -> Idris2 RefC calls
      v
    Idris 2 (dependent types, totality checking)

  ALL computation is performed in the verified Idris 2 core.
  This file ONLY declares the JNA bridge interface.
-}

-- ============================================================================
-- JNA Library Interface
-- ============================================================================

-- Import the JNA-based Java bridge to libproven.
-- The ProvenLibrary Java class uses JNA to map C ABI functions.
import site ProvenFFI = "proven.jna.ProvenLibrary"

-- ============================================================================
-- JNA Result Type Wrappers (Java classes mirroring C structs)
-- ============================================================================

-- IntResult: maps to C struct { int32 status; int64 value; }
import class ProvenIntResult = "proven.jna.IntResult"

-- BoolResult: maps to C struct { int32 status; int32 value; }
import class ProvenBoolResult = "proven.jna.BoolResult"

-- StringResult: maps to C struct { int32 status; Pointer ptr; long len; }
import class ProvenStringResult = "proven.jna.StringResult"

-- FloatResult: maps to C struct { int32 status; double value; }
import class ProvenFloatResult = "proven.jna.FloatResult"

-- ============================================================================
-- Status Codes (matching ProvenStatus enum in Zig FFI)
-- ============================================================================

val STATUS_OK = 0
val STATUS_ERR_NULL_POINTER = -1
val STATUS_ERR_INVALID_ARGUMENT = -2
val STATUS_ERR_OVERFLOW = -3
val STATUS_ERR_UNDERFLOW = -4
val STATUS_ERR_DIVISION_BY_ZERO = -5
val STATUS_ERR_PARSE_FAILURE = -6
val STATUS_ERR_VALIDATION_FAILED = -7
val STATUS_ERR_OUT_OF_BOUNDS = -8
val STATUS_ERR_ENCODING_ERROR = -9
val STATUS_ERR_ALLOCATION_FAILED = -10
val STATUS_ERR_NOT_IMPLEMENTED = -99

-- ============================================================================
-- Lifecycle FFI Calls
-- ============================================================================

-- Initialize the proven runtime. Must be called before any safe operation.
-- Returns status code (0 = success).
def ffi_init() = ProvenFFI.proven_init()

-- Deinitialize the proven runtime.
def ffi_deinit() = ProvenFFI.proven_deinit()

-- Check whether the runtime is initialized.
def ffi_is_initialized() = ProvenFFI.proven_is_initialized()

-- Free a string allocated by libproven.
def ffi_free_string(ptr) = ProvenFFI.proven_free_string(ptr)

-- ============================================================================
-- SafeMath FFI Calls
-- ============================================================================

def ffi_math_add_checked(a, b) = ProvenFFI.proven_math_add_checked(a, b)
def ffi_math_sub_checked(a, b) = ProvenFFI.proven_math_sub_checked(a, b)
def ffi_math_mul_checked(a, b) = ProvenFFI.proven_math_mul_checked(a, b)
def ffi_math_div(a, b) = ProvenFFI.proven_math_div(a, b)
def ffi_math_mod(a, b) = ProvenFFI.proven_math_mod(a, b)
def ffi_math_abs_safe(n) = ProvenFFI.proven_math_abs_safe(n)
def ffi_math_clamp(lo, hi, value) = ProvenFFI.proven_math_clamp(lo, hi, value)
def ffi_math_pow_checked(base, exp) = ProvenFFI.proven_math_pow_checked(base, exp)

-- ============================================================================
-- SafeString FFI Calls
-- ============================================================================

def ffi_string_is_valid_utf8(ptr, len) = ProvenFFI.proven_string_is_valid_utf8(ptr, len)
def ffi_string_escape_sql(ptr, len) = ProvenFFI.proven_string_escape_sql(ptr, len)
def ffi_string_escape_html(ptr, len) = ProvenFFI.proven_string_escape_html(ptr, len)
def ffi_string_escape_js(ptr, len) = ProvenFFI.proven_string_escape_js(ptr, len)

-- ============================================================================
-- SafeEmail FFI Calls
-- ============================================================================

def ffi_email_is_valid(ptr, len) = ProvenFFI.proven_email_is_valid(ptr, len)

-- ============================================================================
-- SafeUrl FFI Calls
-- ============================================================================

def ffi_url_parse(ptr, len) = ProvenFFI.proven_url_parse(ptr, len)
def ffi_url_free(components) = ProvenFFI.proven_url_free(components)

-- ============================================================================
-- SafeCrypto FFI Calls
-- ============================================================================

def ffi_crypto_constant_time_eq(a, a_len, b, b_len) =
  ProvenFFI.proven_crypto_constant_time_eq(a, a_len, b, b_len)
def ffi_crypto_random_bytes(buf, len) = ProvenFFI.proven_crypto_random_bytes(buf, len)
def ffi_hex_encode(ptr, len, uppercase) = ProvenFFI.proven_hex_encode(ptr, len, uppercase)
def ffi_hex_decode(ptr, len) = ProvenFFI.proven_hex_decode(ptr, len)
def ffi_hex_free(result) = ProvenFFI.proven_hex_free(result)

-- ============================================================================
-- SafeJson FFI Calls
-- ============================================================================

def ffi_json_is_valid(ptr, len) = ProvenFFI.proven_json_is_valid(ptr, len)
def ffi_json_get_type(ptr, len) = ProvenFFI.proven_json_get_type(ptr, len)

-- ============================================================================
-- SafeFloat FFI Calls
-- ============================================================================

def ffi_float_div(a, b) = ProvenFFI.proven_float_div(a, b)
def ffi_float_is_finite(x) = ProvenFFI.proven_float_is_finite(x)
def ffi_float_is_nan(x) = ProvenFFI.proven_float_is_nan(x)
def ffi_float_sqrt(x) = ProvenFFI.proven_float_sqrt(x)
def ffi_float_ln(x) = ProvenFFI.proven_float_ln(x)

-- ============================================================================
-- SafeChecksum FFI Calls
-- ============================================================================

def ffi_checksum_crc32(ptr, len) = ProvenFFI.proven_checksum_crc32(ptr, len)
def ffi_checksum_verify_crc32(ptr, len, expected) =
  ProvenFFI.proven_checksum_verify_crc32(ptr, len, expected)
