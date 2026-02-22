// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// lib_proven.bal - FFI declarations for libproven via JNA.
//
// This file declares all external function bindings and types for calling
// libproven from Ballerina. Since Ballerina runs on the JVM, we use JNA
// (Java Native Access) to call the C ABI functions from libproven.
//
// ALL computation is performed in Idris 2 via the Zig FFI bridge.
// Ballerina code MUST NOT reimplement any logic; it only marshals data.

import ballerina/jballerina.java;

// ============================================================================
// Status Codes
// ============================================================================

// Status codes returned by libproven operations.
// Zero indicates success; negative values indicate specific error conditions.
public const int STATUS_OK = 0;
public const int STATUS_ERR_NULL_POINTER = -1;
public const int STATUS_ERR_INVALID_ARGUMENT = -2;
public const int STATUS_ERR_OVERFLOW = -3;
public const int STATUS_ERR_UNDERFLOW = -4;
public const int STATUS_ERR_DIVISION_BY_ZERO = -5;
public const int STATUS_ERR_PARSE_FAILURE = -6;
public const int STATUS_ERR_VALIDATION_FAILED = -7;
public const int STATUS_ERR_OUT_OF_BOUNDS = -8;
public const int STATUS_ERR_ENCODING_ERROR = -9;
public const int STATUS_ERR_ALLOCATION_FAILED = -10;
public const int STATUS_ERR_NOT_IMPLEMENTED = -99;

// ============================================================================
// Result Record Types
// ============================================================================

// Result for integer operations from libproven.
public type IntResult record {|
    int status;
    int value;
|};

// Result for boolean operations from libproven.
public type BoolResult record {|
    int status;
    boolean value;
|};

// Result for string operations from libproven.
// The caller must free the pointer using provenFreeString().
public type StringResult record {|
    int status;
    handle ptr;
    int len;
|};

// Result for floating-point operations from libproven.
public type FloatResult record {|
    int status;
    float value;
|};

// ============================================================================
// JSON Type Constants
// ============================================================================

public const int JSON_NULL = 0;
public const int JSON_BOOL = 1;
public const int JSON_NUMBER = 2;
public const int JSON_STRING = 3;
public const int JSON_ARRAY = 4;
public const int JSON_OBJECT = 5;
public const int JSON_INVALID = -1;

// ============================================================================
// JNA Native Library Bridge
// ============================================================================

// The JNA bridge class name for accessing libproven native functions.
// This class must be generated or written in Java to load "proven" via JNA
// and expose each C function as a static Java method callable from Ballerina.
//
// The bridge follows this pattern for each function:
//   public static int proven_init() { return INSTANCE.proven_init(); }
//
// See: modules/proven/native/ProvenNative.java

// ============================================================================
// Extern Function Declarations - Lifecycle
// ============================================================================

// Initialize the Proven runtime (includes Idris 2 runtime).
// Must be called before any other Proven function.
public isolated function provenInit() returns int = @java:Method {
    name: "proven_init",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Cleanup the Proven runtime.
public isolated function provenDeinit() = @java:Method {
    name: "proven_deinit",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Check if the runtime is initialized.
public isolated function provenIsInitialized() returns boolean = @java:Method {
    name: "proven_is_initialized",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Extern Function Declarations - Memory
// ============================================================================

// Free a string allocated by Proven functions. Safe to call with null.
public isolated function provenFreeString(handle ptr) = @java:Method {
    name: "proven_free_string",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Extern Function Declarations - SafeMath
// ============================================================================

// Safe addition with overflow detection.
public isolated function provenMathAddChecked(int a, int b) returns handle = @java:Method {
    name: "proven_math_add_checked",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Safe subtraction with underflow detection.
public isolated function provenMathSubChecked(int a, int b) returns handle = @java:Method {
    name: "proven_math_sub_checked",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Safe multiplication with overflow detection.
public isolated function provenMathMulChecked(int a, int b) returns handle = @java:Method {
    name: "proven_math_mul_checked",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Safe division.
public isolated function provenMathDiv(int numerator, int denominator) returns handle = @java:Method {
    name: "proven_math_div",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Safe modulo.
public isolated function provenMathMod(int numerator, int denominator) returns handle = @java:Method {
    name: "proven_math_mod",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Safe absolute value.
public isolated function provenMathAbsSafe(int n) returns handle = @java:Method {
    name: "proven_math_abs_safe",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Clamp value to [lo, hi] range.
public isolated function provenMathClamp(int lo, int hi, int value) returns int = @java:Method {
    name: "proven_math_clamp",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Integer power with overflow checking.
public isolated function provenMathPowChecked(int base, int exp) returns handle = @java:Method {
    name: "proven_math_pow_checked",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Extern Function Declarations - SafeString
// ============================================================================

// Check if bytes are valid UTF-8.
public isolated function provenStringIsValidUtf8(handle ptr, int len) returns handle = @java:Method {
    name: "proven_string_is_valid_utf8",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Escape string for SQL.
public isolated function provenStringEscapeSql(handle ptr, int len) returns handle = @java:Method {
    name: "proven_string_escape_sql",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Escape string for HTML.
public isolated function provenStringEscapeHtml(handle ptr, int len) returns handle = @java:Method {
    name: "proven_string_escape_html",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Escape string for JavaScript.
public isolated function provenStringEscapeJs(handle ptr, int len) returns handle = @java:Method {
    name: "proven_string_escape_js",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Extern Function Declarations - SafePath
// ============================================================================

// Check if path has traversal sequences.
public isolated function provenPathHasTraversal(handle ptr, int len) returns handle = @java:Method {
    name: "proven_path_has_traversal",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Sanitize a filename.
public isolated function provenPathSanitizeFilename(handle ptr, int len) returns handle = @java:Method {
    name: "proven_path_sanitize_filename",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Extern Function Declarations - SafeEmail
// ============================================================================

// Validate email address.
public isolated function provenEmailIsValid(handle ptr, int len) returns handle = @java:Method {
    name: "proven_email_is_valid",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Extern Function Declarations - SafeUrl
// ============================================================================

// Parse URL into components.
public isolated function provenUrlParse(handle ptr, int len) returns handle = @java:Method {
    name: "proven_url_parse",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Free URL result.
public isolated function provenUrlFree(handle components) = @java:Method {
    name: "proven_url_free",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Extern Function Declarations - SafeCrypto
// ============================================================================

// Constant-time byte comparison.
public isolated function provenCryptoConstantTimeEq(
    handle ptr1, int len1,
    handle ptr2, int len2
) returns handle = @java:Method {
    name: "proven_crypto_constant_time_eq",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Fill buffer with cryptographically secure random bytes.
public isolated function provenCryptoRandomBytes(handle ptr, int len) returns int = @java:Method {
    name: "proven_crypto_random_bytes",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Extern Function Declarations - SafeJson
// ============================================================================

// Check if string is valid JSON.
public isolated function provenJsonIsValid(handle ptr, int len) returns handle = @java:Method {
    name: "proven_json_is_valid",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Get JSON value type at root level.
public isolated function provenJsonGetType(handle ptr, int len) returns int = @java:Method {
    name: "proven_json_get_type",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Extern Function Declarations - SafeFloat
// ============================================================================

// Safe floating-point division.
public isolated function provenFloatDiv(float a, float b) returns handle = @java:Method {
    name: "proven_float_div",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Check if float is finite.
public isolated function provenFloatIsFinite(float x) returns boolean = @java:Method {
    name: "proven_float_is_finite",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Check if float is NaN.
public isolated function provenFloatIsNan(float x) returns boolean = @java:Method {
    name: "proven_float_is_nan",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Safe square root.
public isolated function provenFloatSqrt(float x) returns handle = @java:Method {
    name: "proven_float_sqrt",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Safe natural logarithm.
public isolated function provenFloatLn(float x) returns handle = @java:Method {
    name: "proven_float_ln",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Extern Function Declarations - SafeHex
// ============================================================================

// Encode bytes to hex string.
public isolated function provenHexEncode(handle ptr, int len, boolean uppercase) returns handle = @java:Method {
    name: "proven_hex_encode",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Extern Function Declarations - SafeChecksum
// ============================================================================

// Calculate CRC32 checksum.
public isolated function provenChecksumCrc32(handle ptr, int len) returns handle = @java:Method {
    name: "proven_checksum_crc32",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Extern Function Declarations - Version
// ============================================================================

// Get FFI ABI version.
public isolated function provenFfiAbiVersion() returns int = @java:Method {
    name: "proven_ffi_abi_version",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Get major version number.
public isolated function provenVersionMajor() returns int = @java:Method {
    name: "proven_version_major",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Get minor version number.
public isolated function provenVersionMinor() returns int = @java:Method {
    name: "proven_version_minor",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// Get patch version number.
public isolated function provenVersionPatch() returns int = @java:Method {
    name: "proven_version_patch",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// ============================================================================
// Helper Functions
// ============================================================================

// Check if a status code indicates success.
public isolated function succeeded(int status) returns boolean {
    return status == STATUS_OK;
}

// Check if a status code indicates failure.
public isolated function failed(int status) returns boolean {
    return status != STATUS_OK;
}

// Convert a status code to a human-readable error message.
public isolated function statusToError(int status) returns error {
    match status {
        STATUS_ERR_NULL_POINTER => {
            return error("null pointer");
        }
        STATUS_ERR_INVALID_ARGUMENT => {
            return error("invalid argument");
        }
        STATUS_ERR_OVERFLOW => {
            return error("overflow");
        }
        STATUS_ERR_UNDERFLOW => {
            return error("underflow");
        }
        STATUS_ERR_DIVISION_BY_ZERO => {
            return error("division by zero");
        }
        STATUS_ERR_PARSE_FAILURE => {
            return error("parse failure");
        }
        STATUS_ERR_VALIDATION_FAILED => {
            return error("validation failed");
        }
        STATUS_ERR_OUT_OF_BOUNDS => {
            return error("out of bounds");
        }
        STATUS_ERR_ENCODING_ERROR => {
            return error("encoding error");
        }
        STATUS_ERR_ALLOCATION_FAILED => {
            return error("allocation failed");
        }
        STATUS_ERR_NOT_IMPLEMENTED => {
            return error("not implemented");
        }
        _ => {
            return error("unknown error: " + status.toString());
        }
    }
}

// JNA helper: Extract status from an IntResult handle.
public isolated function extractIntResultStatus(handle result) returns int = @java:Method {
    name: "extractIntResultStatus",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract value from an IntResult handle.
public isolated function extractIntResultValue(handle result) returns int = @java:Method {
    name: "extractIntResultValue",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract status from a BoolResult handle.
public isolated function extractBoolResultStatus(handle result) returns int = @java:Method {
    name: "extractBoolResultStatus",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract value from a BoolResult handle.
public isolated function extractBoolResultValue(handle result) returns boolean = @java:Method {
    name: "extractBoolResultValue",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract status from a FloatResult handle.
public isolated function extractFloatResultStatus(handle result) returns int = @java:Method {
    name: "extractFloatResultStatus",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract value from a FloatResult handle.
public isolated function extractFloatResultValue(handle result) returns float = @java:Method {
    name: "extractFloatResultValue",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract status from a StringResult handle.
public isolated function extractStringResultStatus(handle result) returns int = @java:Method {
    name: "extractStringResultStatus",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract value string from a StringResult handle and free the native string.
public isolated function extractStringResultValue(handle result) returns string = @java:Method {
    name: "extractStringResultValue",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Allocate native memory from a Ballerina byte array.
public isolated function toNativeBytes(byte[] data) returns handle = @java:Method {
    name: "toNativeBytes",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Free native memory allocated by toNativeBytes.
public isolated function freeNativeBytes(handle ptr) = @java:Method {
    name: "freeNativeBytes",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;
