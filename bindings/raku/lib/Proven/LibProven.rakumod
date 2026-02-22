# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven::LibProven - NativeCall declarations for libproven.
#
# This module declares the raw C ABI types and function signatures exposed by
# libproven.  All computation is performed in Idris 2 (formally verified) via
# the Zig FFI layer.  This file is a thin declaration layer; it does NOT
# reimplement any logic.
#
# Memory management:
#   - ProvenStringResult.ptr must be freed via proven_free_string().
#   - ProvenHexDecodeResult must be freed via proven_hex_free().
#   - ProvenUrlResult must be freed via proven_url_free().
#   - ProvenContentTypeResult must be freed via proven_content_type_free().
#   - Integer, boolean, and float results do not require freeing.

unit module Proven::LibProven;

use NativeCall;

# ============================================================================
# Status Codes
# ============================================================================

enum ProvenStatus is export (
    PROVEN_OK                   =>   0,
    PROVEN_ERR_NULL_POINTER     =>  -1,
    PROVEN_ERR_INVALID_ARGUMENT =>  -2,
    PROVEN_ERR_OVERFLOW         =>  -3,
    PROVEN_ERR_UNDERFLOW        =>  -4,
    PROVEN_ERR_DIVISION_BY_ZERO =>  -5,
    PROVEN_ERR_PARSE_FAILURE    =>  -6,
    PROVEN_ERR_VALIDATION_FAILED => -7,
    PROVEN_ERR_OUT_OF_BOUNDS    =>  -8,
    PROVEN_ERR_ENCODING_ERROR   =>  -9,
    PROVEN_ERR_ALLOCATION_FAILED => -10,
    PROVEN_ERR_NOT_IMPLEMENTED  => -99,
);

# ============================================================================
# Core Result CStructs
# ============================================================================

#| Result for integer operations: { int32_t status; int64_t value; }
class IntResult is repr('CStruct') is export {
    has int32 $.status;
    has int64 $.value;
}

#| Result for boolean operations: { int32_t status; bool value; }
#| Note: bool is 1 byte in C, but CStruct aligns to int32.  We use int32 to
#| capture the padding-included layout.  The actual bool is in the low byte.
class BoolResult is repr('CStruct') is export {
    has int32 $.status;
    has int32 $.bool-raw;   # bool + padding packed into 4 bytes

    method value(--> Bool) { so ($!bool-raw +& 0xFF) }
}

#| Result for string operations: { int32_t status; char* value; size_t length; }
class StringResult is repr('CStruct') is export {
    has int32   $.status;
    has Pointer $.ptr;      # char* value
    has size_t  $.len;      # length
}

#| Result for floating-point operations: { int32_t status; double value; }
class FloatResult is repr('CStruct') is export {
    has int32 $.status;
    has num64 $.value;
}

# ============================================================================
# Composite CStructs
# ============================================================================

#| IPv4 address: { uint8_t octets[4]; }
class IPv4Address is repr('CStruct') is export {
    has uint8 $.o0;
    has uint8 $.o1;
    has uint8 $.o2;
    has uint8 $.o3;

    method octets(--> List) { ($!o0, $!o1, $!o2, $!o3) }
    method Str(--> Str)     { "$!o0.$!o1.$!o2.$!o3" }
}

#| IPv4 parse result: { int32_t status; ProvenIPv4Address address; }
class IPv4Result is repr('CStruct') is export {
    has int32 $.status;
    has uint8 $.o0;
    has uint8 $.o1;
    has uint8 $.o2;
    has uint8 $.o3;
}

#| UUID: { uint8_t bytes[16]; }
class ProvenUUID is repr('CStruct') is export {
    has uint8 $.b0;  has uint8 $.b1;  has uint8 $.b2;  has uint8 $.b3;
    has uint8 $.b4;  has uint8 $.b5;  has uint8 $.b6;  has uint8 $.b7;
    has uint8 $.b8;  has uint8 $.b9;  has uint8 $.b10; has uint8 $.b11;
    has uint8 $.b12; has uint8 $.b13; has uint8 $.b14; has uint8 $.b15;
}

#| UUID result: { int32_t status; ProvenUUID uuid; }
class UUIDResult is repr('CStruct') is export {
    has int32 $.status;
    has uint8 $.b0;  has uint8 $.b1;  has uint8 $.b2;  has uint8 $.b3;
    has uint8 $.b4;  has uint8 $.b5;  has uint8 $.b6;  has uint8 $.b7;
    has uint8 $.b8;  has uint8 $.b9;  has uint8 $.b10; has uint8 $.b11;
    has uint8 $.b12; has uint8 $.b13; has uint8 $.b14; has uint8 $.b15;
}

#| DateTime components.
class ProvenDateTime is repr('CStruct') is export {
    has int32  $.year;
    has uint8  $.month;
    has uint8  $.day;
    has uint8  $.hour;
    has uint8  $.minute;
    has uint8  $.second;
    # 3 bytes of padding before nanosecond (uint32 alignment)
    has uint8  $._pad0;
    has uint8  $._pad1;
    has uint8  $._pad2;
    has uint32 $.nanosecond;
    has int16  $.tz-offset-minutes;
}

#| DateTime parse result.
class DateTimeResult is repr('CStruct') is export {
    has int32  $.status;
    has int32  $.year;
    has uint8  $.month;
    has uint8  $.day;
    has uint8  $.hour;
    has uint8  $.minute;
    has uint8  $.second;
    has uint8  $._pad0;
    has uint8  $._pad1;
    has uint8  $._pad2;
    has uint32 $.nanosecond;
    has int16  $.tz-offset-minutes;
}

#| RGB color: { uint8_t r, g, b; }
class RGBColor is repr('CStruct') is export {
    has uint8 $.r;
    has uint8 $.g;
    has uint8 $.b;
}

#| HSL color: { double h, s, l; }
class HSLColor is repr('CStruct') is export {
    has num64 $.h;
    has num64 $.s;
    has num64 $.l;
}

#| Color parse result: { int32_t status; uint8_t r, g, b; }
class ColorResult is repr('CStruct') is export {
    has int32 $.status;
    has uint8 $.r;
    has uint8 $.g;
    has uint8 $.b;
}

#| Password validation result.
class PasswordResult is repr('CStruct') is export {
    has int32 $.strength;       # ProvenPasswordStrength enum (0-4)
    has int32 $.has-lowercase;  # bool (packed)
    has int32 $.has-uppercase;
    has int32 $.has-digit;
    has int32 $.has-special;
    has size_t $.length;
}

#| Retry configuration.
class RetryConfig is repr('CStruct') is export {
    has uint32 $.max-attempts;
    has uint64 $.base-delay-ms;
    has uint64 $.max-delay-ms;
    has num64  $.multiplier;
}

# ============================================================================
# Memory Management
# ============================================================================

sub proven_free_string(Pointer)
    is native('proven') is export { * }

# ============================================================================
# Lifecycle
# ============================================================================

sub proven_init(--> int32)
    is native('proven') is export { * }

sub proven_deinit()
    is native('proven') is export { * }

sub proven_is_initialized(--> bool)
    is native('proven') is export { * }

# ============================================================================
# Version Information
# ============================================================================

sub proven_ffi_abi_version(--> uint32)
    is native('proven') is export { * }

sub proven_version_major(--> uint32)
    is native('proven') is export { * }

sub proven_version_minor(--> uint32)
    is native('proven') is export { * }

sub proven_version_patch(--> uint32)
    is native('proven') is export { * }

sub proven_module_count(--> uint32)
    is native('proven') is export { * }

# ============================================================================
# SafeMath (9 functions)
# ============================================================================

sub proven_math_add_checked(int64, int64 --> IntResult)
    is native('proven') is export { * }

sub proven_math_sub_checked(int64, int64 --> IntResult)
    is native('proven') is export { * }

sub proven_math_mul_checked(int64, int64 --> IntResult)
    is native('proven') is export { * }

sub proven_math_div(int64, int64 --> IntResult)
    is native('proven') is export { * }

sub proven_math_mod(int64, int64 --> IntResult)
    is native('proven') is export { * }

sub proven_math_abs_safe(int64 --> IntResult)
    is native('proven') is export { * }

sub proven_math_clamp(int64, int64, int64 --> int64)
    is native('proven') is export { * }

sub proven_math_pow_checked(int64, uint32 --> IntResult)
    is native('proven') is export { * }

# ============================================================================
# SafeString (4 functions)
# ============================================================================

sub proven_string_is_valid_utf8(Pointer, size_t --> BoolResult)
    is native('proven') is export { * }

sub proven_string_escape_sql(Pointer, size_t --> StringResult)
    is native('proven') is export { * }

sub proven_string_escape_html(Pointer, size_t --> StringResult)
    is native('proven') is export { * }

sub proven_string_escape_js(Pointer, size_t --> StringResult)
    is native('proven') is export { * }

# ============================================================================
# SafePath (2 functions)
# ============================================================================

sub proven_path_has_traversal(Pointer, size_t --> BoolResult)
    is native('proven') is export { * }

sub proven_path_sanitize_filename(Pointer, size_t --> StringResult)
    is native('proven') is export { * }

# ============================================================================
# SafeEmail (1 function)
# ============================================================================

sub proven_email_is_valid(Pointer, size_t --> BoolResult)
    is native('proven') is export { * }

# ============================================================================
# SafeNetwork (3 functions)
# ============================================================================

sub proven_network_parse_ipv4(Pointer, size_t --> IPv4Result)
    is native('proven') is export { * }

sub proven_network_ipv4_is_private(IPv4Address --> bool)
    is native('proven') is export { * }

sub proven_network_ipv4_is_loopback(IPv4Address --> bool)
    is native('proven') is export { * }

# ============================================================================
# SafeCrypto (2 functions)
# ============================================================================

sub proven_crypto_constant_time_eq(Pointer, size_t, Pointer, size_t --> BoolResult)
    is native('proven') is export { * }

sub proven_crypto_random_bytes(Pointer, size_t --> int32)
    is native('proven') is export { * }

# ============================================================================
# SafeUrl (2 functions)
#   Note: proven_url_parse returns a complex nested struct.  We expose the raw
#   function; the SafeUrl wrapper handles the struct parsing.
# ============================================================================

# proven_url_parse and proven_url_free are handled at the wrapper level via
# manual memory layout because ProvenUrlResult contains nested pointers.

# ============================================================================
# SafeJson (2 functions)
# ============================================================================

sub proven_json_is_valid(Pointer, size_t --> BoolResult)
    is native('proven') is export { * }

sub proven_json_get_type(Pointer, size_t --> int32)
    is native('proven') is export { * }

# ============================================================================
# SafeDateTime (4 functions)
# ============================================================================

sub proven_datetime_parse(Pointer, size_t --> DateTimeResult)
    is native('proven') is export { * }

sub proven_datetime_format_iso8601(ProvenDateTime --> StringResult)
    is native('proven') is export { * }

sub proven_datetime_is_leap_year(int32 --> bool)
    is native('proven') is export { * }

sub proven_datetime_days_in_month(int32, uint8 --> uint8)
    is native('proven') is export { * }

# ============================================================================
# SafeFloat (5 functions)
# ============================================================================

sub proven_float_div(num64, num64 --> FloatResult)
    is native('proven') is export { * }

sub proven_float_is_finite(num64 --> bool)
    is native('proven') is export { * }

sub proven_float_is_nan(num64 --> bool)
    is native('proven') is export { * }

sub proven_float_sqrt(num64 --> FloatResult)
    is native('proven') is export { * }

sub proven_float_ln(num64 --> FloatResult)
    is native('proven') is export { * }

# ============================================================================
# SafeHex (3 functions)
# ============================================================================

sub proven_hex_encode(Pointer, size_t, bool --> StringResult)
    is native('proven') is export { * }

# proven_hex_decode returns ProvenHexDecodeResult (same layout as StringResult)
sub proven_hex_decode(Pointer, size_t --> StringResult)
    is native('proven') is export { * }

# proven_hex_free takes a pointer to ProvenHexDecodeResult; we reuse the
# free-string path since the layout matches for our usage.
sub proven_hex_free(Pointer)
    is native('proven') is export { * }

# ============================================================================
# SafePassword (2 functions)
# ============================================================================

sub proven_password_is_common(Pointer, size_t --> bool)
    is native('proven') is export { * }

# proven_password_validate returns a PasswordResult struct
# handled at wrapper level due to struct complexity

# ============================================================================
# SafeHeader (6 functions)
# ============================================================================

sub proven_header_has_crlf(Pointer, size_t --> BoolResult)
    is native('proven') is export { * }

sub proven_header_is_valid_name(Pointer, size_t --> BoolResult)
    is native('proven') is export { * }

sub proven_header_is_dangerous(Pointer, size_t --> BoolResult)
    is native('proven') is export { * }

sub proven_header_render(Pointer, size_t, Pointer, size_t --> StringResult)
    is native('proven') is export { * }

sub proven_header_build_csp(Pointer, size_t --> StringResult)
    is native('proven') is export { * }

sub proven_header_build_hsts(int64, bool, bool --> StringResult)
    is native('proven') is export { * }

# ============================================================================
# SafeColor (3 functions)
# ============================================================================

sub proven_color_parse_hex(Pointer, size_t --> ColorResult)
    is native('proven') is export { * }

sub proven_color_rgb_to_hsl(RGBColor --> HSLColor)
    is native('proven') is export { * }

sub proven_color_to_hex(RGBColor --> StringResult)
    is native('proven') is export { * }

# ============================================================================
# SafeAngle (4 functions)
# ============================================================================

sub proven_angle_deg_to_rad(num64 --> num64)
    is native('proven') is export { * }

sub proven_angle_rad_to_deg(num64 --> num64)
    is native('proven') is export { * }

sub proven_angle_normalize_degrees(num64 --> num64)
    is native('proven') is export { * }

sub proven_angle_normalize_radians(num64 --> num64)
    is native('proven') is export { * }

# ============================================================================
# SafeUnit (2 functions)
# ============================================================================

sub proven_unit_convert_length(num64, int32, int32 --> FloatResult)
    is native('proven') is export { * }

sub proven_unit_convert_temp(num64, int32, int32 --> FloatResult)
    is native('proven') is export { * }

# ============================================================================
# SafeProbability (4 functions)
# ============================================================================

sub proven_probability_create(num64 --> num64)
    is native('proven') is export { * }

sub proven_probability_and(num64, num64 --> num64)
    is native('proven') is export { * }

sub proven_probability_or_exclusive(num64, num64 --> num64)
    is native('proven') is export { * }

sub proven_probability_not(num64 --> num64)
    is native('proven') is export { * }

# ============================================================================
# SafeCalculator (1 function)
# ============================================================================

sub proven_calculator_eval(Pointer, size_t --> FloatResult)
    is native('proven') is export { * }

# ============================================================================
# SafeML (5 functions)
# ============================================================================

sub proven_ml_sigmoid(num64 --> num64)
    is native('proven') is export { * }

sub proven_ml_relu(num64 --> num64)
    is native('proven') is export { * }

sub proven_ml_leaky_relu(num64, num64 --> num64)
    is native('proven') is export { * }

sub proven_ml_clamp(num64, num64, num64 --> num64)
    is native('proven') is export { * }

sub proven_ml_softmax(Pointer, Pointer, size_t --> int32)
    is native('proven') is export { * }

# ============================================================================
# SafeChecksum (2 functions)
# ============================================================================

sub proven_checksum_crc32(Pointer, size_t --> IntResult)
    is native('proven') is export { * }

sub proven_checksum_verify_crc32(Pointer, size_t, uint32 --> BoolResult)
    is native('proven') is export { * }

# ============================================================================
# SafeHTTP (3 functions)
# ============================================================================

sub proven_http_url_encode(Pointer, size_t --> StringResult)
    is native('proven') is export { * }

sub proven_http_url_decode(Pointer, size_t --> StringResult)
    is native('proven') is export { * }

# proven_http_parse_www_authenticate returns a complex nested struct;
# handled at wrapper level.

# ============================================================================
# SafeVersion (3 functions)
# ============================================================================

# proven_version_parse / proven_version_compare / proven_version_free
# return/accept complex structs with embedded pointers; handled at wrapper level.

# ============================================================================
# SafeCookie (6 functions)
# ============================================================================

sub proven_cookie_has_injection(Pointer, size_t --> BoolResult)
    is native('proven') is export { * }

sub proven_cookie_validate_name(Pointer, size_t --> BoolResult)
    is native('proven') is export { * }

sub proven_cookie_validate_value(Pointer, size_t --> BoolResult)
    is native('proven') is export { * }

sub proven_cookie_get_prefix(Pointer, size_t --> IntResult)
    is native('proven') is export { * }

sub proven_cookie_build_delete(Pointer, size_t --> StringResult)
    is native('proven') is export { * }

# proven_cookie_build_set_cookie takes a ProvenCookieAttributes struct by value;
# handled at wrapper level.

# ============================================================================
# SafeContentType (6 functions)
# ============================================================================

sub proven_content_type_can_sniff_dangerous(Pointer, size_t --> BoolResult)
    is native('proven') is export { * }

# proven_content_type_parse, proven_content_type_free, proven_content_type_render,
# proven_content_type_is_json, proven_content_type_is_xml return/accept complex
# nested structs; these are handled at wrapper level.

# ============================================================================
# SafeUUID (5 functions)
# ============================================================================

sub proven_uuid_v4(--> UUIDResult)
    is native('proven') is export { * }

sub proven_uuid_to_string(ProvenUUID --> StringResult)
    is native('proven') is export { * }

sub proven_uuid_parse(Pointer, size_t --> UUIDResult)
    is native('proven') is export { * }

sub proven_uuid_is_nil(ProvenUUID --> bool)
    is native('proven') is export { * }

sub proven_uuid_version(ProvenUUID --> uint8)
    is native('proven') is export { * }

# ============================================================================
# SafeRetry (2 functions)
# ============================================================================

sub proven_retry_delay(RetryConfig, uint32 --> uint64)
    is native('proven') is export { * }

sub proven_retry_should_retry(RetryConfig, uint32 --> bool)
    is native('proven') is export { * }

# ============================================================================
# Utility: convert Raku Str to (CArray[uint8], size_t) for FFI calls
# ============================================================================

#| Encode a Raku Str to a CArray[uint8] and its byte length.
#| Returns a two-element list: (CArray[uint8], Int).
sub str-to-buf(Str:D $s --> List) is export {
    my $blob = $s.encode('utf-8');
    my $ca   = CArray[uint8].new($blob.list);
    return ($ca, $blob.elems);
}

#| Extract a Raku Str from a StringResult, freeing the C-allocated memory.
#| Returns Nil if the status is non-zero.
sub extract-string(StringResult $r --> Str) is export {
    return Nil unless $r.status == 0;
    my $ptr = $r.ptr;
    return Nil unless $ptr.defined && !$ptr.Bool.not;
    my $len = $r.len;
    my $buf = Buf.new;
    my $raw = nativecast(CArray[uint8], $ptr);
    for ^$len -> $i {
        $buf.push($raw[$i]);
    }
    proven_free_string($ptr);
    return $buf.decode('utf-8');
}
