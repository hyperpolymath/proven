// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Rustler NIF bindings for the Proven safety library.
//!
//! This module provides NIF (Native Implemented Functions) for Elixir
//! that wrap the libproven shared library. ALL computation is performed
//! by Idris 2 verified code via the Zig FFI layer; this Rust code is
//! purely a marshaling wrapper between Erlang terms and C ABI types.

use rustler::{Atom, Binary, Encoder, Env, NifResult, OwnedBinary, Term};

// ---------------------------------------------------------------------------
// Atom definitions
// ---------------------------------------------------------------------------

mod atoms {
    rustler::atoms! {
        ok,
        error,
        // Error reasons (mirror ProvenStatus from libproven)
        null_pointer,
        invalid_argument,
        overflow,
        underflow,
        division_by_zero,
        parse_failure,
        validation_failed,
        out_of_bounds,
        encoding_error,
        allocation_failed,
        not_implemented,
        // Additional error reasons
        nan,
        infinity,
        negative_input,
        non_positive_input,
        invalid_format,
        invalid_email,
        invalid_url,
        invalid_ipv4,
        traversal_detected,
        invalid_hex,
        odd_length,
        length_mismatch,
        invalid_uuid,
        unknown_currency,
        currency_mismatch,
        invalid_phone,
        missing_country_code,
        invalid_json,
        payload_too_large,
        max_depth_exceeded,
        invalid_version,
        invalid_date,
        invalid_timestamp,
        empty_input,
        unknown_unit,
        below_absolute_zero,
        // Bool results
        true_atom = "true",
        false_atom = "false",
    }
}

// ---------------------------------------------------------------------------
// libproven C ABI types (must match ffi/zig/src/main.zig exactly)
// ---------------------------------------------------------------------------

/// Status codes returned by libproven functions.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
enum ProvenStatus {
    Ok = 0,
    ErrNullPointer = -1,
    ErrInvalidArgument = -2,
    ErrOverflow = -3,
    ErrUnderflow = -4,
    ErrDivisionByZero = -5,
    ErrParseFailure = -6,
    ErrValidationFailed = -7,
    ErrOutOfBounds = -8,
    ErrEncodingError = -9,
    ErrAllocationFailed = -10,
    ErrNotImplemented = -99,
}

/// Result for integer operations.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct IntResult {
    status: ProvenStatus,
    value: i64,
}

/// Result for boolean operations.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct BoolResult {
    status: ProvenStatus,
    value: bool,
}

/// Result for string operations (caller must free the string via proven_free_string).
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct StringResult {
    status: ProvenStatus,
    value: *mut u8,
    length: usize,
}

/// Result for float operations.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct FloatResult {
    status: ProvenStatus,
    value: f64,
}

/// IPv4 address.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct IPv4Address {
    octets: [u8; 4],
}

/// Result for IPv4 parsing.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct IPv4Result {
    status: ProvenStatus,
    address: IPv4Address,
}

/// RGB color.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct RGBColor {
    r: u8,
    g: u8,
    b: u8,
}

/// HSL color.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct HSLColor {
    h: f64,
    s: f64,
    l: f64,
}

/// Color parse result.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct ColorParseResult {
    status: ProvenStatus,
    color: RGBColor,
}

/// Geographic coordinate.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct GeoCoordinate {
    latitude: f64,
    longitude: f64,
}

/// Geo result.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct GeoResult {
    status: ProvenStatus,
    coordinate: GeoCoordinate,
}

/// Hex decode result.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct HexDecodeResult {
    status: ProvenStatus,
    data: *mut u8,
    length: usize,
}

/// UUID (128-bit).
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct UUID {
    bytes: [u8; 16],
}

/// UUID result.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct UUIDResult {
    status: ProvenStatus,
    uuid: UUID,
}

/// Password strength.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
enum PasswordStrength {
    VeryWeak = 0,
    Weak = 1,
    Fair = 2,
    Strong = 3,
    VeryStrong = 4,
}

/// Password result.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct PasswordResult {
    status: ProvenStatus,
    strength: PasswordStrength,
    score: u32,
    has_upper: bool,
    has_lower: bool,
    has_digit: bool,
    has_special: bool,
}

/// Length unit enum (must match Zig LengthUnit).
#[repr(i32)]
#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
enum LengthUnit {
    Meter = 0,
    Kilometer = 1,
    Centimeter = 2,
    Millimeter = 3,
    Mile = 4,
    Yard = 5,
    Foot = 6,
    Inch = 7,
    NauticalMile = 8,
}

/// Temperature unit enum (must match Zig TempUnit).
#[repr(i32)]
#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
enum TempUnit {
    Celsius = 0,
    Fahrenheit = 1,
    Kelvin = 2,
}

// ---------------------------------------------------------------------------
// libproven extern declarations
// ---------------------------------------------------------------------------

#[link(name = "proven")]
extern "C" {
    // --- Memory management ---
    fn proven_free_string(ptr: *mut u8);

    // --- Initialization ---
    fn proven_init() -> i32;
    fn proven_deinit();
    fn proven_is_initialized() -> bool;

    // --- SafeMath ---
    fn proven_math_div(numerator: i64, denominator: i64) -> IntResult;
    fn proven_math_mod(numerator: i64, denominator: i64) -> IntResult;
    fn proven_math_add_checked(a: i64, b: i64) -> IntResult;
    fn proven_math_sub_checked(a: i64, b: i64) -> IntResult;
    fn proven_math_mul_checked(a: i64, b: i64) -> IntResult;
    fn proven_math_abs_safe(n: i64) -> IntResult;
    fn proven_math_clamp(lo: i64, hi: i64, value: i64) -> i64;
    fn proven_math_pow_checked(base: i64, exp: u32) -> IntResult;

    // --- SafeString ---
    fn proven_string_escape_html(ptr: *const u8, len: usize) -> StringResult;
    fn proven_string_escape_sql(ptr: *const u8, len: usize) -> StringResult;
    fn proven_string_escape_js(ptr: *const u8, len: usize) -> StringResult;
    fn proven_string_is_valid_utf8(ptr: *const u8, len: usize) -> BoolResult;

    // --- SafePath ---
    fn proven_path_has_traversal(ptr: *const u8, len: usize) -> BoolResult;
    fn proven_path_sanitize_filename(ptr: *const u8, len: usize) -> StringResult;

    // --- SafeEmail ---
    fn proven_email_is_valid(ptr: *const u8, len: usize) -> BoolResult;

    // --- SafeNetwork ---
    fn proven_network_parse_ipv4(ptr: *const u8, len: usize) -> IPv4Result;
    fn proven_network_ipv4_is_private(addr: IPv4Address) -> bool;
    fn proven_network_ipv4_is_loopback(addr: IPv4Address) -> bool;

    // --- SafeCrypto ---
    fn proven_crypto_constant_time_eq(
        ptr1: *const u8,
        len1: usize,
        ptr2: *const u8,
        len2: usize,
    ) -> BoolResult;
    fn proven_crypto_random_bytes(ptr: *mut u8, len: usize) -> ProvenStatus;

    // --- SafeFloat ---
    fn proven_float_div(a: f64, b: f64) -> FloatResult;
    fn proven_float_is_finite(x: f64) -> bool;
    fn proven_float_is_nan(x: f64) -> bool;
    fn proven_float_sqrt(x: f64) -> FloatResult;
    fn proven_float_ln(x: f64) -> FloatResult;

    // --- SafeHex ---
    fn proven_hex_encode(ptr: *const u8, len: usize, uppercase: bool) -> StringResult;
    fn proven_hex_decode(ptr: *const u8, len: usize) -> HexDecodeResult;

    // --- SafeUUID ---
    fn proven_uuid_v4() -> UUIDResult;
    fn proven_uuid_parse(ptr: *const u8, len: usize) -> UUIDResult;
    fn proven_uuid_to_string(uuid: UUID) -> StringResult;
    fn proven_uuid_is_nil(uuid: UUID) -> bool;
    fn proven_uuid_version(uuid: UUID) -> u8;

    // --- SafeHeader ---
    fn proven_header_has_crlf(ptr: *const u8, len: usize) -> BoolResult;
    fn proven_header_is_valid_name(ptr: *const u8, len: usize) -> BoolResult;
    fn proven_header_is_dangerous(ptr: *const u8, len: usize) -> BoolResult;

    // --- SafeCookie ---
    fn proven_cookie_has_injection(ptr: *const u8, len: usize) -> BoolResult;
    fn proven_cookie_get_prefix(ptr: *const u8, len: usize) -> IntResult;

    // --- SafeContentType ---
    fn proven_content_type_can_sniff_dangerous(ptr: *const u8, len: usize) -> BoolResult;
    fn proven_content_type_is_json(
        subtype_ptr: *const u8,
        subtype_len: usize,
        suffix_ptr: *const u8,
        suffix_len: usize,
    ) -> BoolResult;
    fn proven_content_type_is_xml(
        subtype_ptr: *const u8,
        subtype_len: usize,
        suffix_ptr: *const u8,
        suffix_len: usize,
    ) -> BoolResult;

    // --- SafePassword ---
    fn proven_password_validate(ptr: *const u8, len: usize) -> PasswordResult;
    fn proven_password_is_common(ptr: *const u8, len: usize) -> bool;

    // --- SafeColor ---
    fn proven_color_parse_hex(ptr: *const u8, len: usize) -> ColorParseResult;
    fn proven_color_rgb_to_hsl(rgb: RGBColor) -> HSLColor;
    fn proven_color_to_hex(rgb: RGBColor) -> StringResult;

    // --- SafeAngle ---
    fn proven_angle_deg_to_rad(degrees: f64) -> f64;
    fn proven_angle_rad_to_deg(radians: f64) -> f64;
    fn proven_angle_normalize_degrees(degrees: f64) -> f64;
    fn proven_angle_normalize_radians(radians: f64) -> f64;

    // --- SafeUnit ---
    fn proven_unit_convert_length(value: f64, from: LengthUnit, to: LengthUnit) -> FloatResult;
    fn proven_unit_convert_temp(value: f64, from: TempUnit, to: TempUnit) -> FloatResult;

    // --- SafeGeo ---
    fn proven_geo_validate(lat: f64, lon: f64) -> GeoResult;
    fn proven_geo_distance(a: GeoCoordinate, b: GeoCoordinate) -> FloatResult;

    // --- SafeChecksum ---
    fn proven_checksum_crc32(ptr: *const u8, len: usize) -> IntResult;

    // --- SafeProbability ---
    fn proven_probability_create(value: f64) -> f64;
    fn proven_probability_and(a: f64, b: f64) -> f64;
    fn proven_probability_or_exclusive(a: f64, b: f64) -> f64;
    fn proven_probability_not(p: f64) -> f64;

    // --- SafeML ---
    fn proven_ml_softmax(input: *const f64, output: *mut f64, len: usize) -> ProvenStatus;
    fn proven_ml_sigmoid(x: f64) -> f64;
    fn proven_ml_relu(x: f64) -> f64;

    // --- SafeJson ---
    fn proven_json_is_valid(ptr: *const u8, len: usize) -> BoolResult;
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Convert ProvenStatus to an Elixir error atom.
fn status_to_atom(status: ProvenStatus) -> Atom {
    match status {
        ProvenStatus::Ok => atoms::ok(),
        ProvenStatus::ErrNullPointer => atoms::null_pointer(),
        ProvenStatus::ErrInvalidArgument => atoms::invalid_argument(),
        ProvenStatus::ErrOverflow => atoms::overflow(),
        ProvenStatus::ErrUnderflow => atoms::underflow(),
        ProvenStatus::ErrDivisionByZero => atoms::division_by_zero(),
        ProvenStatus::ErrParseFailure => atoms::parse_failure(),
        ProvenStatus::ErrValidationFailed => atoms::validation_failed(),
        ProvenStatus::ErrOutOfBounds => atoms::out_of_bounds(),
        ProvenStatus::ErrEncodingError => atoms::encoding_error(),
        ProvenStatus::ErrAllocationFailed => atoms::allocation_failed(),
        ProvenStatus::ErrNotImplemented => atoms::not_implemented(),
    }
}

/// Convert an IntResult from libproven to an Elixir {:ok, value} | {:error, reason} tuple.
fn int_result_to_term<'a>(env: Env<'a>, result: IntResult) -> Term<'a> {
    if result.status == ProvenStatus::Ok {
        (atoms::ok(), result.value).encode(env)
    } else {
        (atoms::error(), status_to_atom(result.status)).encode(env)
    }
}

/// Convert a FloatResult from libproven to an Elixir {:ok, value} | {:error, reason} tuple.
fn float_result_to_term<'a>(env: Env<'a>, result: FloatResult) -> Term<'a> {
    if result.status == ProvenStatus::Ok {
        (atoms::ok(), result.value).encode(env)
    } else {
        (atoms::error(), status_to_atom(result.status)).encode(env)
    }
}

/// Convert a BoolResult to an Elixir {:ok, bool} | {:error, reason} tuple.
fn bool_result_to_term<'a>(env: Env<'a>, result: BoolResult) -> Term<'a> {
    if result.status == ProvenStatus::Ok {
        (atoms::ok(), result.value).encode(env)
    } else {
        (atoms::error(), status_to_atom(result.status)).encode(env)
    }
}

/// Convert a StringResult to an Elixir {:ok, string} | {:error, reason},
/// freeing the libproven-allocated string after copying.
fn string_result_to_term<'a>(env: Env<'a>, result: StringResult) -> Term<'a> {
    if result.status == ProvenStatus::Ok && !result.value.is_null() && result.length > 0 {
        let slice = unsafe { std::slice::from_raw_parts(result.value, result.length) };
        let elixir_string = std::str::from_utf8(slice).unwrap_or("");
        let term = (atoms::ok(), elixir_string).encode(env);
        unsafe { proven_free_string(result.value) };
        term
    } else if result.status == ProvenStatus::Ok {
        // Ok but empty string
        let term = (atoms::ok(), "").encode(env);
        if !result.value.is_null() {
            unsafe { proven_free_string(result.value) };
        }
        term
    } else {
        (atoms::error(), status_to_atom(result.status)).encode(env)
    }
}

/// Ensure libproven is initialized (idempotent).
fn ensure_init() {
    unsafe {
        if !proven_is_initialized() {
            proven_init();
        }
    }
}

// ===========================================================================
// NIF Functions — SafeMath
// ===========================================================================

#[rustler::nif]
fn nif_math_add(a: i64, b: i64) -> NifResult<(Atom, i64)> {
    ensure_init();
    let result = unsafe { proven_math_add_checked(a, b) };
    if result.status == ProvenStatus::Ok {
        Ok((atoms::ok(), result.value))
    } else {
        Ok((atoms::error(), result.status as i64))
    }
}

#[rustler::nif]
fn nif_math_sub(a: i64, b: i64) -> NifResult<(Atom, i64)> {
    ensure_init();
    let result = unsafe { proven_math_sub_checked(a, b) };
    if result.status == ProvenStatus::Ok {
        Ok((atoms::ok(), result.value))
    } else {
        Ok((atoms::error(), result.status as i64))
    }
}

#[rustler::nif]
fn nif_math_mul(a: i64, b: i64) -> NifResult<(Atom, i64)> {
    ensure_init();
    let result = unsafe { proven_math_mul_checked(a, b) };
    if result.status == ProvenStatus::Ok {
        Ok((atoms::ok(), result.value))
    } else {
        Ok((atoms::error(), result.status as i64))
    }
}

#[rustler::nif]
fn nif_math_div(a: i64, b: i64) -> NifResult<(Atom, i64)> {
    ensure_init();
    let result = unsafe { proven_math_div(a, b) };
    if result.status == ProvenStatus::Ok {
        Ok((atoms::ok(), result.value))
    } else {
        Ok((atoms::error(), result.status as i64))
    }
}

#[rustler::nif]
fn nif_math_mod(a: i64, b: i64) -> NifResult<(Atom, i64)> {
    ensure_init();
    let result = unsafe { proven_math_mod(a, b) };
    if result.status == ProvenStatus::Ok {
        Ok((atoms::ok(), result.value))
    } else {
        Ok((atoms::error(), result.status as i64))
    }
}

#[rustler::nif]
fn nif_math_abs(n: i64) -> NifResult<(Atom, i64)> {
    ensure_init();
    let result = unsafe { proven_math_abs_safe(n) };
    if result.status == ProvenStatus::Ok {
        Ok((atoms::ok(), result.value))
    } else {
        Ok((atoms::error(), result.status as i64))
    }
}

#[rustler::nif]
fn nif_math_clamp(lo: i64, hi: i64, value: i64) -> i64 {
    ensure_init();
    unsafe { proven_math_clamp(lo, hi, value) }
}

#[rustler::nif]
fn nif_math_pow(base: i64, exp: u32) -> NifResult<(Atom, i64)> {
    ensure_init();
    let result = unsafe { proven_math_pow_checked(base, exp) };
    if result.status == ProvenStatus::Ok {
        Ok((atoms::ok(), result.value))
    } else {
        Ok((atoms::error(), result.status as i64))
    }
}

// ===========================================================================
// NIF Functions — SafeString
// ===========================================================================

#[rustler::nif]
fn nif_string_escape_html<'a>(env: Env<'a>, input: &str) -> Term<'a> {
    ensure_init();
    let result = unsafe { proven_string_escape_html(input.as_ptr(), input.len()) };
    string_result_to_term(env, result)
}

#[rustler::nif]
fn nif_string_escape_sql<'a>(env: Env<'a>, input: &str) -> Term<'a> {
    ensure_init();
    let result = unsafe { proven_string_escape_sql(input.as_ptr(), input.len()) };
    string_result_to_term(env, result)
}

#[rustler::nif]
fn nif_string_escape_js<'a>(env: Env<'a>, input: &str) -> Term<'a> {
    ensure_init();
    let result = unsafe { proven_string_escape_js(input.as_ptr(), input.len()) };
    string_result_to_term(env, result)
}

#[rustler::nif]
fn nif_string_is_valid_utf8(input: Binary) -> bool {
    ensure_init();
    let result =
        unsafe { proven_string_is_valid_utf8(input.as_slice().as_ptr(), input.as_slice().len()) };
    result.status == ProvenStatus::Ok && result.value
}

// ===========================================================================
// NIF Functions — SafePath
// ===========================================================================

#[rustler::nif]
fn nif_path_has_traversal(path: &str) -> bool {
    ensure_init();
    let result = unsafe { proven_path_has_traversal(path.as_ptr(), path.len()) };
    result.status == ProvenStatus::Ok && result.value
}

#[rustler::nif]
fn nif_path_sanitize_filename<'a>(env: Env<'a>, filename: &str) -> Term<'a> {
    ensure_init();
    let result = unsafe { proven_path_sanitize_filename(filename.as_ptr(), filename.len()) };
    string_result_to_term(env, result)
}

// ===========================================================================
// NIF Functions — SafeEmail
// ===========================================================================

#[rustler::nif]
fn nif_email_is_valid(email: &str) -> bool {
    ensure_init();
    let result = unsafe { proven_email_is_valid(email.as_ptr(), email.len()) };
    result.status == ProvenStatus::Ok && result.value
}

// ===========================================================================
// NIF Functions — SafeNetwork
// ===========================================================================

#[rustler::nif]
fn nif_network_parse_ipv4(address: &str) -> NifResult<(Atom, (u8, u8, u8, u8))> {
    ensure_init();
    let result = unsafe { proven_network_parse_ipv4(address.as_ptr(), address.len()) };
    if result.status == ProvenStatus::Ok {
        let o = result.address.octets;
        Ok((atoms::ok(), (o[0], o[1], o[2], o[3])))
    } else {
        Ok((atoms::error(), (0, 0, 0, 0)))
    }
}

#[rustler::nif]
fn nif_network_ipv4_is_private(a: u8, b: u8, c: u8, d: u8) -> bool {
    ensure_init();
    let addr = IPv4Address {
        octets: [a, b, c, d],
    };
    unsafe { proven_network_ipv4_is_private(addr) }
}

#[rustler::nif]
fn nif_network_ipv4_is_loopback(a: u8, b: u8, c: u8, d: u8) -> bool {
    ensure_init();
    let addr = IPv4Address {
        octets: [a, b, c, d],
    };
    unsafe { proven_network_ipv4_is_loopback(addr) }
}

// ===========================================================================
// NIF Functions — SafeCrypto
// ===========================================================================

#[rustler::nif]
fn nif_crypto_constant_time_eq(a: Binary, b: Binary) -> bool {
    ensure_init();
    let result = unsafe {
        proven_crypto_constant_time_eq(
            a.as_slice().as_ptr(),
            a.as_slice().len(),
            b.as_slice().as_ptr(),
            b.as_slice().len(),
        )
    };
    result.status == ProvenStatus::Ok && result.value
}

#[rustler::nif]
fn nif_crypto_random_bytes(env: Env, n: usize) -> NifResult<Binary> {
    ensure_init();
    let mut owned = OwnedBinary::new(n).ok_or(rustler::Error::Atom("allocation_failed"))?;
    let status = unsafe { proven_crypto_random_bytes(owned.as_mut_slice().as_mut_ptr(), n) };
    if status == ProvenStatus::Ok {
        Ok(owned.release(env))
    } else {
        Err(rustler::Error::Atom("crypto_error"))
    }
}

// ===========================================================================
// NIF Functions — SafeFloat
// ===========================================================================

#[rustler::nif]
fn nif_float_div<'a>(env: Env<'a>, a: f64, b: f64) -> Term<'a> {
    ensure_init();
    let result = unsafe { proven_float_div(a, b) };
    float_result_to_term(env, result)
}

#[rustler::nif]
fn nif_float_is_finite(x: f64) -> bool {
    ensure_init();
    unsafe { proven_float_is_finite(x) }
}

#[rustler::nif]
fn nif_float_is_nan(x: f64) -> bool {
    ensure_init();
    unsafe { proven_float_is_nan(x) }
}

#[rustler::nif]
fn nif_float_sqrt<'a>(env: Env<'a>, x: f64) -> Term<'a> {
    ensure_init();
    let result = unsafe { proven_float_sqrt(x) };
    float_result_to_term(env, result)
}

#[rustler::nif]
fn nif_float_ln<'a>(env: Env<'a>, x: f64) -> Term<'a> {
    ensure_init();
    let result = unsafe { proven_float_ln(x) };
    float_result_to_term(env, result)
}

// ===========================================================================
// NIF Functions — SafeHex
// ===========================================================================

#[rustler::nif]
fn nif_hex_encode<'a>(env: Env<'a>, data: Binary, uppercase: bool) -> Term<'a> {
    ensure_init();
    let result =
        unsafe { proven_hex_encode(data.as_slice().as_ptr(), data.as_slice().len(), uppercase) };
    string_result_to_term(env, result)
}

#[rustler::nif]
fn nif_hex_decode<'a>(env: Env<'a>, hex_string: &str) -> Term<'a> {
    ensure_init();
    let result = unsafe { proven_hex_decode(hex_string.as_ptr(), hex_string.len()) };
    if result.status == ProvenStatus::Ok && !result.data.is_null() && result.length > 0 {
        let slice = unsafe { std::slice::from_raw_parts(result.data, result.length) };
        let mut owned = match OwnedBinary::new(result.length) {
            Some(b) => b,
            None => return (atoms::error(), atoms::allocation_failed()).encode(env),
        };
        owned.as_mut_slice().copy_from_slice(slice);
        // Note: we do not call proven_free on HexDecodeResult here, as the
        // data may be managed differently. The Zig side should handle this.
        (atoms::ok(), owned.release(env)).encode(env)
    } else {
        (atoms::error(), status_to_atom(result.status)).encode(env)
    }
}

// ===========================================================================
// NIF Functions — SafeUUID
// ===========================================================================

#[rustler::nif]
fn nif_uuid_v4<'a>(env: Env<'a>) -> Term<'a> {
    ensure_init();
    let result = unsafe { proven_uuid_v4() };
    if result.status == ProvenStatus::Ok {
        let str_result = unsafe { proven_uuid_to_string(result.uuid) };
        string_result_to_term(env, str_result)
    } else {
        (atoms::error(), status_to_atom(result.status)).encode(env)
    }
}

#[rustler::nif]
fn nif_uuid_parse<'a>(env: Env<'a>, uuid_string: &str) -> Term<'a> {
    ensure_init();
    let result = unsafe { proven_uuid_parse(uuid_string.as_ptr(), uuid_string.len()) };
    if result.status == ProvenStatus::Ok {
        let str_result = unsafe { proven_uuid_to_string(result.uuid) };
        string_result_to_term(env, str_result)
    } else {
        (atoms::error(), atoms::invalid_uuid()).encode(env)
    }
}

#[rustler::nif]
fn nif_uuid_is_nil(uuid_string: &str) -> bool {
    ensure_init();
    let result = unsafe { proven_uuid_parse(uuid_string.as_ptr(), uuid_string.len()) };
    if result.status == ProvenStatus::Ok {
        unsafe { proven_uuid_is_nil(result.uuid) }
    } else {
        false
    }
}

#[rustler::nif]
fn nif_uuid_version(uuid_string: &str) -> NifResult<(Atom, u8)> {
    ensure_init();
    let result = unsafe { proven_uuid_parse(uuid_string.as_ptr(), uuid_string.len()) };
    if result.status == ProvenStatus::Ok {
        let ver = unsafe { proven_uuid_version(result.uuid) };
        Ok((atoms::ok(), ver))
    } else {
        Ok((atoms::error(), 0))
    }
}

// ===========================================================================
// NIF Functions — SafeHeader
// ===========================================================================

#[rustler::nif]
fn nif_header_has_crlf(s: &str) -> bool {
    ensure_init();
    let result = unsafe { proven_header_has_crlf(s.as_ptr(), s.len()) };
    result.status == ProvenStatus::Ok && result.value
}

#[rustler::nif]
fn nif_header_is_valid_name(name: &str) -> bool {
    ensure_init();
    let result = unsafe { proven_header_is_valid_name(name.as_ptr(), name.len()) };
    result.status == ProvenStatus::Ok && result.value
}

#[rustler::nif]
fn nif_header_is_dangerous(name: &str) -> bool {
    ensure_init();
    let result = unsafe { proven_header_is_dangerous(name.as_ptr(), name.len()) };
    result.status == ProvenStatus::Ok && result.value
}

// ===========================================================================
// NIF Functions — SafeCookie
// ===========================================================================

#[rustler::nif]
fn nif_cookie_has_injection(s: &str) -> bool {
    ensure_init();
    let result = unsafe { proven_cookie_has_injection(s.as_ptr(), s.len()) };
    result.status == ProvenStatus::Ok && result.value
}

#[rustler::nif]
fn nif_cookie_get_prefix(name: &str) -> i64 {
    ensure_init();
    let result = unsafe { proven_cookie_get_prefix(name.as_ptr(), name.len()) };
    if result.status == ProvenStatus::Ok {
        result.value
    } else {
        0 // NoPrefix
    }
}

// ===========================================================================
// NIF Functions — SafeContentType
// ===========================================================================

#[rustler::nif]
fn nif_content_type_can_sniff_dangerous(s: &str) -> bool {
    ensure_init();
    let result = unsafe { proven_content_type_can_sniff_dangerous(s.as_ptr(), s.len()) };
    result.status == ProvenStatus::Ok && result.value
}

#[rustler::nif]
fn nif_content_type_is_json(subtype: &str, suffix: &str) -> bool {
    ensure_init();
    let result = unsafe {
        proven_content_type_is_json(
            subtype.as_ptr(),
            subtype.len(),
            suffix.as_ptr(),
            suffix.len(),
        )
    };
    result.status == ProvenStatus::Ok && result.value
}

#[rustler::nif]
fn nif_content_type_is_xml(subtype: &str, suffix: &str) -> bool {
    ensure_init();
    let result = unsafe {
        proven_content_type_is_xml(
            subtype.as_ptr(),
            subtype.len(),
            suffix.as_ptr(),
            suffix.len(),
        )
    };
    result.status == ProvenStatus::Ok && result.value
}

// ===========================================================================
// NIF Functions — SafePassword
// ===========================================================================

#[rustler::nif]
fn nif_password_validate<'a>(env: Env<'a>, password: &str) -> Term<'a> {
    ensure_init();
    let result = unsafe { proven_password_validate(password.as_ptr(), password.len()) };
    if result.status == ProvenStatus::Ok {
        let strength_str = match result.strength {
            PasswordStrength::VeryWeak => "very_weak",
            PasswordStrength::Weak => "weak",
            PasswordStrength::Fair => "fair",
            PasswordStrength::Strong => "strong",
            PasswordStrength::VeryStrong => "very_strong",
        };
        (atoms::ok(), strength_str).encode(env)
    } else {
        (atoms::error(), status_to_atom(result.status)).encode(env)
    }
}

#[rustler::nif]
fn nif_password_is_common(password: &str) -> bool {
    ensure_init();
    unsafe { proven_password_is_common(password.as_ptr(), password.len()) }
}

// ===========================================================================
// NIF Functions — SafeColor
// ===========================================================================

#[rustler::nif]
fn nif_color_parse_hex(hex: &str) -> NifResult<(Atom, (u8, u8, u8))> {
    ensure_init();
    let result = unsafe { proven_color_parse_hex(hex.as_ptr(), hex.len()) };
    if result.status == ProvenStatus::Ok {
        Ok((
            atoms::ok(),
            (result.color.r, result.color.g, result.color.b),
        ))
    } else {
        Ok((atoms::error(), (0, 0, 0)))
    }
}

#[rustler::nif]
fn nif_color_rgb_to_hsl(r: u8, g: u8, b: u8) -> (f64, f64, f64) {
    ensure_init();
    let rgb = RGBColor { r, g, b };
    let hsl = unsafe { proven_color_rgb_to_hsl(rgb) };
    (hsl.h, hsl.s, hsl.l)
}

#[rustler::nif]
fn nif_color_to_hex<'a>(env: Env<'a>, r: u8, g: u8, b: u8) -> Term<'a> {
    ensure_init();
    let rgb = RGBColor { r, g, b };
    let result = unsafe { proven_color_to_hex(rgb) };
    string_result_to_term(env, result)
}

// ===========================================================================
// NIF Functions — SafeAngle
// ===========================================================================

#[rustler::nif]
fn nif_angle_deg_to_rad(degrees: f64) -> f64 {
    ensure_init();
    unsafe { proven_angle_deg_to_rad(degrees) }
}

#[rustler::nif]
fn nif_angle_rad_to_deg(radians: f64) -> f64 {
    ensure_init();
    unsafe { proven_angle_rad_to_deg(radians) }
}

#[rustler::nif]
fn nif_angle_normalize_degrees(degrees: f64) -> f64 {
    ensure_init();
    unsafe { proven_angle_normalize_degrees(degrees) }
}

#[rustler::nif]
fn nif_angle_normalize_radians(radians: f64) -> f64 {
    ensure_init();
    unsafe { proven_angle_normalize_radians(radians) }
}

// ===========================================================================
// NIF Functions — SafeUnit
// ===========================================================================

#[rustler::nif]
fn nif_unit_convert_length<'a>(env: Env<'a>, value: f64, from: i32, to: i32) -> Term<'a> {
    ensure_init();
    let from_unit: LengthUnit = unsafe { std::mem::transmute(from) };
    let to_unit: LengthUnit = unsafe { std::mem::transmute(to) };
    let result = unsafe { proven_unit_convert_length(value, from_unit, to_unit) };
    float_result_to_term(env, result)
}

#[rustler::nif]
fn nif_unit_convert_temp<'a>(env: Env<'a>, value: f64, from: i32, to: i32) -> Term<'a> {
    ensure_init();
    let from_unit: TempUnit = unsafe { std::mem::transmute(from) };
    let to_unit: TempUnit = unsafe { std::mem::transmute(to) };
    let result = unsafe { proven_unit_convert_temp(value, from_unit, to_unit) };
    float_result_to_term(env, result)
}

// ===========================================================================
// NIF Functions — SafeGeo
// ===========================================================================

#[rustler::nif]
fn nif_geo_validate<'a>(env: Env<'a>, lat: f64, lon: f64) -> Term<'a> {
    ensure_init();
    let result = unsafe { proven_geo_validate(lat, lon) };
    if result.status == ProvenStatus::Ok {
        (
            atoms::ok(),
            (result.coordinate.latitude, result.coordinate.longitude),
        )
            .encode(env)
    } else {
        (atoms::error(), status_to_atom(result.status)).encode(env)
    }
}

#[rustler::nif]
fn nif_geo_distance<'a>(env: Env<'a>, lat1: f64, lon1: f64, lat2: f64, lon2: f64) -> Term<'a> {
    ensure_init();
    let a = GeoCoordinate {
        latitude: lat1,
        longitude: lon1,
    };
    let b = GeoCoordinate {
        latitude: lat2,
        longitude: lon2,
    };
    let result = unsafe { proven_geo_distance(a, b) };
    float_result_to_term(env, result)
}

// ===========================================================================
// NIF Functions — SafeChecksum
// ===========================================================================

#[rustler::nif]
fn nif_checksum_crc32(data: Binary) -> NifResult<(Atom, i64)> {
    ensure_init();
    let result =
        unsafe { proven_checksum_crc32(data.as_slice().as_ptr(), data.as_slice().len()) };
    if result.status == ProvenStatus::Ok {
        Ok((atoms::ok(), result.value))
    } else {
        Ok((atoms::error(), result.status as i64))
    }
}

// ===========================================================================
// NIF Functions — SafeProbability
// ===========================================================================

#[rustler::nif]
fn nif_probability_create(value: f64) -> f64 {
    ensure_init();
    unsafe { proven_probability_create(value) }
}

#[rustler::nif]
fn nif_probability_and(a: f64, b: f64) -> f64 {
    ensure_init();
    unsafe { proven_probability_and(a, b) }
}

#[rustler::nif]
fn nif_probability_or(a: f64, b: f64) -> f64 {
    ensure_init();
    unsafe { proven_probability_or_exclusive(a, b) }
}

#[rustler::nif]
fn nif_probability_not(p: f64) -> f64 {
    ensure_init();
    unsafe { proven_probability_not(p) }
}

// ===========================================================================
// NIF Functions — SafeML
// ===========================================================================

#[rustler::nif]
fn nif_ml_sigmoid(x: f64) -> f64 {
    ensure_init();
    unsafe { proven_ml_sigmoid(x) }
}

#[rustler::nif]
fn nif_ml_relu(x: f64) -> f64 {
    ensure_init();
    unsafe { proven_ml_relu(x) }
}

// ===========================================================================
// NIF Functions — SafeJson
// ===========================================================================

#[rustler::nif]
fn nif_json_is_valid(json_string: &str) -> bool {
    ensure_init();
    let result = unsafe { proven_json_is_valid(json_string.as_ptr(), json_string.len()) };
    result.status == ProvenStatus::Ok && result.value
}

// ===========================================================================
// NIF registration
// ===========================================================================

rustler::init!(
    "Elixir.Proven.NIF",
    [
        // SafeMath
        nif_math_add,
        nif_math_sub,
        nif_math_mul,
        nif_math_div,
        nif_math_mod,
        nif_math_abs,
        nif_math_clamp,
        nif_math_pow,
        // SafeString
        nif_string_escape_html,
        nif_string_escape_sql,
        nif_string_escape_js,
        nif_string_is_valid_utf8,
        // SafePath
        nif_path_has_traversal,
        nif_path_sanitize_filename,
        // SafeEmail
        nif_email_is_valid,
        // SafeNetwork
        nif_network_parse_ipv4,
        nif_network_ipv4_is_private,
        nif_network_ipv4_is_loopback,
        // SafeCrypto
        nif_crypto_constant_time_eq,
        nif_crypto_random_bytes,
        // SafeFloat
        nif_float_div,
        nif_float_is_finite,
        nif_float_is_nan,
        nif_float_sqrt,
        nif_float_ln,
        // SafeHex
        nif_hex_encode,
        nif_hex_decode,
        // SafeUUID
        nif_uuid_v4,
        nif_uuid_parse,
        nif_uuid_is_nil,
        nif_uuid_version,
        // SafeHeader
        nif_header_has_crlf,
        nif_header_is_valid_name,
        nif_header_is_dangerous,
        // SafeCookie
        nif_cookie_has_injection,
        nif_cookie_get_prefix,
        // SafeContentType
        nif_content_type_can_sniff_dangerous,
        nif_content_type_is_json,
        nif_content_type_is_xml,
        // SafePassword
        nif_password_validate,
        nif_password_is_common,
        // SafeColor
        nif_color_parse_hex,
        nif_color_rgb_to_hsl,
        nif_color_to_hex,
        // SafeAngle
        nif_angle_deg_to_rad,
        nif_angle_rad_to_deg,
        nif_angle_normalize_degrees,
        nif_angle_normalize_radians,
        // SafeUnit
        nif_unit_convert_length,
        nif_unit_convert_temp,
        // SafeGeo
        nif_geo_validate,
        nif_geo_distance,
        // SafeChecksum
        nif_checksum_crc32,
        // SafeProbability
        nif_probability_create,
        nif_probability_and,
        nif_probability_or,
        nif_probability_not,
        // SafeML
        nif_ml_sigmoid,
        nif_ml_relu,
        // SafeJson
        nif_json_is_valid,
    ]
);
