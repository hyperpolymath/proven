// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Raw FFI declarations for libproven.
//!
//! This module contains all `extern "C"` function declarations that match
//! the symbols exported by the Zig FFI layer. These are the low-level,
//! unsafe bindings. For safe Rust wrappers, use the public API modules.
//!
//! ALL computation is performed in Idris 2 via the Zig bridge.
//! These declarations merely describe the C ABI interface.

#![allow(non_camel_case_types)]

use std::os::raw::c_void;

// ============================================================================
// Result status codes (matches ProvenStatus enum in Zig)
// ============================================================================

/// Status codes returned by libproven functions.
///
/// Matches the `ProvenStatus` enum defined in the Zig FFI layer.
pub const STATUS_OK: i32 = 0;
pub const STATUS_ERR_NULL_POINTER: i32 = -1;
pub const STATUS_ERR_INVALID_ARGUMENT: i32 = -2;
pub const STATUS_ERR_OVERFLOW: i32 = -3;
pub const STATUS_ERR_UNDERFLOW: i32 = -4;
pub const STATUS_ERR_DIVISION_BY_ZERO: i32 = -5;
pub const STATUS_ERR_PARSE_FAILURE: i32 = -6;
pub const STATUS_ERR_VALIDATION_FAILED: i32 = -7;
pub const STATUS_ERR_OUT_OF_BOUNDS: i32 = -8;
pub const STATUS_ERR_ENCODING_ERROR: i32 = -9;
pub const STATUS_ERR_ALLOCATION_FAILED: i32 = -10;
pub const STATUS_ERR_NOT_IMPLEMENTED: i32 = -99;

// ============================================================================
// Result structures (matches extern struct definitions in Zig)
// ============================================================================

/// Result for integer operations.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct IntResult {
    pub status: i32,
    pub value: i64,
}

/// Result for boolean operations.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct BoolResult {
    pub status: i32,
    pub value: bool,
}

/// Result for string operations. Caller must free `value` via `proven_free_string`.
#[repr(C)]
#[derive(Debug)]
pub struct StringResult {
    pub status: i32,
    pub value: *mut u8,
    pub length: usize,
}

/// Result for floating-point operations.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FloatResult {
    pub status: i32,
    pub value: f64,
}

// ============================================================================
// Complex result structures
// ============================================================================

/// IPv4 address (4 octets).
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct IPv4Address {
    pub octets: [u8; 4],
}

/// Result for IPv4 parsing.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct IPv4Result {
    pub status: i32,
    pub address: IPv4Address,
}

/// URL components.
#[repr(C)]
#[derive(Debug)]
pub struct UrlComponents {
    pub scheme: *mut u8,
    pub scheme_len: usize,
    pub host: *mut u8,
    pub host_len: usize,
    pub port: u16,
    pub has_port: bool,
    pub path: *mut u8,
    pub path_len: usize,
    pub query: *mut u8,
    pub query_len: usize,
    pub fragment: *mut u8,
    pub fragment_len: usize,
}

/// Result for URL parsing.
#[repr(C)]
#[derive(Debug)]
pub struct UrlResult {
    pub status: i32,
    pub components: UrlComponents,
}

/// Password strength level.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PasswordStrength {
    VeryWeak = 0,
    Weak = 1,
    Fair = 2,
    Strong = 3,
    VeryStrong = 4,
}

/// Password validation result.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct PasswordResult {
    pub strength: PasswordStrength,
    pub has_lowercase: bool,
    pub has_uppercase: bool,
    pub has_digit: bool,
    pub has_special: bool,
    pub length: usize,
}

/// SameSite cookie attribute.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SameSite {
    Strict = 0,
    Lax = 1,
    None = 2,
}

/// Cookie attributes for building Set-Cookie headers.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct CookieAttributes {
    pub domain: *const u8,
    pub domain_len: usize,
    pub path: *const u8,
    pub path_len: usize,
    pub max_age: i64,
    pub secure: bool,
    pub http_only: bool,
    pub same_site: SameSite,
    pub partitioned: bool,
}

/// Media type category.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MediaCategory {
    Text = 0,
    Image = 1,
    Audio = 2,
    Video = 3,
    Application = 4,
    Multipart = 5,
    Message = 6,
    Font = 7,
    Model = 8,
    Custom = 9,
}

/// Charset encoding.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Charset {
    Utf8 = 0,
    Utf16Le = 1,
    Utf16Be = 2,
    Iso8859_1 = 3,
    Ascii = 4,
    Windows1252 = 5,
    Other = 6,
}

/// Content type parse result.
#[repr(C)]
#[derive(Debug)]
pub struct ContentTypeResult {
    pub status: i32,
    pub media_type: *mut u8,
    pub media_type_len: usize,
    pub subtype: *mut u8,
    pub subtype_len: usize,
    pub suffix: *mut u8,
    pub suffix_len: usize,
    pub category: MediaCategory,
    pub charset: Charset,
    pub has_charset: bool,
}

/// JSON value type.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JsonType {
    Null = 0,
    Bool = 1,
    Number = 2,
    String = 3,
    Array = 4,
    Object = 5,
    Invalid = -1,
}

/// DateTime components.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct DateTime {
    pub year: i32,
    pub month: u8,
    pub day: u8,
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
    pub nanosecond: u32,
    pub tz_offset_minutes: i16,
}

/// DateTime result.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct DateTimeResult {
    pub status: i32,
    pub datetime: DateTime,
}

/// UUID (128 bits).
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UUID {
    pub bytes: [u8; 16],
}

/// UUID result.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct UUIDResult {
    pub status: i32,
    pub uuid: UUID,
}

/// Currency result.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CurrencyResult {
    pub status: i32,
    pub amount_minor: i64,
    pub currency_code: [u8; 3],
    pub decimal_places: u8,
}

/// Phone number result.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct PhoneResult {
    pub status: i32,
    pub country_code: u16,
    pub national_number: u64,
    pub is_valid: bool,
}

/// Hex decode result.
#[repr(C)]
#[derive(Debug)]
pub struct HexDecodeResult {
    pub status: i32,
    pub data: *mut u8,
    pub length: usize,
}

/// Semantic version.
#[repr(C)]
#[derive(Debug)]
pub struct SemanticVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub prerelease_len: usize,
    pub prerelease: *mut u8,
}

/// Version result.
#[repr(C)]
#[derive(Debug)]
pub struct VersionResult {
    pub status: i32,
    pub version: SemanticVersion,
}

/// Geographic coordinate.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct GeoCoordinate {
    pub latitude: f64,
    pub longitude: f64,
}

/// Geo result.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct GeoResult {
    pub status: i32,
    pub coordinate: GeoCoordinate,
}

/// RGB color.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RGBColor {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

/// HSL color.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HSLColor {
    pub h: f64,
    pub s: f64,
    pub l: f64,
}

/// Color parse result.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct ColorParseResult {
    pub status: i32,
    pub color: RGBColor,
}

/// Length unit.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LengthUnit {
    Meters = 0,
    Kilometers = 1,
    Centimeters = 2,
    Millimeters = 3,
    Feet = 4,
    Inches = 5,
    Miles = 6,
    Yards = 7,
}

/// Temperature unit.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TempUnit {
    Celsius = 0,
    Fahrenheit = 1,
    Kelvin = 2,
}

/// OCI image reference.
#[repr(C)]
#[derive(Debug)]
pub struct ImageReference {
    pub registry: *mut u8,
    pub registry_len: usize,
    pub repository: *mut u8,
    pub repository_len: usize,
    pub tag: *mut u8,
    pub tag_len: usize,
    pub digest: *mut u8,
    pub digest_len: usize,
}

/// Image reference result.
#[repr(C)]
#[derive(Debug)]
pub struct ImageRefResult {
    pub status: i32,
    pub reference: ImageReference,
}

/// Hash algorithm.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HashAlgorithm {
    SHA256 = 0,
    SHA384 = 1,
    SHA512 = 2,
    Blake3 = 3,
}

/// Cryptographic digest.
#[repr(C)]
#[derive(Debug)]
pub struct Digest {
    pub algorithm: HashAlgorithm,
    pub value: *mut u8,
    pub value_len: usize,
}

/// Digest result.
#[repr(C)]
#[derive(Debug)]
pub struct DigestResult {
    pub status: i32,
    pub digest: Digest,
}

/// WWW-Authenticate challenge.
#[repr(C)]
#[derive(Debug)]
pub struct AuthChallenge {
    pub scheme: *mut u8,
    pub scheme_len: usize,
    pub realm: *mut u8,
    pub realm_len: usize,
    pub service: *mut u8,
    pub service_len: usize,
    pub scope: *mut u8,
    pub scope_len: usize,
}

/// Auth challenge result.
#[repr(C)]
#[derive(Debug)]
pub struct AuthChallengeResult {
    pub status: i32,
    pub challenge: AuthChallenge,
}

/// Retry configuration.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct RetryConfig {
    pub max_attempts: u32,
    pub base_delay_ms: u64,
    pub max_delay_ms: u64,
    pub multiplier: f64,
}

/// Circuit breaker state.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CircuitState {
    Closed = 0,
    Open = 1,
    HalfOpen = 2,
}

/// Event types for callback system.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProvenEventType {
    ValidationFailed = 1,
    ResourceAcquired = 2,
    ResourceReleased = 3,
    RateLimitHit = 4,
    CircuitStateChange = 5,
    SignalReceived = 6,
    RetryAttempt = 7,
    CryptoOperation = 8,
    Custom = 100,
}

/// Event data passed to callbacks.
#[repr(C)]
#[derive(Debug)]
pub struct ProvenEvent {
    pub event_type: ProvenEventType,
    pub data_ptr: *const u8,
    pub data_len: usize,
    pub timestamp_us: u64,
    pub code: i32,
}

/// Callback function signature.
pub type ProvenCallbackFn =
    Option<unsafe extern "C" fn(context: *mut c_void, event: *const ProvenEvent) -> i32>;

/// Callback handle (0 = invalid).
pub type CallbackHandle = u32;

// ============================================================================
// Opaque types for heap-allocated structures managed by libproven
// ============================================================================

/// Opaque bounded buffer (managed by libproven).
#[repr(C)]
pub struct BoundedBuffer {
    _opaque: [u8; 0],
}

/// Opaque rate limiter (managed by libproven).
#[repr(C)]
pub struct RateLimiter {
    _opaque: [u8; 0],
}

/// Opaque circuit breaker (managed by libproven).
#[repr(C)]
pub struct CircuitBreaker {
    _opaque: [u8; 0],
}

/// Opaque monotonic counter (managed by libproven).
#[repr(C)]
pub struct MonotonicCounter {
    _opaque: [u8; 0],
}

/// Opaque state machine (managed by libproven).
#[repr(C)]
pub struct StateMachine {
    _opaque: [u8; 0],
}

/// Opaque 2D tensor (managed by libproven).
#[repr(C)]
pub struct Tensor2D {
    _opaque: [u8; 0],
}

/// Opaque LRU cache (managed by libproven).
#[repr(C)]
pub struct LRUCache {
    _opaque: [u8; 0],
}

/// Opaque graph (managed by libproven).
#[repr(C)]
pub struct Graph {
    _opaque: [u8; 0],
}

/// Opaque bounded queue (managed by libproven).
#[repr(C)]
pub struct BoundedQueue {
    _opaque: [u8; 0],
}

/// Opaque bloom filter (managed by libproven).
#[repr(C)]
pub struct BloomFilter {
    _opaque: [u8; 0],
}

/// Buffer result.
#[repr(C)]
#[derive(Debug)]
pub struct BufferResult {
    pub status: i32,
    pub buffer: *mut BoundedBuffer,
}

// ============================================================================
// Extern "C" function declarations
// ============================================================================

extern "C" {
    // --- Memory management ---

    /// Free a string allocated by libproven.
    pub fn proven_free_string(ptr: *mut u8);

    // --- Lifecycle ---

    /// Initialize the Proven runtime (includes Idris 2 runtime).
    pub fn proven_init() -> i32;

    /// Cleanup the Proven runtime.
    pub fn proven_deinit();

    /// Check if runtime is initialized.
    pub fn proven_is_initialized() -> bool;

    // --- Version info ---

    /// Get major version number.
    pub fn proven_version_major() -> u32;

    /// Get minor version number.
    pub fn proven_version_minor() -> u32;

    /// Get patch version number.
    pub fn proven_version_patch() -> u32;

    /// Get number of modules.
    pub fn proven_module_count() -> u32;

    /// Get FFI ABI version for compatibility checking.
    pub fn proven_ffi_abi_version() -> u32;

    // --- SafeMath (8 functions) ---

    /// Safe division with zero-check.
    pub fn proven_math_div(numerator: i64, denominator: i64) -> IntResult;

    /// Safe modulo with zero-check.
    pub fn proven_math_mod(numerator: i64, denominator: i64) -> IntResult;

    /// Checked addition with overflow detection.
    pub fn proven_math_add_checked(a: i64, b: i64) -> IntResult;

    /// Checked subtraction with underflow detection.
    pub fn proven_math_sub_checked(a: i64, b: i64) -> IntResult;

    /// Checked multiplication with overflow detection.
    pub fn proven_math_mul_checked(a: i64, b: i64) -> IntResult;

    /// Safe absolute value (handles MIN_INT).
    pub fn proven_math_abs_safe(n: i64) -> IntResult;

    /// Clamp value to range [lo, hi].
    pub fn proven_math_clamp(lo: i64, hi: i64, value: i64) -> i64;

    /// Integer power with overflow checking.
    pub fn proven_math_pow_checked(base: i64, exp: u32) -> IntResult;

    // --- SafeString (4 functions) ---

    /// Check if byte sequence is valid UTF-8.
    pub fn proven_string_is_valid_utf8(ptr: *const u8, len: usize) -> BoolResult;

    /// Escape string for SQL.
    pub fn proven_string_escape_sql(ptr: *const u8, len: usize) -> StringResult;

    /// Escape string for HTML.
    pub fn proven_string_escape_html(ptr: *const u8, len: usize) -> StringResult;

    /// Escape string for JavaScript.
    pub fn proven_string_escape_js(ptr: *const u8, len: usize) -> StringResult;

    // --- SafePath (2 functions) ---

    /// Check if path attempts directory traversal.
    pub fn proven_path_has_traversal(ptr: *const u8, len: usize) -> BoolResult;

    /// Sanitize a filename.
    pub fn proven_path_sanitize_filename(ptr: *const u8, len: usize) -> StringResult;

    // --- SafeCrypto (2 functions) ---

    /// Constant-time byte comparison.
    pub fn proven_crypto_constant_time_eq(
        ptr1: *const u8,
        len1: usize,
        ptr2: *const u8,
        len2: usize,
    ) -> BoolResult;

    /// Fill buffer with cryptographically secure random bytes.
    pub fn proven_crypto_random_bytes(ptr: *mut u8, len: usize) -> i32;

    // --- SafeUrl (2 functions) ---

    /// Parse URL into components.
    pub fn proven_url_parse(ptr: *const u8, len: usize) -> UrlResult;

    /// Free URL components.
    pub fn proven_url_free(components: *mut UrlComponents);

    // --- SafeEmail (1 function) ---

    /// Validate email address.
    pub fn proven_email_is_valid(ptr: *const u8, len: usize) -> BoolResult;

    // --- SafeNetwork (3 functions) ---

    /// Parse IPv4 address.
    pub fn proven_network_parse_ipv4(ptr: *const u8, len: usize) -> IPv4Result;

    /// Check if IPv4 is private (RFC 1918).
    pub fn proven_network_ipv4_is_private(addr: IPv4Address) -> bool;

    /// Check if IPv4 is loopback.
    pub fn proven_network_ipv4_is_loopback(addr: IPv4Address) -> bool;

    // --- SafeHeader (6 functions) ---

    /// Check for CRLF injection in header value.
    pub fn proven_header_has_crlf(ptr: *const u8, len: usize) -> BoolResult;

    /// Check if header name is valid per RFC 7230.
    pub fn proven_header_is_valid_name(ptr: *const u8, len: usize) -> BoolResult;

    /// Check if header name is dangerous.
    pub fn proven_header_is_dangerous(ptr: *const u8, len: usize) -> BoolResult;

    /// Create validated header string "Name: Value".
    pub fn proven_header_render(
        name_ptr: *const u8,
        name_len: usize,
        value_ptr: *const u8,
        value_len: usize,
    ) -> StringResult;

    /// Build Content-Security-Policy header value.
    pub fn proven_header_build_csp(
        directives_json: *const u8,
        json_len: usize,
    ) -> StringResult;

    /// Build HSTS header value.
    pub fn proven_header_build_hsts(
        max_age: i64,
        include_subdomains: bool,
        preload: bool,
    ) -> StringResult;

    // --- SafeCookie (6 functions) ---

    /// Check for cookie injection characters.
    pub fn proven_cookie_has_injection(ptr: *const u8, len: usize) -> BoolResult;

    /// Validate cookie name.
    pub fn proven_cookie_validate_name(ptr: *const u8, len: usize) -> BoolResult;

    /// Validate cookie value.
    pub fn proven_cookie_validate_value(ptr: *const u8, len: usize) -> BoolResult;

    /// Get cookie prefix type.
    pub fn proven_cookie_get_prefix(ptr: *const u8, len: usize) -> IntResult;

    /// Build Set-Cookie header value.
    pub fn proven_cookie_build_set_cookie(
        name_ptr: *const u8,
        name_len: usize,
        value_ptr: *const u8,
        value_len: usize,
        attrs: CookieAttributes,
    ) -> StringResult;

    /// Build delete cookie header.
    pub fn proven_cookie_build_delete(name_ptr: *const u8, name_len: usize) -> StringResult;

    // --- SafeContentType (6 functions) ---

    /// Parse Content-Type header.
    pub fn proven_content_type_parse(ptr: *const u8, len: usize) -> ContentTypeResult;

    /// Free content type result.
    pub fn proven_content_type_free(result: *mut ContentTypeResult);

    /// Check if content type can be sniffed to something dangerous.
    pub fn proven_content_type_can_sniff_dangerous(
        ptr: *const u8,
        len: usize,
    ) -> BoolResult;

    /// Render content type to string.
    pub fn proven_content_type_render(
        type_ptr: *const u8,
        type_len: usize,
        subtype_ptr: *const u8,
        subtype_len: usize,
        suffix_ptr: *const u8,
        suffix_len: usize,
        charset: Charset,
        has_charset: bool,
    ) -> StringResult;

    /// Check if content type is JSON.
    pub fn proven_content_type_is_json(
        subtype_ptr: *const u8,
        subtype_len: usize,
        suffix_ptr: *const u8,
        suffix_len: usize,
    ) -> BoolResult;

    /// Check if content type is XML.
    pub fn proven_content_type_is_xml(
        subtype_ptr: *const u8,
        subtype_len: usize,
        suffix_ptr: *const u8,
        suffix_len: usize,
    ) -> BoolResult;

    // --- SafeUUID (5 functions) ---

    /// Generate UUID v4.
    pub fn proven_uuid_v4() -> UUIDResult;

    /// Format UUID as string.
    pub fn proven_uuid_to_string(uuid: UUID) -> StringResult;

    /// Parse UUID from string.
    pub fn proven_uuid_parse(ptr: *const u8, len: usize) -> UUIDResult;

    /// Check if UUID is nil.
    pub fn proven_uuid_is_nil(uuid: UUID) -> bool;

    /// Get UUID version.
    pub fn proven_uuid_version(uuid: UUID) -> u8;

    // --- SafeJson (2 functions) ---

    /// Check if string is valid JSON.
    pub fn proven_json_is_valid(ptr: *const u8, len: usize) -> BoolResult;

    /// Get JSON value type at root level.
    pub fn proven_json_get_type(ptr: *const u8, len: usize) -> JsonType;

    // --- SafeDateTime (4 functions) ---

    /// Parse ISO 8601 date string.
    pub fn proven_datetime_parse(ptr: *const u8, len: usize) -> DateTimeResult;

    /// Format DateTime as ISO 8601.
    pub fn proven_datetime_format_iso8601(dt: DateTime) -> StringResult;

    /// Check if year is leap year.
    pub fn proven_datetime_is_leap_year(year: i32) -> bool;

    /// Get days in month.
    pub fn proven_datetime_days_in_month(year: i32, month: u8) -> u8;

    // --- SafeFloat (5 functions) ---

    /// Safe floating-point division.
    pub fn proven_float_div(a: f64, b: f64) -> FloatResult;

    /// Check if float is finite.
    pub fn proven_float_is_finite(x: f64) -> bool;

    /// Check if float is NaN.
    pub fn proven_float_is_nan(x: f64) -> bool;

    /// Safe square root.
    pub fn proven_float_sqrt(x: f64) -> FloatResult;

    /// Safe natural logarithm.
    pub fn proven_float_ln(x: f64) -> FloatResult;

    // --- SafePassword (2 functions) ---

    /// Validate password strength.
    pub fn proven_password_validate(ptr: *const u8, len: usize) -> PasswordResult;

    /// Check if password is common.
    pub fn proven_password_is_common(ptr: *const u8, len: usize) -> bool;

    // --- SafeHex (3 functions) ---

    /// Hex encode bytes to string.
    pub fn proven_hex_encode(ptr: *const u8, len: usize, uppercase: bool) -> StringResult;

    /// Hex decode string to bytes.
    pub fn proven_hex_decode(ptr: *const u8, len: usize) -> HexDecodeResult;

    /// Free hex decode result.
    pub fn proven_hex_free(result: *mut HexDecodeResult);

    // --- SafeCurrency (2 functions) ---

    /// Parse currency amount.
    pub fn proven_currency_parse(ptr: *const u8, len: usize) -> CurrencyResult;

    /// Format currency amount.
    pub fn proven_currency_format(
        amount_minor: i64,
        code: [u8; 3],
        decimal_places: u8,
    ) -> StringResult;

    // --- SafePhone (2 functions) ---

    /// Parse phone number.
    pub fn proven_phone_parse(ptr: *const u8, len: usize) -> PhoneResult;

    /// Format phone number as E.164.
    pub fn proven_phone_format_e164(country_code: u16, national_number: u64) -> StringResult;

    // --- SafeVersion (3 functions) ---

    /// Parse semantic version string.
    pub fn proven_version_parse(ptr: *const u8, len: usize) -> VersionResult;

    /// Compare two semantic versions.
    pub fn proven_version_compare(a: SemanticVersion, b: SemanticVersion) -> i32;

    /// Free version result.
    pub fn proven_version_free(version: *mut SemanticVersion);

    // --- SafeGeo (3 functions) ---

    /// Validate geographic coordinate.
    pub fn proven_geo_validate(lat: f64, lon: f64) -> GeoResult;

    /// Calculate Haversine distance (meters).
    pub fn proven_geo_distance(a: GeoCoordinate, b: GeoCoordinate) -> FloatResult;

    /// Check if coordinate is in bounding box.
    pub fn proven_geo_in_bounds(
        coord: GeoCoordinate,
        min_lat: f64,
        max_lat: f64,
        min_lon: f64,
        max_lon: f64,
    ) -> bool;

    // --- SafeChecksum (2 functions) ---

    /// Calculate CRC32.
    pub fn proven_checksum_crc32(ptr: *const u8, len: usize) -> IntResult;

    /// Verify CRC32 matches expected.
    pub fn proven_checksum_verify_crc32(
        ptr: *const u8,
        len: usize,
        expected: u32,
    ) -> BoolResult;

    // --- SafeProbability (4 functions) ---

    /// Create probability (clamped to [0,1]).
    pub fn proven_probability_create(value: f64) -> f64;

    /// Multiply probabilities (independent events).
    pub fn proven_probability_and(a: f64, b: f64) -> f64;

    /// Add probabilities (mutually exclusive events).
    pub fn proven_probability_or_exclusive(a: f64, b: f64) -> f64;

    /// Complement.
    pub fn proven_probability_not(p: f64) -> f64;

    // --- SafeCalculator (1 function) ---

    /// Evaluate expression.
    pub fn proven_calculator_eval(ptr: *const u8, len: usize) -> FloatResult;

    // --- SafeBuffer (4 functions) ---

    /// Create bounded buffer.
    pub fn proven_buffer_create(capacity: usize) -> BufferResult;

    /// Append to buffer.
    pub fn proven_buffer_append(
        buffer: *mut BoundedBuffer,
        ptr: *const u8,
        len: usize,
    ) -> i32;

    /// Get buffer contents.
    pub fn proven_buffer_get(
        buffer: *mut BoundedBuffer,
        out_ptr: *mut *const u8,
        out_len: *mut usize,
    ) -> i32;

    /// Free buffer.
    pub fn proven_buffer_free(buffer: *mut BoundedBuffer);

    // --- SafeRateLimiter (3 functions) ---

    /// Create rate limiter.
    pub fn proven_rate_limiter_create(capacity: f64, refill_rate: f64) -> *mut RateLimiter;

    /// Try to acquire tokens.
    pub fn proven_rate_limiter_try_acquire(limiter: *mut RateLimiter, tokens: f64) -> bool;

    /// Free rate limiter.
    pub fn proven_rate_limiter_free(limiter: *mut RateLimiter);

    // --- SafeCircuitBreaker (6 functions) ---

    /// Create circuit breaker.
    pub fn proven_circuit_breaker_create(
        failure_threshold: u32,
        success_threshold: u32,
        timeout_ms: i64,
    ) -> *mut CircuitBreaker;

    /// Check if request should be allowed.
    pub fn proven_circuit_breaker_allow(cb: *mut CircuitBreaker) -> bool;

    /// Record success.
    pub fn proven_circuit_breaker_success(cb: *mut CircuitBreaker);

    /// Record failure.
    pub fn proven_circuit_breaker_failure(cb: *mut CircuitBreaker);

    /// Get circuit state.
    pub fn proven_circuit_breaker_state(cb: *mut CircuitBreaker) -> CircuitState;

    /// Free circuit breaker.
    pub fn proven_circuit_breaker_free(cb: *mut CircuitBreaker);

    // --- SafeRetry (2 functions) ---

    /// Calculate delay for attempt.
    pub fn proven_retry_delay(config: RetryConfig, attempt: u32) -> u64;

    /// Check if should retry.
    pub fn proven_retry_should_retry(config: RetryConfig, attempt: u32) -> bool;

    // --- SafeMonotonic (3 functions) ---

    /// Create monotonic counter.
    pub fn proven_monotonic_create(initial: u64, max_value: u64) -> *mut MonotonicCounter;

    /// Get next value.
    pub fn proven_monotonic_next(counter: *mut MonotonicCounter) -> IntResult;

    /// Free counter.
    pub fn proven_monotonic_free(counter: *mut MonotonicCounter);

    // --- SafeStateMachine (5 functions) ---

    /// Create state machine.
    pub fn proven_state_machine_create(
        state_count: u32,
        initial_state: u32,
    ) -> *mut StateMachine;

    /// Allow transition.
    pub fn proven_state_machine_allow(sm: *mut StateMachine, from: u32, to: u32) -> bool;

    /// Try to transition.
    pub fn proven_state_machine_transition(sm: *mut StateMachine, to: u32) -> bool;

    /// Get current state.
    pub fn proven_state_machine_state(sm: *mut StateMachine) -> u32;

    /// Free state machine.
    pub fn proven_state_machine_free(sm: *mut StateMachine);

    // --- SafeTensor (5 functions) ---

    /// Create 2D tensor.
    pub fn proven_tensor_create(rows: usize, cols: usize) -> *mut Tensor2D;

    /// Set tensor value.
    pub fn proven_tensor_set(
        tensor: *mut Tensor2D,
        row: usize,
        col: usize,
        value: f64,
    ) -> i32;

    /// Get tensor value.
    pub fn proven_tensor_get(tensor: *mut Tensor2D, row: usize, col: usize) -> FloatResult;

    /// Matrix multiplication.
    pub fn proven_tensor_matmul(a: *mut Tensor2D, b: *mut Tensor2D) -> *mut Tensor2D;

    /// Free tensor.
    pub fn proven_tensor_free(tensor: *mut Tensor2D);

    // --- SafeML (5 functions) ---

    /// Softmax normalization.
    pub fn proven_ml_softmax(
        input: *const f64,
        output: *mut f64,
        len: usize,
    ) -> i32;

    /// Sigmoid function.
    pub fn proven_ml_sigmoid(x: f64) -> f64;

    /// ReLU function.
    pub fn proven_ml_relu(x: f64) -> f64;

    /// Leaky ReLU.
    pub fn proven_ml_leaky_relu(x: f64, alpha: f64) -> f64;

    /// Clamp value to range.
    pub fn proven_ml_clamp(x: f64, min_val: f64, max_val: f64) -> f64;

    // --- SafeLRU (4 functions) ---

    /// Create LRU cache.
    pub fn proven_lru_create(capacity: usize) -> *mut LRUCache;

    /// Get from LRU cache.
    pub fn proven_lru_get(cache: *mut LRUCache, key: u64) -> IntResult;

    /// Put in LRU cache.
    pub fn proven_lru_put(cache: *mut LRUCache, key: u64, value: i64) -> i32;

    /// Free LRU cache.
    pub fn proven_lru_free(cache: *mut LRUCache);

    // --- SafeGraph (4 functions) ---

    /// Create graph.
    pub fn proven_graph_create(node_count: usize) -> *mut Graph;

    /// Add edge.
    pub fn proven_graph_add_edge(graph: *mut Graph, from: usize, to: usize) -> i32;

    /// Check if edge exists.
    pub fn proven_graph_has_edge(graph: *mut Graph, from: usize, to: usize) -> bool;

    /// Free graph.
    pub fn proven_graph_free(graph: *mut Graph);

    // --- SafeQueue (5 functions) ---

    /// Create bounded queue.
    pub fn proven_queue_create(capacity: usize) -> *mut BoundedQueue;

    /// Push to queue.
    pub fn proven_queue_push(queue: *mut BoundedQueue, value: i64) -> bool;

    /// Pop from queue.
    pub fn proven_queue_pop(queue: *mut BoundedQueue) -> IntResult;

    /// Get queue size.
    pub fn proven_queue_size(queue: *mut BoundedQueue) -> usize;

    /// Free queue.
    pub fn proven_queue_free(queue: *mut BoundedQueue);

    // --- SafeBloom (4 functions) ---

    /// Create bloom filter.
    pub fn proven_bloom_create(
        expected_elements: usize,
        false_positive_rate: f64,
    ) -> *mut BloomFilter;

    /// Add element to bloom filter.
    pub fn proven_bloom_add(filter: *mut BloomFilter, ptr: *const u8, len: usize);

    /// Check if element might be in filter.
    pub fn proven_bloom_contains(filter: *mut BloomFilter, ptr: *const u8, len: usize) -> bool;

    /// Free bloom filter.
    pub fn proven_bloom_free(filter: *mut BloomFilter);

    // --- SafeRegistry (3 functions) ---

    /// Parse OCI image reference.
    pub fn proven_registry_parse(ptr: *const u8, len: usize) -> ImageRefResult;

    /// Convert image reference to string.
    pub fn proven_registry_to_string(reference: *const ImageReference) -> StringResult;

    /// Check if image reference has a registry hostname.
    pub fn proven_registry_has_registry(reference: *const ImageReference) -> BoolResult;

    // --- SafeDigest (3 functions) ---

    /// Parse digest string.
    pub fn proven_digest_parse(ptr: *const u8, len: usize) -> DigestResult;

    /// Constant-time digest comparison.
    pub fn proven_digest_verify(expected: *const Digest, actual: *const Digest) -> BoolResult;

    /// Convert digest to string.
    pub fn proven_digest_to_string(digest: *const Digest) -> StringResult;

    // --- SafeHTTP (3 functions) ---

    /// URL-encode a string.
    pub fn proven_http_url_encode(ptr: *const u8, len: usize) -> StringResult;

    /// URL-decode a string.
    pub fn proven_http_url_decode(ptr: *const u8, len: usize) -> StringResult;

    /// Parse WWW-Authenticate header.
    pub fn proven_http_parse_www_authenticate(
        ptr: *const u8,
        len: usize,
    ) -> AuthChallengeResult;

    // --- SafeColor (3 functions) ---

    /// Parse hex color string.
    pub fn proven_color_parse_hex(ptr: *const u8, len: usize) -> ColorParseResult;

    /// Convert RGB to HSL.
    pub fn proven_color_rgb_to_hsl(rgb: RGBColor) -> HSLColor;

    /// Format RGB as hex string.
    pub fn proven_color_to_hex(rgb: RGBColor) -> StringResult;

    // --- SafeAngle (4 functions) ---

    /// Convert degrees to radians.
    pub fn proven_angle_deg_to_rad(degrees: f64) -> f64;

    /// Convert radians to degrees.
    pub fn proven_angle_rad_to_deg(radians: f64) -> f64;

    /// Normalize angle to 0-360 degrees.
    pub fn proven_angle_normalize_degrees(degrees: f64) -> f64;

    /// Normalize angle to 0-2pi radians.
    pub fn proven_angle_normalize_radians(radians: f64) -> f64;

    // --- SafeUnit (2 functions) ---

    /// Convert length between units.
    pub fn proven_unit_convert_length(value: f64, from: LengthUnit, to: LengthUnit)
        -> FloatResult;

    /// Convert temperature between units.
    pub fn proven_unit_convert_temp(value: f64, from: TempUnit, to: TempUnit) -> FloatResult;

    // --- Callbacks (6 functions) ---

    /// Register a callback for a specific event type.
    pub fn proven_callback_register(
        event_type: ProvenEventType,
        callback: ProvenCallbackFn,
        context: *mut c_void,
    ) -> CallbackHandle;

    /// Unregister a callback by handle.
    pub fn proven_callback_unregister(handle: CallbackHandle) -> i32;

    /// Fire an event.
    pub fn proven_callback_fire(
        event_type: ProvenEventType,
        data_ptr: *const u8,
        data_len: usize,
        code: i32,
    ) -> i32;

    /// Query callback count for an event type.
    pub fn proven_callback_count(event_type: ProvenEventType) -> u32;

    /// Unregister all callbacks.
    pub fn proven_callback_clear_all() -> u32;
}
