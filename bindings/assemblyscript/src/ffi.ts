// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * FFI declarations for libproven host functions.
 *
 * AssemblyScript compiles to WASM, which cannot use dlopen or native FFI.
 * Instead, all libproven functions are declared as host-provided imports
 * using the @external decorator. The WASM embedder (host runtime) must
 * provide these functions by bridging to the actual libproven shared
 * library.
 *
 * Convention for result passing:
 *   - Scalar results (i32, i64, f64, bool) use a result pointer into
 *     WASM linear memory. The host writes the struct at that pointer.
 *   - String results: the host writes (status: i32, ptr: u32, len: u32)
 *     into WASM linear memory. The ptr/len reference a region the host
 *     has allocated in WASM memory via the exported __alloc function.
 *   - Input strings are passed as (ptr, len) pointing into WASM linear
 *     memory. The host reads UTF-8 bytes from that region.
 *
 * Memory layout for result structs written by the host:
 *   IntResult:    [status: i32 @ +0] [value: i64 @ +8]  (16 bytes, aligned)
 *   BoolResult:   [status: i32 @ +0] [value: i32 @ +4]  (8 bytes)
 *   FloatResult:  [status: i32 @ +0] [value: f64 @ +8]  (16 bytes, aligned)
 *   StringResult: [status: i32 @ +0] [ptr: u32 @ +4] [len: u32 @ +8] (12 bytes)
 */

// ============================================================================
// Lifecycle
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_init")
export declare function proven_init(): i32;

// @ts-ignore: decorator
@external("proven", "proven_deinit")
export declare function proven_deinit(): void;

// @ts-ignore: decorator
@external("proven", "proven_is_initialized")
export declare function proven_is_initialized(): i32;

// ============================================================================
// Version Information
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_ffi_abi_version")
export declare function proven_ffi_abi_version(): u32;

// @ts-ignore: decorator
@external("proven", "proven_version_major")
export declare function proven_version_major(): u32;

// @ts-ignore: decorator
@external("proven", "proven_version_minor")
export declare function proven_version_minor(): u32;

// @ts-ignore: decorator
@external("proven", "proven_version_patch")
export declare function proven_version_patch(): u32;

// @ts-ignore: decorator
@external("proven", "proven_module_count")
export declare function proven_module_count(): u32;

// ============================================================================
// Memory Management
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_free_string")
export declare function proven_free_string(ptr: usize): void;

// ============================================================================
// SafeMath (9 functions)
// ============================================================================

/**
 * proven_math_div: writes IntResult at result_ptr.
 * IntResult layout: [status: i32 @ +0] [padding @ +4] [value: i64 @ +8]
 */
// @ts-ignore: decorator
@external("proven", "proven_math_div")
export declare function proven_math_div(
  numerator: i64, denominator: i64, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_math_mod")
export declare function proven_math_mod(
  numerator: i64, denominator: i64, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_math_add_checked")
export declare function proven_math_add_checked(
  a: i64, b: i64, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_math_sub_checked")
export declare function proven_math_sub_checked(
  a: i64, b: i64, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_math_mul_checked")
export declare function proven_math_mul_checked(
  a: i64, b: i64, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_math_abs_safe")
export declare function proven_math_abs_safe(
  n: i64, result_ptr: usize
): void;

/**
 * proven_math_clamp: returns i64 directly (infallible).
 */
// @ts-ignore: decorator
@external("proven", "proven_math_clamp")
export declare function proven_math_clamp(lo: i64, hi: i64, value: i64): i64;

// @ts-ignore: decorator
@external("proven", "proven_math_pow_checked")
export declare function proven_math_pow_checked(
  base: i64, exp: u32, result_ptr: usize
): void;

// ============================================================================
// SafeString (4 functions)
// ============================================================================

/**
 * proven_string_is_valid_utf8: writes BoolResult at result_ptr.
 * BoolResult layout: [status: i32 @ +0] [value: i32 @ +4]
 */
// @ts-ignore: decorator
@external("proven", "proven_string_is_valid_utf8")
export declare function proven_string_is_valid_utf8(
  ptr: usize, len: usize, result_ptr: usize
): void;

/**
 * proven_string_escape_sql: writes StringResult at result_ptr.
 * StringResult layout: [status: i32 @ +0] [ptr: u32 @ +4] [len: u32 @ +8]
 */
// @ts-ignore: decorator
@external("proven", "proven_string_escape_sql")
export declare function proven_string_escape_sql(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_string_escape_html")
export declare function proven_string_escape_html(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_string_escape_js")
export declare function proven_string_escape_js(
  ptr: usize, len: usize, result_ptr: usize
): void;

// ============================================================================
// SafePath (2 functions)
// ============================================================================

/**
 * proven_path_has_traversal: writes BoolResult at result_ptr.
 */
// @ts-ignore: decorator
@external("proven", "proven_path_has_traversal")
export declare function proven_path_has_traversal(
  ptr: usize, len: usize, result_ptr: usize
): void;

/**
 * proven_path_sanitize_filename: writes StringResult at result_ptr.
 */
// @ts-ignore: decorator
@external("proven", "proven_path_sanitize_filename")
export declare function proven_path_sanitize_filename(
  ptr: usize, len: usize, result_ptr: usize
): void;

// ============================================================================
// SafeEmail (1 function)
// ============================================================================

/**
 * proven_email_is_valid: writes BoolResult at result_ptr.
 */
// @ts-ignore: decorator
@external("proven", "proven_email_is_valid")
export declare function proven_email_is_valid(
  ptr: usize, len: usize, result_ptr: usize
): void;

// ============================================================================
// SafeCrypto (2 functions)
// ============================================================================

/**
 * proven_crypto_constant_time_eq: writes BoolResult at result_ptr.
 */
// @ts-ignore: decorator
@external("proven", "proven_crypto_constant_time_eq")
export declare function proven_crypto_constant_time_eq(
  ptr1: usize, len1: usize,
  ptr2: usize, len2: usize,
  result_ptr: usize
): void;

/**
 * proven_crypto_random_bytes: fills buffer at ptr with len random bytes.
 * Returns status code directly.
 */
// @ts-ignore: decorator
@external("proven", "proven_crypto_random_bytes")
export declare function proven_crypto_random_bytes(
  ptr: usize, len: usize
): i32;

// ============================================================================
// SafeUrl (1 function + 1 free)
// ============================================================================

/**
 * proven_url_parse: writes UrlResult at result_ptr.
 * UrlResult layout (packed for WASM):
 *   [status: i32 @ +0]
 *   [scheme_ptr: u32 @ +4]  [scheme_len: u32 @ +8]
 *   [host_ptr: u32 @ +12]   [host_len: u32 @ +16]
 *   [port: u16 @ +20]       [has_port: u8 @ +22]
 *   [path_ptr: u32 @ +24]   [path_len: u32 @ +28]
 *   [query_ptr: u32 @ +32]  [query_len: u32 @ +36]
 *   [fragment_ptr: u32 @ +40] [fragment_len: u32 @ +44]
 * Total: 48 bytes
 */
// @ts-ignore: decorator
@external("proven", "proven_url_parse")
export declare function proven_url_parse(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_url_free")
export declare function proven_url_free(result_ptr: usize): void;

// ============================================================================
// SafeNetwork (3 functions)
// ============================================================================

/**
 * proven_network_parse_ipv4: writes IPv4Result at result_ptr.
 * IPv4Result layout: [status: i32 @ +0] [octets: 4 bytes @ +4]
 */
// @ts-ignore: decorator
@external("proven", "proven_network_parse_ipv4")
export declare function proven_network_parse_ipv4(
  ptr: usize, len: usize, result_ptr: usize
): void;

/**
 * proven_network_ipv4_is_private: takes 4 octets packed as u32.
 */
// @ts-ignore: decorator
@external("proven", "proven_network_ipv4_is_private")
export declare function proven_network_ipv4_is_private(addr_packed: u32): i32;

// @ts-ignore: decorator
@external("proven", "proven_network_ipv4_is_loopback")
export declare function proven_network_ipv4_is_loopback(addr_packed: u32): i32;

// ============================================================================
// SafeHeader (6 functions)
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_header_has_crlf")
export declare function proven_header_has_crlf(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_header_is_valid_name")
export declare function proven_header_is_valid_name(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_header_is_dangerous")
export declare function proven_header_is_dangerous(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_header_render")
export declare function proven_header_render(
  name_ptr: usize, name_len: usize,
  value_ptr: usize, value_len: usize,
  result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_header_build_csp")
export declare function proven_header_build_csp(
  directives_json_ptr: usize, json_len: usize,
  result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_header_build_hsts")
export declare function proven_header_build_hsts(
  max_age: i64, include_subdomains: i32, preload: i32,
  result_ptr: usize
): void;

// ============================================================================
// SafeCookie (6 functions)
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_cookie_has_injection")
export declare function proven_cookie_has_injection(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_cookie_validate_name")
export declare function proven_cookie_validate_name(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_cookie_validate_value")
export declare function proven_cookie_validate_value(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_cookie_get_prefix")
export declare function proven_cookie_get_prefix(
  ptr: usize, len: usize, result_ptr: usize
): void;

/**
 * proven_cookie_build_set_cookie: complex struct input.
 * Cookie attributes are passed as individual parameters since WASM cannot
 * pass C structs directly. The host reconstructs ProvenCookieAttributes.
 */
// @ts-ignore: decorator
@external("proven", "proven_cookie_build_set_cookie")
export declare function proven_cookie_build_set_cookie(
  name_ptr: usize, name_len: usize,
  value_ptr: usize, value_len: usize,
  domain_ptr: usize, domain_len: usize,
  path_ptr: usize, path_len: usize,
  max_age: i64, secure: i32, http_only: i32,
  same_site: i32, partitioned: i32,
  result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_cookie_build_delete")
export declare function proven_cookie_build_delete(
  name_ptr: usize, name_len: usize,
  result_ptr: usize
): void;

// ============================================================================
// SafeContentType (6 functions)
// ============================================================================

/**
 * proven_content_type_parse: writes ContentTypeResult at result_ptr.
 * ContentTypeResult layout (packed for WASM):
 *   [status: i32 @ +0]
 *   [media_type_ptr: u32 @ +4]  [media_type_len: u32 @ +8]
 *   [subtype_ptr: u32 @ +12]    [subtype_len: u32 @ +16]
 *   [suffix_ptr: u32 @ +20]     [suffix_len: u32 @ +24]
 *   [category: i32 @ +28]
 *   [charset: i32 @ +32]
 *   [has_charset: i32 @ +36]
 * Total: 40 bytes
 */
// @ts-ignore: decorator
@external("proven", "proven_content_type_parse")
export declare function proven_content_type_parse(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_content_type_free")
export declare function proven_content_type_free(result_ptr: usize): void;

// @ts-ignore: decorator
@external("proven", "proven_content_type_can_sniff_dangerous")
export declare function proven_content_type_can_sniff_dangerous(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_content_type_render")
export declare function proven_content_type_render(
  type_ptr: usize, type_len: usize,
  subtype_ptr: usize, subtype_len: usize,
  suffix_ptr: usize, suffix_len: usize,
  charset: i32, has_charset: i32,
  result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_content_type_is_json")
export declare function proven_content_type_is_json(
  subtype_ptr: usize, subtype_len: usize,
  suffix_ptr: usize, suffix_len: usize,
  result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_content_type_is_xml")
export declare function proven_content_type_is_xml(
  subtype_ptr: usize, subtype_len: usize,
  suffix_ptr: usize, suffix_len: usize,
  result_ptr: usize
): void;

// ============================================================================
// SafeUUID (5 functions)
// ============================================================================

/**
 * proven_uuid_v4: writes UUIDResult at result_ptr.
 * UUIDResult layout: [status: i32 @ +0] [bytes: 16 bytes @ +4]
 * Total: 20 bytes
 */
// @ts-ignore: decorator
@external("proven", "proven_uuid_v4")
export declare function proven_uuid_v4(result_ptr: usize): void;

// @ts-ignore: decorator
@external("proven", "proven_uuid_to_string")
export declare function proven_uuid_to_string(
  uuid_ptr: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_uuid_parse")
export declare function proven_uuid_parse(
  ptr: usize, len: usize, result_ptr: usize
): void;

/**
 * proven_uuid_is_nil: uuid_ptr points to 16 bytes.
 */
// @ts-ignore: decorator
@external("proven", "proven_uuid_is_nil")
export declare function proven_uuid_is_nil(uuid_ptr: usize): i32;

// @ts-ignore: decorator
@external("proven", "proven_uuid_version")
export declare function proven_uuid_version(uuid_ptr: usize): u32;

// ============================================================================
// SafeJson (2 functions)
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_json_is_valid")
export declare function proven_json_is_valid(
  ptr: usize, len: usize, result_ptr: usize
): void;

/**
 * proven_json_get_type: returns JsonType directly as i32.
 */
// @ts-ignore: decorator
@external("proven", "proven_json_get_type")
export declare function proven_json_get_type(ptr: usize, len: usize): i32;

// ============================================================================
// SafeDateTime (4 functions)
// ============================================================================

/**
 * proven_datetime_parse: writes DateTimeResult at result_ptr.
 * DateTimeResult layout:
 *   [status: i32 @ +0]
 *   [year: i32 @ +4]  [month: u8 @ +8]  [day: u8 @ +9]
 *   [hour: u8 @ +10]  [minute: u8 @ +11]  [second: u8 @ +12]
 *   [nanosecond: u32 @ +16]
 *   [tz_offset_minutes: i16 @ +20]
 * Total: 24 bytes (with alignment padding)
 */
// @ts-ignore: decorator
@external("proven", "proven_datetime_parse")
export declare function proven_datetime_parse(
  ptr: usize, len: usize, result_ptr: usize
): void;

/**
 * proven_datetime_format_iso8601: takes datetime fields as flat parameters.
 * The host reconstructs the ProvenDateTime struct.
 */
// @ts-ignore: decorator
@external("proven", "proven_datetime_format_iso8601")
export declare function proven_datetime_format_iso8601(
  year: i32, month: u32, day: u32,
  hour: u32, minute: u32, second: u32,
  nanosecond: u32, tz_offset_minutes: i32,
  result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_datetime_is_leap_year")
export declare function proven_datetime_is_leap_year(year: i32): i32;

// @ts-ignore: decorator
@external("proven", "proven_datetime_days_in_month")
export declare function proven_datetime_days_in_month(year: i32, month: u32): u32;

// ============================================================================
// SafeFloat (5 functions)
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_float_div")
export declare function proven_float_div(
  a: f64, b: f64, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_float_is_finite")
export declare function proven_float_is_finite(x: f64): i32;

// @ts-ignore: decorator
@external("proven", "proven_float_is_nan")
export declare function proven_float_is_nan(x: f64): i32;

// @ts-ignore: decorator
@external("proven", "proven_float_sqrt")
export declare function proven_float_sqrt(x: f64, result_ptr: usize): void;

// @ts-ignore: decorator
@external("proven", "proven_float_ln")
export declare function proven_float_ln(x: f64, result_ptr: usize): void;

// ============================================================================
// SafePassword (2 functions)
// ============================================================================

/**
 * proven_password_validate: writes PasswordResult at result_ptr.
 * PasswordResult layout:
 *   [strength: i32 @ +0]
 *   [has_lowercase: i32 @ +4]  [has_uppercase: i32 @ +8]
 *   [has_digit: i32 @ +12]     [has_special: i32 @ +16]
 *   [length: u32 @ +20]
 * Total: 24 bytes
 */
// @ts-ignore: decorator
@external("proven", "proven_password_validate")
export declare function proven_password_validate(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_password_is_common")
export declare function proven_password_is_common(ptr: usize, len: usize): i32;

// ============================================================================
// SafeHex (3 functions)
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_hex_encode")
export declare function proven_hex_encode(
  ptr: usize, len: usize, uppercase: i32, result_ptr: usize
): void;

/**
 * proven_hex_decode: writes HexDecodeResult at result_ptr.
 * HexDecodeResult layout: [status: i32 @ +0] [data_ptr: u32 @ +4] [len: u32 @ +8]
 */
// @ts-ignore: decorator
@external("proven", "proven_hex_decode")
export declare function proven_hex_decode(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_hex_free")
export declare function proven_hex_free(result_ptr: usize): void;

// ============================================================================
// SafeCurrency (2 functions)
// ============================================================================

/**
 * proven_currency_parse: writes CurrencyResult at result_ptr.
 * CurrencyResult layout:
 *   [status: i32 @ +0]  [padding: 4 bytes]
 *   [amount_minor: i64 @ +8]
 *   [currency_code: 3 bytes @ +16]  [decimal_places: u8 @ +19]
 * Total: 20 bytes (padded to 24)
 */
// @ts-ignore: decorator
@external("proven", "proven_currency_parse")
export declare function proven_currency_parse(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_currency_format")
export declare function proven_currency_format(
  amount_minor: i64,
  code_ptr: usize,
  decimal_places: u32,
  result_ptr: usize
): void;

// ============================================================================
// SafePhone (2 functions)
// ============================================================================

/**
 * proven_phone_parse: writes PhoneResult at result_ptr.
 * PhoneResult layout:
 *   [status: i32 @ +0]
 *   [country_code: u16 @ +4]  [padding: 2 bytes]
 *   [national_number: u64 @ +8]
 *   [is_valid: i32 @ +16]
 * Total: 20 bytes (padded to 24)
 */
// @ts-ignore: decorator
@external("proven", "proven_phone_parse")
export declare function proven_phone_parse(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_phone_format_e164")
export declare function proven_phone_format_e164(
  country_code: u32, national_number: u64,
  result_ptr: usize
): void;

// ============================================================================
// SafeVersion (3 functions)
// ============================================================================

/**
 * proven_version_parse: writes VersionResult at result_ptr.
 * VersionResult layout:
 *   [status: i32 @ +0]
 *   [major: u32 @ +4]  [minor: u32 @ +8]  [patch: u32 @ +12]
 *   [prerelease_ptr: u32 @ +16]  [prerelease_len: u32 @ +20]
 * Total: 24 bytes
 */
// @ts-ignore: decorator
@external("proven", "proven_version_parse")
export declare function proven_version_parse(
  ptr: usize, len: usize, result_ptr: usize
): void;

/**
 * proven_version_compare: takes flattened version fields.
 */
// @ts-ignore: decorator
@external("proven", "proven_version_compare")
export declare function proven_version_compare(
  a_major: u32, a_minor: u32, a_patch: u32,
  a_prerelease_ptr: usize, a_prerelease_len: usize,
  b_major: u32, b_minor: u32, b_patch: u32,
  b_prerelease_ptr: usize, b_prerelease_len: usize
): i32;

// @ts-ignore: decorator
@external("proven", "proven_version_free")
export declare function proven_version_free(result_ptr: usize): void;

// ============================================================================
// SafeGeo (3 functions)
// ============================================================================

/**
 * proven_geo_validate: writes GeoResult at result_ptr.
 * GeoResult layout:
 *   [status: i32 @ +0]  [padding: 4 bytes]
 *   [latitude: f64 @ +8]  [longitude: f64 @ +16]
 * Total: 24 bytes
 */
// @ts-ignore: decorator
@external("proven", "proven_geo_validate")
export declare function proven_geo_validate(
  lat: f64, lon: f64, result_ptr: usize
): void;

/**
 * proven_geo_distance: writes FloatResult at result_ptr.
 */
// @ts-ignore: decorator
@external("proven", "proven_geo_distance")
export declare function proven_geo_distance(
  a_lat: f64, a_lon: f64,
  b_lat: f64, b_lon: f64,
  result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_geo_in_bounds")
export declare function proven_geo_in_bounds(
  lat: f64, lon: f64,
  min_lat: f64, max_lat: f64,
  min_lon: f64, max_lon: f64
): i32;

// ============================================================================
// SafeChecksum (2 functions)
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_checksum_crc32")
export declare function proven_checksum_crc32(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_checksum_verify_crc32")
export declare function proven_checksum_verify_crc32(
  ptr: usize, len: usize, expected: u32, result_ptr: usize
): void;

// ============================================================================
// SafeProbability (4 functions)
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_probability_create")
export declare function proven_probability_create(value: f64): f64;

// @ts-ignore: decorator
@external("proven", "proven_probability_and")
export declare function proven_probability_and(a: f64, b: f64): f64;

// @ts-ignore: decorator
@external("proven", "proven_probability_or_exclusive")
export declare function proven_probability_or_exclusive(a: f64, b: f64): f64;

// @ts-ignore: decorator
@external("proven", "proven_probability_not")
export declare function proven_probability_not(p: f64): f64;

// ============================================================================
// SafeCalculator (1 function)
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_calculator_eval")
export declare function proven_calculator_eval(
  ptr: usize, len: usize, result_ptr: usize
): void;

// ============================================================================
// SafeColor (3 functions)
// ============================================================================

/**
 * proven_color_parse_hex: writes ColorResult at result_ptr.
 * ColorResult layout: [status: i32 @ +0] [r: u8 @ +4] [g: u8 @ +5] [b: u8 @ +6]
 * Total: 8 bytes
 */
// @ts-ignore: decorator
@external("proven", "proven_color_parse_hex")
export declare function proven_color_parse_hex(
  ptr: usize, len: usize, result_ptr: usize
): void;

/**
 * proven_color_rgb_to_hsl: writes HSL at result_ptr.
 * HSL layout: [h: f64 @ +0] [s: f64 @ +8] [l: f64 @ +16]
 * Total: 24 bytes
 */
// @ts-ignore: decorator
@external("proven", "proven_color_rgb_to_hsl")
export declare function proven_color_rgb_to_hsl(
  r: u32, g: u32, b: u32, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_color_to_hex")
export declare function proven_color_to_hex(
  r: u32, g: u32, b: u32, result_ptr: usize
): void;

// ============================================================================
// SafeAngle (4 functions)
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_angle_deg_to_rad")
export declare function proven_angle_deg_to_rad(degrees: f64): f64;

// @ts-ignore: decorator
@external("proven", "proven_angle_rad_to_deg")
export declare function proven_angle_rad_to_deg(radians: f64): f64;

// @ts-ignore: decorator
@external("proven", "proven_angle_normalize_degrees")
export declare function proven_angle_normalize_degrees(degrees: f64): f64;

// @ts-ignore: decorator
@external("proven", "proven_angle_normalize_radians")
export declare function proven_angle_normalize_radians(radians: f64): f64;

// ============================================================================
// SafeUnit (2 functions)
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_unit_convert_length")
export declare function proven_unit_convert_length(
  value: f64, from: i32, to: i32, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_unit_convert_temp")
export declare function proven_unit_convert_temp(
  value: f64, from: i32, to: i32, result_ptr: usize
): void;

// ============================================================================
// SafeHttp (3 functions)
// ============================================================================

// @ts-ignore: decorator
@external("proven", "proven_http_url_encode")
export declare function proven_http_url_encode(
  ptr: usize, len: usize, result_ptr: usize
): void;

// @ts-ignore: decorator
@external("proven", "proven_http_url_decode")
export declare function proven_http_url_decode(
  ptr: usize, len: usize, result_ptr: usize
): void;

/**
 * proven_http_parse_www_authenticate: writes AuthChallengeResult at result_ptr.
 * AuthChallengeResult layout:
 *   [status: i32 @ +0]
 *   [scheme_ptr: u32 @ +4]   [scheme_len: u32 @ +8]
 *   [realm_ptr: u32 @ +12]   [realm_len: u32 @ +16]
 *   [service_ptr: u32 @ +20] [service_len: u32 @ +24]
 *   [scope_ptr: u32 @ +28]   [scope_len: u32 @ +32]
 * Total: 36 bytes
 */
// @ts-ignore: decorator
@external("proven", "proven_http_parse_www_authenticate")
export declare function proven_http_parse_www_authenticate(
  ptr: usize, len: usize, result_ptr: usize
): void;

// ============================================================================
// SafeMl (5 functions)
// ============================================================================

/**
 * proven_ml_softmax: operates on arrays in WASM linear memory.
 */
// @ts-ignore: decorator
@external("proven", "proven_ml_softmax")
export declare function proven_ml_softmax(
  input_ptr: usize, output_ptr: usize, len: usize
): i32;

// @ts-ignore: decorator
@external("proven", "proven_ml_sigmoid")
export declare function proven_ml_sigmoid(x: f64): f64;

// @ts-ignore: decorator
@external("proven", "proven_ml_relu")
export declare function proven_ml_relu(x: f64): f64;

// @ts-ignore: decorator
@external("proven", "proven_ml_leaky_relu")
export declare function proven_ml_leaky_relu(x: f64, alpha: f64): f64;

// @ts-ignore: decorator
@external("proven", "proven_ml_clamp")
export declare function proven_ml_clamp(x: f64, min_val: f64, max_val: f64): f64;
