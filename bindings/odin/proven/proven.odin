// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven Odin Bindings - Thin FFI wrapper around libproven.
//
// All computation is performed in formally verified Idris 2 code via Zig FFI.
// This module contains ONLY data marshalling and foreign declarations; NO
// reimplemented logic.

package proven

import "core:c"
import "core:strings"

// ============================================================================
// Foreign library linkage
// ============================================================================

when ODIN_OS == .Linux {
    foreign import proven_lib {
        "system:proven",
    }
} else when ODIN_OS == .Darwin {
    foreign import proven_lib {
        "system:proven",
    }
} else when ODIN_OS == .Windows {
    foreign import proven_lib {
        "system:proven.lib",
    }
}

// ============================================================================
// Type definitions matching proven.h
// ============================================================================

// Status codes
Proven_Status :: enum c.int32_t {
    OK                   =   0,
    ERR_NULL_POINTER     =  -1,
    ERR_INVALID_ARGUMENT =  -2,
    ERR_OVERFLOW         =  -3,
    ERR_UNDERFLOW        =  -4,
    ERR_DIVISION_BY_ZERO =  -5,
    ERR_PARSE_FAILURE    =  -6,
    ERR_VALIDATION_FAILED=  -7,
    ERR_OUT_OF_BOUNDS    =  -8,
    ERR_ENCODING_ERROR   =  -9,
    ERR_ALLOCATION_FAILED= -10,
    ERR_NOT_IMPLEMENTED  = -99,
}

// Core result types
Int_Result :: struct {
    status: c.int32_t,
    value:  c.int64_t,
}

Bool_Result :: struct {
    status: c.int32_t,
    value:  c.bool,
}

String_Result :: struct {
    status: c.int32_t,
    value:  [^]u8,
    length: c.size_t,
}

Float_Result :: struct {
    status: c.int32_t,
    value:  c.double,
}

// URL types
Url_Components :: struct {
    scheme:       [^]u8, scheme_len:   c.size_t,
    host:         [^]u8, host_len:     c.size_t,
    port:         c.uint16_t, has_port: c.bool,
    path:         [^]u8, path_len:     c.size_t,
    query:        [^]u8, query_len:    c.size_t,
    fragment:     [^]u8, fragment_len: c.size_t,
}

Url_Result :: struct {
    status:     c.int32_t,
    components: Url_Components,
}

// IPv4
IPv4_Address :: struct {
    octets: [4]u8,
}

IPv4_Result :: struct {
    status:  c.int32_t,
    address: IPv4_Address,
}

// Content type
Media_Category :: enum c.int32_t {
    TEXT = 0, IMAGE, AUDIO, VIDEO, APPLICATION, MULTIPART, MESSAGE, FONT, MODEL, CUSTOM,
}

Charset :: enum c.int32_t {
    UTF8 = 0, UTF16LE, UTF16BE, ISO8859_1, ASCII, WINDOWS1252, OTHER,
}

Content_Type_Result :: struct {
    status:         c.int32_t,
    media_type:     [^]u8, media_type_len: c.size_t,
    subtype:        [^]u8, subtype_len:    c.size_t,
    suffix:         [^]u8, suffix_len:     c.size_t,
    category:       Media_Category,
    charset:        Charset,
    has_charset:    c.bool,
}

// UUID
UUID :: struct {
    bytes: [16]u8,
}

UUID_Result :: struct {
    status: c.int32_t,
    uuid:   UUID,
}

// DateTime
Date_Time :: struct {
    year:              c.int32_t,
    month:             u8,
    day:               u8,
    hour:              u8,
    minute:            u8,
    second:            u8,
    nanosecond:        c.uint32_t,
    tz_offset_minutes: c.int16_t,
}

Date_Time_Result :: struct {
    status:   c.int32_t,
    datetime: Date_Time,
}

// Password
Password_Strength :: enum c.int32_t {
    VERY_WEAK = 0, WEAK, FAIR, STRONG, VERY_STRONG,
}

Password_Result :: struct {
    strength:      Password_Strength,
    has_lowercase: c.bool,
    has_uppercase: c.bool,
    has_digit:     c.bool,
    has_special:   c.bool,
    length:        c.size_t,
}

// Hex decode
Hex_Decode_Result :: struct {
    status: c.int32_t,
    data:   [^]u8,
    length: c.size_t,
}

// Currency
Currency_Result :: struct {
    status:         c.int32_t,
    amount_minor:   c.int64_t,
    currency_code:  [3]u8,
    decimal_places: u8,
}

// Phone
Phone_Result :: struct {
    status:          c.int32_t,
    country_code:    c.uint16_t,
    national_number: c.uint64_t,
    is_valid:        c.bool,
}

// Version
Semantic_Version :: struct {
    major:          c.uint32_t,
    minor:          c.uint32_t,
    patch:          c.uint32_t,
    prerelease_len: c.size_t,
    prerelease:     [^]u8,
}

Version_Result :: struct {
    status:  c.int32_t,
    version: Semantic_Version,
}

// Geo
Geo_Coordinate :: struct {
    latitude:  c.double,
    longitude: c.double,
}

Geo_Result :: struct {
    status:     c.int32_t,
    coordinate: Geo_Coordinate,
}

// Cookie
Same_Site :: enum c.int32_t {
    STRICT = 0, LAX, NONE,
}

Cookie_Attributes :: struct {
    domain:      [^]u8, domain_len: c.size_t,
    path:        [^]u8, path_len:   c.size_t,
    max_age:     c.int64_t,
    secure:      c.bool,
    http_only:   c.bool,
    same_site:   Same_Site,
    partitioned: c.bool,
}

// Bounded buffer
Bounded_Buffer :: struct {
    data:     [^]u8,
    capacity: c.size_t,
    length:   c.size_t,
}

Buffer_Result :: struct {
    status: c.int32_t,
    buffer: ^Bounded_Buffer,
}

// Rate limiter
Rate_Limiter :: struct {
    tokens:      c.double,
    capacity:    c.double,
    refill_rate: c.double,
    last_refill: c.int64_t,
}

// Circuit breaker
Circuit_State :: enum c.int32_t {
    CLOSED = 0, OPEN, HALF_OPEN,
}

Circuit_Breaker :: struct {
    state:             Circuit_State,
    failure_count:     c.uint32_t,
    failure_threshold: c.uint32_t,
    success_count:     c.uint32_t,
    success_threshold: c.uint32_t,
    last_failure:      c.int64_t,
    timeout_ms:        c.int64_t,
}

// Retry config
Retry_Config :: struct {
    max_attempts:  c.uint32_t,
    base_delay_ms: c.uint64_t,
    max_delay_ms:  c.uint64_t,
    multiplier:    c.double,
}

// Monotonic counter
Monotonic_Counter :: struct {
    value:     c.uint64_t,
    max_value: c.uint64_t,
}

// State machine
State_Machine :: struct {
    current_state: c.uint32_t,
    state_count:   c.uint32_t,
    transitions:   [^]u8,
}

// Tensor
Tensor_2D :: struct {
    data: [^]c.double,
    rows: c.size_t,
    cols: c.size_t,
}

// Queue
Bounded_Queue :: struct {
    data:     [^]c.int64_t,
    capacity: c.size_t,
    head:     c.size_t,
    tail:     c.size_t,
    count:    c.size_t,
}

// Bloom filter
Bloom_Filter :: struct {
    bits:       [^]u8,
    bit_count:  c.size_t,
    hash_count: c.uint32_t,
}

// LRU cache
LRU_Entry :: struct {
    key:   c.uint64_t,
    value: c.int64_t,
    prev:  c.size_t,
    next:  c.size_t,
    valid: c.bool,
}

LRU_Cache :: struct {
    entries:  [^]LRU_Entry,
    capacity: c.size_t,
    head:     c.size_t,
    tail:     c.size_t,
    count:    c.size_t,
}

// Graph
Graph :: struct {
    edges:      [^]u8,
    node_count: c.size_t,
}

// Registry
Image_Reference :: struct {
    registry:       [^]u8, registry_len:   c.size_t,
    repository:     [^]u8, repository_len: c.size_t,
    tag:            [^]u8, tag_len:        c.size_t,
    digest:         [^]u8, digest_len:     c.size_t,
}

Image_Ref_Result :: struct {
    status:    c.int32_t,
    reference: Image_Reference,
}

// Digest
Hash_Algorithm :: enum c.int32_t {
    SHA256 = 0, SHA384, SHA512, BLAKE3,
}

Digest :: struct {
    algorithm: Hash_Algorithm,
    value:     [^]u8,
    value_len: c.size_t,
}

Digest_Result :: struct {
    status: c.int32_t,
    digest: Digest,
}

// HTTP auth
Auth_Challenge :: struct {
    scheme:  [^]u8, scheme_len:  c.size_t,
    realm:   [^]u8, realm_len:   c.size_t,
    service: [^]u8, service_len: c.size_t,
    scope:   [^]u8, scope_len:   c.size_t,
}

Auth_Challenge_Result :: struct {
    status:    c.int32_t,
    challenge: Auth_Challenge,
}

// Color
RGB_Color :: struct {
    r: u8, g: u8, b: u8,
}

HSL_Color :: struct {
    h: c.double,
    s: c.double,
    l: c.double,
}

Color_Result :: struct {
    status: c.int32_t,
    color:  RGB_Color,
}

// Length unit
Length_Unit :: enum c.int32_t {
    METERS = 0, KILOMETERS, CENTIMETERS, MILLIMETERS, FEET, INCHES, MILES, YARDS,
}

// Temp unit
Temp_Unit :: enum c.int32_t {
    CELSIUS = 0, FAHRENHEIT, KELVIN,
}

// JSON type
Json_Type :: enum c.int32_t {
    NULL    =  0,
    BOOL    =  1,
    NUMBER  =  2,
    STRING  =  3,
    ARRAY   =  4,
    OBJECT  =  5,
    INVALID = -1,
}

// Event types
Event_Type :: enum c.int32_t {
    VALIDATION_FAILED    = 1,
    RESOURCE_ACQUIRED    = 2,
    RESOURCE_RELEASED    = 3,
    RATE_LIMIT_HIT       = 4,
    CIRCUIT_STATE_CHANGE = 5,
    SIGNAL_RECEIVED      = 6,
    RETRY_ATTEMPT        = 7,
    CRYPTO_OPERATION     = 8,
    CUSTOM               = 100,
}

Event :: struct {
    event_type:   Event_Type,
    data_ptr:     [^]u8,
    data_len:     c.size_t,
    timestamp_us: c.uint64_t,
    code:         c.int32_t,
}

Callback_Fn :: #type proc "c" (context: rawptr, event: ^Event) -> c.int32_t

Callback_Handle :: c.uint32_t

// ============================================================================
// Foreign function declarations - every exported symbol from libproven
// ============================================================================

@(default_calling_convention = "c")
foreign proven_lib {
    // Memory management
    proven_free_string :: proc(ptr: [^]u8) ---

    // Lifecycle
    proven_init :: proc() -> c.int32_t ---
    proven_deinit :: proc() ---
    proven_is_initialized :: proc() -> c.bool ---

    // Version
    proven_ffi_abi_version :: proc() -> c.uint32_t ---
    proven_version_major :: proc() -> c.uint32_t ---
    proven_version_minor :: proc() -> c.uint32_t ---
    proven_version_patch :: proc() -> c.uint32_t ---
    proven_module_count :: proc() -> c.uint32_t ---

    // SafeMath
    proven_math_div :: proc(numerator, denominator: c.int64_t) -> Int_Result ---
    proven_math_mod :: proc(numerator, denominator: c.int64_t) -> Int_Result ---
    proven_math_add_checked :: proc(a, b: c.int64_t) -> Int_Result ---
    proven_math_sub_checked :: proc(a, b: c.int64_t) -> Int_Result ---
    proven_math_mul_checked :: proc(a, b: c.int64_t) -> Int_Result ---
    proven_math_abs_safe :: proc(n: c.int64_t) -> Int_Result ---
    proven_math_clamp :: proc(lo, hi, value: c.int64_t) -> c.int64_t ---
    proven_math_pow_checked :: proc(base: c.int64_t, exp: c.uint32_t) -> Int_Result ---

    // SafeString
    proven_string_is_valid_utf8 :: proc(ptr: [^]u8, len: c.size_t) -> Bool_Result ---
    proven_string_escape_sql :: proc(ptr: [^]u8, len: c.size_t) -> String_Result ---
    proven_string_escape_html :: proc(ptr: [^]u8, len: c.size_t) -> String_Result ---
    proven_string_escape_js :: proc(ptr: [^]u8, len: c.size_t) -> String_Result ---

    // SafePath
    proven_path_has_traversal :: proc(ptr: [^]u8, len: c.size_t) -> Bool_Result ---
    proven_path_sanitize_filename :: proc(ptr: [^]u8, len: c.size_t) -> String_Result ---

    // SafeCrypto
    proven_crypto_constant_time_eq :: proc(ptr1: [^]u8, len1: c.size_t, ptr2: [^]u8, len2: c.size_t) -> Bool_Result ---
    proven_crypto_random_bytes :: proc(ptr: [^]u8, len: c.size_t) -> c.int32_t ---

    // SafeUrl
    proven_url_parse :: proc(ptr: [^]u8, len: c.size_t) -> Url_Result ---
    proven_url_free :: proc(components: ^Url_Components) ---

    // SafeEmail
    proven_email_is_valid :: proc(ptr: [^]u8, len: c.size_t) -> Bool_Result ---

    // SafeNetwork
    proven_network_parse_ipv4 :: proc(ptr: [^]u8, len: c.size_t) -> IPv4_Result ---
    proven_network_ipv4_is_private :: proc(addr: IPv4_Address) -> c.bool ---
    proven_network_ipv4_is_loopback :: proc(addr: IPv4_Address) -> c.bool ---

    // SafeHeader
    proven_header_has_crlf :: proc(ptr: [^]u8, len: c.size_t) -> Bool_Result ---
    proven_header_is_valid_name :: proc(ptr: [^]u8, len: c.size_t) -> Bool_Result ---
    proven_header_is_dangerous :: proc(ptr: [^]u8, len: c.size_t) -> Bool_Result ---
    proven_header_render :: proc(name_ptr: [^]u8, name_len: c.size_t, value_ptr: [^]u8, value_len: c.size_t) -> String_Result ---
    proven_header_build_csp :: proc(directives_json: [^]u8, json_len: c.size_t) -> String_Result ---
    proven_header_build_hsts :: proc(max_age: c.int64_t, include_subdomains: c.bool, preload: c.bool) -> String_Result ---

    // SafeCookie
    proven_cookie_has_injection :: proc(ptr: [^]u8, len: c.size_t) -> Bool_Result ---
    proven_cookie_validate_name :: proc(ptr: [^]u8, len: c.size_t) -> Bool_Result ---
    proven_cookie_validate_value :: proc(ptr: [^]u8, len: c.size_t) -> Bool_Result ---
    proven_cookie_get_prefix :: proc(ptr: [^]u8, len: c.size_t) -> Int_Result ---
    proven_cookie_build_set_cookie :: proc(name_ptr: [^]u8, name_len: c.size_t, value_ptr: [^]u8, value_len: c.size_t, attrs: Cookie_Attributes) -> String_Result ---
    proven_cookie_build_delete :: proc(name_ptr: [^]u8, name_len: c.size_t) -> String_Result ---

    // SafeContentType
    proven_content_type_parse :: proc(ptr: [^]u8, len: c.size_t) -> Content_Type_Result ---
    proven_content_type_free :: proc(result: ^Content_Type_Result) ---
    proven_content_type_can_sniff_dangerous :: proc(ptr: [^]u8, len: c.size_t) -> Bool_Result ---
    proven_content_type_render :: proc(type_ptr: [^]u8, type_len: c.size_t, subtype_ptr: [^]u8, subtype_len: c.size_t, suffix_ptr: [^]u8, suffix_len: c.size_t, charset: Charset, has_charset: c.bool) -> String_Result ---
    proven_content_type_is_json :: proc(subtype_ptr: [^]u8, subtype_len: c.size_t, suffix_ptr: [^]u8, suffix_len: c.size_t) -> Bool_Result ---
    proven_content_type_is_xml :: proc(subtype_ptr: [^]u8, subtype_len: c.size_t, suffix_ptr: [^]u8, suffix_len: c.size_t) -> Bool_Result ---

    // SafeUUID
    proven_uuid_v4 :: proc() -> UUID_Result ---
    proven_uuid_to_string :: proc(uuid: UUID) -> String_Result ---
    proven_uuid_parse :: proc(ptr: [^]u8, len: c.size_t) -> UUID_Result ---
    proven_uuid_is_nil :: proc(uuid: UUID) -> c.bool ---
    proven_uuid_version :: proc(uuid: UUID) -> u8 ---

    // SafeJson
    proven_json_is_valid :: proc(ptr: [^]u8, len: c.size_t) -> Bool_Result ---
    proven_json_get_type :: proc(ptr: [^]u8, len: c.size_t) -> Json_Type ---

    // SafeDateTime
    proven_datetime_parse :: proc(ptr: [^]u8, len: c.size_t) -> Date_Time_Result ---
    proven_datetime_format_iso8601 :: proc(dt: Date_Time) -> String_Result ---
    proven_datetime_is_leap_year :: proc(year: c.int32_t) -> c.bool ---
    proven_datetime_days_in_month :: proc(year: c.int32_t, month: u8) -> u8 ---

    // SafeFloat
    proven_float_div :: proc(a, b: c.double) -> Float_Result ---
    proven_float_is_finite :: proc(x: c.double) -> c.bool ---
    proven_float_is_nan :: proc(x: c.double) -> c.bool ---
    proven_float_sqrt :: proc(x: c.double) -> Float_Result ---
    proven_float_ln :: proc(x: c.double) -> Float_Result ---

    // SafePassword
    proven_password_validate :: proc(ptr: [^]u8, len: c.size_t) -> Password_Result ---
    proven_password_is_common :: proc(ptr: [^]u8, len: c.size_t) -> c.bool ---

    // SafeHex
    proven_hex_encode :: proc(ptr: [^]u8, len: c.size_t, uppercase: c.bool) -> String_Result ---
    proven_hex_decode :: proc(ptr: [^]u8, len: c.size_t) -> Hex_Decode_Result ---
    proven_hex_free :: proc(result: ^Hex_Decode_Result) ---

    // SafeCurrency
    proven_currency_parse :: proc(ptr: [^]u8, len: c.size_t) -> Currency_Result ---
    proven_currency_format :: proc(amount_minor: c.int64_t, code: [^]u8, decimal_places: u8) -> String_Result ---

    // SafePhone
    proven_phone_parse :: proc(ptr: [^]u8, len: c.size_t) -> Phone_Result ---
    proven_phone_format_e164 :: proc(country_code: c.uint16_t, national_number: c.uint64_t) -> String_Result ---

    // SafeVersion
    proven_version_parse :: proc(ptr: [^]u8, len: c.size_t) -> Version_Result ---
    proven_version_compare :: proc(a, b: Semantic_Version) -> c.int32_t ---
    proven_version_free :: proc(version: ^Semantic_Version) ---

    // SafeGeo
    proven_geo_validate :: proc(lat, lon: c.double) -> Geo_Result ---
    proven_geo_distance :: proc(a, b: Geo_Coordinate) -> Float_Result ---
    proven_geo_in_bounds :: proc(coord: Geo_Coordinate, min_lat, max_lat, min_lon, max_lon: c.double) -> c.bool ---

    // SafeChecksum
    proven_checksum_crc32 :: proc(ptr: [^]u8, len: c.size_t) -> Int_Result ---
    proven_checksum_verify_crc32 :: proc(ptr: [^]u8, len: c.size_t, expected: c.uint32_t) -> Bool_Result ---

    // SafeProbability
    proven_probability_create :: proc(value: c.double) -> c.double ---
    proven_probability_and :: proc(a, b: c.double) -> c.double ---
    proven_probability_or_exclusive :: proc(a, b: c.double) -> c.double ---
    proven_probability_not :: proc(p: c.double) -> c.double ---

    // SafeCalculator
    proven_calculator_eval :: proc(ptr: [^]u8, len: c.size_t) -> Float_Result ---

    // SafeBuffer
    proven_buffer_create :: proc(capacity: c.size_t) -> Buffer_Result ---
    proven_buffer_append :: proc(buffer: ^Bounded_Buffer, ptr: [^]u8, len: c.size_t) -> c.int32_t ---
    proven_buffer_get :: proc(buffer: ^Bounded_Buffer, out_ptr: ^[^]u8, out_len: ^c.size_t) -> c.int32_t ---
    proven_buffer_free :: proc(buffer: ^Bounded_Buffer) ---

    // SafeRateLimiter
    proven_rate_limiter_create :: proc(capacity, refill_rate: c.double) -> ^Rate_Limiter ---
    proven_rate_limiter_try_acquire :: proc(limiter: ^Rate_Limiter, tokens: c.double) -> c.bool ---
    proven_rate_limiter_free :: proc(limiter: ^Rate_Limiter) ---

    // SafeCircuitBreaker
    proven_circuit_breaker_create :: proc(failure_threshold, success_threshold: c.uint32_t, timeout_ms: c.int64_t) -> ^Circuit_Breaker ---
    proven_circuit_breaker_allow :: proc(cb: ^Circuit_Breaker) -> c.bool ---
    proven_circuit_breaker_success :: proc(cb: ^Circuit_Breaker) ---
    proven_circuit_breaker_failure :: proc(cb: ^Circuit_Breaker) ---
    proven_circuit_breaker_state :: proc(cb: ^Circuit_Breaker) -> Circuit_State ---
    proven_circuit_breaker_free :: proc(cb: ^Circuit_Breaker) ---

    // SafeRetry
    proven_retry_delay :: proc(config: Retry_Config, attempt: c.uint32_t) -> c.uint64_t ---
    proven_retry_should_retry :: proc(config: Retry_Config, attempt: c.uint32_t) -> c.bool ---

    // SafeMonotonic
    proven_monotonic_create :: proc(initial, max_value: c.uint64_t) -> ^Monotonic_Counter ---
    proven_monotonic_next :: proc(counter: ^Monotonic_Counter) -> Int_Result ---
    proven_monotonic_free :: proc(counter: ^Monotonic_Counter) ---

    // SafeStateMachine
    proven_state_machine_create :: proc(state_count, initial_state: c.uint32_t) -> ^State_Machine ---
    proven_state_machine_allow :: proc(sm: ^State_Machine, from, to: c.uint32_t) -> c.bool ---
    proven_state_machine_transition :: proc(sm: ^State_Machine, to: c.uint32_t) -> c.bool ---
    proven_state_machine_state :: proc(sm: ^State_Machine) -> c.uint32_t ---
    proven_state_machine_free :: proc(sm: ^State_Machine) ---

    // SafeTensor
    proven_tensor_create :: proc(rows, cols: c.size_t) -> ^Tensor_2D ---
    proven_tensor_set :: proc(tensor: ^Tensor_2D, row, col: c.size_t, value: c.double) -> c.int32_t ---
    proven_tensor_get :: proc(tensor: ^Tensor_2D, row, col: c.size_t) -> Float_Result ---
    proven_tensor_matmul :: proc(a, b: ^Tensor_2D) -> ^Tensor_2D ---
    proven_tensor_free :: proc(tensor: ^Tensor_2D) ---

    // SafeMl
    proven_ml_softmax :: proc(input: [^]c.double, output: [^]c.double, len: c.size_t) -> c.int32_t ---
    proven_ml_sigmoid :: proc(x: c.double) -> c.double ---
    proven_ml_relu :: proc(x: c.double) -> c.double ---
    proven_ml_leaky_relu :: proc(x: c.double, alpha: c.double) -> c.double ---
    proven_ml_clamp :: proc(x: c.double, min_val: c.double, max_val: c.double) -> c.double ---

    // SafeLru
    proven_lru_create :: proc(capacity: c.size_t) -> ^LRU_Cache ---
    proven_lru_get :: proc(cache: ^LRU_Cache, key: c.uint64_t) -> Int_Result ---
    proven_lru_put :: proc(cache: ^LRU_Cache, key: c.uint64_t, value: c.int64_t) -> c.int32_t ---
    proven_lru_free :: proc(cache: ^LRU_Cache) ---

    // SafeGraph
    proven_graph_create :: proc(node_count: c.size_t) -> ^Graph ---
    proven_graph_add_edge :: proc(graph: ^Graph, from, to: c.size_t) -> c.int32_t ---
    proven_graph_has_edge :: proc(graph: ^Graph, from, to: c.size_t) -> c.bool ---
    proven_graph_free :: proc(graph: ^Graph) ---

    // SafeQueue
    proven_queue_create :: proc(capacity: c.size_t) -> ^Bounded_Queue ---
    proven_queue_push :: proc(queue: ^Bounded_Queue, value: c.int64_t) -> c.bool ---
    proven_queue_pop :: proc(queue: ^Bounded_Queue) -> Int_Result ---
    proven_queue_size :: proc(queue: ^Bounded_Queue) -> c.size_t ---
    proven_queue_free :: proc(queue: ^Bounded_Queue) ---

    // SafeBloom
    proven_bloom_create :: proc(expected_elements: c.size_t, false_positive_rate: c.double) -> ^Bloom_Filter ---
    proven_bloom_add :: proc(filter: ^Bloom_Filter, ptr: [^]u8, len: c.size_t) ---
    proven_bloom_contains :: proc(filter: ^Bloom_Filter, ptr: [^]u8, len: c.size_t) -> c.bool ---
    proven_bloom_free :: proc(filter: ^Bloom_Filter) ---

    // SafeRegistry
    proven_registry_parse :: proc(ptr: [^]u8, len: c.size_t) -> Image_Ref_Result ---
    proven_registry_to_string :: proc(ref: ^Image_Reference) -> String_Result ---
    proven_registry_has_registry :: proc(ref: ^Image_Reference) -> Bool_Result ---

    // SafeDigest
    proven_digest_parse :: proc(ptr: [^]u8, len: c.size_t) -> Digest_Result ---
    proven_digest_verify :: proc(expected, actual: ^Digest) -> Bool_Result ---
    proven_digest_to_string :: proc(digest: ^Digest) -> String_Result ---

    // SafeHttp
    proven_http_url_encode :: proc(ptr: [^]u8, len: c.size_t) -> String_Result ---
    proven_http_url_decode :: proc(ptr: [^]u8, len: c.size_t) -> String_Result ---
    proven_http_parse_www_authenticate :: proc(ptr: [^]u8, len: c.size_t) -> Auth_Challenge_Result ---

    // SafeColor
    proven_color_parse_hex :: proc(ptr: [^]u8, len: c.size_t) -> Color_Result ---
    proven_color_rgb_to_hsl :: proc(rgb: RGB_Color) -> HSL_Color ---
    proven_color_to_hex :: proc(rgb: RGB_Color) -> String_Result ---

    // SafeAngle
    proven_angle_deg_to_rad :: proc(degrees: c.double) -> c.double ---
    proven_angle_rad_to_deg :: proc(radians: c.double) -> c.double ---
    proven_angle_normalize_degrees :: proc(degrees: c.double) -> c.double ---
    proven_angle_normalize_radians :: proc(radians: c.double) -> c.double ---

    // SafeUnit
    proven_unit_convert_length :: proc(value: c.double, from, to: Length_Unit) -> Float_Result ---
    proven_unit_convert_temp :: proc(value: c.double, from, to: Temp_Unit) -> Float_Result ---

    // Callbacks
    proven_callback_register :: proc(event_type: Event_Type, callback: Callback_Fn, context: rawptr) -> Callback_Handle ---
    proven_callback_unregister :: proc(handle: Callback_Handle) -> c.int32_t ---
    proven_callback_fire :: proc(event_type: Event_Type, data_ptr: [^]u8, data_len: c.size_t, code: c.int32_t) -> c.int32_t ---
    proven_callback_count :: proc(event_type: Event_Type) -> c.uint32_t ---
    proven_callback_clear_all :: proc() -> c.uint32_t ---
}

// ============================================================================
// Idiomatic Odin wrapper helpers
// ============================================================================

// Convert C string result to Odin string, freeing the C allocation.
string_from_proven :: proc(result: String_Result, allocator := context.allocator) -> (str: string, ok: bool) {
    if result.status != 0 || result.value == nil {
        return "", false
    }
    odin_str := strings.clone_from_bytes(result.value[:result.length], allocator)
    proven_free_string(result.value)
    return odin_str, true
}

// Idiomatic wrappers for common operations using Odin's multi-return style.

// Safe integer division.
safe_div :: proc(a, b: i64) -> (result: i64, ok: bool) {
    r := proven_math_div(cast(c.int64_t)a, cast(c.int64_t)b)
    if r.status != 0 { return 0, false }
    return cast(i64)r.value, true
}

// Safe modulo.
safe_mod :: proc(a, b: i64) -> (result: i64, ok: bool) {
    r := proven_math_mod(cast(c.int64_t)a, cast(c.int64_t)b)
    if r.status != 0 { return 0, false }
    return cast(i64)r.value, true
}

// Checked addition with overflow detection.
safe_add :: proc(a, b: i64) -> (result: i64, ok: bool) {
    r := proven_math_add_checked(cast(c.int64_t)a, cast(c.int64_t)b)
    if r.status != 0 { return 0, false }
    return cast(i64)r.value, true
}

// Checked subtraction with underflow detection.
safe_sub :: proc(a, b: i64) -> (result: i64, ok: bool) {
    r := proven_math_sub_checked(cast(c.int64_t)a, cast(c.int64_t)b)
    if r.status != 0 { return 0, false }
    return cast(i64)r.value, true
}

// Checked multiplication with overflow detection.
safe_mul :: proc(a, b: i64) -> (result: i64, ok: bool) {
    r := proven_math_mul_checked(cast(c.int64_t)a, cast(c.int64_t)b)
    if r.status != 0 { return 0, false }
    return cast(i64)r.value, true
}

// Safe absolute value.
safe_abs :: proc(n: i64) -> (result: i64, ok: bool) {
    r := proven_math_abs_safe(cast(c.int64_t)n)
    if r.status != 0 { return 0, false }
    return cast(i64)r.value, true
}

// Escape string for HTML (XSS prevention).
escape_html :: proc(input: string, allocator := context.allocator) -> (result: string, ok: bool) {
    return string_from_proven(proven_string_escape_html(raw_data(input), cast(c.size_t)len(input)), allocator)
}

// Escape string for SQL.
escape_sql :: proc(input: string, allocator := context.allocator) -> (result: string, ok: bool) {
    return string_from_proven(proven_string_escape_sql(raw_data(input), cast(c.size_t)len(input)), allocator)
}

// Escape string for JavaScript.
escape_js :: proc(input: string, allocator := context.allocator) -> (result: string, ok: bool) {
    return string_from_proven(proven_string_escape_js(raw_data(input), cast(c.size_t)len(input)), allocator)
}

// Check if path contains directory traversal sequences.
has_traversal :: proc(path: string) -> bool {
    r := proven_path_has_traversal(raw_data(path), cast(c.size_t)len(path))
    return r.status == 0 && bool(r.value)
}

// Sanitize a filename.
sanitize_filename :: proc(filename: string, allocator := context.allocator) -> (result: string, ok: bool) {
    return string_from_proven(proven_path_sanitize_filename(raw_data(filename), cast(c.size_t)len(filename)), allocator)
}

// Validate email address.
is_valid_email :: proc(email: string) -> bool {
    r := proven_email_is_valid(raw_data(email), cast(c.size_t)len(email))
    return r.status == 0 && bool(r.value)
}

// Constant-time byte comparison (timing-attack safe).
constant_time_equals :: proc(a, b: []u8) -> bool {
    r := proven_crypto_constant_time_eq(raw_data(a), cast(c.size_t)len(a), raw_data(b), cast(c.size_t)len(b))
    return r.status == 0 && bool(r.value)
}

// Constant-time string comparison (timing-attack safe).
constant_time_equals_string :: proc(a, b: string) -> bool {
    return constant_time_equals(transmute([]u8)a, transmute([]u8)b)
}

// Parse IPv4 address string.
parse_ipv4 :: proc(address: string) -> (ip: IPv4_Address, ok: bool) {
    r := proven_network_parse_ipv4(raw_data(address), cast(c.size_t)len(address))
    if r.status != 0 { return {}, false }
    return r.address, true
}

// Check if IPv4 address is private.
is_private_ip :: proc(addr: IPv4_Address) -> bool {
    return bool(proven_network_ipv4_is_private(addr))
}

// Check if IPv4 address is loopback.
is_loopback :: proc(addr: IPv4_Address) -> bool {
    return bool(proven_network_ipv4_is_loopback(addr))
}

// URL-encode a string (RFC 3986).
url_encode :: proc(input: string, allocator := context.allocator) -> (result: string, ok: bool) {
    return string_from_proven(proven_http_url_encode(raw_data(input), cast(c.size_t)len(input)), allocator)
}
