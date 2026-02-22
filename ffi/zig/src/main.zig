// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Proven FFI - Stable C ABI for verified safety functions
//!
//! This module provides FFI exports for all Proven modules, allowing
//! safe arithmetic, string handling, JSON parsing, and more from any
//! language that can call C functions.
//!
//! ALL computation delegates to Idris 2 compiled code (RefC backend).
//! This Zig layer is a thin marshaling wrapper only.
//!
//! Built on idris2-zig-ffi for Idris 2 runtime integration.

const std = @import("std");
const ffi = @import("idris2_zig_ffi");

// Re-export idris2-zig-ffi types for Zig callers
pub const IdrisString = ffi.idris_rts.IdrisString;
pub const IdrisValue = ffi.idris_rts.IdrisValue;
pub const types = ffi.types;
pub const memory = ffi.memory;

// ============================================================================
// Idris2 RefC extern declarations
// ============================================================================
// All computation is performed in Idris 2 with dependent types and
// totality checking. These extern fn declarations bind to the compiled
// RefC output from Idris 2.

// --- SafePath ---
extern fn proven_idris_path_has_traversal(arg: IdrisValue) callconv(.C) IdrisValue;
extern fn proven_idris_path_sanitize_filename(arg: IdrisValue) callconv(.C) IdrisValue;

// --- SafeJson ---
extern fn proven_idris_json_is_valid(arg: IdrisValue) callconv(.C) IdrisValue;
extern fn proven_idris_json_get_type(arg: IdrisValue) callconv(.C) IdrisValue;

// --- SafeNetwork ---
extern fn proven_idris_network_parse_ipv4(arg: IdrisValue) callconv(.C) IdrisValue;
extern fn proven_idris_network_ipv4_is_private(arg: IdrisValue) callconv(.C) IdrisValue;
extern fn proven_idris_network_ipv4_is_loopback(arg: IdrisValue) callconv(.C) IdrisValue;

// --- SafeUrl ---
extern fn proven_idris_url_is_valid(arg: IdrisValue) callconv(.C) IdrisValue;
extern fn proven_idris_url_scheme(arg: IdrisValue) callconv(.C) IdrisValue;
extern fn proven_idris_url_host(arg: IdrisValue) callconv(.C) IdrisValue;
extern fn proven_idris_url_port(arg: IdrisValue) callconv(.C) IdrisValue;
extern fn proven_idris_url_path(arg: IdrisValue) callconv(.C) IdrisValue;
extern fn proven_idris_url_query(arg: IdrisValue) callconv(.C) IdrisValue;
extern fn proven_idris_url_fragment(arg: IdrisValue) callconv(.C) IdrisValue;

// --- SafeMath ---
extern fn idris2_proven_math_div(numerator: i64, denominator: i64) callconv(.C) IntResult;
extern fn idris2_proven_math_mod(numerator: i64, denominator: i64) callconv(.C) IntResult;
extern fn idris2_proven_math_add_checked(a: i64, b: i64) callconv(.C) IntResult;
extern fn idris2_proven_math_sub_checked(a: i64, b: i64) callconv(.C) IntResult;
extern fn idris2_proven_math_mul_checked(a: i64, b: i64) callconv(.C) IntResult;
extern fn idris2_proven_math_abs_safe(n: i64) callconv(.C) IntResult;
extern fn idris2_proven_math_clamp(lo: i64, hi: i64, value: i64) callconv(.C) i64;
extern fn idris2_proven_math_pow_checked(base: i64, exp: u32) callconv(.C) IntResult;

// --- SafeString ---
extern fn idris2_proven_string_is_valid_utf8(ptr: ?[*]const u8, len: usize) callconv(.C) BoolResult;
extern fn idris2_proven_string_escape_sql(ptr: ?[*]const u8, len: usize) callconv(.C) StringResult;
extern fn idris2_proven_string_escape_html(ptr: ?[*]const u8, len: usize) callconv(.C) StringResult;
extern fn idris2_proven_string_escape_js(ptr: ?[*]const u8, len: usize) callconv(.C) StringResult;

// --- SafeCrypto ---
extern fn idris2_proven_crypto_constant_time_eq(ptr1: ?[*]const u8, len1: usize, ptr2: ?[*]const u8, len2: usize) callconv(.C) BoolResult;
extern fn idris2_proven_crypto_random_bytes(ptr: ?[*]u8, len: usize) callconv(.C) ProvenStatus;

// --- SafeEmail ---
extern fn idris2_proven_email_is_valid(ptr: ?[*]const u8, len: usize) callconv(.C) BoolResult;

// --- SafeHeader ---
extern fn idris2_proven_header_has_crlf(ptr: ?[*]const u8, len: usize) callconv(.C) BoolResult;
extern fn idris2_proven_header_is_valid_name(ptr: ?[*]const u8, len: usize) callconv(.C) BoolResult;
extern fn idris2_proven_header_is_dangerous(ptr: ?[*]const u8, len: usize) callconv(.C) BoolResult;
extern fn idris2_proven_header_render(name_ptr: ?[*]const u8, name_len: usize, value_ptr: ?[*]const u8, value_len: usize) callconv(.C) StringResult;
extern fn idris2_proven_header_build_csp(directives_json: ?[*]const u8, json_len: usize) callconv(.C) StringResult;
extern fn idris2_proven_header_build_hsts(max_age: i64, include_subdomains: bool, preload: bool) callconv(.C) StringResult;

// --- SafeCookie ---
extern fn idris2_proven_cookie_has_injection(ptr: ?[*]const u8, len: usize) callconv(.C) BoolResult;
extern fn idris2_proven_cookie_validate_name(ptr: ?[*]const u8, len: usize) callconv(.C) BoolResult;
extern fn idris2_proven_cookie_validate_value(ptr: ?[*]const u8, len: usize) callconv(.C) BoolResult;
extern fn idris2_proven_cookie_get_prefix(ptr: ?[*]const u8, len: usize) callconv(.C) IntResult;
extern fn idris2_proven_cookie_build_set_cookie(name_ptr: ?[*]const u8, name_len: usize, value_ptr: ?[*]const u8, value_len: usize, attrs: CookieAttributes) callconv(.C) StringResult;
extern fn idris2_proven_cookie_build_delete(name_ptr: ?[*]const u8, name_len: usize) callconv(.C) StringResult;

// --- SafeContentType ---
extern fn idris2_proven_content_type_parse(ptr: ?[*]const u8, len: usize) callconv(.C) ContentTypeResult;
extern fn idris2_proven_content_type_can_sniff_dangerous(ptr: ?[*]const u8, len: usize) callconv(.C) BoolResult;
extern fn idris2_proven_content_type_render(type_ptr: ?[*]const u8, type_len: usize, subtype_ptr: ?[*]const u8, subtype_len: usize, suffix_ptr: ?[*]const u8, suffix_len: usize, charset: Charset, has_charset: bool) callconv(.C) StringResult;
extern fn idris2_proven_content_type_is_json(subtype_ptr: ?[*]const u8, subtype_len: usize, suffix_ptr: ?[*]const u8, suffix_len: usize) callconv(.C) BoolResult;
extern fn idris2_proven_content_type_is_xml(subtype_ptr: ?[*]const u8, subtype_len: usize, suffix_ptr: ?[*]const u8, suffix_len: usize) callconv(.C) BoolResult;

// --- SafeRegistry ---
extern fn idris2_proven_registry_parse(ptr: ?[*]const u8, len: usize) callconv(.C) ImageRefResult;
extern fn idris2_proven_registry_to_string(ref: *const ImageReference) callconv(.C) StringResult;
extern fn idris2_proven_registry_has_registry(ref: *const ImageReference) callconv(.C) BoolResult;

// --- SafeDigest ---
extern fn idris2_proven_digest_parse(ptr: ?[*]const u8, len: usize) callconv(.C) DigestResult;
extern fn idris2_proven_digest_verify(expected: *const Digest, actual: *const Digest) callconv(.C) BoolResult;
extern fn idris2_proven_digest_to_string(digest: *const Digest) callconv(.C) StringResult;

// --- SafeHTTP ---
extern fn idris2_proven_http_url_encode(ptr: ?[*]const u8, len: usize) callconv(.C) StringResult;
extern fn idris2_proven_http_url_decode(ptr: ?[*]const u8, len: usize) callconv(.C) StringResult;
extern fn idris2_proven_http_parse_www_authenticate(ptr: ?[*]const u8, len: usize) callconv(.C) AuthChallengeResult;

// --- SafeHex ---
extern fn idris2_proven_hex_encode(ptr: ?[*]const u8, len: usize, uppercase: bool) callconv(.C) StringResult;
extern fn idris2_proven_hex_decode(ptr: ?[*]const u8, len: usize) callconv(.C) HexDecodeResult;

// --- SafeUUID ---
extern fn idris2_proven_uuid_v4() callconv(.C) UUIDResult;
extern fn idris2_proven_uuid_to_string(uuid: UUID) callconv(.C) StringResult;
extern fn idris2_proven_uuid_parse(ptr: ?[*]const u8, len: usize) callconv(.C) UUIDResult;
extern fn idris2_proven_uuid_is_nil(uuid: UUID) callconv(.C) bool;
extern fn idris2_proven_uuid_version(uuid: UUID) callconv(.C) u8;

// --- SafeCurrency ---
extern fn idris2_proven_currency_parse(ptr: ?[*]const u8, len: usize) callconv(.C) CurrencyResult;
extern fn idris2_proven_currency_format(amount_minor: i64, code: [3]u8, decimal_places: u8) callconv(.C) StringResult;

// --- SafePhone ---
extern fn idris2_proven_phone_parse(ptr: ?[*]const u8, len: usize) callconv(.C) PhoneResult;
extern fn idris2_proven_phone_format_e164(country_code: u16, national_number: u64) callconv(.C) StringResult;

// --- SafeDateTime ---
extern fn idris2_proven_datetime_parse(ptr: ?[*]const u8, len: usize) callconv(.C) DateTimeResult;
extern fn idris2_proven_datetime_format_iso8601(dt: DateTime) callconv(.C) StringResult;
extern fn idris2_proven_datetime_is_leap_year(year: i32) callconv(.C) bool;
extern fn idris2_proven_datetime_days_in_month(year: i32, month: u8) callconv(.C) u8;

// --- SafeFloat ---
extern fn idris2_proven_float_div(a: f64, b: f64) callconv(.C) FloatResult;
extern fn idris2_proven_float_is_finite(x: f64) callconv(.C) bool;
extern fn idris2_proven_float_is_nan(x: f64) callconv(.C) bool;
extern fn idris2_proven_float_sqrt(x: f64) callconv(.C) FloatResult;
extern fn idris2_proven_float_ln(x: f64) callconv(.C) FloatResult;

// --- SafeVersion ---
extern fn idris2_proven_version_parse(ptr: ?[*]const u8, len: usize) callconv(.C) VersionResult;
extern fn idris2_proven_version_compare(a: SemanticVersion, b: SemanticVersion) callconv(.C) i32;

// --- SafeGeo ---
extern fn idris2_proven_geo_validate(lat: f64, lon: f64) callconv(.C) GeoResult;
extern fn idris2_proven_geo_distance(a: GeoCoordinate, b: GeoCoordinate) callconv(.C) FloatResult;
extern fn idris2_proven_geo_in_bounds(coord: GeoCoordinate, min_lat: f64, max_lat: f64, min_lon: f64, max_lon: f64) callconv(.C) bool;

// --- SafeChecksum ---
extern fn idris2_proven_checksum_crc32(ptr: ?[*]const u8, len: usize) callconv(.C) IntResult;
extern fn idris2_proven_checksum_verify_crc32(ptr: ?[*]const u8, len: usize, expected: u32) callconv(.C) BoolResult;

// --- SafeProbability ---
extern fn idris2_proven_probability_create(value: f64) callconv(.C) f64;
extern fn idris2_proven_probability_and(a: f64, b: f64) callconv(.C) f64;
extern fn idris2_proven_probability_or_exclusive(a: f64, b: f64) callconv(.C) f64;
extern fn idris2_proven_probability_not(p: f64) callconv(.C) f64;

// --- SafeCalculator ---
extern fn idris2_proven_calculator_eval(ptr: ?[*]const u8, len: usize) callconv(.C) FloatResult;

// --- SafeBuffer ---
extern fn idris2_proven_buffer_create(capacity: usize) callconv(.C) BufferResult;
extern fn idris2_proven_buffer_append(buffer: ?*BoundedBuffer, ptr: ?[*]const u8, len: usize) callconv(.C) ProvenStatus;
extern fn idris2_proven_buffer_get(buffer: ?*BoundedBuffer, out_ptr: ?*[*]const u8, out_len: ?*usize) callconv(.C) ProvenStatus;

// --- SafeRateLimiter ---
extern fn idris2_proven_rate_limiter_create(capacity: f64, refill_rate: f64) callconv(.C) ?*RateLimiter;
extern fn idris2_proven_rate_limiter_try_acquire(limiter: ?*RateLimiter, tokens: f64) callconv(.C) bool;

// --- SafeCircuitBreaker ---
extern fn idris2_proven_circuit_breaker_create(failure_threshold: u32, success_threshold: u32, timeout_ms: i64) callconv(.C) ?*CircuitBreaker;
extern fn idris2_proven_circuit_breaker_allow(cb: ?*CircuitBreaker) callconv(.C) bool;
extern fn idris2_proven_circuit_breaker_success(cb: ?*CircuitBreaker) callconv(.C) void;
extern fn idris2_proven_circuit_breaker_failure(cb: ?*CircuitBreaker) callconv(.C) void;
extern fn idris2_proven_circuit_breaker_state(cb: ?*CircuitBreaker) callconv(.C) CircuitState;

// --- SafePassword ---
extern fn idris2_proven_password_validate(ptr: ?[*]const u8, len: usize) callconv(.C) PasswordResult;
extern fn idris2_proven_password_is_common(ptr: ?[*]const u8, len: usize) callconv(.C) bool;

// --- SafeColor ---
extern fn idris2_proven_color_parse_hex(ptr: ?[*]const u8, len: usize) callconv(.C) ColorParseResult;
extern fn idris2_proven_color_rgb_to_hsl(rgb: RGBColor) callconv(.C) HSLColor;
extern fn idris2_proven_color_to_hex(rgb: RGBColor) callconv(.C) StringResult;

// --- SafeAngle ---
extern fn idris2_proven_angle_deg_to_rad(degrees: f64) callconv(.C) f64;
extern fn idris2_proven_angle_rad_to_deg(radians: f64) callconv(.C) f64;
extern fn idris2_proven_angle_normalize_degrees(degrees: f64) callconv(.C) f64;
extern fn idris2_proven_angle_normalize_radians(radians: f64) callconv(.C) f64;

// --- SafeUnit ---
extern fn idris2_proven_unit_convert_length(value: f64, from: LengthUnit, to: LengthUnit) callconv(.C) FloatResult;
extern fn idris2_proven_unit_convert_temp(value: f64, from: TempUnit, to: TempUnit) callconv(.C) FloatResult;

// --- SafeQueue ---
extern fn idris2_proven_queue_create(capacity: usize) callconv(.C) ?*BoundedQueue;
extern fn idris2_proven_queue_push(queue: ?*BoundedQueue, value: i64) callconv(.C) bool;
extern fn idris2_proven_queue_pop(queue: ?*BoundedQueue) callconv(.C) IntResult;
extern fn idris2_proven_queue_size(queue: ?*BoundedQueue) callconv(.C) usize;

// --- SafeBloom ---
extern fn idris2_proven_bloom_create(expected_elements: usize, false_positive_rate: f64) callconv(.C) ?*BloomFilter;
extern fn idris2_proven_bloom_add(filter: ?*BloomFilter, ptr: ?[*]const u8, len: usize) callconv(.C) void;
extern fn idris2_proven_bloom_contains(filter: ?*BloomFilter, ptr: ?[*]const u8, len: usize) callconv(.C) bool;

// --- SafeRetry ---
extern fn idris2_proven_retry_delay(config: RetryConfig, attempt: u32) callconv(.C) u64;
extern fn idris2_proven_retry_should_retry(config: RetryConfig, attempt: u32) callconv(.C) bool;

// --- SafeMonotonic ---
extern fn idris2_proven_monotonic_create(initial: u64, max_value: u64) callconv(.C) ?*MonotonicCounter;
extern fn idris2_proven_monotonic_next(counter: ?*MonotonicCounter) callconv(.C) IntResult;

// --- SafeStateMachine ---
extern fn idris2_proven_state_machine_create(state_count: u32, initial_state: u32) callconv(.C) ?*StateMachine;
extern fn idris2_proven_state_machine_allow(sm: ?*StateMachine, from: u32, to: u32) callconv(.C) bool;
extern fn idris2_proven_state_machine_transition(sm: ?*StateMachine, to: u32) callconv(.C) bool;
extern fn idris2_proven_state_machine_state(sm: ?*StateMachine) callconv(.C) u32;

// --- SafeTensor ---
extern fn idris2_proven_tensor_create(rows: usize, cols: usize) callconv(.C) ?*Tensor2D;
extern fn idris2_proven_tensor_set(tensor: ?*Tensor2D, row: usize, col: usize, value: f64) callconv(.C) ProvenStatus;
extern fn idris2_proven_tensor_get(tensor: ?*Tensor2D, row: usize, col: usize) callconv(.C) FloatResult;
extern fn idris2_proven_tensor_matmul(a: ?*Tensor2D, b: ?*Tensor2D) callconv(.C) ?*Tensor2D;

// --- SafeML ---
extern fn idris2_proven_ml_softmax(input: ?[*]const f64, output: ?[*]f64, len: usize) callconv(.C) ProvenStatus;
extern fn idris2_proven_ml_sigmoid(x: f64) callconv(.C) f64;
extern fn idris2_proven_ml_relu(x: f64) callconv(.C) f64;
extern fn idris2_proven_ml_leaky_relu(x: f64, alpha: f64) callconv(.C) f64;
extern fn idris2_proven_ml_clamp(x: f64, min_val: f64, max_val: f64) callconv(.C) f64;

// --- SafeLRU ---
extern fn idris2_proven_lru_create(capacity: usize) callconv(.C) ?*LRUCache;
extern fn idris2_proven_lru_get(cache: ?*LRUCache, key: u64) callconv(.C) IntResult;
extern fn idris2_proven_lru_put(cache: ?*LRUCache, key: u64, value: i64) callconv(.C) ProvenStatus;

// --- SafeGraph ---
extern fn idris2_proven_graph_create(node_count: usize) callconv(.C) ?*Graph;
extern fn idris2_proven_graph_add_edge(graph: ?*Graph, from: usize, to: usize) callconv(.C) ProvenStatus;
extern fn idris2_proven_graph_has_edge(graph: ?*Graph, from: usize, to: usize) callconv(.C) bool;

// ============================================================================
// Result Types
// ============================================================================

/// Result status codes
pub const ProvenStatus = enum(i32) {
    ok = 0,
    err_null_pointer = -1,
    err_invalid_argument = -2,
    err_overflow = -3,
    err_underflow = -4,
    err_division_by_zero = -5,
    err_parse_failure = -6,
    err_validation_failed = -7,
    err_out_of_bounds = -8,
    err_encoding_error = -9,
    err_allocation_failed = -10,
    err_not_implemented = -99,
};

/// Result for integer operations
pub const IntResult = extern struct {
    status: ProvenStatus,
    value: i64,
};

/// Result for boolean operations
pub const BoolResult = extern struct {
    status: ProvenStatus,
    value: bool,
};

/// Result for string operations (caller must free the string)
pub const StringResult = extern struct {
    status: ProvenStatus,
    value: ?[*:0]u8,
    length: usize,
};

// ============================================================================
// Allocator for FFI strings
// ============================================================================

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

/// Free a string allocated by proven functions
export fn proven_free_string(ptr: ?[*:0]u8) void {
    if (ptr) |p| {
        // Find length by scanning for null terminator
        var len: usize = 0;
        while (p[len] != 0) : (len += 1) {}
        allocator.free(p[0 .. len + 1]);
    }
}

// ============================================================================
// Callback Infrastructure (Bidirectional FFI)
// ============================================================================
//
// Enables host languages to register callback functions that Idris2 code
// can invoke. This is the reverse direction of the normal FFI flow:
//   Normal:  Host → Zig → Idris2
//   Callback: Idris2 → Zig → Host
//
// Thread-safe: all callback operations are protected by a mutex.

/// Event types that can trigger callbacks
pub const ProvenEventType = enum(i32) {
    /// Validation failure (SafeJson, SafeXML, SafeEmail, etc.)
    validation_failed = 1,
    /// Resource acquired (SafeResource, SafeFile)
    resource_acquired = 2,
    /// Resource released (SafeResource, SafeFile)
    resource_released = 3,
    /// Rate limit hit (SafeRateLimiter)
    rate_limit_hit = 4,
    /// Circuit breaker state change (SafeCircuitBreaker)
    circuit_state_change = 5,
    /// Signal received (SafeSignal)
    signal_received = 6,
    /// Retry attempt (SafeRetry)
    retry_attempt = 7,
    /// Cryptographic operation completed (SafeCrypto)
    crypto_operation = 8,
    /// Custom event (user-defined)
    custom = 100,
};

/// Event data passed to callbacks
pub const ProvenEvent = extern struct {
    /// Event type identifier
    event_type: ProvenEventType,
    /// Optional data payload (null-terminated string or null)
    data_ptr: ?[*:0]const u8,
    /// Length of data payload (0 if no data)
    data_len: usize,
    /// Monotonic timestamp (microseconds since epoch)
    timestamp_us: u64,
    /// Event-specific integer code (e.g., signal number, HTTP status)
    code: i32,
};

/// Callback function signature: fn(context, event) -> status
/// Returns 0 on success, non-zero to request deregistration
pub const ProvenCallbackFn = ?*const fn (?*anyopaque, *const ProvenEvent) callconv(.C) i32;

/// Predicate callback: fn(context, data, len) -> bool
pub const ProvenPredicateFn = ?*const fn (?*anyopaque, ?[*]const u8, usize) callconv(.C) bool;

/// Transform callback: fn(context, input, input_len, output_buf, output_buf_len) -> actual_len
pub const ProvenTransformFn = ?*const fn (?*anyopaque, ?[*]const u8, usize, ?[*]u8, usize) callconv(.C) usize;

/// Opaque handle for a registered callback
pub const CallbackHandle = u32;

const MAX_CALLBACKS = 256;
const INVALID_HANDLE: CallbackHandle = 0;

const CallbackEntry = struct {
    handle: CallbackHandle,
    event_type: ProvenEventType,
    callback: ProvenCallbackFn,
    context: ?*anyopaque,
    active: bool,
};

var callback_registry: [MAX_CALLBACKS]CallbackEntry = [_]CallbackEntry{.{
    .handle = INVALID_HANDLE,
    .event_type = .validation_failed,
    .callback = null,
    .context = null,
    .active = false,
}} ** MAX_CALLBACKS;
var next_handle: CallbackHandle = 1;
var callback_mutex: std.Thread.Mutex = .{};

/// Register a callback for a specific event type.
/// Returns a non-zero handle on success, 0 on failure.
export fn proven_callback_register(
    event_type: ProvenEventType,
    callback: ProvenCallbackFn,
    context: ?*anyopaque,
) CallbackHandle {
    if (callback == null) return INVALID_HANDLE;

    callback_mutex.lock();
    defer callback_mutex.unlock();

    for (&callback_registry) |*entry| {
        if (!entry.active) {
            const handle = next_handle;
            next_handle +%= 1;
            if (next_handle == INVALID_HANDLE) next_handle = 1;

            entry.* = .{
                .handle = handle,
                .event_type = event_type,
                .callback = callback,
                .context = context,
                .active = true,
            };
            return handle;
        }
    }
    return INVALID_HANDLE; // Registry full
}

/// Unregister a callback by handle.
/// Returns ok on success, err_not_found if handle invalid.
export fn proven_callback_unregister(handle: CallbackHandle) ProvenStatus {
    if (handle == INVALID_HANDLE) return .err_invalid_argument;

    callback_mutex.lock();
    defer callback_mutex.unlock();

    for (&callback_registry) |*entry| {
        if (entry.active and entry.handle == handle) {
            entry.active = false;
            entry.callback = null;
            entry.context = null;
            return .ok;
        }
    }
    return .err_out_of_bounds; // Handle not found
}

/// Fire an event, invoking all registered callbacks for that event type.
/// Returns the number of callbacks invoked.
export fn proven_callback_fire(event_type: ProvenEventType, data_ptr: ?[*:0]const u8, data_len: usize, code: i32) i32 {
    const event = ProvenEvent{
        .event_type = event_type,
        .data_ptr = data_ptr,
        .data_len = data_len,
        .timestamp_us = @intCast(std.time.microTimestamp()),
        .code = code,
    };

    callback_mutex.lock();
    defer callback_mutex.unlock();

    var invoked: i32 = 0;
    for (&callback_registry) |*entry| {
        if (entry.active and entry.event_type == event_type) {
            if (entry.callback) |cb| {
                const result = cb(entry.context, &event);
                invoked += 1;
                // Non-zero return = request deregistration
                if (result != 0) {
                    entry.active = false;
                    entry.callback = null;
                    entry.context = null;
                }
            }
        }
    }
    return invoked;
}

/// Query how many callbacks are registered for an event type.
export fn proven_callback_count(event_type: ProvenEventType) u32 {
    callback_mutex.lock();
    defer callback_mutex.unlock();

    var count: u32 = 0;
    for (callback_registry) |entry| {
        if (entry.active and entry.event_type == event_type) {
            count += 1;
        }
    }
    return count;
}

/// Unregister all callbacks. Returns count of callbacks removed.
export fn proven_callback_clear_all() u32 {
    callback_mutex.lock();
    defer callback_mutex.unlock();

    var count: u32 = 0;
    for (&callback_registry) |*entry| {
        if (entry.active) {
            entry.active = false;
            entry.callback = null;
            entry.context = null;
            count += 1;
        }
    }
    return count;
}

// ============================================================================
// Marshaling helpers for IdrisValue-based extern fns
// ============================================================================

fn idrisStringArg(ptr: ?[*]const u8, len: usize) ?IdrisString {
    if (ptr == null) return null;
    const slice = ptr.?[0..len];
    const idris_str = ffi.toIdrisString(slice);
    if (idris_str.data == null and slice.len != 0) return null;
    return idris_str;
}

fn idrisCallBool(func: *const fn (IdrisValue) callconv(.C) IdrisValue, ptr: ?[*]const u8, len: usize) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    const idris_str = idrisStringArg(ptr, len) orelse {
        return .{ .status = .err_allocation_failed, .value = false };
    };
    defer ffi.freeIdrisString(idris_str);

    const raw = ffi.idris_rts.callRaw1(func, .{ .string = idris_str });
    return .{ .status = .ok, .value = raw.int != 0 };
}

fn idrisCallInt(func: *const fn (IdrisValue) callconv(.C) IdrisValue, ptr: ?[*]const u8, len: usize) IntResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = 0 };
    }
    const idris_str = idrisStringArg(ptr, len) orelse {
        return .{ .status = .err_allocation_failed, .value = 0 };
    };
    defer ffi.freeIdrisString(idris_str);

    const raw = ffi.idris_rts.callRaw1(func, .{ .string = idris_str });
    return .{ .status = .ok, .value = raw.int };
}

fn idrisCallStringSlice(func: *const fn (IdrisValue) callconv(.C) IdrisValue, ptr: ?[*]const u8, len: usize) ?[]const u8 {
    const idris_str = idrisStringArg(ptr, len) orelse return null;
    defer ffi.freeIdrisString(idris_str);

    const raw = ffi.idris_rts.callRaw1(func, .{ .string = idris_str });
    return types.fromIdrisString(raw.string);
}

// ============================================================================
// SafeMath - Arithmetic operations that cannot crash
// ============================================================================

/// Safe division: returns status and result
/// Returns err_division_by_zero if denominator is 0
export fn proven_math_div(numerator: i64, denominator: i64) IntResult {
    return idris2_proven_math_div(numerator, denominator);
}

/// Safe modulo: returns status and result
export fn proven_math_mod(numerator: i64, denominator: i64) IntResult {
    return idris2_proven_math_mod(numerator, denominator);
}

/// Checked addition: returns err_overflow if result would overflow
export fn proven_math_add_checked(a: i64, b: i64) IntResult {
    return idris2_proven_math_add_checked(a, b);
}

/// Checked subtraction: returns err_underflow if result would underflow
export fn proven_math_sub_checked(a: i64, b: i64) IntResult {
    return idris2_proven_math_sub_checked(a, b);
}

/// Checked multiplication: returns err_overflow if result would overflow
export fn proven_math_mul_checked(a: i64, b: i64) IntResult {
    return idris2_proven_math_mul_checked(a, b);
}

/// Safe absolute value: handles MIN_INT correctly
export fn proven_math_abs_safe(n: i64) IntResult {
    return idris2_proven_math_abs_safe(n);
}

/// Clamp value to range [lo, hi]
export fn proven_math_clamp(lo: i64, hi: i64, value: i64) i64 {
    return idris2_proven_math_clamp(lo, hi, value);
}

/// Integer power with overflow checking
export fn proven_math_pow_checked(base: i64, exp: u32) IntResult {
    return idris2_proven_math_pow_checked(base, exp);
}

// ============================================================================
// SafeString - Text operations that handle encoding safely
// ============================================================================

/// Check if a byte sequence is valid UTF-8
export fn proven_string_is_valid_utf8(ptr: ?[*]const u8, len: usize) BoolResult {
    return idris2_proven_string_is_valid_utf8(ptr, len);
}

/// Escape string for SQL (single quotes)
export fn proven_string_escape_sql(ptr: ?[*]const u8, len: usize) StringResult {
    return idris2_proven_string_escape_sql(ptr, len);
}

/// Escape string for HTML (< > & " ')
export fn proven_string_escape_html(ptr: ?[*]const u8, len: usize) StringResult {
    return idris2_proven_string_escape_html(ptr, len);
}

/// Escape string for JavaScript (within quotes)
export fn proven_string_escape_js(ptr: ?[*]const u8, len: usize) StringResult {
    return idris2_proven_string_escape_js(ptr, len);
}

// ============================================================================
// SafePath - Filesystem operations that prevent traversal attacks
// ============================================================================

/// Check if a path attempts directory traversal
export fn proven_path_has_traversal(ptr: ?[*]const u8, len: usize) BoolResult {
    return idrisCallBool(proven_idris_path_has_traversal, ptr, len);
}

/// Sanitize a filename (remove dangerous characters)
export fn proven_path_sanitize_filename(ptr: ?[*]const u8, len: usize) StringResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }

    const result = idrisCallStringSlice(proven_idris_path_sanitize_filename, ptr, len) orelse {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };

    if (result.len == 0) {
        return .{ .status = .err_validation_failed, .value = null, .length = 0 };
    }

    const output = allocator.allocSentinel(u8, result.len, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };
    @memcpy(output, result);

    return .{ .status = .ok, .value = output.ptr, .length = result.len };
}

// ============================================================================
// SafeCrypto - Cryptographic primitives
// ============================================================================

/// Constant-time byte comparison (timing-safe)
export fn proven_crypto_constant_time_eq(
    ptr1: ?[*]const u8,
    len1: usize,
    ptr2: ?[*]const u8,
    len2: usize,
) BoolResult {
    return idris2_proven_crypto_constant_time_eq(ptr1, len1, ptr2, len2);
}

/// Fill buffer with cryptographically secure random bytes
export fn proven_crypto_random_bytes(ptr: ?[*]u8, len: usize) ProvenStatus {
    return idris2_proven_crypto_random_bytes(ptr, len);
}

// ============================================================================
// SafeUrl - URL parsing and validation
// ============================================================================

/// URL components structure
pub const UrlComponents = extern struct {
    scheme: ?[*:0]u8,
    scheme_len: usize,
    host: ?[*:0]u8,
    host_len: usize,
    port: u16,
    has_port: bool,
    path: ?[*:0]u8,
    path_len: usize,
    query: ?[*:0]u8,
    query_len: usize,
    fragment: ?[*:0]u8,
    fragment_len: usize,
};

/// Result for URL parsing
pub const UrlResult = extern struct {
    status: ProvenStatus,
    components: UrlComponents,
};

/// Parse a URL into components
export fn proven_url_parse(ptr: ?[*]const u8, len: usize) UrlResult {
    const empty_components = UrlComponents{
        .scheme = null,
        .scheme_len = 0,
        .host = null,
        .host_len = 0,
        .port = 0,
        .has_port = false,
        .path = null,
        .path_len = 0,
        .query = null,
        .query_len = 0,
        .fragment = null,
        .fragment_len = 0,
    };

    if (ptr == null) {
        return .{ .status = .err_null_pointer, .components = empty_components };
    }
    const valid = idrisCallBool(proven_idris_url_is_valid, ptr, len);
    if (valid.status != .ok) {
        return .{ .status = valid.status, .components = empty_components };
    }
    if (!valid.value) {
        return .{ .status = .err_parse_failure, .components = empty_components };
    }

    var components = empty_components;

    if (idrisCallStringSlice(proven_idris_url_scheme, ptr, len)) |scheme_raw| {
        if (scheme_raw.len > 0) {
            const scheme = allocator.allocSentinel(u8, scheme_raw.len, 0) catch {
                return .{ .status = .err_allocation_failed, .components = empty_components };
            };
            @memcpy(scheme, scheme_raw);
            components.scheme = scheme.ptr;
            components.scheme_len = scheme_raw.len;
        }
    } else {
        return .{ .status = .err_allocation_failed, .components = empty_components };
    }

    if (idrisCallStringSlice(proven_idris_url_host, ptr, len)) |host_raw| {
        if (host_raw.len > 0) {
            const host = allocator.allocSentinel(u8, host_raw.len, 0) catch {
                return .{ .status = .err_allocation_failed, .components = empty_components };
            };
            @memcpy(host, host_raw);
            components.host = host.ptr;
            components.host_len = host_raw.len;
        }
    } else {
        return .{ .status = .err_allocation_failed, .components = empty_components };
    }

    const port_result = idrisCallInt(proven_idris_url_port, ptr, len);
    if (port_result.status != .ok) {
        return .{ .status = port_result.status, .components = empty_components };
    }
    if (port_result.value >= 0) {
        components.port = @intCast(port_result.value);
        components.has_port = true;
    }

    if (idrisCallStringSlice(proven_idris_url_path, ptr, len)) |path_raw| {
        if (path_raw.len > 0) {
            const path = allocator.allocSentinel(u8, path_raw.len, 0) catch {
                return .{ .status = .err_allocation_failed, .components = empty_components };
            };
            @memcpy(path, path_raw);
            components.path = path.ptr;
            components.path_len = path_raw.len;
        }
    } else {
        return .{ .status = .err_allocation_failed, .components = empty_components };
    }

    if (idrisCallStringSlice(proven_idris_url_query, ptr, len)) |query_raw| {
        if (query_raw.len > 0) {
            const query = allocator.allocSentinel(u8, query_raw.len, 0) catch {
                return .{ .status = .err_allocation_failed, .components = empty_components };
            };
            @memcpy(query, query_raw);
            components.query = query.ptr;
            components.query_len = query_raw.len;
        }
    } else {
        return .{ .status = .err_allocation_failed, .components = empty_components };
    }

    if (idrisCallStringSlice(proven_idris_url_fragment, ptr, len)) |fragment_raw| {
        if (fragment_raw.len > 0) {
            const fragment = allocator.allocSentinel(u8, fragment_raw.len, 0) catch {
                return .{ .status = .err_allocation_failed, .components = empty_components };
            };
            @memcpy(fragment, fragment_raw);
            components.fragment = fragment.ptr;
            components.fragment_len = fragment_raw.len;
        }
    } else {
        return .{ .status = .err_allocation_failed, .components = empty_components };
    }

    return .{ .status = .ok, .components = components };
}

/// Free URL components
export fn proven_url_free(components: *UrlComponents) void {
    if (components.scheme) |s| {
        allocator.free(s[0 .. components.scheme_len + 1]);
    }
    if (components.host) |h| {
        allocator.free(h[0 .. components.host_len + 1]);
    }
    if (components.path) |p| {
        allocator.free(p[0 .. components.path_len + 1]);
    }
    if (components.query) |q| {
        allocator.free(q[0 .. components.query_len + 1]);
    }
    if (components.fragment) |f| {
        allocator.free(f[0 .. components.fragment_len + 1]);
    }
    components.* = .{
        .scheme = null,
        .scheme_len = 0,
        .host = null,
        .host_len = 0,
        .port = 0,
        .has_port = false,
        .path = null,
        .path_len = 0,
        .query = null,
        .query_len = 0,
        .fragment = null,
        .fragment_len = 0,
    };
}

// ============================================================================
// SafeEmail - Email validation
// ============================================================================

/// Validate email address (RFC 5321 simplified)
export fn proven_email_is_valid(ptr: ?[*]const u8, len: usize) BoolResult {
    return idris2_proven_email_is_valid(ptr, len);
}

// ============================================================================
// SafeNetwork - IP address parsing
// ============================================================================

/// IPv4 address structure
pub const IPv4Address = extern struct {
    octets: [4]u8,
};

/// Result for IPv4 parsing
pub const IPv4Result = extern struct {
    status: ProvenStatus,
    address: IPv4Address,
};

/// Parse IPv4 address
export fn proven_network_parse_ipv4(ptr: ?[*]const u8, len: usize) IPv4Result {
    const empty = IPv4Address{ .octets = .{ 0, 0, 0, 0 } };

    const idris_result = idrisCallInt(proven_idris_network_parse_ipv4, ptr, len);
    if (idris_result.status != .ok) {
        return .{ .status = idris_result.status, .address = empty };
    }
    if (idris_result.value < 0) {
        return .{ .status = .err_parse_failure, .address = empty };
    }

    const value: u32 = @intCast(idris_result.value);
    const octets: [4]u8 = .{
        @intCast((value >> 24) & 0xFF),
        @intCast((value >> 16) & 0xFF),
        @intCast((value >> 8) & 0xFF),
        @intCast(value & 0xFF),
    };

    return .{ .status = .ok, .address = .{ .octets = octets } };
}

/// Check if IPv4 is private (RFC 1918)
export fn proven_network_ipv4_is_private(addr: IPv4Address) bool {
    const value: i64 = (@as(i64, addr.octets[0]) << 24) |
        (@as(i64, addr.octets[1]) << 16) |
        (@as(i64, addr.octets[2]) << 8) |
        @as(i64, addr.octets[3]);
    const raw = ffi.idris_rts.callRaw1(proven_idris_network_ipv4_is_private, .{ .int = value });
    return raw.int != 0;
}

/// Check if IPv4 is loopback (127.0.0.0/8)
export fn proven_network_ipv4_is_loopback(addr: IPv4Address) bool {
    const value: i64 = (@as(i64, addr.octets[0]) << 24) |
        (@as(i64, addr.octets[1]) << 16) |
        (@as(i64, addr.octets[2]) << 8) |
        @as(i64, addr.octets[3]);
    const raw = ffi.idris_rts.callRaw1(proven_idris_network_ipv4_is_loopback, .{ .int = value });
    return raw.int != 0;
}

// ============================================================================
// Runtime Initialization (using idris2-zig-ffi)
// ============================================================================

var runtime_initialized: bool = false;

/// Initialize the Proven runtime (includes Idris 2 runtime)
export fn proven_init() i32 {
    if (runtime_initialized) {
        return @intFromEnum(ProvenStatus.ok);
    }

    ffi.init() catch {
        return @intFromEnum(ProvenStatus.err_allocation_failed);
    };

    runtime_initialized = true;
    return @intFromEnum(ProvenStatus.ok);
}

/// Cleanup the Proven runtime
export fn proven_deinit() void {
    if (runtime_initialized) {
        ffi.deinit();
        runtime_initialized = false;
    }
}

/// Check if runtime is initialized
export fn proven_is_initialized() bool {
    return runtime_initialized;
}

/// Get idris2-zig-ffi ABI version for compatibility checking
export fn proven_ffi_abi_version() u32 {
    return ffi.ABI_VERSION;
}

// ============================================================================
// SafeHeader - HTTP header operations that prevent CRLF injection
// ============================================================================

/// Header validation result
pub const HeaderResult = extern struct {
    status: ProvenStatus,
    value: ?[*:0]u8,
    length: usize,
};

/// Check for CRLF injection characters in header value
export fn proven_header_has_crlf(ptr: ?[*]const u8, len: usize) BoolResult {
    return idris2_proven_header_has_crlf(ptr, len);
}

/// Check if header name is valid token per RFC 7230
export fn proven_header_is_valid_name(ptr: ?[*]const u8, len: usize) BoolResult {
    return idris2_proven_header_is_valid_name(ptr, len);
}

/// Check if header name is in dangerous headers list
export fn proven_header_is_dangerous(ptr: ?[*]const u8, len: usize) BoolResult {
    return idris2_proven_header_is_dangerous(ptr, len);
}

/// Create validated header string "Name: Value"
export fn proven_header_render(
    name_ptr: ?[*]const u8,
    name_len: usize,
    value_ptr: ?[*]const u8,
    value_len: usize,
) StringResult {
    return idris2_proven_header_render(name_ptr, name_len, value_ptr, value_len);
}

/// Build Content-Security-Policy header value from directives
export fn proven_header_build_csp(directives_json: ?[*]const u8, json_len: usize) StringResult {
    return idris2_proven_header_build_csp(directives_json, json_len);
}

/// Build HSTS header value
export fn proven_header_build_hsts(max_age: i64, include_subdomains: bool, preload: bool) StringResult {
    return idris2_proven_header_build_hsts(max_age, include_subdomains, preload);
}

// ============================================================================
// SafeCookie - HTTP cookie operations that prevent injection attacks
// ============================================================================

/// SameSite attribute values
pub const SameSite = enum(i32) {
    strict = 0,
    lax = 1,
    none = 2,
};

/// Cookie attributes structure
pub const CookieAttributes = extern struct {
    domain: ?[*:0]const u8,
    domain_len: usize,
    path: ?[*:0]const u8,
    path_len: usize,
    max_age: i64, // -1 means not set
    secure: bool,
    http_only: bool,
    same_site: SameSite,
    partitioned: bool,
};

/// Check for cookie injection characters (semicolon, CR, LF)
export fn proven_cookie_has_injection(ptr: ?[*]const u8, len: usize) BoolResult {
    return idris2_proven_cookie_has_injection(ptr, len);
}

/// Validate cookie name
export fn proven_cookie_validate_name(ptr: ?[*]const u8, len: usize) BoolResult {
    return idris2_proven_cookie_validate_name(ptr, len);
}

/// Validate cookie value
export fn proven_cookie_validate_value(ptr: ?[*]const u8, len: usize) BoolResult {
    return idris2_proven_cookie_validate_value(ptr, len);
}

/// Get cookie prefix type (0=none, 1=__Secure-, 2=__Host-)
export fn proven_cookie_get_prefix(ptr: ?[*]const u8, len: usize) IntResult {
    return idris2_proven_cookie_get_prefix(ptr, len);
}

/// Build Set-Cookie header value
export fn proven_cookie_build_set_cookie(
    name_ptr: ?[*]const u8,
    name_len: usize,
    value_ptr: ?[*]const u8,
    value_len: usize,
    attrs: CookieAttributes,
) StringResult {
    return idris2_proven_cookie_build_set_cookie(name_ptr, name_len, value_ptr, value_len, attrs);
}

/// Build delete cookie header value
export fn proven_cookie_build_delete(name_ptr: ?[*]const u8, name_len: usize) StringResult {
    return idris2_proven_cookie_build_delete(name_ptr, name_len);
}

// ============================================================================
// SafeContentType - Content-Type operations preventing MIME sniffing
// ============================================================================

/// Media type category
pub const MediaCategory = enum(i32) {
    text = 0,
    image = 1,
    audio = 2,
    video = 3,
    application = 4,
    multipart = 5,
    message = 6,
    font = 7,
    model = 8,
    custom = 9,
};

/// Charset encoding
pub const Charset = enum(i32) {
    utf8 = 0,
    utf16le = 1,
    utf16be = 2,
    iso8859_1 = 3,
    ascii = 4,
    windows1252 = 5,
    other = 6,
};

/// Content type parse result
pub const ContentTypeResult = extern struct {
    status: ProvenStatus,
    media_type: ?[*:0]u8,
    media_type_len: usize,
    subtype: ?[*:0]u8,
    subtype_len: usize,
    suffix: ?[*:0]u8,
    suffix_len: usize,
    category: MediaCategory,
    charset: Charset,
    has_charset: bool,
};

/// Parse Content-Type header
export fn proven_content_type_parse(ptr: ?[*]const u8, len: usize) ContentTypeResult {
    return idris2_proven_content_type_parse(ptr, len);
}

/// Free content type result
export fn proven_content_type_free(result: *ContentTypeResult) void {
    if (result.media_type) |m| {
        allocator.free(m[0 .. result.media_type_len + 1]);
    }
    if (result.subtype) |s| {
        allocator.free(s[0 .. result.subtype_len + 1]);
    }
    if (result.suffix) |f| {
        allocator.free(f[0 .. result.suffix_len + 1]);
    }
    result.* = .{
        .status = .ok,
        .media_type = null,
        .media_type_len = 0,
        .subtype = null,
        .subtype_len = 0,
        .suffix = null,
        .suffix_len = 0,
        .category = .custom,
        .charset = .utf8,
        .has_charset = false,
    };
}

/// Check if content type can be sniffed to something dangerous
export fn proven_content_type_can_sniff_dangerous(ptr: ?[*]const u8, len: usize) BoolResult {
    return idris2_proven_content_type_can_sniff_dangerous(ptr, len);
}

/// Render content type to string
export fn proven_content_type_render(
    type_ptr: ?[*]const u8,
    type_len: usize,
    subtype_ptr: ?[*]const u8,
    subtype_len: usize,
    suffix_ptr: ?[*]const u8,
    suffix_len: usize,
    charset: Charset,
    has_charset: bool,
) StringResult {
    return idris2_proven_content_type_render(type_ptr, type_len, subtype_ptr, subtype_len, suffix_ptr, suffix_len, charset, has_charset);
}

/// Check if content type is JSON
export fn proven_content_type_is_json(subtype_ptr: ?[*]const u8, subtype_len: usize, suffix_ptr: ?[*]const u8, suffix_len: usize) BoolResult {
    return idris2_proven_content_type_is_json(subtype_ptr, subtype_len, suffix_ptr, suffix_len);
}

/// Check if content type is XML
export fn proven_content_type_is_xml(subtype_ptr: ?[*]const u8, subtype_len: usize, suffix_ptr: ?[*]const u8, suffix_len: usize) BoolResult {
    return idris2_proven_content_type_is_xml(subtype_ptr, subtype_len, suffix_ptr, suffix_len);
}

// ============================================================================
// Version info
// ============================================================================

export fn proven_version_major() u32 {
    return 0;
}

export fn proven_version_minor() u32 {
    return 9;
}

export fn proven_version_patch() u32 {
    return 0;
}

/// Get module count (41 modules total - added SafeRegistry, SafeDigest, SafeHTTP)
export fn proven_module_count() u32 {
    return 41;
}

// ============================================================================
// SafeRegistry - OCI image reference parsing (formally verified)
// ============================================================================

/// Parsed OCI image reference
pub const ImageReference = extern struct {
    registry: ?[*:0]u8,
    registry_len: usize,
    repository: ?[*:0]u8,
    repository_len: usize,
    tag: ?[*:0]u8,
    tag_len: usize,
    digest: ?[*:0]u8,
    digest_len: usize,
};

/// Result for image reference parsing
pub const ImageRefResult = extern struct {
    status: ProvenStatus,
    reference: ImageReference,
};

/// Parse OCI image reference (e.g., "ghcr.io/user/repo:v1.0")
///
/// Format: [registry/]repository[:tag][@digest]
/// Handles Docker Hub conventions (library/ prefix, docker.io default)
export fn proven_registry_parse(ptr: ?[*]const u8, len: usize) ImageRefResult {
    return idris2_proven_registry_parse(ptr, len);
}

/// Convert image reference to string
export fn proven_registry_to_string(ref: *const ImageReference) StringResult {
    return idris2_proven_registry_to_string(ref);
}

/// Check if image reference looks like it has a registry hostname
export fn proven_registry_has_registry(ref: *const ImageReference) BoolResult {
    return idris2_proven_registry_has_registry(ref);
}

// ============================================================================
// SafeDigest - Cryptographic digest operations (formally verified)
// ============================================================================

/// Hash algorithm types
pub const HashAlgorithm = enum(u8) {
    SHA256 = 0,
    SHA384 = 1,
    SHA512 = 2,
    Blake3 = 3,
};

/// Digest with algorithm and hex value
pub const Digest = extern struct {
    algorithm: HashAlgorithm,
    value: ?[*:0]u8,
    value_len: usize,
};

/// Result for digest operations
pub const DigestResult = extern struct {
    status: ProvenStatus,
    digest: Digest,
};

/// Parse digest string (e.g., "sha256:abc123...")
///
/// Validates algorithm and hex encoding
export fn proven_digest_parse(ptr: ?[*]const u8, len: usize) DigestResult {
    return idris2_proven_digest_parse(ptr, len);
}

/// Constant-time digest comparison (timing-attack resistant)
///
/// Verifies two digests match using constant-time comparison
export fn proven_digest_verify(
    expected: *const Digest,
    actual: *const Digest,
) BoolResult {
    return idris2_proven_digest_verify(expected, actual);
}

/// Convert digest to string (algorithm:hex)
export fn proven_digest_to_string(digest: *const Digest) StringResult {
    return idris2_proven_digest_to_string(digest);
}

// ============================================================================
// SafeHTTP - HTTP URL encoding and header parsing (formally verified)
// ============================================================================

/// URL-encode a string (RFC 3986 percent encoding)
///
/// Unreserved chars (A-Za-z0-9-._~) pass through, others become %XX
export fn proven_http_url_encode(ptr: ?[*]const u8, len: usize) StringResult {
    return idris2_proven_http_url_encode(ptr, len);
}

/// URL-decode a percent-encoded string
export fn proven_http_url_decode(ptr: ?[*]const u8, len: usize) StringResult {
    return idris2_proven_http_url_decode(ptr, len);
}

/// WWW-Authenticate challenge components
pub const AuthChallenge = extern struct {
    scheme: ?[*:0]u8,
    scheme_len: usize,
    realm: ?[*:0]u8,
    realm_len: usize,
    service: ?[*:0]u8,
    service_len: usize,
    scope: ?[*:0]u8,
    scope_len: usize,
};

/// Result for auth challenge parsing
pub const AuthChallengeResult = extern struct {
    status: ProvenStatus,
    challenge: AuthChallenge,
};

/// Parse WWW-Authenticate header (for Docker Registry v2 OAuth2)
///
/// Format: Bearer realm="...",service="...",scope="..."
export fn proven_http_parse_www_authenticate(ptr: ?[*]const u8, len: usize) AuthChallengeResult {
    return idris2_proven_http_parse_www_authenticate(ptr, len);
}

// ============================================================================
// SafeHex - Hexadecimal encoding/decoding
// ============================================================================

/// Hex decode result with bytes
pub const HexDecodeResult = extern struct {
    status: ProvenStatus,
    data: ?[*]u8,
    length: usize,
};

/// Hex encode bytes to string
export fn proven_hex_encode(ptr: ?[*]const u8, len: usize, uppercase: bool) StringResult {
    return idris2_proven_hex_encode(ptr, len, uppercase);
}

/// Hex decode string to bytes
export fn proven_hex_decode(ptr: ?[*]const u8, len: usize) HexDecodeResult {
    return idris2_proven_hex_decode(ptr, len);
}

/// Free hex decode result
export fn proven_hex_free(result: *HexDecodeResult) void {
    if (result.data) |d| {
        allocator.free(d[0..result.length]);
    }
    result.* = .{ .status = .ok, .data = null, .length = 0 };
}

// ============================================================================
// SafeUUID - UUID generation and validation
// ============================================================================

/// UUID structure (128 bits)
pub const UUID = extern struct {
    bytes: [16]u8,
};

/// UUID result
pub const UUIDResult = extern struct {
    status: ProvenStatus,
    uuid: UUID,
};

/// Generate UUID v4 (random)
export fn proven_uuid_v4() UUIDResult {
    return idris2_proven_uuid_v4();
}

/// Format UUID as string (36 chars with hyphens)
export fn proven_uuid_to_string(uuid: UUID) StringResult {
    return idris2_proven_uuid_to_string(uuid);
}

/// Parse UUID from string
export fn proven_uuid_parse(ptr: ?[*]const u8, len: usize) UUIDResult {
    return idris2_proven_uuid_parse(ptr, len);
}

/// Check if UUID is nil (all zeros)
export fn proven_uuid_is_nil(uuid: UUID) bool {
    return idris2_proven_uuid_is_nil(uuid);
}

/// Get UUID version
export fn proven_uuid_version(uuid: UUID) u8 {
    return idris2_proven_uuid_version(uuid);
}

// ============================================================================
// SafeCurrency - Monetary values with ISO 4217 codes
// ============================================================================

/// Currency result with minor units (cents, etc.)
pub const CurrencyResult = extern struct {
    status: ProvenStatus,
    amount_minor: i64, // Amount in minor units (cents)
    currency_code: [3]u8,
    decimal_places: u8,
};

/// Parse currency amount (e.g., "USD 123.45" or "123.45 EUR")
export fn proven_currency_parse(ptr: ?[*]const u8, len: usize) CurrencyResult {
    return idris2_proven_currency_parse(ptr, len);
}

/// Format currency amount
export fn proven_currency_format(amount_minor: i64, code: [3]u8, decimal_places: u8) StringResult {
    return idris2_proven_currency_format(amount_minor, code, decimal_places);
}

// ============================================================================
// SafePhone - E.164 phone number handling
// ============================================================================

/// Phone number result
pub const PhoneResult = extern struct {
    status: ProvenStatus,
    country_code: u16,
    national_number: u64,
    is_valid: bool,
};

/// Parse phone number to E.164 format
export fn proven_phone_parse(ptr: ?[*]const u8, len: usize) PhoneResult {
    return idris2_proven_phone_parse(ptr, len);
}

/// Format phone number as E.164
export fn proven_phone_format_e164(country_code: u16, national_number: u64) StringResult {
    return idris2_proven_phone_format_e164(country_code, national_number);
}

// ============================================================================
// SafeJson - JSON validation and parsing status
// ============================================================================

/// JSON value type
pub const JsonType = enum(i32) {
    null_ = 0,
    bool_ = 1,
    number = 2,
    string = 3,
    array = 4,
    object = 5,
    invalid = -1,
};

/// Check if string is valid JSON
export fn proven_json_is_valid(ptr: ?[*]const u8, len: usize) BoolResult {
    return idrisCallBool(proven_idris_json_is_valid, ptr, len);
}

/// Get JSON value type at root level
export fn proven_json_get_type(ptr: ?[*]const u8, len: usize) JsonType {
    const result = idrisCallInt(proven_idris_json_get_type, ptr, len);
    if (result.status != .ok) return .invalid;
    return switch (result.value) {
        0 => .null_,
        1 => .bool_,
        2 => .number,
        3 => .string,
        4 => .array,
        5 => .object,
        else => .invalid,
    };
}

// ============================================================================
// SafeDateTime - ISO 8601 date/time handling
// ============================================================================

/// DateTime components
pub const DateTime = extern struct {
    year: i32,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
    nanosecond: u32,
    tz_offset_minutes: i16, // 0 for UTC, negative for west of UTC
};

/// DateTime result
pub const DateTimeResult = extern struct {
    status: ProvenStatus,
    datetime: DateTime,
};

/// Parse ISO 8601 date string
export fn proven_datetime_parse(ptr: ?[*]const u8, len: usize) DateTimeResult {
    return idris2_proven_datetime_parse(ptr, len);
}

/// Format DateTime as ISO 8601
export fn proven_datetime_format_iso8601(dt: DateTime) StringResult {
    return idris2_proven_datetime_format_iso8601(dt);
}

/// Check if year is leap year
export fn proven_datetime_is_leap_year(year: i32) bool {
    return idris2_proven_datetime_is_leap_year(year);
}

/// Get days in month
export fn proven_datetime_days_in_month(year: i32, month: u8) u8 {
    return idris2_proven_datetime_days_in_month(year, month);
}

// ============================================================================
// SafeFloat - Safe floating-point operations
// ============================================================================

/// Float result
pub const FloatResult = extern struct {
    status: ProvenStatus,
    value: f64,
};

/// Safe floating-point division
export fn proven_float_div(a: f64, b: f64) FloatResult {
    return idris2_proven_float_div(a, b);
}

/// Check if float is finite (not NaN or Inf)
export fn proven_float_is_finite(x: f64) bool {
    return idris2_proven_float_is_finite(x);
}

/// Check if float is NaN
export fn proven_float_is_nan(x: f64) bool {
    return idris2_proven_float_is_nan(x);
}

/// Safe square root (returns error for negative)
export fn proven_float_sqrt(x: f64) FloatResult {
    return idris2_proven_float_sqrt(x);
}

/// Safe natural logarithm
export fn proven_float_ln(x: f64) FloatResult {
    return idris2_proven_float_ln(x);
}

// ============================================================================
// SafeVersion - Semantic versioning
// ============================================================================

/// Semantic version
pub const SemanticVersion = extern struct {
    major: u32,
    minor: u32,
    patch: u32,
    prerelease_len: usize,
    prerelease: ?[*:0]u8,
};

/// Version result
pub const VersionResult = extern struct {
    status: ProvenStatus,
    version: SemanticVersion,
};

/// Parse semantic version string
export fn proven_version_parse(ptr: ?[*]const u8, len: usize) VersionResult {
    return idris2_proven_version_parse(ptr, len);
}

/// Compare two semantic versions
export fn proven_version_compare(a: SemanticVersion, b: SemanticVersion) i32 {
    return idris2_proven_version_compare(a, b);
}

/// Free version result
export fn proven_version_free(version: *SemanticVersion) void {
    if (version.prerelease) |p| {
        allocator.free(p[0 .. version.prerelease_len + 1]);
    }
    version.* = .{ .major = 0, .minor = 0, .patch = 0, .prerelease_len = 0, .prerelease = null };
}

// ============================================================================
// SafeGeo - Geographic coordinate operations
// ============================================================================

/// Geographic coordinate
pub const GeoCoordinate = extern struct {
    latitude: f64, // -90 to 90
    longitude: f64, // -180 to 180
};

/// Geo result
pub const GeoResult = extern struct {
    status: ProvenStatus,
    coordinate: GeoCoordinate,
};

/// Validate and normalize geographic coordinate
export fn proven_geo_validate(lat: f64, lon: f64) GeoResult {
    return idris2_proven_geo_validate(lat, lon);
}

/// Calculate distance between two points (Haversine formula, returns meters)
export fn proven_geo_distance(a: GeoCoordinate, b: GeoCoordinate) FloatResult {
    return idris2_proven_geo_distance(a, b);
}

/// Check if coordinate is in bounding box
export fn proven_geo_in_bounds(coord: GeoCoordinate, min_lat: f64, max_lat: f64, min_lon: f64, max_lon: f64) bool {
    return idris2_proven_geo_in_bounds(coord, min_lat, max_lat, min_lon, max_lon);
}

// ============================================================================
// SafeChecksum - CRC and hash verification
// ============================================================================

/// Calculate CRC32
export fn proven_checksum_crc32(ptr: ?[*]const u8, len: usize) IntResult {
    return idris2_proven_checksum_crc32(ptr, len);
}

/// Verify CRC32 matches expected
export fn proven_checksum_verify_crc32(ptr: ?[*]const u8, len: usize, expected: u32) BoolResult {
    return idris2_proven_checksum_verify_crc32(ptr, len, expected);
}

// ============================================================================
// SafeProbability - Probability values clamped to [0, 1]
// ============================================================================

/// Create probability (clamped to 0-1)
export fn proven_probability_create(value: f64) f64 {
    return idris2_proven_probability_create(value);
}

/// Multiply probabilities (independent events)
export fn proven_probability_and(a: f64, b: f64) f64 {
    return idris2_proven_probability_and(a, b);
}

/// Add probabilities (mutually exclusive events)
export fn proven_probability_or_exclusive(a: f64, b: f64) f64 {
    return idris2_proven_probability_or_exclusive(a, b);
}

/// Complement (not event)
export fn proven_probability_not(p: f64) f64 {
    return idris2_proven_probability_not(p);
}

// ============================================================================
// SafeCalculator - Expression evaluation
// ============================================================================

/// Calculate expression result
export fn proven_calculator_eval(ptr: ?[*]const u8, len: usize) FloatResult {
    return idris2_proven_calculator_eval(ptr, len);
}

// ============================================================================
// SafeBuffer - Bounded buffer operations
// ============================================================================

/// Bounded buffer structure
pub const BoundedBuffer = extern struct {
    data: [*]u8,
    capacity: usize,
    length: usize,
};

/// Buffer result
pub const BufferResult = extern struct {
    status: ProvenStatus,
    buffer: ?*BoundedBuffer,
};

/// Create bounded buffer
export fn proven_buffer_create(capacity: usize) BufferResult {
    return idris2_proven_buffer_create(capacity);
}

/// Append to buffer (with bounds checking)
export fn proven_buffer_append(buffer: ?*BoundedBuffer, ptr: ?[*]const u8, len: usize) ProvenStatus {
    return idris2_proven_buffer_append(buffer, ptr, len);
}

/// Get buffer contents
export fn proven_buffer_get(buffer: ?*BoundedBuffer, out_ptr: ?*[*]const u8, out_len: ?*usize) ProvenStatus {
    return idris2_proven_buffer_get(buffer, out_ptr, out_len);
}

/// Free buffer
export fn proven_buffer_free(buffer: ?*BoundedBuffer) void {
    if (buffer) |buf| {
        allocator.free(buf.data[0..buf.capacity]);
        allocator.destroy(buf);
    }
}

// ============================================================================
// SafeRateLimiter - Token bucket rate limiting
// ============================================================================

/// Rate limiter state
pub const RateLimiter = extern struct {
    tokens: f64,
    capacity: f64,
    refill_rate: f64, // tokens per second
    last_refill: i64, // timestamp in milliseconds
};

/// Create rate limiter
export fn proven_rate_limiter_create(capacity: f64, refill_rate: f64) ?*RateLimiter {
    return idris2_proven_rate_limiter_create(capacity, refill_rate);
}

/// Try to consume tokens
export fn proven_rate_limiter_try_acquire(limiter: ?*RateLimiter, tokens: f64) bool {
    return idris2_proven_rate_limiter_try_acquire(limiter, tokens);
}

/// Free rate limiter
export fn proven_rate_limiter_free(limiter: ?*RateLimiter) void {
    if (limiter) |rl| {
        allocator.destroy(rl);
    }
}

// ============================================================================
// SafeCircuitBreaker - Fault tolerance
// ============================================================================

/// Circuit breaker state
pub const CircuitState = enum(i32) {
    closed = 0, // Normal operation
    open = 1, // Failing, reject requests
    half_open = 2, // Testing recovery
};

/// Circuit breaker
pub const CircuitBreaker = extern struct {
    state: CircuitState,
    failure_count: u32,
    failure_threshold: u32,
    success_count: u32,
    success_threshold: u32,
    last_failure: i64,
    timeout_ms: i64,
};

/// Create circuit breaker
export fn proven_circuit_breaker_create(failure_threshold: u32, success_threshold: u32, timeout_ms: i64) ?*CircuitBreaker {
    return idris2_proven_circuit_breaker_create(failure_threshold, success_threshold, timeout_ms);
}

/// Check if request should be allowed
export fn proven_circuit_breaker_allow(cb: ?*CircuitBreaker) bool {
    return idris2_proven_circuit_breaker_allow(cb);
}

/// Record success
export fn proven_circuit_breaker_success(cb: ?*CircuitBreaker) void {
    return idris2_proven_circuit_breaker_success(cb);
}

/// Record failure
export fn proven_circuit_breaker_failure(cb: ?*CircuitBreaker) void {
    return idris2_proven_circuit_breaker_failure(cb);
}

/// Get circuit state
export fn proven_circuit_breaker_state(cb: ?*CircuitBreaker) CircuitState {
    return idris2_proven_circuit_breaker_state(cb);
}

/// Free circuit breaker
export fn proven_circuit_breaker_free(cb: ?*CircuitBreaker) void {
    if (cb) |breaker| {
        allocator.destroy(breaker);
    }
}

// ============================================================================
// SafePassword - Password validation
// ============================================================================

/// Password strength level
pub const PasswordStrength = enum(i32) {
    very_weak = 0,
    weak = 1,
    fair = 2,
    strong = 3,
    very_strong = 4,
};

/// Password validation result
pub const PasswordResult = extern struct {
    strength: PasswordStrength,
    has_lowercase: bool,
    has_uppercase: bool,
    has_digit: bool,
    has_special: bool,
    length: usize,
};

/// Validate password strength
export fn proven_password_validate(ptr: ?[*]const u8, len: usize) PasswordResult {
    return idris2_proven_password_validate(ptr, len);
}

/// Check if password is in common passwords list
export fn proven_password_is_common(ptr: ?[*]const u8, len: usize) bool {
    return idris2_proven_password_is_common(ptr, len);
}

// ============================================================================
// SafeColor - Color space conversions
// ============================================================================

/// RGB color
pub const RGBColor = extern struct {
    r: u8,
    g: u8,
    b: u8,
};

/// HSL color
pub const HSLColor = extern struct {
    h: f64, // 0-360
    s: f64, // 0-1
    l: f64, // 0-1
};

/// Color parse result (named type for extern fn compatibility)
pub const ColorParseResult = extern struct {
    status: ProvenStatus,
    color: RGBColor,
};

/// Parse hex color string (#RRGGBB or #RGB)
export fn proven_color_parse_hex(ptr: ?[*]const u8, len: usize) ColorParseResult {
    return idris2_proven_color_parse_hex(ptr, len);
}

/// Convert RGB to HSL
export fn proven_color_rgb_to_hsl(rgb: RGBColor) HSLColor {
    return idris2_proven_color_rgb_to_hsl(rgb);
}

/// Format RGB as hex string
export fn proven_color_to_hex(rgb: RGBColor) StringResult {
    return idris2_proven_color_to_hex(rgb);
}

// ============================================================================
// SafeAngle - Angle conversions and normalization
// ============================================================================

/// Convert degrees to radians
export fn proven_angle_deg_to_rad(degrees: f64) f64 {
    return idris2_proven_angle_deg_to_rad(degrees);
}

/// Convert radians to degrees
export fn proven_angle_rad_to_deg(radians: f64) f64 {
    return idris2_proven_angle_rad_to_deg(radians);
}

/// Normalize angle to 0-360 degrees
export fn proven_angle_normalize_degrees(degrees: f64) f64 {
    return idris2_proven_angle_normalize_degrees(degrees);
}

/// Normalize angle to 0-2pi radians
export fn proven_angle_normalize_radians(radians: f64) f64 {
    return idris2_proven_angle_normalize_radians(radians);
}

// ============================================================================
// SafeUnit - Physical unit conversions
// ============================================================================

/// Length unit
pub const LengthUnit = enum(i32) {
    meters = 0,
    kilometers = 1,
    centimeters = 2,
    millimeters = 3,
    feet = 4,
    inches = 5,
    miles = 6,
    yards = 7,
};

/// Convert length between units
export fn proven_unit_convert_length(value: f64, from: LengthUnit, to: LengthUnit) FloatResult {
    return idris2_proven_unit_convert_length(value, from, to);
}

/// Temperature unit
pub const TempUnit = enum(i32) {
    celsius = 0,
    fahrenheit = 1,
    kelvin = 2,
};

/// Convert temperature between units
export fn proven_unit_convert_temp(value: f64, from: TempUnit, to: TempUnit) FloatResult {
    return idris2_proven_unit_convert_temp(value, from, to);
}

// ============================================================================
// SafeQueue - Bounded FIFO queue
// ============================================================================

/// Bounded queue
pub const BoundedQueue = extern struct {
    data: [*]i64,
    capacity: usize,
    head: usize,
    tail: usize,
    count: usize,
};

/// Create bounded queue
export fn proven_queue_create(capacity: usize) ?*BoundedQueue {
    return idris2_proven_queue_create(capacity);
}

/// Push to queue (returns false if full)
export fn proven_queue_push(queue: ?*BoundedQueue, value: i64) bool {
    return idris2_proven_queue_push(queue, value);
}

/// Pop from queue
export fn proven_queue_pop(queue: ?*BoundedQueue) IntResult {
    return idris2_proven_queue_pop(queue);
}

/// Get queue size
export fn proven_queue_size(queue: ?*BoundedQueue) usize {
    return idris2_proven_queue_size(queue);
}

/// Free queue
export fn proven_queue_free(queue: ?*BoundedQueue) void {
    if (queue) |q| {
        allocator.free(q.data[0..q.capacity]);
        allocator.destroy(q);
    }
}

// ============================================================================
// SafeBloom - Probabilistic set membership
// ============================================================================

/// Bloom filter
pub const BloomFilter = extern struct {
    bits: [*]u8,
    bit_count: usize,
    hash_count: u32,
};

/// Create bloom filter
export fn proven_bloom_create(expected_elements: usize, false_positive_rate: f64) ?*BloomFilter {
    return idris2_proven_bloom_create(expected_elements, false_positive_rate);
}

/// Add element to bloom filter
export fn proven_bloom_add(filter: ?*BloomFilter, ptr: ?[*]const u8, len: usize) void {
    return idris2_proven_bloom_add(filter, ptr, len);
}

/// Check if element might be in filter
export fn proven_bloom_contains(filter: ?*BloomFilter, ptr: ?[*]const u8, len: usize) bool {
    return idris2_proven_bloom_contains(filter, ptr, len);
}

/// Free bloom filter
export fn proven_bloom_free(filter: ?*BloomFilter) void {
    if (filter) |f| {
        allocator.free(f.bits[0..(f.bit_count + 7) / 8]);
        allocator.destroy(f);
    }
}

// ============================================================================
// SafeRetry - Exponential backoff
// ============================================================================

/// Retry configuration
pub const RetryConfig = extern struct {
    max_attempts: u32,
    base_delay_ms: u64,
    max_delay_ms: u64,
    multiplier: f64,
};

/// Calculate delay for attempt (with jitter)
export fn proven_retry_delay(config: RetryConfig, attempt: u32) u64 {
    return idris2_proven_retry_delay(config, attempt);
}

/// Check if should retry
export fn proven_retry_should_retry(config: RetryConfig, attempt: u32) bool {
    return idris2_proven_retry_should_retry(config, attempt);
}

// ============================================================================
// SafeMonotonic - Monotonically increasing sequences
// ============================================================================

/// Monotonic counter
pub const MonotonicCounter = extern struct {
    value: u64,
    max_value: u64,
};

/// Create monotonic counter
export fn proven_monotonic_create(initial: u64, max_value: u64) ?*MonotonicCounter {
    return idris2_proven_monotonic_create(initial, max_value);
}

/// Get next value (atomic-like increment)
export fn proven_monotonic_next(counter: ?*MonotonicCounter) IntResult {
    return idris2_proven_monotonic_next(counter);
}

/// Free counter
export fn proven_monotonic_free(counter: ?*MonotonicCounter) void {
    if (counter) |c| {
        allocator.destroy(c);
    }
}

// ============================================================================
// SafeStateMachine - Type-safe state transitions
// ============================================================================

/// State machine
pub const StateMachine = extern struct {
    current_state: u32,
    state_count: u32,
    transitions: [*]u8, // Packed transition matrix
};

/// Create state machine
export fn proven_state_machine_create(state_count: u32, initial_state: u32) ?*StateMachine {
    return idris2_proven_state_machine_create(state_count, initial_state);
}

/// Allow transition from state A to state B
export fn proven_state_machine_allow(sm: ?*StateMachine, from: u32, to: u32) bool {
    return idris2_proven_state_machine_allow(sm, from, to);
}

/// Try to transition to new state
export fn proven_state_machine_transition(sm: ?*StateMachine, to: u32) bool {
    return idris2_proven_state_machine_transition(sm, to);
}

/// Get current state
export fn proven_state_machine_state(sm: ?*StateMachine) u32 {
    return idris2_proven_state_machine_state(sm);
}

/// Free state machine
export fn proven_state_machine_free(sm: ?*StateMachine) void {
    if (sm) |machine| {
        allocator.free(machine.transitions[0 .. machine.state_count * machine.state_count]);
        allocator.destroy(machine);
    }
}

// ============================================================================
// SafeTensor - Basic tensor operations
// ============================================================================

/// Tensor (2D matrix)
pub const Tensor2D = extern struct {
    data: [*]f64,
    rows: usize,
    cols: usize,
};

/// Create 2D tensor
export fn proven_tensor_create(rows: usize, cols: usize) ?*Tensor2D {
    return idris2_proven_tensor_create(rows, cols);
}

/// Set tensor value
export fn proven_tensor_set(tensor: ?*Tensor2D, row: usize, col: usize, value: f64) ProvenStatus {
    return idris2_proven_tensor_set(tensor, row, col, value);
}

/// Get tensor value
export fn proven_tensor_get(tensor: ?*Tensor2D, row: usize, col: usize) FloatResult {
    return idris2_proven_tensor_get(tensor, row, col);
}

/// Matrix multiplication
export fn proven_tensor_matmul(a: ?*Tensor2D, b: ?*Tensor2D) ?*Tensor2D {
    return idris2_proven_tensor_matmul(a, b);
}

/// Free tensor
export fn proven_tensor_free(tensor: ?*Tensor2D) void {
    if (tensor) |t| {
        allocator.free(t.data[0 .. t.rows * t.cols]);
        allocator.destroy(t);
    }
}

// ============================================================================
// SafeML - Machine learning utilities
// ============================================================================

/// Softmax normalization
export fn proven_ml_softmax(input: ?[*]const f64, output: ?[*]f64, len: usize) ProvenStatus {
    return idris2_proven_ml_softmax(input, output, len);
}

/// Sigmoid function
export fn proven_ml_sigmoid(x: f64) f64 {
    return idris2_proven_ml_sigmoid(x);
}

/// ReLU function
export fn proven_ml_relu(x: f64) f64 {
    return idris2_proven_ml_relu(x);
}

/// Leaky ReLU
export fn proven_ml_leaky_relu(x: f64, alpha: f64) f64 {
    return idris2_proven_ml_leaky_relu(x, alpha);
}

/// Clamp value to range
export fn proven_ml_clamp(x: f64, min_val: f64, max_val: f64) f64 {
    return idris2_proven_ml_clamp(x, min_val, max_val);
}

// ============================================================================
// SafeLRU - Least Recently Used cache (simplified)
// ============================================================================

/// LRU entry
pub const LRUEntry = extern struct {
    key: u64,
    value: i64,
    prev: usize,
    next: usize,
    valid: bool,
};

/// LRU cache
pub const LRUCache = extern struct {
    entries: [*]LRUEntry,
    capacity: usize,
    head: usize,
    tail: usize,
    count: usize,
};

/// Create LRU cache
export fn proven_lru_create(capacity: usize) ?*LRUCache {
    return idris2_proven_lru_create(capacity);
}

/// Get from LRU cache
export fn proven_lru_get(cache: ?*LRUCache, key: u64) IntResult {
    return idris2_proven_lru_get(cache, key);
}

/// Put in LRU cache
export fn proven_lru_put(cache: ?*LRUCache, key: u64, value: i64) ProvenStatus {
    return idris2_proven_lru_put(cache, key, value);
}

/// Free LRU cache
export fn proven_lru_free(cache: ?*LRUCache) void {
    if (cache) |c| {
        allocator.free(c.entries[0..c.capacity]);
        allocator.destroy(c);
    }
}

// ============================================================================
// SafeGraph - Graph operations with cycle detection
// ============================================================================

/// Graph (adjacency list, simplified)
pub const Graph = extern struct {
    edges: [*]u8, // Packed adjacency matrix
    node_count: usize,
};

/// Create graph
export fn proven_graph_create(node_count: usize) ?*Graph {
    return idris2_proven_graph_create(node_count);
}

/// Add edge
export fn proven_graph_add_edge(graph: ?*Graph, from: usize, to: usize) ProvenStatus {
    return idris2_proven_graph_add_edge(graph, from, to);
}

/// Check if edge exists
export fn proven_graph_has_edge(graph: ?*Graph, from: usize, to: usize) bool {
    return idris2_proven_graph_has_edge(graph, from, to);
}

/// Free graph
export fn proven_graph_free(graph: ?*Graph) void {
    if (graph) |g| {
        const edge_bytes = (g.node_count * g.node_count + 7) / 8;
        allocator.free(g.edges[0..edge_bytes]);
        allocator.destroy(g);
    }
}

// ============================================================================
// Tests
// ============================================================================

test "math_div" {
    const r1 = proven_math_div(10, 2);
    try std.testing.expectEqual(ProvenStatus.ok, r1.status);
    try std.testing.expectEqual(@as(i64, 5), r1.value);

    const r2 = proven_math_div(10, 0);
    try std.testing.expectEqual(ProvenStatus.err_division_by_zero, r2.status);
}

test "math_add_checked" {
    const r1 = proven_math_add_checked(5, 3);
    try std.testing.expectEqual(ProvenStatus.ok, r1.status);
    try std.testing.expectEqual(@as(i64, 8), r1.value);

    const max = std.math.maxInt(i64);
    const r2 = proven_math_add_checked(max, 1);
    try std.testing.expectEqual(ProvenStatus.err_overflow, r2.status);
}

test "string_is_valid_utf8" {
    const valid = "Hello, World!";
    const r1 = proven_string_is_valid_utf8(valid.ptr, valid.len);
    try std.testing.expectEqual(ProvenStatus.ok, r1.status);
    try std.testing.expect(r1.value);

    const invalid = [_]u8{ 0xFF, 0xFE };
    const r2 = proven_string_is_valid_utf8(&invalid, invalid.len);
    try std.testing.expectEqual(ProvenStatus.ok, r2.status);
    try std.testing.expect(!r2.value);
}

test "path_has_traversal" {
    const safe = "path/to/file.txt";
    const r1 = proven_path_has_traversal(safe.ptr, safe.len);
    try std.testing.expect(!r1.value);

    const unsafe_path = "../../../etc/passwd";
    const r2 = proven_path_has_traversal(unsafe_path.ptr, unsafe_path.len);
    try std.testing.expect(r2.value);
}

test "email_is_valid" {
    const valid = "user@example.com";
    const r1 = proven_email_is_valid(valid.ptr, valid.len);
    try std.testing.expect(r1.value);

    const invalid = "not-an-email";
    const r2 = proven_email_is_valid(invalid.ptr, invalid.len);
    try std.testing.expect(!r2.value);
}

test "network_parse_ipv4" {
    const ip = "192.168.1.1";
    const r = proven_network_parse_ipv4(ip.ptr, ip.len);
    try std.testing.expectEqual(ProvenStatus.ok, r.status);
    try std.testing.expectEqual(@as(u8, 192), r.address.octets[0]);
    try std.testing.expectEqual(@as(u8, 168), r.address.octets[1]);
    try std.testing.expectEqual(@as(u8, 1), r.address.octets[2]);
    try std.testing.expectEqual(@as(u8, 1), r.address.octets[3]);
    try std.testing.expect(proven_network_ipv4_is_private(r.address));
}

test "header_has_crlf" {
    const safe = "application/json";
    const r1 = proven_header_has_crlf(safe.ptr, safe.len);
    try std.testing.expect(!r1.value);

    const unsafe_header = "text\r\nX-Injected: evil";
    const r2 = proven_header_has_crlf(unsafe_header.ptr, unsafe_header.len);
    try std.testing.expect(r2.value);
}

test "header_is_valid_name" {
    const valid = "Content-Type";
    const r1 = proven_header_is_valid_name(valid.ptr, valid.len);
    try std.testing.expect(r1.value);

    const invalid = "Content:Type";
    const r2 = proven_header_is_valid_name(invalid.ptr, invalid.len);
    try std.testing.expect(!r2.value);
}

test "header_is_dangerous" {
    const dangerous = "Host";
    const r1 = proven_header_is_dangerous(dangerous.ptr, dangerous.len);
    try std.testing.expect(r1.value);

    const safe = "X-Custom-Header";
    const r2 = proven_header_is_dangerous(safe.ptr, safe.len);
    try std.testing.expect(!r2.value);
}

test "header_build_hsts" {
    const r = proven_header_build_hsts(31536000, true, true);
    try std.testing.expectEqual(ProvenStatus.ok, r.status);
    defer proven_free_string(r.value);

    const expected = "max-age=31536000; includeSubDomains; preload";
    try std.testing.expectEqualStrings(expected, r.value.?[0..r.length]);
}

test "cookie_has_injection" {
    const safe = "session_token";
    const r1 = proven_cookie_has_injection(safe.ptr, safe.len);
    try std.testing.expect(!r1.value);

    const unsafe_cookie = "value;injected=true";
    const r2 = proven_cookie_has_injection(unsafe_cookie.ptr, unsafe_cookie.len);
    try std.testing.expect(r2.value);
}

test "cookie_get_prefix" {
    const host = "__Host-session";
    const r1 = proven_cookie_get_prefix(host.ptr, host.len);
    try std.testing.expectEqual(@as(i64, 2), r1.value);

    const secure = "__Secure-token";
    const r2 = proven_cookie_get_prefix(secure.ptr, secure.len);
    try std.testing.expectEqual(@as(i64, 1), r2.value);

    const normal = "regular";
    const r3 = proven_cookie_get_prefix(normal.ptr, normal.len);
    try std.testing.expectEqual(@as(i64, 0), r3.value);
}

test "content_type_parse" {
    const ct = "application/json; charset=utf-8";
    const r = proven_content_type_parse(ct.ptr, ct.len);
    try std.testing.expectEqual(ProvenStatus.ok, r.status);
    defer {
        var result = r;
        proven_content_type_free(&result);
    }

    try std.testing.expectEqualStrings("application", r.media_type.?[0..r.media_type_len]);
    try std.testing.expectEqualStrings("json", r.subtype.?[0..r.subtype_len]);
    try std.testing.expectEqual(MediaCategory.application, r.category);
    try std.testing.expect(r.has_charset);
    try std.testing.expectEqual(Charset.utf8, r.charset);
}

test "content_type_can_sniff_dangerous" {
    const dangerous = "text/plain";
    const r1 = proven_content_type_can_sniff_dangerous(dangerous.ptr, dangerous.len);
    try std.testing.expect(r1.value);

    const safe = "application/json";
    const r2 = proven_content_type_can_sniff_dangerous(safe.ptr, safe.len);
    try std.testing.expect(!r2.value);
}

test "hex_encode_decode" {
    const input = "Hello";
    const encoded = proven_hex_encode(input.ptr, input.len, false);
    try std.testing.expectEqual(ProvenStatus.ok, encoded.status);
    defer proven_free_string(encoded.value);

    try std.testing.expectEqualStrings("48656c6c6f", encoded.value.?[0..encoded.length]);

    const decoded = proven_hex_decode(encoded.value.?, encoded.length);
    try std.testing.expectEqual(ProvenStatus.ok, decoded.status);
    defer {
        var result = decoded;
        proven_hex_free(&result);
    }

    try std.testing.expectEqualStrings("Hello", decoded.data.?[0..decoded.length]);
}

test "uuid_v4" {
    const result = proven_uuid_v4();
    try std.testing.expectEqual(ProvenStatus.ok, result.status);
    try std.testing.expectEqual(@as(u8, 4), proven_uuid_version(result.uuid));
    try std.testing.expect(!proven_uuid_is_nil(result.uuid));
}

test "datetime_parse" {
    const input = "2024-12-15T10:30:00Z";
    const result = proven_datetime_parse(input.ptr, input.len);
    try std.testing.expectEqual(ProvenStatus.ok, result.status);
    try std.testing.expectEqual(@as(i32, 2024), result.datetime.year);
    try std.testing.expectEqual(@as(u8, 12), result.datetime.month);
    try std.testing.expectEqual(@as(u8, 15), result.datetime.day);
    try std.testing.expectEqual(@as(u8, 10), result.datetime.hour);
    try std.testing.expectEqual(@as(u8, 30), result.datetime.minute);
}

test "calculator_eval" {
    const r1 = proven_calculator_eval("2 + 3 * 4".ptr, "2 + 3 * 4".len);
    try std.testing.expectEqual(ProvenStatus.ok, r1.status);
    try std.testing.expectApproxEqAbs(@as(f64, 14), r1.value, 0.001);

    const r2 = proven_calculator_eval("(2 + 3) * 4".ptr, "(2 + 3) * 4".len);
    try std.testing.expectEqual(ProvenStatus.ok, r2.status);
    try std.testing.expectApproxEqAbs(@as(f64, 20), r2.value, 0.001);
}

test "password_validate" {
    const weak = proven_password_validate("abc".ptr, 3);
    try std.testing.expectEqual(PasswordStrength.very_weak, weak.strength);

    const strong = proven_password_validate("MyP@ssw0rd!".ptr, 11);
    try std.testing.expect(strong.has_lowercase);
    try std.testing.expect(strong.has_uppercase);
    try std.testing.expect(strong.has_digit);
    try std.testing.expect(strong.has_special);
}

test "geo_distance" {
    // New York to Los Angeles (approximately 3935 km)
    const ny = GeoCoordinate{ .latitude = 40.7128, .longitude = -74.0060 };
    const la = GeoCoordinate{ .latitude = 34.0522, .longitude = -118.2437 };

    const result = proven_geo_distance(ny, la);
    try std.testing.expectEqual(ProvenStatus.ok, result.status);
    // Should be approximately 3935 km = 3935000 m
    try std.testing.expect(result.value > 3900000 and result.value < 4000000);
}

test "rate_limiter" {
    const limiter = proven_rate_limiter_create(10, 1);
    try std.testing.expect(limiter != null);
    defer proven_rate_limiter_free(limiter);

    // Should allow first 10 requests
    var i: usize = 0;
    while (i < 10) : (i += 1) {
        try std.testing.expect(proven_rate_limiter_try_acquire(limiter, 1));
    }

    // Should deny 11th request (no time has passed)
    try std.testing.expect(!proven_rate_limiter_try_acquire(limiter, 1));
}

test "circuit_breaker" {
    const cb = proven_circuit_breaker_create(3, 2, 1000);
    try std.testing.expect(cb != null);
    defer proven_circuit_breaker_free(cb);

    // Initial state should be closed
    try std.testing.expectEqual(CircuitState.closed, proven_circuit_breaker_state(cb));
    try std.testing.expect(proven_circuit_breaker_allow(cb));

    // Record 3 failures to trip the circuit
    proven_circuit_breaker_failure(cb);
    proven_circuit_breaker_failure(cb);
    proven_circuit_breaker_failure(cb);

    try std.testing.expectEqual(CircuitState.open, proven_circuit_breaker_state(cb));
}

test "color_parse_hex" {
    const r1 = proven_color_parse_hex("#FF0000".ptr, 7);
    try std.testing.expectEqual(ProvenStatus.ok, r1.status);
    try std.testing.expectEqual(@as(u8, 255), r1.color.r);
    try std.testing.expectEqual(@as(u8, 0), r1.color.g);
    try std.testing.expectEqual(@as(u8, 0), r1.color.b);
}

test "angle_conversion" {
    const rad = proven_angle_deg_to_rad(180);
    try std.testing.expectApproxEqAbs(std.math.pi, rad, 0.001);

    const deg = proven_angle_rad_to_deg(std.math.pi);
    try std.testing.expectApproxEqAbs(@as(f64, 180), deg, 0.001);
}

test "unit_conversion" {
    const r = proven_unit_convert_length(1, .kilometers, .meters);
    try std.testing.expectEqual(ProvenStatus.ok, r.status);
    try std.testing.expectApproxEqAbs(@as(f64, 1000), r.value, 0.001);

    const temp = proven_unit_convert_temp(0, .celsius, .kelvin);
    try std.testing.expectEqual(ProvenStatus.ok, temp.status);
    try std.testing.expectApproxEqAbs(@as(f64, 273.15), temp.value, 0.001);
}

test "queue" {
    const q = proven_queue_create(3);
    try std.testing.expect(q != null);
    defer proven_queue_free(q);

    try std.testing.expect(proven_queue_push(q, 10));
    try std.testing.expect(proven_queue_push(q, 20));
    try std.testing.expect(proven_queue_push(q, 30));
    try std.testing.expect(!proven_queue_push(q, 40)); // Full

    try std.testing.expectEqual(@as(usize, 3), proven_queue_size(q));

    const v1 = proven_queue_pop(q);
    try std.testing.expectEqual(@as(i64, 10), v1.value);
}

test "bloom_filter" {
    const filter = proven_bloom_create(100, 0.01);
    try std.testing.expect(filter != null);
    defer proven_bloom_free(filter);

    const key = "test_key";
    proven_bloom_add(filter, key.ptr, key.len);
    try std.testing.expect(proven_bloom_contains(filter, key.ptr, key.len));

    // May have false positives, but shouldn't have for completely different key
}

test "state_machine" {
    const sm = proven_state_machine_create(3, 0);
    try std.testing.expect(sm != null);
    defer proven_state_machine_free(sm);

    // Allow: 0 -> 1, 1 -> 2
    try std.testing.expect(proven_state_machine_allow(sm, 0, 1));
    try std.testing.expect(proven_state_machine_allow(sm, 1, 2));

    try std.testing.expectEqual(@as(u32, 0), proven_state_machine_state(sm));
    try std.testing.expect(proven_state_machine_transition(sm, 1));
    try std.testing.expectEqual(@as(u32, 1), proven_state_machine_state(sm));
    try std.testing.expect(!proven_state_machine_transition(sm, 0)); // Not allowed
    try std.testing.expect(proven_state_machine_transition(sm, 2));
    try std.testing.expectEqual(@as(u32, 2), proven_state_machine_state(sm));
}

test "ml_sigmoid" {
    try std.testing.expectApproxEqAbs(@as(f64, 0.5), proven_ml_sigmoid(0), 0.001);
    try std.testing.expect(proven_ml_sigmoid(10) > 0.999);
    try std.testing.expect(proven_ml_sigmoid(-10) < 0.001);
}

// ============================================================================
// Callback Infrastructure Tests
// ============================================================================

var test_callback_invoked: bool = false;
var test_callback_event_code: i32 = -1;

fn testCallback(_: ?*anyopaque, event: *const ProvenEvent) callconv(.C) i32 {
    test_callback_invoked = true;
    test_callback_event_code = event.code;
    return 0; // Keep registered
}

fn testCallbackOnce(_: ?*anyopaque, _: *const ProvenEvent) callconv(.C) i32 {
    return 1; // Request deregistration after first call
}

test "callback_register_unregister" {
    const handle = proven_callback_register(.validation_failed, testCallback, null);
    try std.testing.expect(handle != INVALID_HANDLE);

    try std.testing.expectEqual(@as(u32, 1), proven_callback_count(.validation_failed));
    try std.testing.expectEqual(@as(u32, 0), proven_callback_count(.rate_limit_hit));

    try std.testing.expectEqual(ProvenStatus.ok, proven_callback_unregister(handle));
    try std.testing.expectEqual(@as(u32, 0), proven_callback_count(.validation_failed));
}

test "callback_fire" {
    test_callback_invoked = false;
    test_callback_event_code = -1;

    const handle = proven_callback_register(.rate_limit_hit, testCallback, null);
    try std.testing.expect(handle != INVALID_HANDLE);
    defer _ = proven_callback_unregister(handle);

    const msg = "test_key";
    const invoked = proven_callback_fire(.rate_limit_hit, msg, msg.len, 42);
    try std.testing.expectEqual(@as(i32, 1), invoked);
    try std.testing.expect(test_callback_invoked);
    try std.testing.expectEqual(@as(i32, 42), test_callback_event_code);
}

test "callback_auto_deregister" {
    const handle = proven_callback_register(.signal_received, testCallbackOnce, null);
    try std.testing.expect(handle != INVALID_HANDLE);

    try std.testing.expectEqual(@as(u32, 1), proven_callback_count(.signal_received));

    // Fire — callback returns 1, requesting deregistration
    _ = proven_callback_fire(.signal_received, null, 0, 0);
    try std.testing.expectEqual(@as(u32, 0), proven_callback_count(.signal_received));
}

test "callback_clear_all" {
    _ = proven_callback_register(.validation_failed, testCallback, null);
    _ = proven_callback_register(.resource_acquired, testCallback, null);
    _ = proven_callback_register(.rate_limit_hit, testCallback, null);

    const removed = proven_callback_clear_all();
    try std.testing.expect(removed >= 3);
    try std.testing.expectEqual(@as(u32, 0), proven_callback_count(.validation_failed));
    try std.testing.expectEqual(@as(u32, 0), proven_callback_count(.resource_acquired));
    try std.testing.expectEqual(@as(u32, 0), proven_callback_count(.rate_limit_hit));
}

test "callback_invalid_handle" {
    try std.testing.expectEqual(ProvenStatus.err_invalid_argument, proven_callback_unregister(INVALID_HANDLE));
    try std.testing.expectEqual(ProvenStatus.err_out_of_bounds, proven_callback_unregister(99999));
}

test "callback_null_function" {
    const handle = proven_callback_register(.validation_failed, null, null);
    try std.testing.expectEqual(INVALID_HANDLE, handle);
}
