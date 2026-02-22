// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// Package proven provides Go bindings for the Proven formally-verified safety library.
//
// All computation is performed in Idris 2 with dependent types and totality checking.
// This Go package is a thin FFI wrapper that calls libproven (compiled Zig/Idris2
// shared library) via CGO. No logic is reimplemented in Go.
//
// Before calling any function, call Init() to initialize the Proven runtime.
// When done, call Deinit() to clean up resources.
package proven

/*
#cgo LDFLAGS: -lproven -L../../ffi/zig/zig-out/lib -L/usr/local/lib -L/usr/lib
#cgo CFLAGS: -I../../ffi/zig/include

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

// ============================================================================
// Result types (matching Zig extern struct layout)
// ============================================================================

typedef struct { int32_t status; int64_t value; } IntResult;
typedef struct { int32_t status; _Bool value; } BoolResult;
typedef struct { int32_t status; char* value; size_t length; } StringResult;
typedef struct { int32_t status; double value; } FloatResult;

// ============================================================================
// Lifecycle
// ============================================================================

extern int32_t proven_init();
extern void proven_deinit();
extern _Bool proven_is_initialized();
extern void proven_free_string(char* ptr);

// ============================================================================
// Version
// ============================================================================

extern uint32_t proven_ffi_abi_version();
extern uint32_t proven_version_major();
extern uint32_t proven_version_minor();
extern uint32_t proven_version_patch();
extern uint32_t proven_module_count();

// ============================================================================
// SafeMath
// ============================================================================

extern IntResult proven_math_div(int64_t numerator, int64_t denominator);
extern IntResult proven_math_mod(int64_t numerator, int64_t denominator);
extern IntResult proven_math_add_checked(int64_t a, int64_t b);
extern IntResult proven_math_sub_checked(int64_t a, int64_t b);
extern IntResult proven_math_mul_checked(int64_t a, int64_t b);
extern IntResult proven_math_abs_safe(int64_t n);
extern int64_t proven_math_clamp(int64_t lo, int64_t hi, int64_t value);
extern IntResult proven_math_pow_checked(int64_t base, uint32_t exp);

// ============================================================================
// SafeString
// ============================================================================

extern BoolResult proven_string_is_valid_utf8(const char* ptr, size_t len);
extern StringResult proven_string_escape_sql(const char* ptr, size_t len);
extern StringResult proven_string_escape_html(const char* ptr, size_t len);
extern StringResult proven_string_escape_js(const char* ptr, size_t len);

// ============================================================================
// SafePath
// ============================================================================

extern BoolResult proven_path_has_traversal(const char* ptr, size_t len);
extern StringResult proven_path_sanitize_filename(const char* ptr, size_t len);

// ============================================================================
// SafeCrypto
// ============================================================================

extern BoolResult proven_crypto_constant_time_eq(const char* ptr1, size_t len1, const char* ptr2, size_t len2);
extern int32_t proven_crypto_random_bytes(char* ptr, size_t len);

// ============================================================================
// SafeUrl
// ============================================================================

typedef struct {
    char* scheme;
    size_t scheme_len;
    char* host;
    size_t host_len;
    uint16_t port;
    _Bool has_port;
    char* path;
    size_t path_len;
    char* query;
    size_t query_len;
    char* fragment;
    size_t fragment_len;
} UrlComponents;

typedef struct {
    int32_t status;
    UrlComponents components;
} UrlResult;

extern UrlResult proven_url_parse(const char* ptr, size_t len);
extern void proven_url_free(UrlComponents* components);

// ============================================================================
// SafeEmail
// ============================================================================

extern BoolResult proven_email_is_valid(const char* ptr, size_t len);

// ============================================================================
// SafeNetwork
// ============================================================================

typedef struct { uint8_t octets[4]; } IPv4Address;
typedef struct { int32_t status; IPv4Address address; } IPv4Result;

extern IPv4Result proven_network_parse_ipv4(const char* ptr, size_t len);
extern _Bool proven_network_ipv4_is_private(IPv4Address addr);
extern _Bool proven_network_ipv4_is_loopback(IPv4Address addr);

// ============================================================================
// SafeHeader
// ============================================================================

extern BoolResult proven_header_has_crlf(const char* ptr, size_t len);
extern BoolResult proven_header_is_valid_name(const char* ptr, size_t len);
extern BoolResult proven_header_is_dangerous(const char* ptr, size_t len);
extern StringResult proven_header_render(const char* name_ptr, size_t name_len, const char* value_ptr, size_t value_len);
extern StringResult proven_header_build_csp(const char* directives_json, size_t json_len);
extern StringResult proven_header_build_hsts(int64_t max_age, _Bool include_subdomains, _Bool preload);

// ============================================================================
// SafeCookie
// ============================================================================

typedef struct {
    const char* domain;
    size_t domain_len;
    const char* path;
    size_t path_len;
    int64_t max_age;
    _Bool secure;
    _Bool http_only;
    int32_t same_site;
    _Bool partitioned;
} CookieAttributes;

extern BoolResult proven_cookie_has_injection(const char* ptr, size_t len);
extern BoolResult proven_cookie_validate_name(const char* ptr, size_t len);
extern BoolResult proven_cookie_validate_value(const char* ptr, size_t len);
extern IntResult proven_cookie_get_prefix(const char* ptr, size_t len);
extern StringResult proven_cookie_build_set_cookie(const char* name_ptr, size_t name_len, const char* value_ptr, size_t value_len, CookieAttributes attrs);
extern StringResult proven_cookie_build_delete(const char* name_ptr, size_t name_len);

// ============================================================================
// SafeContentType
// ============================================================================

typedef struct {
    int32_t status;
    char* media_type;
    size_t media_type_len;
    char* subtype;
    size_t subtype_len;
    char* suffix;
    size_t suffix_len;
    int32_t category;
    int32_t charset;
    _Bool has_charset;
} ContentTypeResult;

extern ContentTypeResult proven_content_type_parse(const char* ptr, size_t len);
extern void proven_content_type_free(ContentTypeResult* result);
extern BoolResult proven_content_type_can_sniff_dangerous(const char* ptr, size_t len);
extern StringResult proven_content_type_render(const char* type_ptr, size_t type_len, const char* subtype_ptr, size_t subtype_len, const char* suffix_ptr, size_t suffix_len, int32_t charset, _Bool has_charset);
extern BoolResult proven_content_type_is_json(const char* subtype_ptr, size_t subtype_len, const char* suffix_ptr, size_t suffix_len);
extern BoolResult proven_content_type_is_xml(const char* subtype_ptr, size_t subtype_len, const char* suffix_ptr, size_t suffix_len);

// ============================================================================
// SafeUUID
// ============================================================================

typedef struct { uint8_t bytes[16]; } UUID;
typedef struct { int32_t status; UUID uuid; } UUIDResult;

extern UUIDResult proven_uuid_v4();
extern StringResult proven_uuid_to_string(UUID uuid);
extern UUIDResult proven_uuid_parse(const char* ptr, size_t len);
extern _Bool proven_uuid_is_nil(UUID uuid);
extern uint8_t proven_uuid_version(UUID uuid);

// ============================================================================
// SafeJson
// ============================================================================

extern BoolResult proven_json_is_valid(const char* ptr, size_t len);
extern int32_t proven_json_get_type(const char* ptr, size_t len);

// ============================================================================
// SafeDateTime
// ============================================================================

typedef struct {
    int32_t year;
    uint8_t month;
    uint8_t day;
    uint8_t hour;
    uint8_t minute;
    uint8_t second;
    uint32_t nanosecond;
    int16_t tz_offset_minutes;
} DateTime;

typedef struct {
    int32_t status;
    DateTime datetime;
} DateTimeResult;

extern DateTimeResult proven_datetime_parse(const char* ptr, size_t len);
extern StringResult proven_datetime_format_iso8601(DateTime dt);
extern _Bool proven_datetime_is_leap_year(int32_t year);
extern uint8_t proven_datetime_days_in_month(int32_t year, uint8_t month);

// ============================================================================
// SafeFloat
// ============================================================================

extern FloatResult proven_float_div(double a, double b);
extern _Bool proven_float_is_finite(double x);
extern _Bool proven_float_is_nan(double x);
extern FloatResult proven_float_sqrt(double x);
extern FloatResult proven_float_ln(double x);

// ============================================================================
// SafePassword
// ============================================================================

typedef struct {
    int32_t strength;
    _Bool has_lowercase;
    _Bool has_uppercase;
    _Bool has_digit;
    _Bool has_special;
    size_t length;
} PasswordResult;

extern PasswordResult proven_password_validate(const char* ptr, size_t len);
extern _Bool proven_password_is_common(const char* ptr, size_t len);

// ============================================================================
// SafeHex
// ============================================================================

typedef struct { int32_t status; char* data; size_t length; } HexDecodeResult;

extern StringResult proven_hex_encode(const char* ptr, size_t len, _Bool uppercase);
extern HexDecodeResult proven_hex_decode(const char* ptr, size_t len);
extern void proven_hex_free(HexDecodeResult* result);

// ============================================================================
// SafeCurrency
// ============================================================================

typedef struct {
    int32_t status;
    int64_t amount_minor;
    uint8_t currency_code[3];
    uint8_t decimal_places;
} CurrencyResult;

extern CurrencyResult proven_currency_parse(const char* ptr, size_t len);
extern StringResult proven_currency_format(int64_t amount_minor, uint8_t code[3], uint8_t decimal_places);

// ============================================================================
// SafePhone
// ============================================================================

typedef struct {
    int32_t status;
    uint16_t country_code;
    uint64_t national_number;
    _Bool is_valid;
} PhoneResult;

extern PhoneResult proven_phone_parse(const char* ptr, size_t len);
extern StringResult proven_phone_format_e164(uint16_t country_code, uint64_t national_number);

// ============================================================================
// SafeVersion
// ============================================================================

typedef struct {
    uint32_t major;
    uint32_t minor;
    uint32_t patch;
    size_t prerelease_len;
    char* prerelease;
} SemanticVersion;

typedef struct {
    int32_t status;
    SemanticVersion version;
} VersionResult;

extern VersionResult proven_version_parse(const char* ptr, size_t len);
extern int32_t proven_version_compare(SemanticVersion a, SemanticVersion b);
extern void proven_version_free(SemanticVersion* version);

// ============================================================================
// SafeGeo
// ============================================================================

typedef struct { double latitude; double longitude; } GeoCoordinate;
typedef struct { int32_t status; GeoCoordinate coordinate; } GeoResult;

extern GeoResult proven_geo_validate(double lat, double lon);
extern FloatResult proven_geo_distance(GeoCoordinate a, GeoCoordinate b);
extern _Bool proven_geo_in_bounds(GeoCoordinate coord, double min_lat, double max_lat, double min_lon, double max_lon);

// ============================================================================
// SafeChecksum
// ============================================================================

extern IntResult proven_checksum_crc32(const char* ptr, size_t len);
extern BoolResult proven_checksum_verify_crc32(const char* ptr, size_t len, uint32_t expected);

// ============================================================================
// SafeProbability
// ============================================================================

extern double proven_probability_create(double value);
extern double proven_probability_and(double a, double b);
extern double proven_probability_or_exclusive(double a, double b);
extern double proven_probability_not(double p);

// ============================================================================
// SafeCalculator
// ============================================================================

extern FloatResult proven_calculator_eval(const char* ptr, size_t len);

// ============================================================================
// SafeBuffer
// ============================================================================

typedef struct { char* data; size_t capacity; size_t length; } BoundedBuffer;
typedef struct { int32_t status; BoundedBuffer* buffer; } BufferResult;

extern BufferResult proven_buffer_create(size_t capacity);
extern int32_t proven_buffer_append(BoundedBuffer* buffer, const char* ptr, size_t len);
extern int32_t proven_buffer_get(BoundedBuffer* buffer, const char** out_ptr, size_t* out_len);
extern void proven_buffer_free(BoundedBuffer* buffer);

// ============================================================================
// SafeRateLimiter
// ============================================================================

typedef struct { double tokens; double capacity; double refill_rate; int64_t last_refill; } RateLimiter;

extern RateLimiter* proven_rate_limiter_create(double capacity, double refill_rate);
extern _Bool proven_rate_limiter_try_acquire(RateLimiter* limiter, double tokens);
extern void proven_rate_limiter_free(RateLimiter* limiter);

// ============================================================================
// SafeCircuitBreaker
// ============================================================================

typedef struct {
    int32_t state;
    uint32_t failure_count;
    uint32_t failure_threshold;
    uint32_t success_count;
    uint32_t success_threshold;
    int64_t last_failure;
    int64_t timeout_ms;
} CircuitBreaker;

extern CircuitBreaker* proven_circuit_breaker_create(uint32_t failure_threshold, uint32_t success_threshold, int64_t timeout_ms);
extern _Bool proven_circuit_breaker_allow(CircuitBreaker* cb);
extern void proven_circuit_breaker_success(CircuitBreaker* cb);
extern void proven_circuit_breaker_failure(CircuitBreaker* cb);
extern int32_t proven_circuit_breaker_state(CircuitBreaker* cb);
extern void proven_circuit_breaker_free(CircuitBreaker* cb);

// ============================================================================
// SafeRetry
// ============================================================================

typedef struct { uint32_t max_attempts; uint64_t base_delay_ms; uint64_t max_delay_ms; double multiplier; } RetryConfig;

extern uint64_t proven_retry_delay(RetryConfig config, uint32_t attempt);
extern _Bool proven_retry_should_retry(RetryConfig config, uint32_t attempt);

// ============================================================================
// SafeMonotonic
// ============================================================================

typedef struct { uint64_t value; uint64_t max_value; } MonotonicCounter;

extern MonotonicCounter* proven_monotonic_create(uint64_t initial, uint64_t max_value);
extern IntResult proven_monotonic_next(MonotonicCounter* counter);
extern void proven_monotonic_free(MonotonicCounter* counter);

// ============================================================================
// SafeStateMachine
// ============================================================================

typedef struct { uint32_t current_state; uint32_t state_count; uint8_t* transitions; } StateMachine;

extern StateMachine* proven_state_machine_create(uint32_t state_count, uint32_t initial_state);
extern _Bool proven_state_machine_allow(StateMachine* sm, uint32_t from, uint32_t to);
extern _Bool proven_state_machine_transition(StateMachine* sm, uint32_t to);
extern uint32_t proven_state_machine_state(StateMachine* sm);
extern void proven_state_machine_free(StateMachine* sm);

// ============================================================================
// SafeTensor
// ============================================================================

typedef struct { double* data; size_t rows; size_t cols; } Tensor2D;

extern Tensor2D* proven_tensor_create(size_t rows, size_t cols);
extern int32_t proven_tensor_set(Tensor2D* tensor, size_t row, size_t col, double value);
extern FloatResult proven_tensor_get(Tensor2D* tensor, size_t row, size_t col);
extern Tensor2D* proven_tensor_matmul(Tensor2D* a, Tensor2D* b);
extern void proven_tensor_free(Tensor2D* tensor);

// ============================================================================
// SafeML
// ============================================================================

extern int32_t proven_ml_softmax(const double* input, double* output, size_t len);
extern double proven_ml_sigmoid(double x);
extern double proven_ml_relu(double x);
extern double proven_ml_leaky_relu(double x, double alpha);
extern double proven_ml_clamp(double x, double min_val, double max_val);

// ============================================================================
// SafeLRU
// ============================================================================

typedef struct { uint64_t key; int64_t value; size_t prev; size_t next; _Bool valid; } LRUEntry;
typedef struct { LRUEntry* entries; size_t capacity; size_t head; size_t tail; size_t count; } LRUCache;

extern LRUCache* proven_lru_create(size_t capacity);
extern IntResult proven_lru_get(LRUCache* cache, uint64_t key);
extern int32_t proven_lru_put(LRUCache* cache, uint64_t key, int64_t value);
extern void proven_lru_free(LRUCache* cache);

// ============================================================================
// SafeGraph
// ============================================================================

typedef struct { uint8_t* edges; size_t node_count; } Graph;

extern Graph* proven_graph_create(size_t node_count);
extern int32_t proven_graph_add_edge(Graph* graph, size_t from, size_t to);
extern _Bool proven_graph_has_edge(Graph* graph, size_t from, size_t to);
extern void proven_graph_free(Graph* graph);

// ============================================================================
// SafeQueue
// ============================================================================

typedef struct { int64_t* data; size_t capacity; size_t head; size_t tail; size_t count; } BoundedQueue;

extern BoundedQueue* proven_queue_create(size_t capacity);
extern _Bool proven_queue_push(BoundedQueue* queue, int64_t value);
extern IntResult proven_queue_pop(BoundedQueue* queue);
extern size_t proven_queue_size(BoundedQueue* queue);
extern void proven_queue_free(BoundedQueue* queue);

// ============================================================================
// SafeBloom
// ============================================================================

typedef struct { uint8_t* bits; size_t bit_count; uint32_t hash_count; } BloomFilter;

extern BloomFilter* proven_bloom_create(size_t expected_elements, double false_positive_rate);
extern void proven_bloom_add(BloomFilter* filter, const char* ptr, size_t len);
extern _Bool proven_bloom_contains(BloomFilter* filter, const char* ptr, size_t len);
extern void proven_bloom_free(BloomFilter* filter);

// ============================================================================
// SafeRegistry (OCI image references)
// ============================================================================

typedef struct {
    char* registry;
    size_t registry_len;
    char* repository;
    size_t repository_len;
    char* tag;
    size_t tag_len;
    char* digest;
    size_t digest_len;
} ImageReference;

typedef struct {
    int32_t status;
    ImageReference reference;
} ImageRefResult;

extern ImageRefResult proven_registry_parse(const char* ptr, size_t len);
extern StringResult proven_registry_to_string(const ImageReference* ref);
extern BoolResult proven_registry_has_registry(const ImageReference* ref);

// ============================================================================
// SafeDigest
// ============================================================================

typedef struct { uint8_t algorithm; char* value; size_t value_len; } Digest;
typedef struct { int32_t status; Digest digest; } DigestResult;

extern DigestResult proven_digest_parse(const char* ptr, size_t len);
extern BoolResult proven_digest_verify(const Digest* expected, const Digest* actual);
extern StringResult proven_digest_to_string(const Digest* digest);

// ============================================================================
// SafeHTTP
// ============================================================================

extern StringResult proven_http_url_encode(const char* ptr, size_t len);
extern StringResult proven_http_url_decode(const char* ptr, size_t len);

typedef struct {
    char* scheme;
    size_t scheme_len;
    char* realm;
    size_t realm_len;
    char* service;
    size_t service_len;
    char* scope;
    size_t scope_len;
} AuthChallenge;

typedef struct {
    int32_t status;
    AuthChallenge challenge;
} AuthChallengeResult;

extern AuthChallengeResult proven_http_parse_www_authenticate(const char* ptr, size_t len);

// ============================================================================
// SafeColor
// ============================================================================

typedef struct { uint8_t r; uint8_t g; uint8_t b; } RGBColor;
typedef struct { double h; double s; double l; } HSLColor;
typedef struct { int32_t status; RGBColor color; } ColorParseResult;

extern ColorParseResult proven_color_parse_hex(const char* ptr, size_t len);
extern HSLColor proven_color_rgb_to_hsl(RGBColor rgb);
extern StringResult proven_color_to_hex(RGBColor rgb);

// ============================================================================
// SafeAngle
// ============================================================================

extern double proven_angle_deg_to_rad(double degrees);
extern double proven_angle_rad_to_deg(double radians);
extern double proven_angle_normalize_degrees(double degrees);
extern double proven_angle_normalize_radians(double radians);

// ============================================================================
// SafeUnit
// ============================================================================

extern FloatResult proven_unit_convert_length(double value, int32_t from, int32_t to);
extern FloatResult proven_unit_convert_temp(double value, int32_t from, int32_t to);
*/
import "C"
import (
	"fmt"
	"unsafe"
)

// ---------------------------------------------------------------------------
// Status codes (matching ProvenStatus enum from Zig FFI layer)
// ---------------------------------------------------------------------------

const (
	// StatusOK indicates success.
	StatusOK = 0
	// StatusErrNullPointer indicates a null pointer was passed.
	StatusErrNullPointer = -1
	// StatusErrInvalidArg indicates an invalid argument.
	StatusErrInvalidArg = -2
	// StatusErrOverflow indicates arithmetic overflow.
	StatusErrOverflow = -3
	// StatusErrUnderflow indicates arithmetic underflow.
	StatusErrUnderflow = -4
	// StatusErrDivByZero indicates division by zero.
	StatusErrDivByZero = -5
	// StatusErrParseFailed indicates a parse failure.
	StatusErrParseFailed = -6
	// StatusErrValidation indicates a validation failure.
	StatusErrValidation = -7
	// StatusErrOutOfBounds indicates an out-of-bounds access.
	StatusErrOutOfBounds = -8
	// StatusErrEncoding indicates an encoding error.
	StatusErrEncoding = -9
	// StatusErrAllocFailed indicates a memory allocation failure.
	StatusErrAllocFailed = -10
	// StatusErrNotImpl indicates functionality not yet implemented.
	StatusErrNotImpl = -99
)

// statusMessages maps status codes to human-readable error messages.
var statusMessages = map[int]string{
	StatusOK:             "ok",
	StatusErrNullPointer: "null pointer",
	StatusErrInvalidArg:  "invalid argument",
	StatusErrOverflow:    "arithmetic overflow",
	StatusErrUnderflow:   "arithmetic underflow",
	StatusErrDivByZero:   "division by zero",
	StatusErrParseFailed: "parse failure",
	StatusErrValidation:  "validation failure",
	StatusErrOutOfBounds: "out of bounds",
	StatusErrEncoding:    "encoding error",
	StatusErrAllocFailed: "memory allocation failed",
	StatusErrNotImpl:     "not implemented",
}

// ---------------------------------------------------------------------------
// ProvenError represents an error returned from the Proven FFI layer.
// ---------------------------------------------------------------------------

// ProvenError wraps a status code from the C FFI layer into a Go error.
type ProvenError struct {
	Status  int
	Message string
}

// Error implements the error interface for ProvenError.
func (e *ProvenError) Error() string {
	return fmt.Sprintf("proven: %s (status=%d)", e.Message, e.Status)
}

// newError creates a ProvenError from a status code.
func newError(status int) *ProvenError {
	msg, ok := statusMessages[status]
	if !ok {
		msg = fmt.Sprintf("unknown error (code %d)", status)
	}
	return &ProvenError{Status: status, Message: msg}
}

// ---------------------------------------------------------------------------
// Lifecycle functions
// ---------------------------------------------------------------------------

// Init initializes the Proven runtime (includes Idris 2 runtime).
// Must be called before any other Proven function.
func Init() error {
	rc := int(C.proven_init())
	if rc != StatusOK {
		return newError(rc)
	}
	return nil
}

// Deinit cleans up the Proven runtime. Call when done using the library.
func Deinit() {
	C.proven_deinit()
}

// IsInitialized returns true if the Proven runtime has been initialized.
func IsInitialized() bool {
	return bool(C.proven_is_initialized())
}

// ---------------------------------------------------------------------------
// Version information
// ---------------------------------------------------------------------------

// ABIVersion returns the FFI ABI version for compatibility checking.
func ABIVersion() uint32 {
	return uint32(C.proven_ffi_abi_version())
}

// VersionMajor returns the major version number.
func VersionMajor() uint32 {
	return uint32(C.proven_version_major())
}

// VersionMinor returns the minor version number.
func VersionMinor() uint32 {
	return uint32(C.proven_version_minor())
}

// VersionPatch returns the patch version number.
func VersionPatch() uint32 {
	return uint32(C.proven_version_patch())
}

// ModuleCount returns the total number of modules in the library.
func ModuleCount() uint32 {
	return uint32(C.proven_module_count())
}

// ---------------------------------------------------------------------------
// Internal helpers for string marshaling
// ---------------------------------------------------------------------------

// goStringResult extracts a Go string from a C StringResult, freeing the
// C-allocated memory. Returns an error if the status is not OK.
func goStringResult(result C.StringResult) (string, error) {
	status := int(result.status)
	if status != StatusOK {
		return "", newError(status)
	}
	if result.value == nil {
		return "", newError(StatusErrNullPointer)
	}
	goStr := C.GoStringN(result.value, C.int(result.length))
	C.proven_free_string(result.value)
	return goStr, nil
}

// cString converts a Go string to a C string pointer and length.
// The caller must free the returned pointer with C.free.
func cString(s string) (*C.char, C.size_t) {
	cs := C.CString(s)
	return cs, C.size_t(len(s))
}

// cBytes converts a Go byte slice to a C char pointer and length.
// The caller must free the returned pointer with C.free.
func cBytes(b []byte) (*C.char, C.size_t) {
	if len(b) == 0 {
		return nil, 0
	}
	cs := (*C.char)(C.CBytes(b))
	return cs, C.size_t(len(b))
}

// unsafeFree frees a C-allocated pointer.
func unsafeFree(ptr *C.char) {
	C.free(unsafe.Pointer(ptr))
}
