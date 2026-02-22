/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> */

/**
 * @file proven.h
 * @brief Unified C header for the Proven FFI (Swift binding).
 *
 * This header declares all types and functions exported by libproven.
 * Swift imports this module via the CProven system library target.
 */

#ifndef PROVEN_SWIFT_H
#define PROVEN_SWIFT_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Status Codes
 * ============================================================================ */

typedef enum ProvenStatus {
    PROVEN_OK = 0,
    PROVEN_ERR_NULL_POINTER = -1,
    PROVEN_ERR_INVALID_ARGUMENT = -2,
    PROVEN_ERR_OVERFLOW = -3,
    PROVEN_ERR_UNDERFLOW = -4,
    PROVEN_ERR_DIVISION_BY_ZERO = -5,
    PROVEN_ERR_PARSE_FAILURE = -6,
    PROVEN_ERR_VALIDATION_FAILED = -7,
    PROVEN_ERR_OUT_OF_BOUNDS = -8,
    PROVEN_ERR_ENCODING_ERROR = -9,
    PROVEN_ERR_ALLOCATION_FAILED = -10,
    PROVEN_ERR_NOT_IMPLEMENTED = -99
} ProvenStatus;

/* ============================================================================
 * Result Types
 * ============================================================================ */

typedef struct ProvenIntResult {
    ProvenStatus status;
    int64_t value;
} ProvenIntResult;

typedef struct ProvenBoolResult {
    ProvenStatus status;
    bool value;
} ProvenBoolResult;

typedef struct ProvenStringResult {
    ProvenStatus status;
    char* value;
    size_t length;
} ProvenStringResult;

typedef struct ProvenFloatResult {
    ProvenStatus status;
    double value;
} ProvenFloatResult;

/* ============================================================================
 * Runtime Management
 * ============================================================================ */

int32_t proven_init(void);
void proven_deinit(void);
bool proven_is_initialized(void);
uint32_t proven_ffi_abi_version(void);

/* ============================================================================
 * Memory Management
 * ============================================================================ */

void proven_free_string(char* ptr);

/* ============================================================================
 * SafeMath
 * ============================================================================ */

ProvenIntResult proven_math_div(int64_t numerator, int64_t denominator);
ProvenIntResult proven_math_mod(int64_t numerator, int64_t denominator);
ProvenIntResult proven_math_add_checked(int64_t a, int64_t b);
ProvenIntResult proven_math_sub_checked(int64_t a, int64_t b);
ProvenIntResult proven_math_mul_checked(int64_t a, int64_t b);
ProvenIntResult proven_math_abs_safe(int64_t n);
int64_t proven_math_clamp(int64_t lo, int64_t hi, int64_t value);
ProvenIntResult proven_math_pow_checked(int64_t base, uint32_t exp);

/* ============================================================================
 * SafeString
 * ============================================================================ */

ProvenBoolResult proven_string_is_valid_utf8(const uint8_t* ptr, size_t len);
ProvenStringResult proven_string_escape_sql(const uint8_t* ptr, size_t len);
ProvenStringResult proven_string_escape_html(const uint8_t* ptr, size_t len);
ProvenStringResult proven_string_escape_js(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafePath
 * ============================================================================ */

ProvenBoolResult proven_path_has_traversal(const uint8_t* ptr, size_t len);
ProvenStringResult proven_path_sanitize_filename(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeCrypto
 * ============================================================================ */

ProvenBoolResult proven_crypto_constant_time_eq(
    const uint8_t* ptr1, size_t len1,
    const uint8_t* ptr2, size_t len2
);
ProvenStatus proven_crypto_random_bytes(uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeUrl
 * ============================================================================ */

typedef struct ProvenUrlComponents {
    char* scheme;
    size_t scheme_len;
    char* host;
    size_t host_len;
    uint16_t port;
    bool has_port;
    char* path;
    size_t path_len;
    char* query;
    size_t query_len;
    char* fragment;
    size_t fragment_len;
} ProvenUrlComponents;

typedef struct ProvenUrlResult {
    ProvenStatus status;
    ProvenUrlComponents components;
} ProvenUrlResult;

ProvenUrlResult proven_url_parse(const uint8_t* ptr, size_t len);
void proven_url_free(ProvenUrlComponents* components);

/* ============================================================================
 * SafeEmail
 * ============================================================================ */

ProvenBoolResult proven_email_is_valid(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeNetwork
 * ============================================================================ */

typedef struct ProvenIPv4Address {
    uint8_t octets[4];
} ProvenIPv4Address;

typedef struct ProvenIPv4Result {
    ProvenStatus status;
    ProvenIPv4Address address;
} ProvenIPv4Result;

ProvenIPv4Result proven_network_parse_ipv4(const uint8_t* ptr, size_t len);
bool proven_network_ipv4_is_private(ProvenIPv4Address addr);
bool proven_network_ipv4_is_loopback(ProvenIPv4Address addr);

/* ============================================================================
 * SafeFloat
 * ============================================================================ */

ProvenFloatResult proven_float_div(double a, double b);
bool proven_float_is_finite(double x);
bool proven_float_is_nan(double x);
ProvenFloatResult proven_float_sqrt(double x);
ProvenFloatResult proven_float_ln(double x);

/* ============================================================================
 * SafeJson
 * ============================================================================ */

typedef enum ProvenJsonType {
    PROVEN_JSON_NULL = 0,
    PROVEN_JSON_BOOL = 1,
    PROVEN_JSON_NUMBER = 2,
    PROVEN_JSON_STRING = 3,
    PROVEN_JSON_ARRAY = 4,
    PROVEN_JSON_OBJECT = 5,
    PROVEN_JSON_INVALID = -1
} ProvenJsonType;

ProvenBoolResult proven_json_is_valid(const uint8_t* ptr, size_t len);
ProvenJsonType proven_json_get_type(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeDatetime
 * ============================================================================ */

typedef struct ProvenDateTime {
    int32_t year;
    uint8_t month;
    uint8_t day;
    uint8_t hour;
    uint8_t minute;
    uint8_t second;
    uint32_t nanosecond;
    int16_t tz_offset_minutes;
} ProvenDateTime;

typedef struct ProvenDateTimeResult {
    ProvenStatus status;
    ProvenDateTime datetime;
} ProvenDateTimeResult;

ProvenDateTimeResult proven_datetime_parse(const uint8_t* ptr, size_t len);
ProvenStringResult proven_datetime_format_iso8601(ProvenDateTime dt);
bool proven_datetime_is_leap_year(int32_t year);
uint8_t proven_datetime_days_in_month(int32_t year, uint8_t month);

/* ============================================================================
 * SafeVersion
 * ============================================================================ */

typedef struct ProvenSemanticVersion {
    uint32_t major;
    uint32_t minor;
    uint32_t patch;
    size_t prerelease_len;
    char* prerelease;
} ProvenSemanticVersion;

typedef struct ProvenVersionResult {
    ProvenStatus status;
    ProvenSemanticVersion version;
} ProvenVersionResult;

ProvenVersionResult proven_version_parse(const uint8_t* ptr, size_t len);
int32_t proven_version_compare(ProvenSemanticVersion a, ProvenSemanticVersion b);
void proven_version_free(ProvenSemanticVersion* version);

/* ============================================================================
 * SafeAngle
 * ============================================================================ */

double proven_angle_deg_to_rad(double degrees);
double proven_angle_rad_to_deg(double radians);
double proven_angle_normalize_degrees(double degrees);
double proven_angle_normalize_radians(double radians);

/* ============================================================================
 * SafeColor
 * ============================================================================ */

typedef struct ProvenRGBColor {
    uint8_t r;
    uint8_t g;
    uint8_t b;
} ProvenRGBColor;

typedef struct ProvenHSLColor {
    double h;
    double s;
    double l;
} ProvenHSLColor;

typedef struct ProvenColorResult {
    ProvenStatus status;
    ProvenRGBColor color;
} ProvenColorResult;

ProvenColorResult proven_color_parse_hex(const uint8_t* ptr, size_t len);
ProvenHSLColor proven_color_rgb_to_hsl(ProvenRGBColor rgb);
ProvenStringResult proven_color_to_hex(ProvenRGBColor rgb);

/* ============================================================================
 * SafeUnit
 * ============================================================================ */

typedef enum ProvenLengthUnit {
    PROVEN_LENGTH_METERS = 0,
    PROVEN_LENGTH_KILOMETERS = 1,
    PROVEN_LENGTH_CENTIMETERS = 2,
    PROVEN_LENGTH_MILLIMETERS = 3,
    PROVEN_LENGTH_FEET = 4,
    PROVEN_LENGTH_INCHES = 5,
    PROVEN_LENGTH_MILES = 6,
    PROVEN_LENGTH_YARDS = 7
} ProvenLengthUnit;

typedef enum ProvenTempUnit {
    PROVEN_TEMP_CELSIUS = 0,
    PROVEN_TEMP_FAHRENHEIT = 1,
    PROVEN_TEMP_KELVIN = 2
} ProvenTempUnit;

ProvenFloatResult proven_unit_convert_length(double value, ProvenLengthUnit from, ProvenLengthUnit to);
ProvenFloatResult proven_unit_convert_temp(double value, ProvenTempUnit from, ProvenTempUnit to);

/* ============================================================================
 * SafeGeo
 * ============================================================================ */

typedef struct ProvenGeoCoordinate {
    double lat;
    double lon;
} ProvenGeoCoordinate;

typedef struct ProvenGeoResult {
    ProvenStatus status;
    ProvenGeoCoordinate coord;
} ProvenGeoResult;

ProvenGeoResult proven_geo_validate(double lat, double lon);
ProvenFloatResult proven_geo_distance(ProvenGeoCoordinate a, ProvenGeoCoordinate b);
bool proven_geo_in_bounds(ProvenGeoCoordinate coord, double min_lat, double max_lat, double min_lon, double max_lon);

/* ============================================================================
 * SafeChecksum
 * ============================================================================ */

ProvenIntResult proven_checksum_crc32(const uint8_t* ptr, size_t len);
ProvenBoolResult proven_checksum_verify_crc32(const uint8_t* ptr, size_t len, uint32_t expected);

/* ============================================================================
 * SafeProbability
 * ============================================================================ */

double proven_probability_create(double value);
double proven_probability_and(double a, double b);
double proven_probability_or_exclusive(double a, double b);
double proven_probability_not(double p);

/* ============================================================================
 * SafeCalculator
 * ============================================================================ */

ProvenFloatResult proven_calculator_eval(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeHex (FFI-level: proven_hex_encode / proven_hex_decode)
 * ============================================================================ */

ProvenStringResult proven_hex_encode(const uint8_t* ptr, size_t len, bool uppercase);

typedef struct ProvenHexDecodeResult {
    ProvenStatus status;
    uint8_t* data;
    size_t length;
} ProvenHexDecodeResult;

ProvenHexDecodeResult proven_hex_decode(const uint8_t* ptr, size_t len);
void proven_hex_free(ProvenHexDecodeResult* result);

/* ============================================================================
 * SafeUUID (FFI-level: proven_uuid_*)
 * ============================================================================ */

typedef struct ProvenUUID {
    uint8_t bytes[16];
} ProvenUUID;

typedef struct ProvenUUIDResult {
    ProvenStatus status;
    ProvenUUID uuid;
} ProvenUUIDResult;

ProvenUUIDResult proven_uuid_v4(void);
ProvenStringResult proven_uuid_to_string(ProvenUUID uuid);
ProvenUUIDResult proven_uuid_parse(const uint8_t* ptr, size_t len);
bool proven_uuid_is_nil(ProvenUUID uuid);
uint8_t proven_uuid_version(ProvenUUID uuid);

/* ============================================================================
 * SafePhone (FFI-level: proven_phone_*)
 * ============================================================================ */

typedef struct ProvenPhoneResult {
    ProvenStatus status;
    uint16_t country_code;
    uint64_t national_number;
} ProvenPhoneResult;

ProvenPhoneResult proven_phone_parse(const uint8_t* ptr, size_t len);
ProvenStringResult proven_phone_format_e164(uint16_t country_code, uint64_t national_number);

/* ============================================================================
 * SafeCurrency (FFI-level: proven_currency_*)
 * ============================================================================ */

typedef struct ProvenCurrencyResult {
    ProvenStatus status;
    int64_t amount_minor;
    uint8_t code[3];
    uint8_t decimal_places;
} ProvenCurrencyResult;

ProvenCurrencyResult proven_currency_parse(const uint8_t* ptr, size_t len);
ProvenStringResult proven_currency_format(int64_t amount_minor, uint8_t code[3], uint8_t decimal_places);

/* ============================================================================
 * SafeHeader (FFI-level: proven_header_*)
 * ============================================================================ */

ProvenBoolResult proven_header_has_crlf(const uint8_t* ptr, size_t len);
ProvenBoolResult proven_header_is_valid_name(const uint8_t* ptr, size_t len);
ProvenBoolResult proven_header_is_dangerous(const uint8_t* ptr, size_t len);
ProvenStringResult proven_header_render(
    const uint8_t* name_ptr, size_t name_len,
    const uint8_t* value_ptr, size_t value_len
);
ProvenStringResult proven_header_build_csp(const uint8_t* directives_json, size_t json_len);
ProvenStringResult proven_header_build_hsts(int64_t max_age, bool include_subdomains, bool preload);

/* ============================================================================
 * SafeCookie (FFI-level: proven_cookie_*)
 * ============================================================================ */

ProvenBoolResult proven_cookie_has_injection(const uint8_t* ptr, size_t len);
ProvenBoolResult proven_cookie_validate_name(const uint8_t* ptr, size_t len);
ProvenBoolResult proven_cookie_validate_value(const uint8_t* ptr, size_t len);
ProvenIntResult proven_cookie_get_prefix(const uint8_t* ptr, size_t len);
ProvenStringResult proven_cookie_build_set_cookie(
    const uint8_t* name_ptr, size_t name_len,
    const uint8_t* value_ptr, size_t value_len,
    int64_t max_age, bool secure, bool http_only, bool same_site_strict
);
ProvenStringResult proven_cookie_build_delete(const uint8_t* name_ptr, size_t name_len);

/* ============================================================================
 * SafeContentType (FFI-level: proven_content_type_*)
 * ============================================================================ */

typedef struct ProvenContentTypeResult {
    ProvenStatus status;
    char* type_str;
    size_t type_len;
    char* subtype_str;
    size_t subtype_len;
    char* suffix_str;
    size_t suffix_len;
    char* params_str;
    size_t params_len;
} ProvenContentTypeResult;

ProvenContentTypeResult proven_content_type_parse(const uint8_t* ptr, size_t len);
void proven_content_type_free(ProvenContentTypeResult* result);
ProvenBoolResult proven_content_type_can_sniff_dangerous(const uint8_t* ptr, size_t len);
ProvenStringResult proven_content_type_render(
    const uint8_t* type_ptr, size_t type_len,
    const uint8_t* subtype_ptr, size_t subtype_len
);
ProvenBoolResult proven_content_type_is_json(
    const uint8_t* subtype_ptr, size_t subtype_len,
    const uint8_t* suffix_ptr, size_t suffix_len
);
ProvenBoolResult proven_content_type_is_xml(
    const uint8_t* subtype_ptr, size_t subtype_len,
    const uint8_t* suffix_ptr, size_t suffix_len
);

/* ============================================================================
 * SafeBuffer (FFI-level: proven_buffer_*)
 * ============================================================================ */

typedef struct ProvenBoundedBuffer ProvenBoundedBuffer;

typedef struct ProvenBufferResult {
    ProvenStatus status;
    ProvenBoundedBuffer* buffer;
} ProvenBufferResult;

ProvenBufferResult proven_buffer_create(size_t capacity);
ProvenStatus proven_buffer_append(ProvenBoundedBuffer* buffer, const uint8_t* ptr, size_t len);
ProvenStatus proven_buffer_get(ProvenBoundedBuffer* buffer, const uint8_t** out_ptr, size_t* out_len);
void proven_buffer_free(ProvenBoundedBuffer* buffer);

/* ============================================================================
 * SafeQueue (FFI-level: proven_queue_*)
 * ============================================================================ */

typedef struct ProvenBoundedQueue ProvenBoundedQueue;

ProvenBoundedQueue* proven_queue_create(size_t capacity);
bool proven_queue_push(ProvenBoundedQueue* queue, int64_t value);
ProvenIntResult proven_queue_pop(ProvenBoundedQueue* queue);
size_t proven_queue_size(ProvenBoundedQueue* queue);
void proven_queue_free(ProvenBoundedQueue* queue);

/* ============================================================================
 * SafeBloom (FFI-level: proven_bloom_*)
 * ============================================================================ */

typedef struct ProvenBloomFilter ProvenBloomFilter;

ProvenBloomFilter* proven_bloom_create(size_t expected_elements, double false_positive_rate);
void proven_bloom_add(ProvenBloomFilter* filter, const uint8_t* ptr, size_t len);
bool proven_bloom_contains(ProvenBloomFilter* filter, const uint8_t* ptr, size_t len);
void proven_bloom_free(ProvenBloomFilter* filter);

/* ============================================================================
 * SafeRateLimiter (FFI-level: proven_rate_limiter_*)
 * ============================================================================ */

typedef struct ProvenRateLimiter ProvenRateLimiter;

ProvenRateLimiter* proven_rate_limiter_create(double capacity, double refill_rate);
bool proven_rate_limiter_try_acquire(ProvenRateLimiter* limiter, double tokens);
void proven_rate_limiter_free(ProvenRateLimiter* limiter);

/* ============================================================================
 * SafeCircuitBreaker (FFI-level: proven_circuit_breaker_*)
 * ============================================================================ */

typedef enum ProvenCircuitState {
    PROVEN_CIRCUIT_CLOSED = 0,
    PROVEN_CIRCUIT_OPEN = 1,
    PROVEN_CIRCUIT_HALF_OPEN = 2
} ProvenCircuitState;

typedef struct ProvenCircuitBreaker ProvenCircuitBreaker;

ProvenCircuitBreaker* proven_circuit_breaker_create(uint32_t failure_threshold, uint32_t success_threshold, int64_t timeout_ms);
bool proven_circuit_breaker_allow(ProvenCircuitBreaker* cb);
void proven_circuit_breaker_success(ProvenCircuitBreaker* cb);
void proven_circuit_breaker_failure(ProvenCircuitBreaker* cb);
ProvenCircuitState proven_circuit_breaker_state(ProvenCircuitBreaker* cb);
void proven_circuit_breaker_free(ProvenCircuitBreaker* cb);

/* ============================================================================
 * SafeRetry (FFI-level: proven_retry_*)
 * ============================================================================ */

typedef struct ProvenRetryConfig {
    uint32_t max_attempts;
    uint64_t base_delay_ms;
    uint64_t max_delay_ms;
    double multiplier;
} ProvenRetryConfig;

uint64_t proven_retry_delay(ProvenRetryConfig config, uint32_t attempt);
bool proven_retry_should_retry(ProvenRetryConfig config, uint32_t attempt);

/* ============================================================================
 * SafeMonotonic (FFI-level: proven_monotonic_*)
 * ============================================================================ */

typedef struct ProvenMonotonicCounter ProvenMonotonicCounter;

ProvenMonotonicCounter* proven_monotonic_create(uint64_t initial, uint64_t max_value);
ProvenIntResult proven_monotonic_next(ProvenMonotonicCounter* counter);
void proven_monotonic_free(ProvenMonotonicCounter* counter);

/* ============================================================================
 * SafeStateMachine (FFI-level: proven_state_machine_*)
 * ============================================================================ */

typedef struct ProvenStateMachine ProvenStateMachine;

ProvenStateMachine* proven_state_machine_create(uint32_t state_count, uint32_t initial_state);
bool proven_state_machine_allow(ProvenStateMachine* sm, uint32_t from, uint32_t to);
bool proven_state_machine_transition(ProvenStateMachine* sm, uint32_t to);
uint32_t proven_state_machine_state(ProvenStateMachine* sm);
void proven_state_machine_free(ProvenStateMachine* sm);

/* ============================================================================
 * SafeTensor (FFI-level: proven_tensor_*)
 * ============================================================================ */

typedef struct ProvenTensor2D ProvenTensor2D;

ProvenTensor2D* proven_tensor_create(size_t rows, size_t cols);
ProvenStatus proven_tensor_set(ProvenTensor2D* tensor, size_t row, size_t col, double value);
ProvenFloatResult proven_tensor_get(ProvenTensor2D* tensor, size_t row, size_t col);
ProvenTensor2D* proven_tensor_matmul(ProvenTensor2D* a, ProvenTensor2D* b);
void proven_tensor_free(ProvenTensor2D* tensor);

/* ============================================================================
 * SafeML (FFI-level: proven_ml_*)
 * ============================================================================ */

ProvenStatus proven_ml_softmax(const double* input, double* output, size_t len);
double proven_ml_sigmoid(double x);
double proven_ml_relu(double x);

/* ============================================================================
 * SafePassword (FFI-level: proven_password_*)
 * ============================================================================ */

typedef struct ProvenPasswordResult {
    ProvenStatus status;
    uint32_t score;
    bool has_uppercase;
    bool has_lowercase;
    bool has_digit;
    bool has_special;
} ProvenPasswordResult;

ProvenPasswordResult proven_password_validate(const uint8_t* ptr, size_t len);
bool proven_password_is_common(const uint8_t* ptr, size_t len);

/* ============================================================================
 * SafeLRU (FFI-level: proven_lru_*)
 * ============================================================================ */

typedef struct ProvenLRUCache ProvenLRUCache;

ProvenLRUCache* proven_lru_create(size_t capacity);
ProvenIntResult proven_lru_get(ProvenLRUCache* cache, uint64_t key);
ProvenStatus proven_lru_put(ProvenLRUCache* cache, uint64_t key, int64_t value);
void proven_lru_free(ProvenLRUCache* cache);

/* ============================================================================
 * SafeGraph (FFI-level: proven_graph_*)
 * ============================================================================ */

typedef struct ProvenGraph ProvenGraph;

ProvenGraph* proven_graph_create(size_t node_count);
ProvenStatus proven_graph_add_edge(ProvenGraph* graph, size_t from, size_t to);
bool proven_graph_has_edge(ProvenGraph* graph, size_t from, size_t to);
void proven_graph_free(ProvenGraph* graph);

/* ============================================================================
 * HTTP URL Encoding (FFI-level: proven_http_*)
 * ============================================================================ */

ProvenStringResult proven_http_url_encode(const uint8_t* ptr, size_t len);
ProvenStringResult proven_http_url_decode(const uint8_t* ptr, size_t len);

/* ============================================================================
 * Version Information
 * ============================================================================ */

uint32_t proven_version_major(void);
uint32_t proven_version_minor(void);
uint32_t proven_version_patch(void);
uint32_t proven_module_count(void);

#ifdef __cplusplus
}
#endif

#endif /* PROVEN_SWIFT_H */
