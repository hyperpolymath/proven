;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven/ffi.fnl - LuaJIT FFI declarations for libproven.
;; All computation is performed in Idris 2 via the Zig FFI layer.
;; This module declares C types and loads the shared library.
;; Do NOT reimplement any logic.

;; Load LuaJIT FFI
(local ffi (require :ffi))

;; ============================================================================
;; C ABI type declarations
;; ============================================================================

(ffi.cdef "
    /* ------------------------------------------------------------------ */
    /* Status codes                                                       */
    /* ------------------------------------------------------------------ */
    typedef enum {
        PROVEN_OK                    =   0,
        PROVEN_ERR_NULL_POINTER      =  -1,
        PROVEN_ERR_INVALID_ARGUMENT  =  -2,
        PROVEN_ERR_OVERFLOW          =  -3,
        PROVEN_ERR_UNDERFLOW         =  -4,
        PROVEN_ERR_DIVISION_BY_ZERO  =  -5,
        PROVEN_ERR_PARSE_FAILURE     =  -6,
        PROVEN_ERR_VALIDATION_FAILED =  -7,
        PROVEN_ERR_OUT_OF_BOUNDS     =  -8,
        PROVEN_ERR_ENCODING_ERROR    =  -9,
        PROVEN_ERR_ALLOCATION_FAILED = -10,
        PROVEN_ERR_NOT_IMPLEMENTED   = -99
    } ProvenStatus;

    /* ------------------------------------------------------------------ */
    /* Core result types                                                   */
    /* ------------------------------------------------------------------ */
    typedef struct { int32_t status; int64_t value;         } IntResult;
    typedef struct { int32_t status; bool    value;         } BoolResult;
    typedef struct { int32_t status; char*   value; size_t length; } StringResult;
    typedef struct { int32_t status; double  value;         } FloatResult;

    /* ------------------------------------------------------------------ */
    /* Runtime lifecycle                                                   */
    /* ------------------------------------------------------------------ */
    int32_t  proven_init(void);
    void     proven_deinit(void);
    bool     proven_is_initialized(void);
    uint32_t proven_ffi_abi_version(void);
    uint32_t proven_version_major(void);
    uint32_t proven_version_minor(void);
    uint32_t proven_version_patch(void);
    uint32_t proven_module_count(void);

    /* ------------------------------------------------------------------ */
    /* Memory management                                                   */
    /* ------------------------------------------------------------------ */
    void proven_free_string(char* ptr);

    /* ------------------------------------------------------------------ */
    /* SafeMath                                                            */
    /* ------------------------------------------------------------------ */
    IntResult proven_math_div(int64_t a, int64_t b);
    IntResult proven_math_mod(int64_t a, int64_t b);
    IntResult proven_math_add_checked(int64_t a, int64_t b);
    IntResult proven_math_sub_checked(int64_t a, int64_t b);
    IntResult proven_math_mul_checked(int64_t a, int64_t b);
    IntResult proven_math_abs_safe(int64_t n);
    int64_t   proven_math_clamp(int64_t lo, int64_t hi, int64_t value);
    IntResult proven_math_pow_checked(int64_t base, uint32_t exp);

    /* ------------------------------------------------------------------ */
    /* SafeString                                                          */
    /* ------------------------------------------------------------------ */
    BoolResult   proven_string_is_valid_utf8(const char* ptr, size_t len);
    StringResult proven_string_escape_sql(const char* ptr, size_t len);
    StringResult proven_string_escape_html(const char* ptr, size_t len);
    StringResult proven_string_escape_js(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafePath                                                            */
    /* ------------------------------------------------------------------ */
    BoolResult   proven_path_has_traversal(const char* ptr, size_t len);
    StringResult proven_path_sanitize_filename(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafeEmail                                                           */
    /* ------------------------------------------------------------------ */
    BoolResult proven_email_is_valid(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafeNetwork                                                         */
    /* ------------------------------------------------------------------ */
    typedef struct { uint8_t octets[4]; } IPv4Address;
    typedef struct { int32_t status; uint8_t octets[4]; } IPv4Result;

    IPv4Result proven_network_parse_ipv4(const char* ptr, size_t len);
    bool       proven_network_ipv4_is_private(IPv4Address addr);
    bool       proven_network_ipv4_is_loopback(IPv4Address addr);

    /* ------------------------------------------------------------------ */
    /* SafeCrypto                                                          */
    /* ------------------------------------------------------------------ */
    BoolResult proven_crypto_constant_time_eq(const char* p1, size_t l1,
                                              const char* p2, size_t l2);
    int32_t    proven_crypto_random_bytes(char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafeUrl                                                             */
    /* ------------------------------------------------------------------ */
    typedef struct {
        char*  scheme;      size_t scheme_len;
        char*  host;        size_t host_len;
        uint16_t port;      bool   has_port;
        char*  path;        size_t path_len;
        char*  query;       size_t query_len;
        char*  fragment;    size_t fragment_len;
    } UrlComponents;
    typedef struct { int32_t status; UrlComponents components; } UrlResult;

    UrlResult proven_url_parse(const char* ptr, size_t len);
    void      proven_url_free(UrlComponents* components);

    /* ------------------------------------------------------------------ */
    /* SafeJson                                                            */
    /* ------------------------------------------------------------------ */
    BoolResult proven_json_is_valid(const char* ptr, size_t len);
    int32_t    proven_json_get_type(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafeHeader                                                          */
    /* ------------------------------------------------------------------ */
    BoolResult   proven_header_has_crlf(const char* ptr, size_t len);
    BoolResult   proven_header_is_valid_name(const char* ptr, size_t len);
    BoolResult   proven_header_is_dangerous(const char* ptr, size_t len);
    StringResult proven_header_render(const char* name, size_t name_len,
                                      const char* value, size_t value_len);
    StringResult proven_header_build_csp(const char* json, size_t len);
    StringResult proven_header_build_hsts(int64_t max_age,
                                          bool include_subdomains,
                                          bool preload);

    /* ------------------------------------------------------------------ */
    /* SafeFloat                                                           */
    /* ------------------------------------------------------------------ */
    FloatResult proven_float_div(double a, double b);
    bool        proven_float_is_finite(double x);
    bool        proven_float_is_nan(double x);
    FloatResult proven_float_sqrt(double x);
    FloatResult proven_float_ln(double x);

    /* ------------------------------------------------------------------ */
    /* SafeHex                                                             */
    /* ------------------------------------------------------------------ */
    typedef struct { int32_t status; char* data; size_t length; } HexDecodeResult;

    StringResult    proven_hex_encode(const char* ptr, size_t len, bool uppercase);
    HexDecodeResult proven_hex_decode(const char* ptr, size_t len);
    void            proven_hex_free(HexDecodeResult* result);

    /* ------------------------------------------------------------------ */
    /* SafeUUID                                                            */
    /* ------------------------------------------------------------------ */
    typedef struct { uint8_t bytes[16]; } UUID;
    typedef struct { int32_t status; UUID uuid; } UUIDResult;

    UUIDResult   proven_uuid_v4(void);
    StringResult proven_uuid_to_string(UUID uuid);
    UUIDResult   proven_uuid_parse(const char* ptr, size_t len);
    bool         proven_uuid_is_nil(UUID uuid);
    uint8_t      proven_uuid_version(UUID uuid);

    /* ------------------------------------------------------------------ */
    /* SafeDateTime                                                        */
    /* ------------------------------------------------------------------ */
    typedef struct {
        int32_t  year;
        uint8_t  month;
        uint8_t  day;
        uint8_t  hour;
        uint8_t  minute;
        uint8_t  second;
        uint32_t nanosecond;
        int16_t  tz_offset_minutes;
    } DateTime;
    typedef struct { int32_t status; DateTime datetime; } DateTimeResult;

    DateTimeResult proven_datetime_parse(const char* ptr, size_t len);
    StringResult   proven_datetime_format_iso8601(DateTime dt);
    bool           proven_datetime_is_leap_year(int32_t year);
    uint8_t        proven_datetime_days_in_month(int32_t year, uint8_t month);

    /* ------------------------------------------------------------------ */
    /* SafeVersion                                                         */
    /* ------------------------------------------------------------------ */
    typedef struct {
        uint32_t major;
        uint32_t minor;
        uint32_t patch;
        size_t   prerelease_len;
        char*    prerelease;
    } SemanticVersion;
    typedef struct { int32_t status; SemanticVersion version; } VersionResult;

    VersionResult proven_version_parse(const char* ptr, size_t len);
    int32_t       proven_version_compare(SemanticVersion a, SemanticVersion b);
    void          proven_version_free(SemanticVersion* version);

    /* ------------------------------------------------------------------ */
    /* SafePassword                                                        */
    /* ------------------------------------------------------------------ */
    typedef struct {
        int32_t strength;
        bool    has_lowercase;
        bool    has_uppercase;
        bool    has_digit;
        bool    has_special;
        size_t  length;
    } PasswordResult;

    PasswordResult proven_password_validate(const char* ptr, size_t len);
    bool           proven_password_is_common(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafeColor                                                           */
    /* ------------------------------------------------------------------ */
    typedef struct { uint8_t r; uint8_t g; uint8_t b; } RGBColor;
    typedef struct { double h; double s; double l; } HSLColor;
    typedef struct { int32_t status; RGBColor color; } ColorParseResult;

    ColorParseResult proven_color_parse_hex(const char* ptr, size_t len);
    HSLColor         proven_color_rgb_to_hsl(RGBColor rgb);
    StringResult     proven_color_to_hex(RGBColor rgb);

    /* ------------------------------------------------------------------ */
    /* SafeChecksum                                                        */
    /* ------------------------------------------------------------------ */
    IntResult  proven_checksum_crc32(const char* ptr, size_t len);
    BoolResult proven_checksum_verify_crc32(const char* ptr, size_t len,
                                            uint32_t expected);

    /* ------------------------------------------------------------------ */
    /* SafeProbability                                                     */
    /* ------------------------------------------------------------------ */
    double proven_probability_create(double value);
    double proven_probability_and(double a, double b);
    double proven_probability_or_exclusive(double a, double b);
    double proven_probability_not(double p);

    /* ------------------------------------------------------------------ */
    /* SafeCalculator                                                      */
    /* ------------------------------------------------------------------ */
    FloatResult proven_calculator_eval(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafeHTTP                                                            */
    /* ------------------------------------------------------------------ */
    StringResult proven_http_url_encode(const char* ptr, size_t len);
    StringResult proven_http_url_decode(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafeAngle                                                           */
    /* ------------------------------------------------------------------ */
    double proven_angle_deg_to_rad(double degrees);
    double proven_angle_rad_to_deg(double radians);
    double proven_angle_normalize_degrees(double degrees);
    double proven_angle_normalize_radians(double radians);

    /* ------------------------------------------------------------------ */
    /* SafeUnit                                                            */
    /* ------------------------------------------------------------------ */
    FloatResult proven_unit_convert_length(double value, int32_t from, int32_t to);
    FloatResult proven_unit_convert_temp(double value, int32_t from, int32_t to);

    /* ------------------------------------------------------------------ */
    /* SafeML                                                              */
    /* ------------------------------------------------------------------ */
    int32_t proven_ml_softmax(const double* input, double* output, size_t len);
    double  proven_ml_sigmoid(double x);
    double  proven_ml_relu(double x);
    double  proven_ml_leaky_relu(double x, double alpha);
    double  proven_ml_clamp(double x, double min_val, double max_val);
")

;; ============================================================================
;; Load the shared library
;; ============================================================================

(local lib (ffi.load :proven))

;; ============================================================================
;; Result extraction helpers
;; ============================================================================

(fn int-result [r]
  "Extract value from IntResult. Returns nil on error."
  (when (= r.status 0)
    (tonumber r.value)))

(fn bool-result [r]
  "Extract value from BoolResult. Returns nil on error."
  (when (= r.status 0)
    r.value))

(fn float-result [r]
  "Extract value from FloatResult. Returns nil on error."
  (when (= r.status 0)
    (tonumber r.value)))

(fn string-result [r]
  "Extract string from StringResult, free C memory. Returns nil on error."
  (when (and (= r.status 0) (not= r.value nil))
    (let [s (ffi.string r.value r.length)]
      (lib.proven_free_string r.value)
      s)))

;; ============================================================================
;; Export module table
;; ============================================================================

{: lib
 : ffi
 : int-result
 : bool-result
 : float-result
 : string-result}
