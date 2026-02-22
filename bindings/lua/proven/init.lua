-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- Proven - FFI binding to libproven (formally verified safety library).
-- All computation is performed in Idris 2 via the Zig FFI layer.
-- This module is a thin wrapper; it does NOT reimplement any logic.

--- @module proven

local ffi = require("ffi")

ffi.cdef[[
    /* ------------------------------------------------------------------ */
    /* Status and result types                                            */
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

    typedef struct { int32_t status; int64_t value;         } IntResult;
    typedef struct { int32_t status; bool    value;         } BoolResult;
    typedef struct { int32_t status; char*   value; size_t length; } StringResult;
    typedef struct { int32_t status; double  value;         } FloatResult;

    /* ------------------------------------------------------------------ */
    /* Runtime lifecycle                                                  */
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
    /* Memory management                                                  */
    /* ------------------------------------------------------------------ */
    void proven_free_string(char* ptr);

    /* ------------------------------------------------------------------ */
    /* SafeMath (8 functions)                                             */
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
    /* SafeString (4 functions)                                           */
    /* ------------------------------------------------------------------ */
    BoolResult   proven_string_is_valid_utf8(const char* ptr, size_t len);
    StringResult proven_string_escape_sql(const char* ptr, size_t len);
    StringResult proven_string_escape_html(const char* ptr, size_t len);
    StringResult proven_string_escape_js(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafePath (2 functions)                                             */
    /* ------------------------------------------------------------------ */
    BoolResult   proven_path_has_traversal(const char* ptr, size_t len);
    StringResult proven_path_sanitize_filename(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafeEmail (1 function)                                             */
    /* ------------------------------------------------------------------ */
    BoolResult proven_email_is_valid(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafeNetwork (3 functions)                                          */
    /* ------------------------------------------------------------------ */
    typedef struct { int32_t status; uint8_t octets[4]; } IPv4Result;
    typedef struct { uint8_t octets[4]; } IPv4Address;

    IPv4Result proven_network_parse_ipv4(const char* ptr, size_t len);
    bool       proven_network_ipv4_is_private(IPv4Address addr);
    bool       proven_network_ipv4_is_loopback(IPv4Address addr);

    /* ------------------------------------------------------------------ */
    /* SafeCrypto (2 functions)                                           */
    /* ------------------------------------------------------------------ */
    BoolResult proven_crypto_constant_time_eq(const char* p1, size_t l1,
                                              const char* p2, size_t l2);
    int32_t    proven_crypto_random_bytes(char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafeUrl (2+1 functions)                                            */
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
    /* SafeJson (2 functions)                                             */
    /* ------------------------------------------------------------------ */
    BoolResult proven_json_is_valid(const char* ptr, size_t len);
    int32_t    proven_json_get_type(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafeHeader (6 functions)                                           */
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
    /* SafeCookie (6 functions)                                           */
    /* ------------------------------------------------------------------ */
    typedef struct {
        const char* domain;     size_t domain_len;
        const char* path;       size_t path_len;
        int64_t max_age;
        bool    secure;
        bool    http_only;
        int32_t same_site;
        bool    partitioned;
    } CookieAttributes;

    BoolResult   proven_cookie_has_injection(const char* ptr, size_t len);
    BoolResult   proven_cookie_validate_name(const char* ptr, size_t len);
    BoolResult   proven_cookie_validate_value(const char* ptr, size_t len);
    IntResult    proven_cookie_get_prefix(const char* ptr, size_t len);
    StringResult proven_cookie_build_set_cookie(const char* name, size_t nl,
                                                const char* value, size_t vl,
                                                CookieAttributes attrs);
    StringResult proven_cookie_build_delete(const char* name, size_t nl);

    /* ------------------------------------------------------------------ */
    /* SafeContentType (5 functions + free)                               */
    /* ------------------------------------------------------------------ */
    typedef struct {
        int32_t status;
        char*   media_type;  size_t media_type_len;
        char*   subtype;     size_t subtype_len;
        char*   suffix;      size_t suffix_len;
        int32_t category;
        int32_t charset;
        bool    has_charset;
    } ContentTypeResult;

    ContentTypeResult proven_content_type_parse(const char* ptr, size_t len);
    void              proven_content_type_free(ContentTypeResult* result);
    BoolResult        proven_content_type_can_sniff_dangerous(const char* ptr, size_t len);
    StringResult      proven_content_type_render(const char* type_p, size_t tl,
                                                 const char* sub_p, size_t sl,
                                                 const char* suf_p, size_t fl,
                                                 int32_t charset, bool has_cs);
    BoolResult        proven_content_type_is_json(const char* sub, size_t sl,
                                                   const char* suf, size_t fl);
    BoolResult        proven_content_type_is_xml(const char* sub, size_t sl,
                                                  const char* suf, size_t fl);

    /* ------------------------------------------------------------------ */
    /* SafeHex (2+1 functions)                                            */
    /* ------------------------------------------------------------------ */
    typedef struct { int32_t status; char* data; size_t length; } HexDecodeResult;

    StringResult   proven_hex_encode(const char* ptr, size_t len, bool uppercase);
    HexDecodeResult proven_hex_decode(const char* ptr, size_t len);
    void           proven_hex_free(HexDecodeResult* result);

    /* ------------------------------------------------------------------ */
    /* SafeUUID (5 functions)                                             */
    /* ------------------------------------------------------------------ */
    typedef struct { uint8_t bytes[16]; } UUID;
    typedef struct { int32_t status; UUID uuid; } UUIDResult;

    UUIDResult   proven_uuid_v4(void);
    StringResult proven_uuid_to_string(UUID uuid);
    UUIDResult   proven_uuid_parse(const char* ptr, size_t len);
    bool         proven_uuid_is_nil(UUID uuid);
    uint8_t      proven_uuid_version(UUID uuid);

    /* ------------------------------------------------------------------ */
    /* SafeCurrency (2 functions)                                         */
    /* ------------------------------------------------------------------ */
    typedef struct {
        int32_t  status;
        int64_t  amount_minor;
        uint8_t  currency_code[3];
        uint8_t  decimal_places;
    } CurrencyResult;

    CurrencyResult proven_currency_parse(const char* ptr, size_t len);
    StringResult   proven_currency_format(int64_t amount_minor,
                                          uint8_t code[3], uint8_t dp);

    /* ------------------------------------------------------------------ */
    /* SafePhone (2 functions)                                            */
    /* ------------------------------------------------------------------ */
    typedef struct {
        int32_t  status;
        uint16_t country_code;
        uint64_t national_number;
        bool     is_valid;
    } PhoneResult;

    PhoneResult  proven_phone_parse(const char* ptr, size_t len);
    StringResult proven_phone_format_e164(uint16_t cc, uint64_t nn);

    /* ------------------------------------------------------------------ */
    /* SafeDateTime (4 functions)                                         */
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
    /* SafeFloat (5 functions)                                            */
    /* ------------------------------------------------------------------ */
    FloatResult proven_float_div(double a, double b);
    bool        proven_float_is_finite(double x);
    bool        proven_float_is_nan(double x);
    FloatResult proven_float_sqrt(double x);
    FloatResult proven_float_ln(double x);

    /* ------------------------------------------------------------------ */
    /* SafeVersion (2+1 functions)                                        */
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
    /* SafeGeo (3 functions)                                              */
    /* ------------------------------------------------------------------ */
    typedef struct { double latitude; double longitude; } GeoCoordinate;
    typedef struct { int32_t status; GeoCoordinate coordinate; } GeoResult;

    GeoResult   proven_geo_validate(double lat, double lon);
    FloatResult proven_geo_distance(GeoCoordinate a, GeoCoordinate b);
    bool        proven_geo_in_bounds(GeoCoordinate coord,
                                     double min_lat, double max_lat,
                                     double min_lon, double max_lon);

    /* ------------------------------------------------------------------ */
    /* SafeChecksum (2 functions)                                         */
    /* ------------------------------------------------------------------ */
    IntResult  proven_checksum_crc32(const char* ptr, size_t len);
    BoolResult proven_checksum_verify_crc32(const char* ptr, size_t len,
                                            uint32_t expected);

    /* ------------------------------------------------------------------ */
    /* SafeProbability (4 functions)                                      */
    /* ------------------------------------------------------------------ */
    double proven_probability_create(double value);
    double proven_probability_and(double a, double b);
    double proven_probability_or_exclusive(double a, double b);
    double proven_probability_not(double p);

    /* ------------------------------------------------------------------ */
    /* SafeCalculator (1 function)                                        */
    /* ------------------------------------------------------------------ */
    FloatResult proven_calculator_eval(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafePassword (2 functions)                                         */
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
    /* SafeColor (3 functions)                                            */
    /* ------------------------------------------------------------------ */
    typedef struct { uint8_t r; uint8_t g; uint8_t b; } RGBColor;
    typedef struct { double h; double s; double l; } HSLColor;
    typedef struct { int32_t status; RGBColor color; } ColorParseResult;

    ColorParseResult proven_color_parse_hex(const char* ptr, size_t len);
    HSLColor         proven_color_rgb_to_hsl(RGBColor rgb);
    StringResult     proven_color_to_hex(RGBColor rgb);

    /* ------------------------------------------------------------------ */
    /* SafeAngle (4 functions)                                            */
    /* ------------------------------------------------------------------ */
    double proven_angle_deg_to_rad(double degrees);
    double proven_angle_rad_to_deg(double radians);
    double proven_angle_normalize_degrees(double degrees);
    double proven_angle_normalize_radians(double radians);

    /* ------------------------------------------------------------------ */
    /* SafeUnit (2 functions)                                             */
    /* ------------------------------------------------------------------ */
    FloatResult proven_unit_convert_length(double value, int32_t from, int32_t to);
    FloatResult proven_unit_convert_temp(double value, int32_t from, int32_t to);

    /* ------------------------------------------------------------------ */
    /* SafeML (5 functions)                                               */
    /* ------------------------------------------------------------------ */
    int32_t proven_ml_softmax(const double* input, double* output, size_t len);
    double  proven_ml_sigmoid(double x);
    double  proven_ml_relu(double x);
    double  proven_ml_leaky_relu(double x, double alpha);
    double  proven_ml_clamp(double x, double min_val, double max_val);

    /* ------------------------------------------------------------------ */
    /* SafeHTTP (3 functions)                                             */
    /* ------------------------------------------------------------------ */
    StringResult proven_http_url_encode(const char* ptr, size_t len);
    StringResult proven_http_url_decode(const char* ptr, size_t len);

    typedef struct {
        int32_t status;
        struct {
            char*  scheme;  size_t scheme_len;
            char*  realm;   size_t realm_len;
            char*  service; size_t service_len;
            char*  scope;   size_t scope_len;
        } challenge;
    } AuthChallengeResult;
    AuthChallengeResult proven_http_parse_www_authenticate(const char* ptr, size_t len);

    /* ------------------------------------------------------------------ */
    /* SafeRegistry (3 functions)                                         */
    /* ------------------------------------------------------------------ */
    typedef struct {
        char*  registry;   size_t registry_len;
        char*  repository; size_t repository_len;
        char*  tag;        size_t tag_len;
        char*  digest;     size_t digest_len;
    } ImageReference;
    typedef struct { int32_t status; ImageReference reference; } ImageRefResult;

    ImageRefResult proven_registry_parse(const char* ptr, size_t len);
    StringResult   proven_registry_to_string(const ImageReference* ref);
    BoolResult     proven_registry_has_registry(const ImageReference* ref);

    /* ------------------------------------------------------------------ */
    /* SafeDigest (3 functions)                                           */
    /* ------------------------------------------------------------------ */
    typedef struct { uint8_t algorithm; char* value; size_t value_len; } Digest;
    typedef struct { int32_t status; Digest digest; } DigestResult;

    DigestResult proven_digest_parse(const char* ptr, size_t len);
    BoolResult   proven_digest_verify(const Digest* expected, const Digest* actual);
    StringResult proven_digest_to_string(const Digest* digest);

    /* ------------------------------------------------------------------ */
    /* Callback infrastructure (6 functions)                              */
    /* ------------------------------------------------------------------ */
    typedef int32_t (*ProvenCallbackFn)(void* context, const void* event);

    uint32_t proven_callback_register(int32_t event_type,
                                      ProvenCallbackFn callback,
                                      void* context);
    int32_t  proven_callback_unregister(uint32_t handle);
    int32_t  proven_callback_fire(int32_t event_type, const char* data,
                                  size_t data_len, int32_t code);
    uint32_t proven_callback_count(int32_t event_type);
    uint32_t proven_callback_clear_all(void);
]]

-- Load the shared library
local lib = ffi.load("proven")

-- ============================================================================
-- Helper: extract value from IntResult, return nil on error
-- ============================================================================
local function int_result(r)
    if r.status ~= 0 then return nil end
    return tonumber(r.value)
end

-- ============================================================================
-- Helper: extract value from BoolResult, return nil on error
-- ============================================================================
local function bool_result(r)
    if r.status ~= 0 then return nil end
    return r.value
end

-- ============================================================================
-- Helper: extract value from FloatResult, return nil on error
-- ============================================================================
local function float_result(r)
    if r.status ~= 0 then return nil end
    return tonumber(r.value)
end

-- ============================================================================
-- Helper: extract string from StringResult, free C memory, return nil on error
-- ============================================================================
local function string_result(r)
    if r.status ~= 0 or r.value == nil then return nil end
    local s = ffi.string(r.value, r.length)
    lib.proven_free_string(r.value)
    return s
end

-- ============================================================================
-- Module table
-- ============================================================================
local proven = {
    _VERSION     = "0.9.0",
    _DESCRIPTION = "FFI binding to libproven - formally verified safety library",
    _LICENSE     = "PMPL-1.0-or-later",
}

-- ============================================================================
-- Runtime lifecycle
-- ============================================================================
function proven.init()
    return lib.proven_init() == 0
end

function proven.deinit()
    lib.proven_deinit()
end

function proven.is_initialized()
    return lib.proven_is_initialized()
end

function proven.abi_version()
    return tonumber(lib.proven_ffi_abi_version())
end

function proven.version()
    return string.format("%d.%d.%d",
        lib.proven_version_major(),
        lib.proven_version_minor(),
        lib.proven_version_patch())
end

function proven.module_count()
    return tonumber(lib.proven_module_count())
end

-- ============================================================================
-- SafeMath
-- ============================================================================
proven.SafeMath = {}

function proven.SafeMath.div(a, b)
    return int_result(lib.proven_math_div(a, b))
end

function proven.SafeMath.mod(a, b)
    return int_result(lib.proven_math_mod(a, b))
end

function proven.SafeMath.add_checked(a, b)
    return int_result(lib.proven_math_add_checked(a, b))
end

function proven.SafeMath.sub_checked(a, b)
    return int_result(lib.proven_math_sub_checked(a, b))
end

function proven.SafeMath.mul_checked(a, b)
    return int_result(lib.proven_math_mul_checked(a, b))
end

function proven.SafeMath.abs_safe(n)
    return int_result(lib.proven_math_abs_safe(n))
end

function proven.SafeMath.clamp(lo, hi, value)
    return tonumber(lib.proven_math_clamp(lo, hi, value))
end

function proven.SafeMath.pow_checked(base, exp)
    return int_result(lib.proven_math_pow_checked(base, exp))
end

-- ============================================================================
-- SafeString
-- ============================================================================
proven.SafeString = {}

function proven.SafeString.is_valid_utf8(s)
    return bool_result(lib.proven_string_is_valid_utf8(s, #s))
end

function proven.SafeString.escape_sql(s)
    return string_result(lib.proven_string_escape_sql(s, #s))
end

function proven.SafeString.escape_html(s)
    return string_result(lib.proven_string_escape_html(s, #s))
end

function proven.SafeString.escape_js(s)
    return string_result(lib.proven_string_escape_js(s, #s))
end

-- ============================================================================
-- SafePath
-- ============================================================================
proven.SafePath = {}

function proven.SafePath.has_traversal(s)
    return bool_result(lib.proven_path_has_traversal(s, #s))
end

function proven.SafePath.sanitize_filename(s)
    return string_result(lib.proven_path_sanitize_filename(s, #s))
end

-- ============================================================================
-- SafeEmail
-- ============================================================================
proven.SafeEmail = {}

function proven.SafeEmail.is_valid(s)
    return bool_result(lib.proven_email_is_valid(s, #s))
end

-- ============================================================================
-- SafeNetwork
-- ============================================================================
proven.SafeNetwork = {}

function proven.SafeNetwork.parse_ipv4(s)
    local r = lib.proven_network_parse_ipv4(s, #s)
    if r.status ~= 0 then return nil end
    return {
        a = r.octets[0],
        b = r.octets[1],
        c = r.octets[2],
        d = r.octets[3],
    }
end

function proven.SafeNetwork.ipv4_is_private(addr)
    local a = ffi.new("IPv4Address")
    a.octets[0] = addr.a
    a.octets[1] = addr.b
    a.octets[2] = addr.c
    a.octets[3] = addr.d
    return lib.proven_network_ipv4_is_private(a)
end

function proven.SafeNetwork.ipv4_is_loopback(addr)
    local a = ffi.new("IPv4Address")
    a.octets[0] = addr.a
    a.octets[1] = addr.b
    a.octets[2] = addr.c
    a.octets[3] = addr.d
    return lib.proven_network_ipv4_is_loopback(a)
end

-- ============================================================================
-- SafeCrypto
-- ============================================================================
proven.SafeCrypto = {}

function proven.SafeCrypto.constant_time_eq(a, b)
    return bool_result(lib.proven_crypto_constant_time_eq(a, #a, b, #b))
end

function proven.SafeCrypto.random_bytes(n)
    local buf = ffi.new("char[?]", n)
    local status = lib.proven_crypto_random_bytes(buf, n)
    if status ~= 0 then return nil end
    return ffi.string(buf, n)
end

-- ============================================================================
-- SafeUrl
-- ============================================================================
proven.SafeUrl = {}

function proven.SafeUrl.parse(s)
    local r = lib.proven_url_parse(s, #s)
    if r.status ~= 0 then return nil end
    local c = r.components
    local result = {}
    if c.scheme ~= nil and c.scheme_len > 0 then
        result.scheme = ffi.string(c.scheme, c.scheme_len)
    end
    if c.host ~= nil and c.host_len > 0 then
        result.host = ffi.string(c.host, c.host_len)
    end
    if c.has_port then
        result.port = tonumber(c.port)
    end
    if c.path ~= nil and c.path_len > 0 then
        result.path = ffi.string(c.path, c.path_len)
    end
    if c.query ~= nil and c.query_len > 0 then
        result.query = ffi.string(c.query, c.query_len)
    end
    if c.fragment ~= nil and c.fragment_len > 0 then
        result.fragment = ffi.string(c.fragment, c.fragment_len)
    end
    lib.proven_url_free(r.components)
    return result
end

-- ============================================================================
-- SafeJson
-- ============================================================================
proven.SafeJson = {}

function proven.SafeJson.is_valid(s)
    return bool_result(lib.proven_json_is_valid(s, #s))
end

function proven.SafeJson.get_type(s)
    local t = lib.proven_json_get_type(s, #s)
    local type_names = {
        [0]  = "null",
        [1]  = "boolean",
        [2]  = "number",
        [3]  = "string",
        [4]  = "array",
        [5]  = "object",
        [-1] = "invalid",
    }
    return type_names[t] or "invalid"
end

-- ============================================================================
-- SafeHeader
-- ============================================================================
proven.SafeHeader = {}

function proven.SafeHeader.has_crlf(s)
    return bool_result(lib.proven_header_has_crlf(s, #s))
end

function proven.SafeHeader.is_valid_name(s)
    return bool_result(lib.proven_header_is_valid_name(s, #s))
end

function proven.SafeHeader.is_dangerous(s)
    return bool_result(lib.proven_header_is_dangerous(s, #s))
end

function proven.SafeHeader.render(name, value)
    return string_result(lib.proven_header_render(name, #name, value, #value))
end

function proven.SafeHeader.build_csp(directives_json)
    return string_result(lib.proven_header_build_csp(directives_json, #directives_json))
end

function proven.SafeHeader.build_hsts(max_age, include_subdomains, preload)
    return string_result(lib.proven_header_build_hsts(max_age,
        include_subdomains or false, preload or false))
end

-- ============================================================================
-- SafeCookie
-- ============================================================================
proven.SafeCookie = {}

function proven.SafeCookie.has_injection(s)
    return bool_result(lib.proven_cookie_has_injection(s, #s))
end

function proven.SafeCookie.validate_name(s)
    return bool_result(lib.proven_cookie_validate_name(s, #s))
end

function proven.SafeCookie.validate_value(s)
    return bool_result(lib.proven_cookie_validate_value(s, #s))
end

function proven.SafeCookie.get_prefix(s)
    return int_result(lib.proven_cookie_get_prefix(s, #s))
end

function proven.SafeCookie.build_set_cookie(name, value, attrs)
    attrs = attrs or {}
    local ca = ffi.new("CookieAttributes")
    if attrs.domain then
        ca.domain = attrs.domain
        ca.domain_len = #attrs.domain
    end
    if attrs.path then
        ca.path = attrs.path
        ca.path_len = #attrs.path
    end
    ca.max_age = attrs.max_age or -1
    ca.secure = attrs.secure or false
    ca.http_only = attrs.http_only or false
    ca.same_site = attrs.same_site or 1
    ca.partitioned = attrs.partitioned or false
    return string_result(lib.proven_cookie_build_set_cookie(
        name, #name, value, #value, ca))
end

function proven.SafeCookie.build_delete(name)
    return string_result(lib.proven_cookie_build_delete(name, #name))
end

-- ============================================================================
-- SafeContentType
-- ============================================================================
proven.SafeContentType = {}

function proven.SafeContentType.parse(s)
    local r = lib.proven_content_type_parse(s, #s)
    if r.status ~= 0 then return nil end
    local result = { category = tonumber(r.category), charset = tonumber(r.charset),
                     has_charset = r.has_charset }
    if r.media_type ~= nil and r.media_type_len > 0 then
        result.media_type = ffi.string(r.media_type, r.media_type_len)
    end
    if r.subtype ~= nil and r.subtype_len > 0 then
        result.subtype = ffi.string(r.subtype, r.subtype_len)
    end
    if r.suffix ~= nil and r.suffix_len > 0 then
        result.suffix = ffi.string(r.suffix, r.suffix_len)
    end
    lib.proven_content_type_free(ffi.new("ContentTypeResult[1]", {r}))
    return result
end

function proven.SafeContentType.can_sniff_dangerous(s)
    return bool_result(lib.proven_content_type_can_sniff_dangerous(s, #s))
end

function proven.SafeContentType.is_json(subtype, suffix)
    suffix = suffix or ""
    return bool_result(lib.proven_content_type_is_json(subtype, #subtype, suffix, #suffix))
end

function proven.SafeContentType.is_xml(subtype, suffix)
    suffix = suffix or ""
    return bool_result(lib.proven_content_type_is_xml(subtype, #subtype, suffix, #suffix))
end

-- ============================================================================
-- SafeHex
-- ============================================================================
proven.SafeHex = {}

function proven.SafeHex.encode(data, uppercase)
    return string_result(lib.proven_hex_encode(data, #data, uppercase or false))
end

function proven.SafeHex.decode(hex_string)
    local r = lib.proven_hex_decode(hex_string, #hex_string)
    if r.status ~= 0 or r.data == nil then return nil end
    local s = ffi.string(r.data, r.length)
    lib.proven_hex_free(ffi.new("HexDecodeResult[1]", {r}))
    return s
end

-- ============================================================================
-- SafeUUID
-- ============================================================================
proven.SafeUUID = {}

function proven.SafeUUID.v4()
    local r = lib.proven_uuid_v4()
    if r.status ~= 0 then return nil end
    return string_result(lib.proven_uuid_to_string(r.uuid))
end

function proven.SafeUUID.parse(s)
    local r = lib.proven_uuid_parse(s, #s)
    if r.status ~= 0 then return nil end
    local bytes = {}
    for i = 0, 15 do bytes[i + 1] = r.uuid.bytes[i] end
    return bytes
end

function proven.SafeUUID.to_string(uuid_bytes)
    local uuid = ffi.new("UUID")
    for i = 0, 15 do uuid.bytes[i] = uuid_bytes[i + 1] end
    return string_result(lib.proven_uuid_to_string(uuid))
end

function proven.SafeUUID.is_nil(uuid_bytes)
    local uuid = ffi.new("UUID")
    for i = 0, 15 do uuid.bytes[i] = uuid_bytes[i + 1] end
    return lib.proven_uuid_is_nil(uuid)
end

function proven.SafeUUID.version(uuid_bytes)
    local uuid = ffi.new("UUID")
    for i = 0, 15 do uuid.bytes[i] = uuid_bytes[i + 1] end
    return tonumber(lib.proven_uuid_version(uuid))
end

-- ============================================================================
-- SafeCurrency
-- ============================================================================
proven.SafeCurrency = {}

function proven.SafeCurrency.parse(s)
    local r = lib.proven_currency_parse(s, #s)
    if r.status ~= 0 then return nil end
    local code = string.char(r.currency_code[0], r.currency_code[1], r.currency_code[2])
    return {
        amount_minor   = tonumber(r.amount_minor),
        currency_code  = code,
        decimal_places = tonumber(r.decimal_places),
    }
end

function proven.SafeCurrency.format(amount_minor, code, decimal_places)
    local c = ffi.new("uint8_t[3]", {code:byte(1), code:byte(2), code:byte(3)})
    return string_result(lib.proven_currency_format(amount_minor, c, decimal_places))
end

-- ============================================================================
-- SafePhone
-- ============================================================================
proven.SafePhone = {}

function proven.SafePhone.parse(s)
    local r = lib.proven_phone_parse(s, #s)
    if r.status ~= 0 then return nil end
    return {
        country_code    = tonumber(r.country_code),
        national_number = tonumber(r.national_number),
        is_valid        = r.is_valid,
    }
end

function proven.SafePhone.format_e164(country_code, national_number)
    return string_result(lib.proven_phone_format_e164(country_code, national_number))
end

-- ============================================================================
-- SafeDateTime
-- ============================================================================
proven.SafeDateTime = {}

function proven.SafeDateTime.parse(s)
    local r = lib.proven_datetime_parse(s, #s)
    if r.status ~= 0 then return nil end
    local dt = r.datetime
    return {
        year              = tonumber(dt.year),
        month             = tonumber(dt.month),
        day               = tonumber(dt.day),
        hour              = tonumber(dt.hour),
        minute            = tonumber(dt.minute),
        second            = tonumber(dt.second),
        nanosecond        = tonumber(dt.nanosecond),
        tz_offset_minutes = tonumber(dt.tz_offset_minutes),
    }
end

function proven.SafeDateTime.format_iso8601(dt)
    local c_dt = ffi.new("DateTime")
    c_dt.year              = dt.year
    c_dt.month             = dt.month
    c_dt.day               = dt.day
    c_dt.hour              = dt.hour   or 0
    c_dt.minute            = dt.minute or 0
    c_dt.second            = dt.second or 0
    c_dt.nanosecond        = dt.nanosecond or 0
    c_dt.tz_offset_minutes = dt.tz_offset_minutes or 0
    return string_result(lib.proven_datetime_format_iso8601(c_dt))
end

function proven.SafeDateTime.is_leap_year(year)
    return lib.proven_datetime_is_leap_year(year)
end

function proven.SafeDateTime.days_in_month(year, month)
    return tonumber(lib.proven_datetime_days_in_month(year, month))
end

-- ============================================================================
-- SafeFloat
-- ============================================================================
proven.SafeFloat = {}

function proven.SafeFloat.div(a, b)
    return float_result(lib.proven_float_div(a, b))
end

function proven.SafeFloat.is_finite(x)
    return lib.proven_float_is_finite(x)
end

function proven.SafeFloat.is_nan(x)
    return lib.proven_float_is_nan(x)
end

function proven.SafeFloat.sqrt(x)
    return float_result(lib.proven_float_sqrt(x))
end

function proven.SafeFloat.ln(x)
    return float_result(lib.proven_float_ln(x))
end

-- ============================================================================
-- SafeVersion
-- ============================================================================
proven.SafeVersion = {}

function proven.SafeVersion.parse(s)
    local r = lib.proven_version_parse(s, #s)
    if r.status ~= 0 then return nil end
    local v = r.version
    local result = {
        major = tonumber(v.major),
        minor = tonumber(v.minor),
        patch = tonumber(v.patch),
    }
    if v.prerelease ~= nil and v.prerelease_len > 0 then
        result.prerelease = ffi.string(v.prerelease, v.prerelease_len)
    end
    lib.proven_version_free(ffi.new("SemanticVersion[1]", {v}))
    return result
end

function proven.SafeVersion.compare(a, b)
    local va = ffi.new("SemanticVersion")
    va.major = a.major; va.minor = a.minor; va.patch = a.patch
    va.prerelease_len = 0; va.prerelease = nil
    if a.prerelease then
        va.prerelease = ffi.cast("char*", a.prerelease)
        va.prerelease_len = #a.prerelease
    end
    local vb = ffi.new("SemanticVersion")
    vb.major = b.major; vb.minor = b.minor; vb.patch = b.patch
    vb.prerelease_len = 0; vb.prerelease = nil
    if b.prerelease then
        vb.prerelease = ffi.cast("char*", b.prerelease)
        vb.prerelease_len = #b.prerelease
    end
    return tonumber(lib.proven_version_compare(va, vb))
end

-- ============================================================================
-- SafeGeo
-- ============================================================================
proven.SafeGeo = {}

function proven.SafeGeo.validate(lat, lon)
    local r = lib.proven_geo_validate(lat, lon)
    if r.status ~= 0 then return nil end
    return {
        latitude  = tonumber(r.coordinate.latitude),
        longitude = tonumber(r.coordinate.longitude),
    }
end

function proven.SafeGeo.distance(a, b)
    local ca = ffi.new("GeoCoordinate", {a.latitude, a.longitude})
    local cb = ffi.new("GeoCoordinate", {b.latitude, b.longitude})
    return float_result(lib.proven_geo_distance(ca, cb))
end

function proven.SafeGeo.in_bounds(coord, min_lat, max_lat, min_lon, max_lon)
    local cc = ffi.new("GeoCoordinate", {coord.latitude, coord.longitude})
    return lib.proven_geo_in_bounds(cc, min_lat, max_lat, min_lon, max_lon)
end

-- ============================================================================
-- SafeChecksum
-- ============================================================================
proven.SafeChecksum = {}

function proven.SafeChecksum.crc32(s)
    return int_result(lib.proven_checksum_crc32(s, #s))
end

function proven.SafeChecksum.verify_crc32(s, expected)
    return bool_result(lib.proven_checksum_verify_crc32(s, #s, expected))
end

-- ============================================================================
-- SafeProbability
-- ============================================================================
proven.SafeProbability = {}

function proven.SafeProbability.create(value)
    return tonumber(lib.proven_probability_create(value))
end

function proven.SafeProbability.and_(a, b)
    return tonumber(lib.proven_probability_and(a, b))
end

function proven.SafeProbability.or_exclusive(a, b)
    return tonumber(lib.proven_probability_or_exclusive(a, b))
end

function proven.SafeProbability.not_(p)
    return tonumber(lib.proven_probability_not(p))
end

-- ============================================================================
-- SafeCalculator
-- ============================================================================
proven.SafeCalculator = {}

function proven.SafeCalculator.eval(expr)
    return float_result(lib.proven_calculator_eval(expr, #expr))
end

-- ============================================================================
-- SafePassword
-- ============================================================================
proven.SafePassword = {}

function proven.SafePassword.validate(s)
    local r = lib.proven_password_validate(s, #s)
    local strength_names = {
        [0] = "very_weak", [1] = "weak", [2] = "fair",
        [3] = "strong", [4] = "very_strong",
    }
    return {
        strength      = strength_names[r.strength] or "unknown",
        has_lowercase = r.has_lowercase,
        has_uppercase = r.has_uppercase,
        has_digit     = r.has_digit,
        has_special   = r.has_special,
        length        = tonumber(r.length),
    }
end

function proven.SafePassword.is_common(s)
    return lib.proven_password_is_common(s, #s)
end

-- ============================================================================
-- SafeColor
-- ============================================================================
proven.SafeColor = {}

function proven.SafeColor.parse_hex(s)
    local r = lib.proven_color_parse_hex(s, #s)
    if r.status ~= 0 then return nil end
    return { r = tonumber(r.color.r), g = tonumber(r.color.g), b = tonumber(r.color.b) }
end

function proven.SafeColor.rgb_to_hsl(r, g, b)
    local rgb = ffi.new("RGBColor", {r, g, b})
    local hsl = lib.proven_color_rgb_to_hsl(rgb)
    return { h = tonumber(hsl.h), s = tonumber(hsl.s), l = tonumber(hsl.l) }
end

function proven.SafeColor.to_hex(r, g, b)
    local rgb = ffi.new("RGBColor", {r, g, b})
    return string_result(lib.proven_color_to_hex(rgb))
end

-- ============================================================================
-- SafeAngle
-- ============================================================================
proven.SafeAngle = {}

function proven.SafeAngle.deg_to_rad(degrees)
    return tonumber(lib.proven_angle_deg_to_rad(degrees))
end

function proven.SafeAngle.rad_to_deg(radians)
    return tonumber(lib.proven_angle_rad_to_deg(radians))
end

function proven.SafeAngle.normalize_degrees(degrees)
    return tonumber(lib.proven_angle_normalize_degrees(degrees))
end

function proven.SafeAngle.normalize_radians(radians)
    return tonumber(lib.proven_angle_normalize_radians(radians))
end

-- ============================================================================
-- SafeUnit
-- ============================================================================
proven.SafeUnit = {}

-- Length unit constants
proven.SafeUnit.METERS      = 0
proven.SafeUnit.KILOMETERS  = 1
proven.SafeUnit.CENTIMETERS = 2
proven.SafeUnit.MILLIMETERS = 3
proven.SafeUnit.FEET        = 4
proven.SafeUnit.INCHES      = 5
proven.SafeUnit.MILES       = 6
proven.SafeUnit.YARDS       = 7

-- Temperature unit constants
proven.SafeUnit.CELSIUS    = 0
proven.SafeUnit.FAHRENHEIT = 1
proven.SafeUnit.KELVIN     = 2

function proven.SafeUnit.convert_length(value, from, to)
    return float_result(lib.proven_unit_convert_length(value, from, to))
end

function proven.SafeUnit.convert_temp(value, from, to)
    return float_result(lib.proven_unit_convert_temp(value, from, to))
end

-- ============================================================================
-- SafeML
-- ============================================================================
proven.SafeML = {}

function proven.SafeML.sigmoid(x)
    return tonumber(lib.proven_ml_sigmoid(x))
end

function proven.SafeML.relu(x)
    return tonumber(lib.proven_ml_relu(x))
end

function proven.SafeML.leaky_relu(x, alpha)
    return tonumber(lib.proven_ml_leaky_relu(x, alpha))
end

function proven.SafeML.clamp(x, min_val, max_val)
    return tonumber(lib.proven_ml_clamp(x, min_val, max_val))
end

function proven.SafeML.softmax(input)
    local n = #input
    local inp = ffi.new("double[?]", n)
    local out = ffi.new("double[?]", n)
    for i = 1, n do inp[i - 1] = input[i] end
    local status = lib.proven_ml_softmax(inp, out, n)
    if status ~= 0 then return nil end
    local result = {}
    for i = 1, n do result[i] = tonumber(out[i - 1]) end
    return result
end

-- ============================================================================
-- SafeHTTP
-- ============================================================================
proven.SafeHTTP = {}

function proven.SafeHTTP.url_encode(s)
    return string_result(lib.proven_http_url_encode(s, #s))
end

function proven.SafeHTTP.url_decode(s)
    return string_result(lib.proven_http_url_decode(s, #s))
end

-- ============================================================================
-- SafeRegistry
-- ============================================================================
proven.SafeRegistry = {}

function proven.SafeRegistry.parse(s)
    local r = lib.proven_registry_parse(s, #s)
    if r.status ~= 0 then return nil end
    local ref = r.reference
    local result = {}
    if ref.registry ~= nil and ref.registry_len > 0 then
        result.registry = ffi.string(ref.registry, ref.registry_len)
    end
    if ref.repository ~= nil and ref.repository_len > 0 then
        result.repository = ffi.string(ref.repository, ref.repository_len)
    end
    if ref.tag ~= nil and ref.tag_len > 0 then
        result.tag = ffi.string(ref.tag, ref.tag_len)
    end
    if ref.digest ~= nil and ref.digest_len > 0 then
        result.digest = ffi.string(ref.digest, ref.digest_len)
    end
    return result
end

-- ============================================================================
-- SafeDigest
-- ============================================================================
proven.SafeDigest = {}

function proven.SafeDigest.parse(s)
    local r = lib.proven_digest_parse(s, #s)
    if r.status ~= 0 then return nil end
    local d = r.digest
    local result = { algorithm = tonumber(d.algorithm) }
    if d.value ~= nil and d.value_len > 0 then
        result.value = ffi.string(d.value, d.value_len)
    end
    return result
end

-- ============================================================================
-- Callback infrastructure
-- ============================================================================
proven.Callback = {}

function proven.Callback.register(event_type, callback_fn, context)
    return tonumber(lib.proven_callback_register(event_type, callback_fn, context))
end

function proven.Callback.unregister(handle)
    return lib.proven_callback_unregister(handle) == 0
end

function proven.Callback.fire(event_type, data, code)
    data = data or ""
    code = code or 0
    return tonumber(lib.proven_callback_fire(event_type, data, #data, code))
end

function proven.Callback.count(event_type)
    return tonumber(lib.proven_callback_count(event_type))
end

function proven.Callback.clear_all()
    return tonumber(lib.proven_callback_clear_all())
end

return proven
