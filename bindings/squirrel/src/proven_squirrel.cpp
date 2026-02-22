// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven Squirrel Binding - C++ native module for the Squirrel scripting language.
//
// This module registers native closures in a Squirrel VM that internally call
// libproven via C FFI. All computation is performed in the verified Idris 2
// core; this file is exclusively a marshaling layer.
//
// Usage:
//   1. Compile this file against libproven and squirrel headers.
//   2. Call proven_squirrel_register(vm) from your host application.
//   3. Squirrel scripts can then call: proven.safe_add(a, b), etc.
//
// Build (example):
//   g++ -shared -fPIC -o proven_squirrel.so proven_squirrel.cpp \
//       -I<squirrel>/include -L<libproven> -lproven -lsquirrel -lsqstdlib

#include <squirrel.h>
#include <sqstdaux.h>
#include <cstring>
#include <cstdint>

// ---------------------------------------------------------------------------
// libproven C ABI declarations
// ---------------------------------------------------------------------------
extern "C" {

typedef struct { int32_t status; int64_t value; }                  ProvenIntResult;
typedef struct { int32_t status; int32_t value; }                  ProvenBoolResultCompat;
typedef struct { int32_t status; bool    value; }                  ProvenBoolResult;
typedef struct { int32_t status; char*   value; size_t length; }   ProvenStringResult;
typedef struct { int32_t status; double  value; }                  ProvenFloatResult;

// Lifecycle
int32_t  proven_init(void);
void     proven_deinit(void);
bool     proven_is_initialized(void);
uint32_t proven_ffi_abi_version(void);
uint32_t proven_version_major(void);
uint32_t proven_version_minor(void);
uint32_t proven_version_patch(void);
uint32_t proven_module_count(void);

// Memory
void proven_free_string(char* ptr);

// SafeMath
ProvenIntResult proven_math_add_checked(int64_t a, int64_t b);
ProvenIntResult proven_math_sub_checked(int64_t a, int64_t b);
ProvenIntResult proven_math_mul_checked(int64_t a, int64_t b);
ProvenIntResult proven_math_div(int64_t a, int64_t b);
ProvenIntResult proven_math_mod(int64_t a, int64_t b);
ProvenIntResult proven_math_abs_safe(int64_t n);
int64_t         proven_math_clamp(int64_t lo, int64_t hi, int64_t value);
ProvenIntResult proven_math_pow_checked(int64_t base, uint32_t exp);

// SafeString
ProvenBoolResult   proven_string_is_valid_utf8(const uint8_t* ptr, size_t len);
ProvenStringResult proven_string_escape_sql(const uint8_t* ptr, size_t len);
ProvenStringResult proven_string_escape_html(const uint8_t* ptr, size_t len);
ProvenStringResult proven_string_escape_js(const uint8_t* ptr, size_t len);

// SafePath
ProvenBoolResult   proven_path_has_traversal(const uint8_t* ptr, size_t len);
ProvenStringResult proven_path_sanitize_filename(const uint8_t* ptr, size_t len);

// SafeEmail
ProvenBoolResult proven_email_is_valid(const uint8_t* ptr, size_t len);

// SafeUrl
typedef struct {
    char*    scheme;       size_t scheme_len;
    char*    host;         size_t host_len;
    uint16_t port;         bool   has_port;
    char*    path;         size_t path_len;
    char*    query;        size_t query_len;
    char*    fragment;     size_t fragment_len;
} ProvenUrlComponents;

typedef struct {
    int32_t             status;
    ProvenUrlComponents components;
} ProvenUrlResult;

ProvenUrlResult proven_url_parse(const uint8_t* ptr, size_t len);
void            proven_url_free(ProvenUrlComponents* components);

// SafeCrypto
ProvenBoolResult proven_crypto_constant_time_eq(
    const uint8_t* ptr1, size_t len1,
    const uint8_t* ptr2, size_t len2
);
int32_t proven_crypto_random_bytes(uint8_t* ptr, size_t len);

// SafeHex
ProvenStringResult proven_hex_encode(const uint8_t* ptr, size_t len, bool uppercase);

typedef struct {
    int32_t  status;
    uint8_t* data;
    size_t   length;
} ProvenHexDecodeResult;

ProvenHexDecodeResult proven_hex_decode(const uint8_t* ptr, size_t len);
void                  proven_hex_free(ProvenHexDecodeResult* result);

// SafeJson
ProvenBoolResult proven_json_is_valid(const uint8_t* ptr, size_t len);

typedef enum {
    PROVEN_JSON_NULL    =  0,
    PROVEN_JSON_BOOL    =  1,
    PROVEN_JSON_NUMBER  =  2,
    PROVEN_JSON_STRING  =  3,
    PROVEN_JSON_ARRAY   =  4,
    PROVEN_JSON_OBJECT  =  5,
    PROVEN_JSON_INVALID = -1
} ProvenJsonType;

ProvenJsonType proven_json_get_type(const uint8_t* ptr, size_t len);

// SafeFloat
ProvenFloatResult proven_float_div(double a, double b);
ProvenFloatResult proven_float_sqrt(double x);
ProvenFloatResult proven_float_ln(double x);
bool              proven_float_is_finite(double x);
bool              proven_float_is_nan(double x);

// SafeDateTime
ProvenStringResult proven_datetime_format_iso8601(
    /* ProvenDateTime fields are passed as struct; simplified here */
    /* The actual call uses the struct, but we marshal from Squirrel */
    /* See proven.h for full ProvenDateTime layout */
);

// SafeColor
typedef struct { uint8_t r; uint8_t g; uint8_t b; } ProvenRGBColor;
typedef struct {
    int32_t       status;
    ProvenRGBColor color;
} ProvenColorResult;

ProvenColorResult  proven_color_parse_hex(const uint8_t* ptr, size_t len);
ProvenStringResult proven_color_to_hex(ProvenRGBColor rgb);

// SafeVersion
typedef struct {
    uint32_t major;
    uint32_t minor;
    uint32_t patch;
    size_t   prerelease_len;
    char*    prerelease;
} ProvenSemanticVersion;

typedef struct {
    int32_t              status;
    ProvenSemanticVersion version;
} ProvenVersionResult;

ProvenVersionResult proven_version_parse(const uint8_t* ptr, size_t len);
int32_t             proven_version_compare(ProvenSemanticVersion a, ProvenSemanticVersion b);
void                proven_version_free(ProvenSemanticVersion* version);

// SafeChecksum
ProvenIntResult  proven_checksum_crc32(const uint8_t* ptr, size_t len);
ProvenBoolResult proven_checksum_verify_crc32(const uint8_t* ptr, size_t len, uint32_t expected);

// SafeHttp
ProvenStringResult proven_http_url_encode(const uint8_t* ptr, size_t len);
ProvenStringResult proven_http_url_decode(const uint8_t* ptr, size_t len);

// SafePassword
typedef struct {
    int32_t strength;
    bool    has_lowercase;
    bool    has_uppercase;
    bool    has_digit;
    bool    has_special;
    size_t  length;
} ProvenPasswordResult;

ProvenPasswordResult proven_password_validate(const uint8_t* ptr, size_t len);
bool                 proven_password_is_common(const uint8_t* ptr, size_t len);

// SafeNetwork
typedef struct { uint8_t octets[4]; } ProvenIPv4Address;
typedef struct {
    int32_t           status;
    ProvenIPv4Address address;
} ProvenIPv4Result;

ProvenIPv4Result proven_network_parse_ipv4(const uint8_t* ptr, size_t len);
bool             proven_network_ipv4_is_private(ProvenIPv4Address addr);
bool             proven_network_ipv4_is_loopback(ProvenIPv4Address addr);

// SafeCalculator
ProvenFloatResult proven_calculator_eval(const uint8_t* ptr, size_t len);

} // extern "C"

// ---------------------------------------------------------------------------
// Helper: extract a Squirrel string parameter as (ptr, len) pair
// ---------------------------------------------------------------------------
static bool sq_get_string_param(HSQUIRRELVM v, SQInteger idx,
                                const SQChar** out_str, SQInteger* out_len)
{
    if (SQ_FAILED(sq_getstring(v, idx, out_str))) {
        return false;
    }
    *out_len = static_cast<SQInteger>(strlen(*out_str));
    return true;
}

// ---------------------------------------------------------------------------
// Helper: push a ProvenStringResult onto the Squirrel stack.
// Frees the C string. Returns null on error status.
// ---------------------------------------------------------------------------
static SQInteger sq_push_string_result(HSQUIRRELVM v, ProvenStringResult res)
{
    if (res.status != 0) {
        sq_pushnull(v);
        return 1;
    }
    sq_pushstring(v, res.value, static_cast<SQInteger>(res.length));
    proven_free_string(res.value);
    return 1;
}

// ===========================================================================
// SafeMath native closures
// ===========================================================================

/// proven.safe_add(a, b) -> integer or null on overflow
static SQInteger sq_proven_safe_add(HSQUIRRELVM v)
{
    SQInteger a, b;
    sq_getinteger(v, 2, &a);
    sq_getinteger(v, 3, &b);
    ProvenIntResult res = proven_math_add_checked(static_cast<int64_t>(a),
                                                   static_cast<int64_t>(b));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushinteger(v, static_cast<SQInteger>(res.value));
    return 1;
}

/// proven.safe_sub(a, b) -> integer or null on underflow
static SQInteger sq_proven_safe_sub(HSQUIRRELVM v)
{
    SQInteger a, b;
    sq_getinteger(v, 2, &a);
    sq_getinteger(v, 3, &b);
    ProvenIntResult res = proven_math_sub_checked(static_cast<int64_t>(a),
                                                   static_cast<int64_t>(b));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushinteger(v, static_cast<SQInteger>(res.value));
    return 1;
}

/// proven.safe_mul(a, b) -> integer or null on overflow
static SQInteger sq_proven_safe_mul(HSQUIRRELVM v)
{
    SQInteger a, b;
    sq_getinteger(v, 2, &a);
    sq_getinteger(v, 3, &b);
    ProvenIntResult res = proven_math_mul_checked(static_cast<int64_t>(a),
                                                   static_cast<int64_t>(b));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushinteger(v, static_cast<SQInteger>(res.value));
    return 1;
}

/// proven.safe_div(a, b) -> integer or null on division by zero / overflow
static SQInteger sq_proven_safe_div(HSQUIRRELVM v)
{
    SQInteger a, b;
    sq_getinteger(v, 2, &a);
    sq_getinteger(v, 3, &b);
    ProvenIntResult res = proven_math_div(static_cast<int64_t>(a),
                                          static_cast<int64_t>(b));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushinteger(v, static_cast<SQInteger>(res.value));
    return 1;
}

/// proven.safe_mod(a, b) -> integer or null on division by zero
static SQInteger sq_proven_safe_mod(HSQUIRRELVM v)
{
    SQInteger a, b;
    sq_getinteger(v, 2, &a);
    sq_getinteger(v, 3, &b);
    ProvenIntResult res = proven_math_mod(static_cast<int64_t>(a),
                                          static_cast<int64_t>(b));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushinteger(v, static_cast<SQInteger>(res.value));
    return 1;
}

/// proven.safe_abs(n) -> integer or null on overflow (INT64_MIN)
static SQInteger sq_proven_safe_abs(HSQUIRRELVM v)
{
    SQInteger n;
    sq_getinteger(v, 2, &n);
    ProvenIntResult res = proven_math_abs_safe(static_cast<int64_t>(n));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushinteger(v, static_cast<SQInteger>(res.value));
    return 1;
}

/// proven.safe_clamp(lo, hi, value) -> integer
static SQInteger sq_proven_safe_clamp(HSQUIRRELVM v)
{
    SQInteger lo, hi, val;
    sq_getinteger(v, 2, &lo);
    sq_getinteger(v, 3, &hi);
    sq_getinteger(v, 4, &val);
    int64_t result = proven_math_clamp(static_cast<int64_t>(lo),
                                        static_cast<int64_t>(hi),
                                        static_cast<int64_t>(val));
    sq_pushinteger(v, static_cast<SQInteger>(result));
    return 1;
}

/// proven.safe_pow(base, exp) -> integer or null on overflow
static SQInteger sq_proven_safe_pow(HSQUIRRELVM v)
{
    SQInteger base, exp;
    sq_getinteger(v, 2, &base);
    sq_getinteger(v, 3, &exp);
    ProvenIntResult res = proven_math_pow_checked(static_cast<int64_t>(base),
                                                   static_cast<uint32_t>(exp));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushinteger(v, static_cast<SQInteger>(res.value));
    return 1;
}

// ===========================================================================
// SafeString native closures
// ===========================================================================

/// proven.is_valid_utf8(str) -> bool
static SQInteger sq_proven_is_valid_utf8(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenBoolResult res = proven_string_is_valid_utf8(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushbool(v, res.value ? SQTrue : SQFalse);
    return 1;
}

/// proven.escape_sql(str) -> string or null
static SQInteger sq_proven_escape_sql(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenStringResult res = proven_string_escape_sql(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    return sq_push_string_result(v, res);
}

/// proven.escape_html(str) -> string or null
static SQInteger sq_proven_escape_html(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenStringResult res = proven_string_escape_html(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    return sq_push_string_result(v, res);
}

/// proven.escape_js(str) -> string or null
static SQInteger sq_proven_escape_js(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenStringResult res = proven_string_escape_js(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    return sq_push_string_result(v, res);
}

// ===========================================================================
// SafePath native closures
// ===========================================================================

/// proven.path_has_traversal(path) -> bool or null
static SQInteger sq_proven_path_has_traversal(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenBoolResult res = proven_path_has_traversal(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushbool(v, res.value ? SQTrue : SQFalse);
    return 1;
}

/// proven.sanitize_filename(name) -> string or null
static SQInteger sq_proven_sanitize_filename(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenStringResult res = proven_path_sanitize_filename(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    return sq_push_string_result(v, res);
}

// ===========================================================================
// SafeEmail native closures
// ===========================================================================

/// proven.validate_email(email) -> bool or null
static SQInteger sq_proven_validate_email(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenBoolResult res = proven_email_is_valid(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushbool(v, res.value ? SQTrue : SQFalse);
    return 1;
}

// ===========================================================================
// SafeUrl native closures
// ===========================================================================

/// proven.parse_url(url_string) -> table { scheme, host, port, path, query, fragment } or null
static SQInteger sq_proven_parse_url(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenUrlResult res = proven_url_parse(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    if (res.status != 0) { sq_pushnull(v); return 1; }

    // Build a Squirrel table from the URL components
    sq_newtable(v);

    sq_pushstring(v, _SC("scheme"), -1);
    if (res.components.scheme != nullptr) {
        sq_pushstring(v, res.components.scheme, static_cast<SQInteger>(res.components.scheme_len));
    } else {
        sq_pushstring(v, _SC(""), -1);
    }
    sq_newslot(v, -3, SQFalse);

    sq_pushstring(v, _SC("host"), -1);
    if (res.components.host != nullptr) {
        sq_pushstring(v, res.components.host, static_cast<SQInteger>(res.components.host_len));
    } else {
        sq_pushstring(v, _SC(""), -1);
    }
    sq_newslot(v, -3, SQFalse);

    sq_pushstring(v, _SC("port"), -1);
    if (res.components.has_port) {
        sq_pushinteger(v, static_cast<SQInteger>(res.components.port));
    } else {
        sq_pushnull(v);
    }
    sq_newslot(v, -3, SQFalse);

    sq_pushstring(v, _SC("path"), -1);
    if (res.components.path != nullptr) {
        sq_pushstring(v, res.components.path, static_cast<SQInteger>(res.components.path_len));
    } else {
        sq_pushstring(v, _SC(""), -1);
    }
    sq_newslot(v, -3, SQFalse);

    sq_pushstring(v, _SC("query"), -1);
    if (res.components.query != nullptr) {
        sq_pushstring(v, res.components.query, static_cast<SQInteger>(res.components.query_len));
    } else {
        sq_pushnull(v);
    }
    sq_newslot(v, -3, SQFalse);

    sq_pushstring(v, _SC("fragment"), -1);
    if (res.components.fragment != nullptr) {
        sq_pushstring(v, res.components.fragment, static_cast<SQInteger>(res.components.fragment_len));
    } else {
        sq_pushnull(v);
    }
    sq_newslot(v, -3, SQFalse);

    proven_url_free(&res.components);
    return 1;
}

// ===========================================================================
// SafeCrypto native closures
// ===========================================================================

/// proven.constant_time_eq(a, b) -> bool or null
static SQInteger sq_proven_constant_time_eq(HSQUIRRELVM v)
{
    const SQChar* a; SQInteger a_len;
    const SQChar* b; SQInteger b_len;
    if (!sq_get_string_param(v, 2, &a, &a_len)) { sq_pushnull(v); return 1; }
    if (!sq_get_string_param(v, 3, &b, &b_len)) { sq_pushnull(v); return 1; }
    ProvenBoolResult res = proven_crypto_constant_time_eq(
        reinterpret_cast<const uint8_t*>(a), static_cast<size_t>(a_len),
        reinterpret_cast<const uint8_t*>(b), static_cast<size_t>(b_len));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushbool(v, res.value ? SQTrue : SQFalse);
    return 1;
}

/// proven.random_hex(nbytes) -> hex string or null
static SQInteger sq_proven_random_hex(HSQUIRRELVM v)
{
    SQInteger nbytes;
    sq_getinteger(v, 2, &nbytes);
    if (nbytes <= 0 || nbytes > 1024) { sq_pushnull(v); return 1; }

    uint8_t buf[1024];
    int32_t status = proven_crypto_random_bytes(buf, static_cast<size_t>(nbytes));
    if (status != 0) { sq_pushnull(v); return 1; }

    ProvenStringResult hex = proven_hex_encode(buf, static_cast<size_t>(nbytes), false);
    return sq_push_string_result(v, hex);
}

// ===========================================================================
// SafeHex native closures
// ===========================================================================

/// proven.hex_encode(str) -> hex string or null
static SQInteger sq_proven_hex_encode(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenStringResult res = proven_hex_encode(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len), false);
    return sq_push_string_result(v, res);
}

/// proven.hex_decode(hex_str) -> decoded string or null
static SQInteger sq_proven_hex_decode(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenHexDecodeResult res = proven_hex_decode(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushstring(v, reinterpret_cast<const SQChar*>(res.data),
                  static_cast<SQInteger>(res.length));
    proven_hex_free(&res);
    return 1;
}

// ===========================================================================
// SafeJson native closures
// ===========================================================================

/// proven.json_is_valid(json_str) -> bool or null
static SQInteger sq_proven_json_is_valid(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenBoolResult res = proven_json_is_valid(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushbool(v, res.value ? SQTrue : SQFalse);
    return 1;
}

/// proven.json_get_type(json_str) -> string ("null","bool","number","string","array","object","invalid")
static SQInteger sq_proven_json_get_type(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenJsonType t = proven_json_get_type(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    const char* name = "invalid";
    switch (t) {
        case PROVEN_JSON_NULL:    name = "null";    break;
        case PROVEN_JSON_BOOL:    name = "bool";    break;
        case PROVEN_JSON_NUMBER:  name = "number";  break;
        case PROVEN_JSON_STRING:  name = "string";  break;
        case PROVEN_JSON_ARRAY:   name = "array";   break;
        case PROVEN_JSON_OBJECT:  name = "object";  break;
        default:                  name = "invalid"; break;
    }
    sq_pushstring(v, name, -1);
    return 1;
}

// ===========================================================================
// SafeFloat native closures
// ===========================================================================

/// proven.float_div(a, b) -> float or null
static SQInteger sq_proven_float_div(HSQUIRRELVM v)
{
    SQFloat a, b;
    sq_getfloat(v, 2, &a);
    sq_getfloat(v, 3, &b);
    ProvenFloatResult res = proven_float_div(static_cast<double>(a),
                                              static_cast<double>(b));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushfloat(v, static_cast<SQFloat>(res.value));
    return 1;
}

/// proven.float_sqrt(x) -> float or null
static SQInteger sq_proven_float_sqrt(HSQUIRRELVM v)
{
    SQFloat x;
    sq_getfloat(v, 2, &x);
    ProvenFloatResult res = proven_float_sqrt(static_cast<double>(x));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushfloat(v, static_cast<SQFloat>(res.value));
    return 1;
}

/// proven.float_ln(x) -> float or null
static SQInteger sq_proven_float_ln(HSQUIRRELVM v)
{
    SQFloat x;
    sq_getfloat(v, 2, &x);
    ProvenFloatResult res = proven_float_ln(static_cast<double>(x));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushfloat(v, static_cast<SQFloat>(res.value));
    return 1;
}

/// proven.float_is_finite(x) -> bool
static SQInteger sq_proven_float_is_finite(HSQUIRRELVM v)
{
    SQFloat x;
    sq_getfloat(v, 2, &x);
    sq_pushbool(v, proven_float_is_finite(static_cast<double>(x)) ? SQTrue : SQFalse);
    return 1;
}

/// proven.float_is_nan(x) -> bool
static SQInteger sq_proven_float_is_nan(HSQUIRRELVM v)
{
    SQFloat x;
    sq_getfloat(v, 2, &x);
    sq_pushbool(v, proven_float_is_nan(static_cast<double>(x)) ? SQTrue : SQFalse);
    return 1;
}

// ===========================================================================
// SafeColor native closures
// ===========================================================================

/// proven.parse_color(hex_str) -> table { r, g, b } or null
static SQInteger sq_proven_parse_color(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenColorResult res = proven_color_parse_hex(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    if (res.status != 0) { sq_pushnull(v); return 1; }

    sq_newtable(v);
    sq_pushstring(v, _SC("r"), -1);
    sq_pushinteger(v, res.color.r);
    sq_newslot(v, -3, SQFalse);
    sq_pushstring(v, _SC("g"), -1);
    sq_pushinteger(v, res.color.g);
    sq_newslot(v, -3, SQFalse);
    sq_pushstring(v, _SC("b"), -1);
    sq_pushinteger(v, res.color.b);
    sq_newslot(v, -3, SQFalse);
    return 1;
}

/// proven.color_to_hex(r, g, b) -> hex string or null
static SQInteger sq_proven_color_to_hex(HSQUIRRELVM v)
{
    SQInteger r, g, b;
    sq_getinteger(v, 2, &r);
    sq_getinteger(v, 3, &g);
    sq_getinteger(v, 4, &b);
    ProvenRGBColor rgb;
    rgb.r = static_cast<uint8_t>(r);
    rgb.g = static_cast<uint8_t>(g);
    rgb.b = static_cast<uint8_t>(b);
    ProvenStringResult res = proven_color_to_hex(rgb);
    return sq_push_string_result(v, res);
}

// ===========================================================================
// SafeVersion native closures
// ===========================================================================

/// proven.version_compare(a_str, b_str) -> integer (-1, 0, 1) or null
static SQInteger sq_proven_version_compare(HSQUIRRELVM v)
{
    const SQChar* a_str; SQInteger a_len;
    const SQChar* b_str; SQInteger b_len;
    if (!sq_get_string_param(v, 2, &a_str, &a_len)) { sq_pushnull(v); return 1; }
    if (!sq_get_string_param(v, 3, &b_str, &b_len)) { sq_pushnull(v); return 1; }

    ProvenVersionResult va = proven_version_parse(
        reinterpret_cast<const uint8_t*>(a_str), static_cast<size_t>(a_len));
    if (va.status != 0) { sq_pushnull(v); return 1; }

    ProvenVersionResult vb = proven_version_parse(
        reinterpret_cast<const uint8_t*>(b_str), static_cast<size_t>(b_len));
    if (vb.status != 0) {
        proven_version_free(&va.version);
        sq_pushnull(v);
        return 1;
    }

    int32_t cmp = proven_version_compare(va.version, vb.version);
    proven_version_free(&va.version);
    proven_version_free(&vb.version);
    sq_pushinteger(v, static_cast<SQInteger>(cmp));
    return 1;
}

// ===========================================================================
// SafeChecksum native closures
// ===========================================================================

/// proven.crc32(str) -> integer or null
static SQInteger sq_proven_crc32(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenIntResult res = proven_checksum_crc32(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushinteger(v, static_cast<SQInteger>(res.value));
    return 1;
}

// ===========================================================================
// SafeHttp native closures
// ===========================================================================

/// proven.url_encode(str) -> encoded string or null
static SQInteger sq_proven_url_encode(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenStringResult res = proven_http_url_encode(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    return sq_push_string_result(v, res);
}

/// proven.url_decode(str) -> decoded string or null
static SQInteger sq_proven_url_decode(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenStringResult res = proven_http_url_decode(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    return sq_push_string_result(v, res);
}

// ===========================================================================
// SafePassword native closures
// ===========================================================================

/// proven.validate_password(str) -> table { strength, has_lower, has_upper, has_digit, has_special, length }
static SQInteger sq_proven_validate_password(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenPasswordResult res = proven_password_validate(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));

    sq_newtable(v);
    sq_pushstring(v, _SC("strength"), -1);
    sq_pushinteger(v, static_cast<SQInteger>(res.strength));
    sq_newslot(v, -3, SQFalse);
    sq_pushstring(v, _SC("has_lower"), -1);
    sq_pushbool(v, res.has_lowercase ? SQTrue : SQFalse);
    sq_newslot(v, -3, SQFalse);
    sq_pushstring(v, _SC("has_upper"), -1);
    sq_pushbool(v, res.has_uppercase ? SQTrue : SQFalse);
    sq_newslot(v, -3, SQFalse);
    sq_pushstring(v, _SC("has_digit"), -1);
    sq_pushbool(v, res.has_digit ? SQTrue : SQFalse);
    sq_newslot(v, -3, SQFalse);
    sq_pushstring(v, _SC("has_special"), -1);
    sq_pushbool(v, res.has_special ? SQTrue : SQFalse);
    sq_newslot(v, -3, SQFalse);
    sq_pushstring(v, _SC("length"), -1);
    sq_pushinteger(v, static_cast<SQInteger>(res.length));
    sq_newslot(v, -3, SQFalse);
    return 1;
}

/// proven.is_common_password(str) -> bool
static SQInteger sq_proven_is_common_password(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    bool common = proven_password_is_common(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    sq_pushbool(v, common ? SQTrue : SQFalse);
    return 1;
}

// ===========================================================================
// SafeNetwork native closures
// ===========================================================================

/// proven.parse_ipv4(str) -> table { octets = [a,b,c,d] } or null
static SQInteger sq_proven_parse_ipv4(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenIPv4Result res = proven_network_parse_ipv4(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    if (res.status != 0) { sq_pushnull(v); return 1; }

    sq_newtable(v);
    sq_pushstring(v, _SC("octets"), -1);
    sq_newarray(v, 4);
    for (int i = 0; i < 4; i++) {
        sq_pushinteger(v, static_cast<SQInteger>(i));
        sq_pushinteger(v, static_cast<SQInteger>(res.address.octets[i]));
        sq_set(v, -3);
    }
    sq_newslot(v, -3, SQFalse);
    return 1;
}

// ===========================================================================
// SafeCalculator native closures
// ===========================================================================

/// proven.calc_eval(expr) -> float or null
static SQInteger sq_proven_calc_eval(HSQUIRRELVM v)
{
    const SQChar* str; SQInteger len;
    if (!sq_get_string_param(v, 2, &str, &len)) { sq_pushnull(v); return 1; }
    ProvenFloatResult res = proven_calculator_eval(
        reinterpret_cast<const uint8_t*>(str), static_cast<size_t>(len));
    if (res.status != 0) { sq_pushnull(v); return 1; }
    sq_pushfloat(v, static_cast<SQFloat>(res.value));
    return 1;
}

// ===========================================================================
// Lifecycle native closures
// ===========================================================================

/// proven.init() -> integer (status code)
static SQInteger sq_proven_init(HSQUIRRELVM v)
{
    int32_t status = proven_init();
    sq_pushinteger(v, static_cast<SQInteger>(status));
    return 1;
}

/// proven.deinit() -> null
static SQInteger sq_proven_deinit(HSQUIRRELVM v)
{
    proven_deinit();
    sq_pushnull(v);
    return 1;
}

/// proven.is_initialized() -> bool
static SQInteger sq_proven_is_initialized(HSQUIRRELVM v)
{
    sq_pushbool(v, proven_is_initialized() ? SQTrue : SQFalse);
    return 1;
}

/// proven.version() -> string "major.minor.patch"
static SQInteger sq_proven_version(HSQUIRRELVM v)
{
    char buf[64];
    snprintf(buf, sizeof(buf), "%u.%u.%u",
             proven_version_major(), proven_version_minor(), proven_version_patch());
    sq_pushstring(v, buf, -1);
    return 1;
}

// ===========================================================================
// Registration: helper to register a native closure on a table
// ===========================================================================
static void sq_register_func(HSQUIRRELVM v, SQInteger table_idx,
                              const SQChar* name, SQFUNCTION func,
                              SQInteger nparams, const SQChar* typemask)
{
    sq_pushstring(v, name, -1);
    sq_newclosure(v, func, 0);
    sq_setparamscheck(v, nparams, typemask);
    sq_setnativeclosurename(v, -1, name);
    sq_newslot(v, table_idx, SQFalse);
}

// ===========================================================================
// Public API: register all proven functions in the given VM
// ===========================================================================

/**
 * @brief Register the "proven" table with all native closures in the VM.
 *
 * After calling this, Squirrel scripts can use:
 *   proven.safe_add(1, 2)
 *   proven.validate_email("user@example.com")
 *   etc.
 *
 * @param v The Squirrel VM handle.
 * @return SQ_OK on success, SQ_ERROR on failure.
 */
extern "C" SQRESULT proven_squirrel_register(HSQUIRRELVM v)
{
    sq_pushroottable(v);

    // Create the "proven" table
    sq_pushstring(v, _SC("proven"), -1);
    sq_newtable(v);

    // The table index for newslot is -3 because the stack is:
    //   [root_table] [key "proven"] [proven_table]
    // After pushing into proven_table, we do newslot on root at the end.

    SQInteger tbl = sq_gettop(v);

    // Lifecycle
    sq_register_func(v, tbl, _SC("init"),           sq_proven_init,           1, _SC("t"));
    sq_register_func(v, tbl, _SC("deinit"),         sq_proven_deinit,         1, _SC("t"));
    sq_register_func(v, tbl, _SC("is_initialized"), sq_proven_is_initialized, 1, _SC("t"));
    sq_register_func(v, tbl, _SC("version"),        sq_proven_version,        1, _SC("t"));

    // SafeMath
    sq_register_func(v, tbl, _SC("safe_add"),   sq_proven_safe_add,   3, _SC("tnn"));
    sq_register_func(v, tbl, _SC("safe_sub"),   sq_proven_safe_sub,   3, _SC("tnn"));
    sq_register_func(v, tbl, _SC("safe_mul"),   sq_proven_safe_mul,   3, _SC("tnn"));
    sq_register_func(v, tbl, _SC("safe_div"),   sq_proven_safe_div,   3, _SC("tnn"));
    sq_register_func(v, tbl, _SC("safe_mod"),   sq_proven_safe_mod,   3, _SC("tnn"));
    sq_register_func(v, tbl, _SC("safe_abs"),   sq_proven_safe_abs,   2, _SC("tn"));
    sq_register_func(v, tbl, _SC("safe_clamp"), sq_proven_safe_clamp, 4, _SC("tnnn"));
    sq_register_func(v, tbl, _SC("safe_pow"),   sq_proven_safe_pow,   3, _SC("tnn"));

    // SafeString
    sq_register_func(v, tbl, _SC("is_valid_utf8"), sq_proven_is_valid_utf8, 2, _SC("ts"));
    sq_register_func(v, tbl, _SC("escape_sql"),    sq_proven_escape_sql,    2, _SC("ts"));
    sq_register_func(v, tbl, _SC("escape_html"),   sq_proven_escape_html,   2, _SC("ts"));
    sq_register_func(v, tbl, _SC("escape_js"),     sq_proven_escape_js,     2, _SC("ts"));

    // SafePath
    sq_register_func(v, tbl, _SC("path_has_traversal"), sq_proven_path_has_traversal, 2, _SC("ts"));
    sq_register_func(v, tbl, _SC("sanitize_filename"),  sq_proven_sanitize_filename,  2, _SC("ts"));

    // SafeEmail
    sq_register_func(v, tbl, _SC("validate_email"), sq_proven_validate_email, 2, _SC("ts"));

    // SafeUrl
    sq_register_func(v, tbl, _SC("parse_url"), sq_proven_parse_url, 2, _SC("ts"));

    // SafeCrypto
    sq_register_func(v, tbl, _SC("constant_time_eq"), sq_proven_constant_time_eq, 3, _SC("tss"));
    sq_register_func(v, tbl, _SC("random_hex"),        sq_proven_random_hex,       2, _SC("tn"));

    // SafeHex
    sq_register_func(v, tbl, _SC("hex_encode"), sq_proven_hex_encode, 2, _SC("ts"));
    sq_register_func(v, tbl, _SC("hex_decode"), sq_proven_hex_decode, 2, _SC("ts"));

    // SafeJson
    sq_register_func(v, tbl, _SC("json_is_valid"), sq_proven_json_is_valid, 2, _SC("ts"));
    sq_register_func(v, tbl, _SC("json_get_type"), sq_proven_json_get_type, 2, _SC("ts"));

    // SafeFloat
    sq_register_func(v, tbl, _SC("float_div"),       sq_proven_float_div,       3, _SC("tff"));
    sq_register_func(v, tbl, _SC("float_sqrt"),      sq_proven_float_sqrt,      2, _SC("tf"));
    sq_register_func(v, tbl, _SC("float_ln"),        sq_proven_float_ln,        2, _SC("tf"));
    sq_register_func(v, tbl, _SC("float_is_finite"), sq_proven_float_is_finite, 2, _SC("tf"));
    sq_register_func(v, tbl, _SC("float_is_nan"),    sq_proven_float_is_nan,    2, _SC("tf"));

    // SafeColor
    sq_register_func(v, tbl, _SC("parse_color"),  sq_proven_parse_color,  2, _SC("ts"));
    sq_register_func(v, tbl, _SC("color_to_hex"), sq_proven_color_to_hex, 4, _SC("tnnn"));

    // SafeVersion
    sq_register_func(v, tbl, _SC("version_compare"), sq_proven_version_compare, 3, _SC("tss"));

    // SafeChecksum
    sq_register_func(v, tbl, _SC("crc32"), sq_proven_crc32, 2, _SC("ts"));

    // SafeHttp
    sq_register_func(v, tbl, _SC("url_encode"), sq_proven_url_encode, 2, _SC("ts"));
    sq_register_func(v, tbl, _SC("url_decode"), sq_proven_url_decode, 2, _SC("ts"));

    // SafePassword
    sq_register_func(v, tbl, _SC("validate_password"),  sq_proven_validate_password,  2, _SC("ts"));
    sq_register_func(v, tbl, _SC("is_common_password"), sq_proven_is_common_password, 2, _SC("ts"));

    // SafeNetwork
    sq_register_func(v, tbl, _SC("parse_ipv4"), sq_proven_parse_ipv4, 2, _SC("ts"));

    // SafeCalculator
    sq_register_func(v, tbl, _SC("calc_eval"), sq_proven_calc_eval, 2, _SC("ts"));

    // Install the "proven" table into the root table
    sq_newslot(v, -3, SQFalse);
    sq_pop(v, 1); // pop root table

    return SQ_OK;
}
