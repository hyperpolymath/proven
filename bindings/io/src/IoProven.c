/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> */
/*
 * IoProven.c - Io language C addon for the Proven safety library.
 *
 * This file implements an Io addon that registers the "Proven" prototype
 * with methods that call libproven via C FFI. All computation is performed
 * in the verified Idris 2 core; this file is exclusively a marshaling layer.
 *
 * Build:
 *   cc -shared -fPIC -o IoProven.so IoProven.c \
 *       -I/path/to/io/headers -L/path/to/libproven -lproven -liovmall
 *
 * Usage in Io:
 *   Proven init
 *   result := Proven safeAdd(1000000000, 2000000000)
 *   Proven deinit
 */

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

/* -----------------------------------------------------------------------
 * Io VM API declarations
 *
 * These are the minimal Io C API types and functions needed by this addon.
 * In a real build, include IoState.h, IoObject.h, IoMessage.h, IoNumber.h,
 * IoSeq.h, etc. from the Io distribution. We declare the prototypes here
 * to keep this file self-contained for documentation purposes.
 * ----------------------------------------------------------------------- */

/* Forward declarations for Io types */
typedef struct IoObject IoObject;
typedef struct IoMessage IoMessage;
typedef struct IoState IoState;

/* Io object creation and access */
extern IoObject* IoObject_new(IoState* state);
extern IoObject* IoState_protoWithId_(IoState* state, const char* id);
extern void      IoState_registerProtoWithId_(IoState* state, IoObject* proto, const char* id);
extern IoObject* IONIL(IoObject* self);
extern IoObject* IOTRUE(IoObject* self);
extern IoObject* IOFALSE(IoObject* self);
extern IoObject* IONUMBER(double n);
extern IoObject* IoSeq_newWithCString_(IoState* state, const char* str);
extern IoObject* IoSeq_newWithData_length_(IoState* state, const char* data, size_t len);

/* Io message evaluation */
extern IoObject* IoMessage_locals_numberArgAt_(IoMessage* msg, IoObject* locals, int n);
extern IoObject* IoMessage_locals_seqArgAt_(IoMessage* msg, IoObject* locals, int n);
extern double    IoNumber_asDouble(IoObject* self);
extern long      IoNumber_asLong(IoObject* self);
extern const char* IoSeq_asCString(IoObject* self);
extern size_t    IoSeq_rawSize(IoObject* self);

/* Io method table registration */
typedef IoObject* (*IoMethodFunc)(IoObject*, IoObject*, IoMessage*);

typedef struct {
    const char*  name;
    IoMethodFunc func;
} IoMethodTable;

extern void IoObject_addMethodTable_(IoObject* self, IoMethodTable* table);
extern IoObject* IoObject_setSlot_to_(IoObject* self, const char* name, IoObject* value);

/* -----------------------------------------------------------------------
 * libproven C ABI declarations
 * ----------------------------------------------------------------------- */

typedef struct { int32_t status; int64_t value; }                ProvenIntResult;
typedef struct { int32_t status; bool    value; }                ProvenBoolResult;
typedef struct { int32_t status; char*   value; size_t length; } ProvenStringResult;
typedef struct { int32_t status; double  value; }                ProvenFloatResult;

/* Lifecycle */
int32_t  proven_init(void);
void     proven_deinit(void);
bool     proven_is_initialized(void);
uint32_t proven_ffi_abi_version(void);
uint32_t proven_version_major(void);
uint32_t proven_version_minor(void);
uint32_t proven_version_patch(void);
uint32_t proven_module_count(void);

/* Memory */
void proven_free_string(char* ptr);

/* SafeMath */
ProvenIntResult proven_math_add_checked(int64_t a, int64_t b);
ProvenIntResult proven_math_sub_checked(int64_t a, int64_t b);
ProvenIntResult proven_math_mul_checked(int64_t a, int64_t b);
ProvenIntResult proven_math_div(int64_t a, int64_t b);
ProvenIntResult proven_math_mod(int64_t a, int64_t b);
ProvenIntResult proven_math_abs_safe(int64_t n);
int64_t         proven_math_clamp(int64_t lo, int64_t hi, int64_t value);
ProvenIntResult proven_math_pow_checked(int64_t base, uint32_t exp);

/* SafeString */
ProvenBoolResult   proven_string_is_valid_utf8(const uint8_t* ptr, size_t len);
ProvenStringResult proven_string_escape_sql(const uint8_t* ptr, size_t len);
ProvenStringResult proven_string_escape_html(const uint8_t* ptr, size_t len);
ProvenStringResult proven_string_escape_js(const uint8_t* ptr, size_t len);

/* SafePath */
ProvenBoolResult   proven_path_has_traversal(const uint8_t* ptr, size_t len);
ProvenStringResult proven_path_sanitize_filename(const uint8_t* ptr, size_t len);

/* SafeEmail */
ProvenBoolResult proven_email_is_valid(const uint8_t* ptr, size_t len);

/* SafeUrl */
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

/* SafeCrypto */
ProvenBoolResult proven_crypto_constant_time_eq(
    const uint8_t* ptr1, size_t len1,
    const uint8_t* ptr2, size_t len2
);
int32_t proven_crypto_random_bytes(uint8_t* ptr, size_t len);

/* SafeHex */
ProvenStringResult proven_hex_encode(const uint8_t* ptr, size_t len, bool uppercase);

typedef struct {
    int32_t  status;
    uint8_t* data;
    size_t   length;
} ProvenHexDecodeResult;

ProvenHexDecodeResult proven_hex_decode(const uint8_t* ptr, size_t len);
void                  proven_hex_free(ProvenHexDecodeResult* result);

/* SafeJson */
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

/* SafeFloat */
ProvenFloatResult proven_float_div(double a, double b);
ProvenFloatResult proven_float_sqrt(double x);
ProvenFloatResult proven_float_ln(double x);
bool              proven_float_is_finite(double x);
bool              proven_float_is_nan(double x);

/* SafeColor */
typedef struct { uint8_t r; uint8_t g; uint8_t b; } ProvenRGBColor;
typedef struct {
    int32_t        status;
    ProvenRGBColor color;
} ProvenColorResult;

ProvenColorResult  proven_color_parse_hex(const uint8_t* ptr, size_t len);
ProvenStringResult proven_color_to_hex(ProvenRGBColor rgb);

/* SafeVersion */
typedef struct {
    uint32_t major;
    uint32_t minor;
    uint32_t patch;
    size_t   prerelease_len;
    char*    prerelease;
} ProvenSemanticVersion;

typedef struct {
    int32_t               status;
    ProvenSemanticVersion version;
} ProvenVersionResult;

ProvenVersionResult proven_version_parse(const uint8_t* ptr, size_t len);
int32_t             proven_version_compare(ProvenSemanticVersion a, ProvenSemanticVersion b);
void                proven_version_free(ProvenSemanticVersion* version);

/* SafeChecksum */
ProvenIntResult  proven_checksum_crc32(const uint8_t* ptr, size_t len);
ProvenBoolResult proven_checksum_verify_crc32(const uint8_t* ptr, size_t len, uint32_t expected);

/* SafeHttp */
ProvenStringResult proven_http_url_encode(const uint8_t* ptr, size_t len);
ProvenStringResult proven_http_url_decode(const uint8_t* ptr, size_t len);

/* SafePassword */
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

/* SafeNetwork */
typedef struct { uint8_t octets[4]; } ProvenIPv4Address;
typedef struct {
    int32_t           status;
    ProvenIPv4Address address;
} ProvenIPv4Result;

ProvenIPv4Result proven_network_parse_ipv4(const uint8_t* ptr, size_t len);
bool             proven_network_ipv4_is_private(ProvenIPv4Address addr);
bool             proven_network_ipv4_is_loopback(ProvenIPv4Address addr);

/* SafeCalculator */
ProvenFloatResult proven_calculator_eval(const uint8_t* ptr, size_t len);

/* -----------------------------------------------------------------------
 * Helper: convert ProvenStringResult to Io Sequence, freeing the C string.
 * Returns nil on error status.
 * ----------------------------------------------------------------------- */
static IoObject* io_string_result(IoObject* self, ProvenStringResult res)
{
    if (res.status != 0) {
        return IONIL(self);
    }
    IoObject* seq = IoSeq_newWithData_length_(
        ((IoState*)NULL), /* The Io runtime resolves state from self */
        res.value, res.length
    );
    proven_free_string(res.value);
    return seq;
}

/* -----------------------------------------------------------------------
 * Lifecycle methods
 * ----------------------------------------------------------------------- */

/**
 * Proven init
 * Initialize the Proven runtime.
 * Returns status code as a Number.
 */
static IoObject* IoProven_init(IoObject* self, IoObject* locals, IoMessage* msg)
{
    (void)locals; (void)msg;
    int32_t status = proven_init();
    return IONUMBER((double)status);
}

/**
 * Proven deinit
 * Shut down the Proven runtime.
 */
static IoObject* IoProven_deinit(IoObject* self, IoObject* locals, IoMessage* msg)
{
    (void)locals; (void)msg;
    proven_deinit();
    return IONIL(self);
}

/**
 * Proven isInitialized
 * Check if the runtime is initialized.
 */
static IoObject* IoProven_isInitialized(IoObject* self, IoObject* locals, IoMessage* msg)
{
    (void)locals; (void)msg;
    return proven_is_initialized() ? IOTRUE(self) : IOFALSE(self);
}

/**
 * Proven version
 * Returns version as "major.minor.patch" string.
 */
static IoObject* IoProven_version(IoObject* self, IoObject* locals, IoMessage* msg)
{
    (void)locals; (void)msg;
    char buf[64];
    snprintf(buf, sizeof(buf), "%u.%u.%u",
             proven_version_major(), proven_version_minor(), proven_version_patch());
    return IoSeq_newWithCString_(NULL, buf);
}

/* -----------------------------------------------------------------------
 * SafeMath methods
 * ----------------------------------------------------------------------- */

/** Proven safeAdd(a, b) - Checked addition. Returns nil on overflow. */
static IoObject* IoProven_safeAdd(IoObject* self, IoObject* locals, IoMessage* msg)
{
    int64_t a = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    int64_t b = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 1));
    ProvenIntResult res = proven_math_add_checked(a, b);
    if (res.status != 0) return IONIL(self);
    return IONUMBER((double)res.value);
}

/** Proven safeSub(a, b) - Checked subtraction. Returns nil on underflow. */
static IoObject* IoProven_safeSub(IoObject* self, IoObject* locals, IoMessage* msg)
{
    int64_t a = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    int64_t b = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 1));
    ProvenIntResult res = proven_math_sub_checked(a, b);
    if (res.status != 0) return IONIL(self);
    return IONUMBER((double)res.value);
}

/** Proven safeMul(a, b) - Checked multiplication. Returns nil on overflow. */
static IoObject* IoProven_safeMul(IoObject* self, IoObject* locals, IoMessage* msg)
{
    int64_t a = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    int64_t b = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 1));
    ProvenIntResult res = proven_math_mul_checked(a, b);
    if (res.status != 0) return IONIL(self);
    return IONUMBER((double)res.value);
}

/** Proven safeDiv(a, b) - Safe division. Returns nil on division by zero. */
static IoObject* IoProven_safeDiv(IoObject* self, IoObject* locals, IoMessage* msg)
{
    int64_t a = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    int64_t b = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 1));
    ProvenIntResult res = proven_math_div(a, b);
    if (res.status != 0) return IONIL(self);
    return IONUMBER((double)res.value);
}

/** Proven safeMod(a, b) - Safe modulo. Returns nil on division by zero. */
static IoObject* IoProven_safeMod(IoObject* self, IoObject* locals, IoMessage* msg)
{
    int64_t a = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    int64_t b = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 1));
    ProvenIntResult res = proven_math_mod(a, b);
    if (res.status != 0) return IONIL(self);
    return IONUMBER((double)res.value);
}

/** Proven safeAbs(n) - Safe absolute value. Returns nil for INT64_MIN. */
static IoObject* IoProven_safeAbs(IoObject* self, IoObject* locals, IoMessage* msg)
{
    int64_t n = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    ProvenIntResult res = proven_math_abs_safe(n);
    if (res.status != 0) return IONIL(self);
    return IONUMBER((double)res.value);
}

/** Proven clamp(lo, hi, value) - Clamp value to [lo, hi]. */
static IoObject* IoProven_clamp(IoObject* self, IoObject* locals, IoMessage* msg)
{
    (void)self;
    int64_t lo  = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    int64_t hi  = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 1));
    int64_t val = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 2));
    int64_t result = proven_math_clamp(lo, hi, val);
    return IONUMBER((double)result);
}

/** Proven safePow(base, exp) - Checked exponentiation. Returns nil on overflow. */
static IoObject* IoProven_safePow(IoObject* self, IoObject* locals, IoMessage* msg)
{
    int64_t base = (int64_t)IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    uint32_t exp = (uint32_t)IoNumber_asLong(IoMessage_locals_numberArgAt_(msg, locals, 1));
    ProvenIntResult res = proven_math_pow_checked(base, exp);
    if (res.status != 0) return IONIL(self);
    return IONUMBER((double)res.value);
}

/* -----------------------------------------------------------------------
 * SafeString methods
 * ----------------------------------------------------------------------- */

/** Proven isValidUtf8(str) - Check if string is valid UTF-8. */
static IoObject* IoProven_isValidUtf8(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenBoolResult res = proven_string_is_valid_utf8((const uint8_t*)ptr, len);
    if (res.status != 0) return IONIL(self);
    return res.value ? IOTRUE(self) : IOFALSE(self);
}

/** Proven escapeSql(str) - SQL-escape a string. Returns nil on error. */
static IoObject* IoProven_escapeSql(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenStringResult res = proven_string_escape_sql((const uint8_t*)ptr, len);
    return io_string_result(self, res);
}

/** Proven escapeHtml(str) - HTML-escape a string. Returns nil on error. */
static IoObject* IoProven_escapeHtml(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenStringResult res = proven_string_escape_html((const uint8_t*)ptr, len);
    return io_string_result(self, res);
}

/** Proven escapeJs(str) - JavaScript-escape a string. Returns nil on error. */
static IoObject* IoProven_escapeJs(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenStringResult res = proven_string_escape_js((const uint8_t*)ptr, len);
    return io_string_result(self, res);
}

/* -----------------------------------------------------------------------
 * SafePath methods
 * ----------------------------------------------------------------------- */

/** Proven pathHasTraversal(path) - Check for ".." traversal. */
static IoObject* IoProven_pathHasTraversal(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenBoolResult res = proven_path_has_traversal((const uint8_t*)ptr, len);
    if (res.status != 0) return IONIL(self);
    return res.value ? IOTRUE(self) : IOFALSE(self);
}

/** Proven sanitizeFilename(name) - Remove dangerous characters. */
static IoObject* IoProven_sanitizeFilename(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenStringResult res = proven_path_sanitize_filename((const uint8_t*)ptr, len);
    return io_string_result(self, res);
}

/* -----------------------------------------------------------------------
 * SafeEmail methods
 * ----------------------------------------------------------------------- */

/** Proven isValidEmail(email) - Validate email address. */
static IoObject* IoProven_isValidEmail(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenBoolResult res = proven_email_is_valid((const uint8_t*)ptr, len);
    if (res.status != 0) return IONIL(self);
    return res.value ? IOTRUE(self) : IOFALSE(self);
}

/* -----------------------------------------------------------------------
 * SafeUrl methods
 * ----------------------------------------------------------------------- */

/** Proven validateUrl(url) - Parse URL, returns nil on failure. */
static IoObject* IoProven_parseUrl(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenUrlResult res = proven_url_parse((const uint8_t*)ptr, len);
    if (res.status != 0) return IONIL(self);

    /* Build an Io Object with slots for each component */
    IoObject* obj = IoObject_new(NULL);
    if (res.components.scheme != NULL) {
        IoObject_setSlot_to_(obj, "scheme",
            IoSeq_newWithData_length_(NULL, res.components.scheme, res.components.scheme_len));
    }
    if (res.components.host != NULL) {
        IoObject_setSlot_to_(obj, "host",
            IoSeq_newWithData_length_(NULL, res.components.host, res.components.host_len));
    }
    if (res.components.has_port) {
        IoObject_setSlot_to_(obj, "port", IONUMBER((double)res.components.port));
    }
    if (res.components.path != NULL) {
        IoObject_setSlot_to_(obj, "path",
            IoSeq_newWithData_length_(NULL, res.components.path, res.components.path_len));
    }
    if (res.components.query != NULL) {
        IoObject_setSlot_to_(obj, "query",
            IoSeq_newWithData_length_(NULL, res.components.query, res.components.query_len));
    }
    if (res.components.fragment != NULL) {
        IoObject_setSlot_to_(obj, "fragment",
            IoSeq_newWithData_length_(NULL, res.components.fragment, res.components.fragment_len));
    }
    proven_url_free(&res.components);
    return obj;
}

/** Proven urlEncode(str) - Percent-encode a string. */
static IoObject* IoProven_urlEncode(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenStringResult res = proven_http_url_encode((const uint8_t*)ptr, len);
    return io_string_result(self, res);
}

/** Proven urlDecode(str) - Decode percent-encoded string. */
static IoObject* IoProven_urlDecode(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenStringResult res = proven_http_url_decode((const uint8_t*)ptr, len);
    return io_string_result(self, res);
}

/* -----------------------------------------------------------------------
 * SafeCrypto methods
 * ----------------------------------------------------------------------- */

/** Proven constantTimeEq(a, b) - Timing-safe comparison. */
static IoObject* IoProven_constantTimeEq(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seqA = IoMessage_locals_seqArgAt_(msg, locals, 0);
    IoObject* seqB = IoMessage_locals_seqArgAt_(msg, locals, 1);
    const char* ptrA = IoSeq_asCString(seqA);
    size_t lenA = IoSeq_rawSize(seqA);
    const char* ptrB = IoSeq_asCString(seqB);
    size_t lenB = IoSeq_rawSize(seqB);
    ProvenBoolResult res = proven_crypto_constant_time_eq(
        (const uint8_t*)ptrA, lenA, (const uint8_t*)ptrB, lenB);
    if (res.status != 0) return IONIL(self);
    return res.value ? IOTRUE(self) : IOFALSE(self);
}

/** Proven randomHex(nbytes) - Generate random bytes as hex string. */
static IoObject* IoProven_randomHex(IoObject* self, IoObject* locals, IoMessage* msg)
{
    int nbytes = (int)IoNumber_asLong(IoMessage_locals_numberArgAt_(msg, locals, 0));
    if (nbytes <= 0 || nbytes > 1024) return IONIL(self);

    uint8_t buf[1024];
    int32_t status = proven_crypto_random_bytes(buf, (size_t)nbytes);
    if (status != 0) return IONIL(self);

    ProvenStringResult hex = proven_hex_encode(buf, (size_t)nbytes, false);
    return io_string_result(self, hex);
}

/** Proven hexEncode(data) - Encode bytes as hex. */
static IoObject* IoProven_hexEncode(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenStringResult res = proven_hex_encode((const uint8_t*)ptr, len, false);
    return io_string_result(self, res);
}

/** Proven hexDecode(hex_str) - Decode hex string to bytes. */
static IoObject* IoProven_hexDecode(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenHexDecodeResult res = proven_hex_decode((const uint8_t*)ptr, len);
    if (res.status != 0) return IONIL(self);
    IoObject* result = IoSeq_newWithData_length_(NULL, (const char*)res.data, res.length);
    proven_hex_free(&res);
    return result;
}

/** Proven crc32(data) - Compute CRC32 checksum. */
static IoObject* IoProven_crc32(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenIntResult res = proven_checksum_crc32((const uint8_t*)ptr, len);
    if (res.status != 0) return IONIL(self);
    return IONUMBER((double)res.value);
}

/* -----------------------------------------------------------------------
 * SafeJson methods
 * ----------------------------------------------------------------------- */

/** Proven jsonIsValid(json_str) - Check if string is valid JSON. */
static IoObject* IoProven_jsonIsValid(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenBoolResult res = proven_json_is_valid((const uint8_t*)ptr, len);
    if (res.status != 0) return IONIL(self);
    return res.value ? IOTRUE(self) : IOFALSE(self);
}

/** Proven jsonGetType(json_str) - Get JSON root value type as string. */
static IoObject* IoProven_jsonGetType(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenJsonType t = proven_json_get_type((const uint8_t*)ptr, len);
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
    return IoSeq_newWithCString_(NULL, name);
}

/* -----------------------------------------------------------------------
 * SafeFloat methods
 * ----------------------------------------------------------------------- */

/** Proven floatDiv(a, b) - Safe float division. */
static IoObject* IoProven_floatDiv(IoObject* self, IoObject* locals, IoMessage* msg)
{
    double a = IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    double b = IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 1));
    ProvenFloatResult res = proven_float_div(a, b);
    if (res.status != 0) return IONIL(self);
    return IONUMBER(res.value);
}

/** Proven floatSqrt(x) - Safe square root. */
static IoObject* IoProven_floatSqrt(IoObject* self, IoObject* locals, IoMessage* msg)
{
    double x = IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    ProvenFloatResult res = proven_float_sqrt(x);
    if (res.status != 0) return IONIL(self);
    return IONUMBER(res.value);
}

/** Proven floatLn(x) - Safe natural logarithm. */
static IoObject* IoProven_floatLn(IoObject* self, IoObject* locals, IoMessage* msg)
{
    double x = IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    ProvenFloatResult res = proven_float_ln(x);
    if (res.status != 0) return IONIL(self);
    return IONUMBER(res.value);
}

/** Proven floatIsFinite(x) - Check if float is finite. */
static IoObject* IoProven_floatIsFinite(IoObject* self, IoObject* locals, IoMessage* msg)
{
    double x = IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    return proven_float_is_finite(x) ? IOTRUE(self) : IOFALSE(self);
}

/** Proven floatIsNan(x) - Check if float is NaN. */
static IoObject* IoProven_floatIsNan(IoObject* self, IoObject* locals, IoMessage* msg)
{
    double x = IoNumber_asDouble(IoMessage_locals_numberArgAt_(msg, locals, 0));
    return proven_float_is_nan(x) ? IOTRUE(self) : IOFALSE(self);
}

/* -----------------------------------------------------------------------
 * SafeColor methods
 * ----------------------------------------------------------------------- */

/** Proven parseColor(hex_str) - Parse hex color. Returns nil on error. */
static IoObject* IoProven_parseColor(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenColorResult res = proven_color_parse_hex((const uint8_t*)ptr, len);
    if (res.status != 0) return IONIL(self);

    IoObject* obj = IoObject_new(NULL);
    IoObject_setSlot_to_(obj, "r", IONUMBER((double)res.color.r));
    IoObject_setSlot_to_(obj, "g", IONUMBER((double)res.color.g));
    IoObject_setSlot_to_(obj, "b", IONUMBER((double)res.color.b));
    return obj;
}

/** Proven colorToHex(r, g, b) - Format RGB as hex string. */
static IoObject* IoProven_colorToHex(IoObject* self, IoObject* locals, IoMessage* msg)
{
    ProvenRGBColor rgb;
    rgb.r = (uint8_t)IoNumber_asLong(IoMessage_locals_numberArgAt_(msg, locals, 0));
    rgb.g = (uint8_t)IoNumber_asLong(IoMessage_locals_numberArgAt_(msg, locals, 1));
    rgb.b = (uint8_t)IoNumber_asLong(IoMessage_locals_numberArgAt_(msg, locals, 2));
    ProvenStringResult res = proven_color_to_hex(rgb);
    return io_string_result(self, res);
}

/* -----------------------------------------------------------------------
 * SafeVersion methods
 * ----------------------------------------------------------------------- */

/** Proven versionCompare(a, b) - Compare two semver strings. */
static IoObject* IoProven_versionCompare(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seqA = IoMessage_locals_seqArgAt_(msg, locals, 0);
    IoObject* seqB = IoMessage_locals_seqArgAt_(msg, locals, 1);
    const char* ptrA = IoSeq_asCString(seqA);
    size_t lenA = IoSeq_rawSize(seqA);
    const char* ptrB = IoSeq_asCString(seqB);
    size_t lenB = IoSeq_rawSize(seqB);

    ProvenVersionResult va = proven_version_parse((const uint8_t*)ptrA, lenA);
    if (va.status != 0) return IONIL(self);

    ProvenVersionResult vb = proven_version_parse((const uint8_t*)ptrB, lenB);
    if (vb.status != 0) {
        proven_version_free(&va.version);
        return IONIL(self);
    }

    int32_t cmp = proven_version_compare(va.version, vb.version);
    proven_version_free(&va.version);
    proven_version_free(&vb.version);
    return IONUMBER((double)cmp);
}

/* -----------------------------------------------------------------------
 * SafePassword methods
 * ----------------------------------------------------------------------- */

/** Proven validatePassword(str) - Validate password strength. */
static IoObject* IoProven_validatePassword(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenPasswordResult res = proven_password_validate((const uint8_t*)ptr, len);

    IoObject* obj = IoObject_new(NULL);
    IoObject_setSlot_to_(obj, "strength", IONUMBER((double)res.strength));
    IoObject_setSlot_to_(obj, "hasLowercase", res.has_lowercase ? IOTRUE(self) : IOFALSE(self));
    IoObject_setSlot_to_(obj, "hasUppercase", res.has_uppercase ? IOTRUE(self) : IOFALSE(self));
    IoObject_setSlot_to_(obj, "hasDigit", res.has_digit ? IOTRUE(self) : IOFALSE(self));
    IoObject_setSlot_to_(obj, "hasSpecial", res.has_special ? IOTRUE(self) : IOFALSE(self));
    IoObject_setSlot_to_(obj, "length", IONUMBER((double)res.length));
    return obj;
}

/** Proven isCommonPassword(str) - Check if password is in common list. */
static IoObject* IoProven_isCommonPassword(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    bool common = proven_password_is_common((const uint8_t*)ptr, len);
    return common ? IOTRUE(self) : IOFALSE(self);
}

/* -----------------------------------------------------------------------
 * SafeNetwork methods
 * ----------------------------------------------------------------------- */

/** Proven parseIpv4(str) - Parse IPv4 address. Returns nil on error. */
static IoObject* IoProven_parseIpv4(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenIPv4Result res = proven_network_parse_ipv4((const uint8_t*)ptr, len);
    if (res.status != 0) return IONIL(self);

    /* Format as dotted-quad string for convenience */
    char buf[16];
    snprintf(buf, sizeof(buf), "%u.%u.%u.%u",
             res.address.octets[0], res.address.octets[1],
             res.address.octets[2], res.address.octets[3]);

    IoObject* obj = IoObject_new(NULL);
    IoObject_setSlot_to_(obj, "address", IoSeq_newWithCString_(NULL, buf));
    IoObject_setSlot_to_(obj, "isPrivate",
        proven_network_ipv4_is_private(res.address) ? IOTRUE(self) : IOFALSE(self));
    IoObject_setSlot_to_(obj, "isLoopback",
        proven_network_ipv4_is_loopback(res.address) ? IOTRUE(self) : IOFALSE(self));
    return obj;
}

/* -----------------------------------------------------------------------
 * SafeCalculator methods
 * ----------------------------------------------------------------------- */

/** Proven calcEval(expr) - Evaluate arithmetic expression. */
static IoObject* IoProven_calcEval(IoObject* self, IoObject* locals, IoMessage* msg)
{
    IoObject* seq = IoMessage_locals_seqArgAt_(msg, locals, 0);
    const char* ptr = IoSeq_asCString(seq);
    size_t len = IoSeq_rawSize(seq);
    ProvenFloatResult res = proven_calculator_eval((const uint8_t*)ptr, len);
    if (res.status != 0) return IONIL(self);
    return IONUMBER(res.value);
}

/* -----------------------------------------------------------------------
 * Method table registration
 * ----------------------------------------------------------------------- */

static IoMethodTable IoProven_methodTable[] = {
    /* Lifecycle */
    {"init",             IoProven_init},
    {"deinit",           IoProven_deinit},
    {"isInitialized",    IoProven_isInitialized},
    {"version",          IoProven_version},

    /* SafeMath */
    {"safeAdd",          IoProven_safeAdd},
    {"safeSub",          IoProven_safeSub},
    {"safeMul",          IoProven_safeMul},
    {"safeDiv",          IoProven_safeDiv},
    {"safeMod",          IoProven_safeMod},
    {"safeAbs",          IoProven_safeAbs},
    {"clamp",            IoProven_clamp},
    {"safePow",          IoProven_safePow},

    /* SafeString */
    {"isValidUtf8",      IoProven_isValidUtf8},
    {"escapeSql",        IoProven_escapeSql},
    {"escapeHtml",       IoProven_escapeHtml},
    {"escapeJs",         IoProven_escapeJs},

    /* SafePath */
    {"pathHasTraversal", IoProven_pathHasTraversal},
    {"sanitizeFilename", IoProven_sanitizeFilename},

    /* SafeEmail */
    {"isValidEmail",     IoProven_isValidEmail},

    /* SafeUrl */
    {"parseUrl",         IoProven_parseUrl},
    {"urlEncode",        IoProven_urlEncode},
    {"urlDecode",        IoProven_urlDecode},

    /* SafeCrypto */
    {"constantTimeEq",   IoProven_constantTimeEq},
    {"randomHex",        IoProven_randomHex},
    {"hexEncode",        IoProven_hexEncode},
    {"hexDecode",        IoProven_hexDecode},
    {"crc32",            IoProven_crc32},

    /* SafeJson */
    {"jsonIsValid",      IoProven_jsonIsValid},
    {"jsonGetType",      IoProven_jsonGetType},

    /* SafeFloat */
    {"floatDiv",         IoProven_floatDiv},
    {"floatSqrt",        IoProven_floatSqrt},
    {"floatLn",          IoProven_floatLn},
    {"floatIsFinite",    IoProven_floatIsFinite},
    {"floatIsNan",       IoProven_floatIsNan},

    /* SafeColor */
    {"parseColor",       IoProven_parseColor},
    {"colorToHex",       IoProven_colorToHex},

    /* SafeVersion */
    {"versionCompare",   IoProven_versionCompare},

    /* SafePassword */
    {"validatePassword", IoProven_validatePassword},
    {"isCommonPassword", IoProven_isCommonPassword},

    /* SafeNetwork */
    {"parseIpv4",        IoProven_parseIpv4},

    /* SafeCalculator */
    {"calcEval",         IoProven_calcEval},

    {NULL, NULL}
};

/**
 * IoProven_proto - Create and register the Proven prototype.
 *
 * Called by the Io runtime when loading this addon. Registers the "Proven"
 * prototype with all methods in the global state.
 *
 * @param state The Io runtime state.
 * @return The new Proven prototype object.
 */
IoObject* IoProven_proto(void* state)
{
    IoObject* self = IoObject_new((IoState*)state);
    IoObject_addMethodTable_(self, IoProven_methodTable);
    IoState_registerProtoWithId_((IoState*)state, self, "Proven");
    return self;
}
