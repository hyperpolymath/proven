/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> */

/**
 * @file proven_r.c
 * @brief C bridge between R's SEXP types and libproven's C ABI.
 *
 * Every function in this file is a thin wrapper: it converts R SEXP arguments
 * to the types expected by libproven, calls the libproven function, and
 * converts the result back to an R SEXP. NO logic is reimplemented here.
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>
#include <string.h>

/* Forward declarations of libproven types and functions.
 * We declare them here rather than including proven.h to keep the build
 * self-contained; the linker resolves them against libproven.so. */

typedef struct { int32_t status; int64_t value; }   ProvenIntResult;
typedef struct { int32_t status; int    value; }     ProvenBoolResult;
typedef struct { int32_t status; char*  value; size_t length; } ProvenStringResult;
typedef struct { int32_t status; double value; }     ProvenFloatResult;
typedef struct { uint8_t octets[4]; } ProvenIPv4Address;
typedef struct { int32_t status; ProvenIPv4Address address; } ProvenIPv4Result;
typedef struct { uint8_t bytes[16]; } ProvenUUID;
typedef struct { int32_t status; ProvenUUID uuid; } ProvenUUIDResult;

/* Lifecycle */
extern int32_t proven_init(void);
extern void    proven_deinit(void);
extern int     proven_is_initialized(void);

/* Memory */
extern void proven_free_string(char* ptr);

/* Version */
extern uint32_t proven_ffi_abi_version(void);
extern uint32_t proven_version_major(void);
extern uint32_t proven_version_minor(void);
extern uint32_t proven_version_patch(void);
extern uint32_t proven_module_count(void);

/* SafeMath (9 functions) */
extern ProvenIntResult proven_math_div(int64_t a, int64_t b);
extern ProvenIntResult proven_math_mod(int64_t a, int64_t b);
extern ProvenIntResult proven_math_add_checked(int64_t a, int64_t b);
extern ProvenIntResult proven_math_sub_checked(int64_t a, int64_t b);
extern ProvenIntResult proven_math_mul_checked(int64_t a, int64_t b);
extern ProvenIntResult proven_math_abs_safe(int64_t n);
extern int64_t         proven_math_clamp(int64_t lo, int64_t hi, int64_t value);
extern ProvenIntResult proven_math_pow_checked(int64_t base, uint32_t exp);

/* SafeString (4 functions) */
extern ProvenBoolResult   proven_string_is_valid_utf8(const uint8_t* ptr, size_t len);
extern ProvenStringResult proven_string_escape_sql(const uint8_t* ptr, size_t len);
extern ProvenStringResult proven_string_escape_html(const uint8_t* ptr, size_t len);
extern ProvenStringResult proven_string_escape_js(const uint8_t* ptr, size_t len);

/* SafePath (2 functions) */
extern ProvenBoolResult   proven_path_has_traversal(const uint8_t* ptr, size_t len);
extern ProvenStringResult proven_path_sanitize_filename(const uint8_t* ptr, size_t len);

/* SafeCrypto (2 functions) */
extern ProvenBoolResult proven_crypto_constant_time_eq(const uint8_t* p1, size_t l1, const uint8_t* p2, size_t l2);
extern int32_t          proven_crypto_random_bytes(uint8_t* ptr, size_t len);

/* SafeEmail (1 function) */
extern ProvenBoolResult proven_email_is_valid(const uint8_t* ptr, size_t len);

/* SafeNetwork (3 functions) */
extern ProvenIPv4Result proven_network_parse_ipv4(const uint8_t* ptr, size_t len);
extern int              proven_network_ipv4_is_private(ProvenIPv4Address addr);
extern int              proven_network_ipv4_is_loopback(ProvenIPv4Address addr);

/* SafeFloat (5 functions) */
extern ProvenFloatResult proven_float_div(double a, double b);
extern int               proven_float_is_finite(double x);
extern int               proven_float_is_nan(double x);
extern ProvenFloatResult proven_float_sqrt(double x);
extern ProvenFloatResult proven_float_ln(double x);

/* SafeJson (2 functions) */
extern ProvenBoolResult proven_json_is_valid(const uint8_t* ptr, size_t len);
extern int32_t          proven_json_get_type(const uint8_t* ptr, size_t len);

/* SafeAngle (4 functions) */
extern double proven_angle_deg_to_rad(double degrees);
extern double proven_angle_rad_to_deg(double radians);
extern double proven_angle_normalize_degrees(double degrees);
extern double proven_angle_normalize_radians(double radians);

/* SafeProbability (4 functions) */
extern double proven_probability_create(double value);
extern double proven_probability_and(double a, double b);
extern double proven_probability_or_exclusive(double a, double b);
extern double proven_probability_not(double p);

/* SafeMl (4 scalar functions) */
extern double proven_ml_sigmoid(double x);
extern double proven_ml_relu(double x);
extern double proven_ml_leaky_relu(double x, double alpha);
extern double proven_ml_clamp(double x, double min_val, double max_val);

/* SafeChecksum (2 functions) */
extern ProvenIntResult  proven_checksum_crc32(const uint8_t* ptr, size_t len);
extern ProvenBoolResult proven_checksum_verify_crc32(const uint8_t* ptr, size_t len, uint32_t expected);

/* SafeUUID (5 functions) */
extern ProvenUUIDResult  proven_uuid_v4(void);
extern ProvenStringResult proven_uuid_to_string(ProvenUUID uuid);
extern ProvenUUIDResult  proven_uuid_parse(const uint8_t* ptr, size_t len);
extern int               proven_uuid_is_nil(ProvenUUID uuid);
extern uint8_t           proven_uuid_version(ProvenUUID uuid);

/* SafeHex (encode only; decode involves struct with pointer) */
extern ProvenStringResult proven_hex_encode(const uint8_t* ptr, size_t len, int uppercase);

/* SafePassword (2 functions) */
typedef struct {
    int32_t strength;
    int     has_lowercase;
    int     has_uppercase;
    int     has_digit;
    int     has_special;
    size_t  length;
} ProvenPasswordResult;
extern ProvenPasswordResult proven_password_validate(const uint8_t* ptr, size_t len);
extern int                  proven_password_is_common(const uint8_t* ptr, size_t len);

/* SafeCalculator (1 function) */
extern ProvenFloatResult proven_calculator_eval(const uint8_t* ptr, size_t len);

/* ===== Helper: extract string from ProvenStringResult into R SEXP ===== */
static SEXP string_result_to_sexp(ProvenStringResult r) {
    if (r.status != 0 || r.value == NULL) return R_NilValue;
    SEXP s = PROTECT(mkCharLenCE(r.value, (int)r.length, CE_UTF8));
    SEXP out = PROTECT(ScalarString(s));
    proven_free_string(r.value);
    UNPROTECT(2);
    return out;
}

/* ===== Helper: extract R string as (ptr, len) ===== */
static void r_str_args(SEXP s, const uint8_t** ptr, size_t* len) {
    const char* c = CHAR(STRING_ELT(s, 0));
    *ptr = (const uint8_t*)c;
    *len = (size_t)strlen(c);
}

/* ========================================================================
 * R-callable wrappers
 * ======================================================================== */

/* --- Lifecycle --- */

SEXP R_proven_init(void)           { return ScalarLogical(proven_init() == 0); }
SEXP R_proven_deinit(void)         { proven_deinit(); return R_NilValue; }
SEXP R_proven_is_initialized(void) { return ScalarLogical(proven_is_initialized()); }

/* --- Version --- */

SEXP R_proven_version_major(void)  { return ScalarInteger((int)proven_version_major()); }
SEXP R_proven_version_minor(void)  { return ScalarInteger((int)proven_version_minor()); }
SEXP R_proven_version_patch(void)  { return ScalarInteger((int)proven_version_patch()); }
SEXP R_proven_module_count(void)   { return ScalarInteger((int)proven_module_count()); }

/* --- SafeMath --- */

SEXP R_proven_math_div(SEXP a, SEXP b) {
    ProvenIntResult r = proven_math_div((int64_t)asReal(a), (int64_t)asReal(b));
    return r.status == 0 ? ScalarReal((double)r.value) : ScalarReal(NA_REAL);
}

SEXP R_proven_math_mod(SEXP a, SEXP b) {
    ProvenIntResult r = proven_math_mod((int64_t)asReal(a), (int64_t)asReal(b));
    return r.status == 0 ? ScalarReal((double)r.value) : ScalarReal(NA_REAL);
}

SEXP R_proven_math_add_checked(SEXP a, SEXP b) {
    ProvenIntResult r = proven_math_add_checked((int64_t)asReal(a), (int64_t)asReal(b));
    return r.status == 0 ? ScalarReal((double)r.value) : ScalarReal(NA_REAL);
}

SEXP R_proven_math_sub_checked(SEXP a, SEXP b) {
    ProvenIntResult r = proven_math_sub_checked((int64_t)asReal(a), (int64_t)asReal(b));
    return r.status == 0 ? ScalarReal((double)r.value) : ScalarReal(NA_REAL);
}

SEXP R_proven_math_mul_checked(SEXP a, SEXP b) {
    ProvenIntResult r = proven_math_mul_checked((int64_t)asReal(a), (int64_t)asReal(b));
    return r.status == 0 ? ScalarReal((double)r.value) : ScalarReal(NA_REAL);
}

SEXP R_proven_math_abs_safe(SEXP n) {
    ProvenIntResult r = proven_math_abs_safe((int64_t)asReal(n));
    return r.status == 0 ? ScalarReal((double)r.value) : ScalarReal(NA_REAL);
}

SEXP R_proven_math_clamp(SEXP lo, SEXP hi, SEXP value) {
    int64_t result = proven_math_clamp((int64_t)asReal(lo), (int64_t)asReal(hi), (int64_t)asReal(value));
    return ScalarReal((double)result);
}

SEXP R_proven_math_pow_checked(SEXP base, SEXP exp) {
    ProvenIntResult r = proven_math_pow_checked((int64_t)asReal(base), (uint32_t)asInteger(exp));
    return r.status == 0 ? ScalarReal((double)r.value) : ScalarReal(NA_REAL);
}

/* --- SafeString --- */

SEXP R_proven_string_escape_sql(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    return string_result_to_sexp(proven_string_escape_sql(ptr, len));
}

SEXP R_proven_string_escape_html(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    return string_result_to_sexp(proven_string_escape_html(ptr, len));
}

SEXP R_proven_string_escape_js(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    return string_result_to_sexp(proven_string_escape_js(ptr, len));
}

SEXP R_proven_string_is_valid_utf8(SEXP raw_vec) {
    int n = LENGTH(raw_vec);
    ProvenBoolResult r = proven_string_is_valid_utf8(RAW(raw_vec), (size_t)n);
    return r.status == 0 ? ScalarLogical(r.value) : ScalarLogical(NA_LOGICAL);
}

/* --- SafePath --- */

SEXP R_proven_path_has_traversal(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    ProvenBoolResult r = proven_path_has_traversal(ptr, len);
    return r.status == 0 ? ScalarLogical(r.value) : ScalarLogical(NA_LOGICAL);
}

SEXP R_proven_path_sanitize_filename(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    return string_result_to_sexp(proven_path_sanitize_filename(ptr, len));
}

/* --- SafeCrypto --- */

SEXP R_proven_crypto_constant_time_eq(SEXP a, SEXP b) {
    ProvenBoolResult r = proven_crypto_constant_time_eq(
        RAW(a), (size_t)LENGTH(a), RAW(b), (size_t)LENGTH(b));
    return r.status == 0 ? ScalarLogical(r.value) : ScalarLogical(NA_LOGICAL);
}

SEXP R_proven_crypto_random_bytes(SEXP n) {
    int count = asInteger(n);
    SEXP out = PROTECT(allocVector(RAWSXP, count));
    int32_t status = proven_crypto_random_bytes(RAW(out), (size_t)count);
    UNPROTECT(1);
    return status == 0 ? out : R_NilValue;
}

/* --- SafeEmail --- */

SEXP R_proven_email_is_valid(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    ProvenBoolResult r = proven_email_is_valid(ptr, len);
    return r.status == 0 ? ScalarLogical(r.value) : ScalarLogical(NA_LOGICAL);
}

/* --- SafeNetwork --- */

SEXP R_proven_network_parse_ipv4(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    ProvenIPv4Result r = proven_network_parse_ipv4(ptr, len);
    if (r.status != 0) return R_NilValue;
    SEXP out = PROTECT(allocVector(INTSXP, 4));
    INTEGER(out)[0] = r.address.octets[0];
    INTEGER(out)[1] = r.address.octets[1];
    INTEGER(out)[2] = r.address.octets[2];
    INTEGER(out)[3] = r.address.octets[3];
    UNPROTECT(1);
    return out;
}

SEXP R_proven_network_ipv4_is_private(SEXP octets) {
    ProvenIPv4Address addr;
    addr.octets[0] = (uint8_t)INTEGER(octets)[0];
    addr.octets[1] = (uint8_t)INTEGER(octets)[1];
    addr.octets[2] = (uint8_t)INTEGER(octets)[2];
    addr.octets[3] = (uint8_t)INTEGER(octets)[3];
    return ScalarLogical(proven_network_ipv4_is_private(addr));
}

SEXP R_proven_network_ipv4_is_loopback(SEXP octets) {
    ProvenIPv4Address addr;
    addr.octets[0] = (uint8_t)INTEGER(octets)[0];
    addr.octets[1] = (uint8_t)INTEGER(octets)[1];
    addr.octets[2] = (uint8_t)INTEGER(octets)[2];
    addr.octets[3] = (uint8_t)INTEGER(octets)[3];
    return ScalarLogical(proven_network_ipv4_is_loopback(addr));
}

/* --- SafeFloat --- */

SEXP R_proven_float_div(SEXP a, SEXP b) {
    ProvenFloatResult r = proven_float_div(asReal(a), asReal(b));
    return r.status == 0 ? ScalarReal(r.value) : ScalarReal(NA_REAL);
}

SEXP R_proven_float_is_finite(SEXP x) {
    return ScalarLogical(proven_float_is_finite(asReal(x)));
}

SEXP R_proven_float_is_nan(SEXP x) {
    return ScalarLogical(proven_float_is_nan(asReal(x)));
}

SEXP R_proven_float_sqrt(SEXP x) {
    ProvenFloatResult r = proven_float_sqrt(asReal(x));
    return r.status == 0 ? ScalarReal(r.value) : ScalarReal(NA_REAL);
}

SEXP R_proven_float_ln(SEXP x) {
    ProvenFloatResult r = proven_float_ln(asReal(x));
    return r.status == 0 ? ScalarReal(r.value) : ScalarReal(NA_REAL);
}

/* --- SafeJson --- */

SEXP R_proven_json_is_valid(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    ProvenBoolResult r = proven_json_is_valid(ptr, len);
    return r.status == 0 ? ScalarLogical(r.value) : ScalarLogical(NA_LOGICAL);
}

SEXP R_proven_json_get_type(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    return ScalarInteger(proven_json_get_type(ptr, len));
}

/* --- SafeAngle --- */

SEXP R_proven_angle_deg_to_rad(SEXP d)          { return ScalarReal(proven_angle_deg_to_rad(asReal(d))); }
SEXP R_proven_angle_rad_to_deg(SEXP r)          { return ScalarReal(proven_angle_rad_to_deg(asReal(r))); }
SEXP R_proven_angle_normalize_degrees(SEXP d)   { return ScalarReal(proven_angle_normalize_degrees(asReal(d))); }
SEXP R_proven_angle_normalize_radians(SEXP r)   { return ScalarReal(proven_angle_normalize_radians(asReal(r))); }

/* --- SafeProbability --- */

SEXP R_proven_probability_create(SEXP v)              { return ScalarReal(proven_probability_create(asReal(v))); }
SEXP R_proven_probability_and(SEXP a, SEXP b)         { return ScalarReal(proven_probability_and(asReal(a), asReal(b))); }
SEXP R_proven_probability_or_exclusive(SEXP a, SEXP b) { return ScalarReal(proven_probability_or_exclusive(asReal(a), asReal(b))); }
SEXP R_proven_probability_not(SEXP p)                 { return ScalarReal(proven_probability_not(asReal(p))); }

/* --- SafeMl --- */

SEXP R_proven_ml_sigmoid(SEXP x)                            { return ScalarReal(proven_ml_sigmoid(asReal(x))); }
SEXP R_proven_ml_relu(SEXP x)                               { return ScalarReal(proven_ml_relu(asReal(x))); }
SEXP R_proven_ml_leaky_relu(SEXP x, SEXP alpha)             { return ScalarReal(proven_ml_leaky_relu(asReal(x), asReal(alpha))); }
SEXP R_proven_ml_clamp(SEXP x, SEXP min_val, SEXP max_val)  { return ScalarReal(proven_ml_clamp(asReal(x), asReal(min_val), asReal(max_val))); }

/* --- SafeChecksum --- */

SEXP R_proven_checksum_crc32(SEXP raw_vec) {
    ProvenIntResult r = proven_checksum_crc32(RAW(raw_vec), (size_t)LENGTH(raw_vec));
    return r.status == 0 ? ScalarReal((double)r.value) : ScalarReal(NA_REAL);
}

SEXP R_proven_checksum_verify_crc32(SEXP raw_vec, SEXP expected) {
    ProvenBoolResult r = proven_checksum_verify_crc32(RAW(raw_vec), (size_t)LENGTH(raw_vec), (uint32_t)asReal(expected));
    return r.status == 0 ? ScalarLogical(r.value) : ScalarLogical(NA_LOGICAL);
}

/* --- SafeUUID --- */

SEXP R_proven_uuid_v4(void) {
    ProvenUUIDResult r = proven_uuid_v4();
    if (r.status != 0) return R_NilValue;
    ProvenStringResult sr = proven_uuid_to_string(r.uuid);
    return string_result_to_sexp(sr);
}

SEXP R_proven_uuid_parse(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    ProvenUUIDResult r = proven_uuid_parse(ptr, len);
    if (r.status != 0) return R_NilValue;
    ProvenStringResult sr = proven_uuid_to_string(r.uuid);
    return string_result_to_sexp(sr);
}

/* --- SafeHex --- */

SEXP R_proven_hex_encode(SEXP raw_vec, SEXP uppercase) {
    ProvenStringResult r = proven_hex_encode(RAW(raw_vec), (size_t)LENGTH(raw_vec), asLogical(uppercase));
    return string_result_to_sexp(r);
}

/* --- SafePassword --- */

SEXP R_proven_password_validate(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    ProvenPasswordResult r = proven_password_validate(ptr, len);
    /* Return a named list */
    SEXP out = PROTECT(allocVector(VECSXP, 6));
    SEXP names = PROTECT(allocVector(STRSXP, 6));
    SET_STRING_ELT(names, 0, mkChar("strength"));
    SET_STRING_ELT(names, 1, mkChar("has_lowercase"));
    SET_STRING_ELT(names, 2, mkChar("has_uppercase"));
    SET_STRING_ELT(names, 3, mkChar("has_digit"));
    SET_STRING_ELT(names, 4, mkChar("has_special"));
    SET_STRING_ELT(names, 5, mkChar("length"));
    SET_VECTOR_ELT(out, 0, ScalarInteger(r.strength));
    SET_VECTOR_ELT(out, 1, ScalarLogical(r.has_lowercase));
    SET_VECTOR_ELT(out, 2, ScalarLogical(r.has_uppercase));
    SET_VECTOR_ELT(out, 3, ScalarLogical(r.has_digit));
    SET_VECTOR_ELT(out, 4, ScalarLogical(r.has_special));
    SET_VECTOR_ELT(out, 5, ScalarInteger((int)r.length));
    setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(2);
    return out;
}

SEXP R_proven_password_is_common(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    return ScalarLogical(proven_password_is_common(ptr, len));
}

/* --- SafeCalculator --- */

SEXP R_proven_calculator_eval(SEXP s) {
    const uint8_t* ptr; size_t len;
    r_str_args(s, &ptr, &len);
    ProvenFloatResult r = proven_calculator_eval(ptr, len);
    return r.status == 0 ? ScalarReal(r.value) : ScalarReal(NA_REAL);
}

/* ========================================================================
 * Registration table -- maps R names to C functions
 * ======================================================================== */

static const R_CallMethodDef CallEntries[] = {
    /* Lifecycle */
    {"R_proven_init",                  (DL_FUNC) &R_proven_init,                  0},
    {"R_proven_deinit",                (DL_FUNC) &R_proven_deinit,                0},
    {"R_proven_is_initialized",        (DL_FUNC) &R_proven_is_initialized,        0},
    /* Version */
    {"R_proven_version_major",         (DL_FUNC) &R_proven_version_major,         0},
    {"R_proven_version_minor",         (DL_FUNC) &R_proven_version_minor,         0},
    {"R_proven_version_patch",         (DL_FUNC) &R_proven_version_patch,         0},
    {"R_proven_module_count",          (DL_FUNC) &R_proven_module_count,          0},
    /* SafeMath */
    {"R_proven_math_div",              (DL_FUNC) &R_proven_math_div,              2},
    {"R_proven_math_mod",              (DL_FUNC) &R_proven_math_mod,              2},
    {"R_proven_math_add_checked",      (DL_FUNC) &R_proven_math_add_checked,      2},
    {"R_proven_math_sub_checked",      (DL_FUNC) &R_proven_math_sub_checked,      2},
    {"R_proven_math_mul_checked",      (DL_FUNC) &R_proven_math_mul_checked,      2},
    {"R_proven_math_abs_safe",         (DL_FUNC) &R_proven_math_abs_safe,         1},
    {"R_proven_math_clamp",            (DL_FUNC) &R_proven_math_clamp,            3},
    {"R_proven_math_pow_checked",      (DL_FUNC) &R_proven_math_pow_checked,      2},
    /* SafeString */
    {"R_proven_string_escape_sql",     (DL_FUNC) &R_proven_string_escape_sql,     1},
    {"R_proven_string_escape_html",    (DL_FUNC) &R_proven_string_escape_html,    1},
    {"R_proven_string_escape_js",      (DL_FUNC) &R_proven_string_escape_js,      1},
    {"R_proven_string_is_valid_utf8",  (DL_FUNC) &R_proven_string_is_valid_utf8,  1},
    /* SafePath */
    {"R_proven_path_has_traversal",    (DL_FUNC) &R_proven_path_has_traversal,    1},
    {"R_proven_path_sanitize_filename",(DL_FUNC) &R_proven_path_sanitize_filename,1},
    /* SafeCrypto */
    {"R_proven_crypto_constant_time_eq",(DL_FUNC)&R_proven_crypto_constant_time_eq,2},
    {"R_proven_crypto_random_bytes",   (DL_FUNC) &R_proven_crypto_random_bytes,   1},
    /* SafeEmail */
    {"R_proven_email_is_valid",        (DL_FUNC) &R_proven_email_is_valid,        1},
    /* SafeNetwork */
    {"R_proven_network_parse_ipv4",    (DL_FUNC) &R_proven_network_parse_ipv4,    1},
    {"R_proven_network_ipv4_is_private",(DL_FUNC)&R_proven_network_ipv4_is_private,1},
    {"R_proven_network_ipv4_is_loopback",(DL_FUNC)&R_proven_network_ipv4_is_loopback,1},
    /* SafeFloat */
    {"R_proven_float_div",             (DL_FUNC) &R_proven_float_div,             2},
    {"R_proven_float_is_finite",       (DL_FUNC) &R_proven_float_is_finite,       1},
    {"R_proven_float_is_nan",          (DL_FUNC) &R_proven_float_is_nan,          1},
    {"R_proven_float_sqrt",            (DL_FUNC) &R_proven_float_sqrt,            1},
    {"R_proven_float_ln",              (DL_FUNC) &R_proven_float_ln,              1},
    /* SafeJson */
    {"R_proven_json_is_valid",         (DL_FUNC) &R_proven_json_is_valid,         1},
    {"R_proven_json_get_type",         (DL_FUNC) &R_proven_json_get_type,         1},
    /* SafeAngle */
    {"R_proven_angle_deg_to_rad",      (DL_FUNC) &R_proven_angle_deg_to_rad,      1},
    {"R_proven_angle_rad_to_deg",      (DL_FUNC) &R_proven_angle_rad_to_deg,      1},
    {"R_proven_angle_normalize_degrees",(DL_FUNC)&R_proven_angle_normalize_degrees,1},
    {"R_proven_angle_normalize_radians",(DL_FUNC)&R_proven_angle_normalize_radians,1},
    /* SafeProbability */
    {"R_proven_probability_create",    (DL_FUNC) &R_proven_probability_create,    1},
    {"R_proven_probability_and",       (DL_FUNC) &R_proven_probability_and,       2},
    {"R_proven_probability_or_exclusive",(DL_FUNC)&R_proven_probability_or_exclusive,2},
    {"R_proven_probability_not",       (DL_FUNC) &R_proven_probability_not,       1},
    /* SafeMl */
    {"R_proven_ml_sigmoid",            (DL_FUNC) &R_proven_ml_sigmoid,            1},
    {"R_proven_ml_relu",               (DL_FUNC) &R_proven_ml_relu,              1},
    {"R_proven_ml_leaky_relu",         (DL_FUNC) &R_proven_ml_leaky_relu,         2},
    {"R_proven_ml_clamp",              (DL_FUNC) &R_proven_ml_clamp,              3},
    /* SafeChecksum */
    {"R_proven_checksum_crc32",        (DL_FUNC) &R_proven_checksum_crc32,        1},
    {"R_proven_checksum_verify_crc32", (DL_FUNC) &R_proven_checksum_verify_crc32, 2},
    /* SafeUUID */
    {"R_proven_uuid_v4",               (DL_FUNC) &R_proven_uuid_v4,              0},
    {"R_proven_uuid_parse",            (DL_FUNC) &R_proven_uuid_parse,            1},
    /* SafeHex */
    {"R_proven_hex_encode",            (DL_FUNC) &R_proven_hex_encode,            2},
    /* SafePassword */
    {"R_proven_password_validate",     (DL_FUNC) &R_proven_password_validate,     1},
    {"R_proven_password_is_common",    (DL_FUNC) &R_proven_password_is_common,    1},
    /* SafeCalculator */
    {"R_proven_calculator_eval",       (DL_FUNC) &R_proven_calculator_eval,       1},
    {NULL, NULL, 0}
};

void R_init_proven(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
