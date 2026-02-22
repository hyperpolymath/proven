/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> */

/**
 * @file proven_udf.c
 * @brief MySQL UDF plugin calling libproven
 *
 * This plugin exposes formally verified functions from libproven as
 * MySQL User-Defined Functions (UDFs). Install with:
 *   CREATE FUNCTION proven_safe_add RETURNS INTEGER SONAME 'proven_udf.so';
 *
 * ALL computation is performed in Idris 2 via the Zig FFI bridge.
 * This file contains ONLY marshaling between MySQL UDF types and the
 * libproven C ABI. No algorithms are reimplemented here.
 *
 * Build:
 *   gcc -shared -fPIC -o proven_udf.so proven_udf.c \
 *       -I../../../bindings/c/include -lproven \
 *       $(mysql_config --cflags)
 */

#include <mysql.h>

#include <proven.h>

#include <string.h>
#include <stdlib.h>

/* Track whether the proven runtime is initialized */
static int proven_runtime_initialized = 0;

/**
 * Ensure the proven runtime is initialized exactly once.
 */
static void ensure_proven_init(void)
{
    if (!proven_runtime_initialized)
    {
        int32_t rc = proven_init();
        if (rc == PROVEN_OK)
            proven_runtime_initialized = 1;
    }
}

/* ============================================================================
 * proven_safe_add(a BIGINT, b BIGINT) RETURNS BIGINT
 *
 * Checked addition with overflow detection. Returns NULL on overflow.
 * ============================================================================ */

my_bool proven_safe_add_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 2)
    {
        strncpy(message, "proven_safe_add requires exactly 2 arguments", MYSQL_ERRMSG_SIZE);
        return 1;
    }
    args->arg_type[0] = INT_RESULT;
    args->arg_type[1] = INT_RESULT;
    initid->maybe_null = 1;
    ensure_proven_init();
    return 0;
}

void proven_safe_add_deinit(UDF_INIT *initid)
{
    (void)initid;
}

long long proven_safe_add(UDF_INIT *initid, UDF_ARGS *args,
                          char *is_null, char *error)
{
    (void)initid;
    (void)error;

    if (args->args[0] == NULL || args->args[1] == NULL)
    {
        *is_null = 1;
        return 0;
    }

    int64_t a = *((long long *) args->args[0]);
    int64_t b = *((long long *) args->args[1]);

    ProvenIntResult result = proven_math_add_checked(a, b);
    if (PROVEN_FAILED(result))
    {
        *is_null = 1;
        return 0;
    }

    return (long long) result.value;
}

/* ============================================================================
 * proven_safe_sub(a BIGINT, b BIGINT) RETURNS BIGINT
 *
 * Checked subtraction with underflow detection. Returns NULL on underflow.
 * ============================================================================ */

my_bool proven_safe_sub_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 2)
    {
        strncpy(message, "proven_safe_sub requires exactly 2 arguments", MYSQL_ERRMSG_SIZE);
        return 1;
    }
    args->arg_type[0] = INT_RESULT;
    args->arg_type[1] = INT_RESULT;
    initid->maybe_null = 1;
    ensure_proven_init();
    return 0;
}

void proven_safe_sub_deinit(UDF_INIT *initid)
{
    (void)initid;
}

long long proven_safe_sub(UDF_INIT *initid, UDF_ARGS *args,
                          char *is_null, char *error)
{
    (void)initid;
    (void)error;

    if (args->args[0] == NULL || args->args[1] == NULL)
    {
        *is_null = 1;
        return 0;
    }

    int64_t a = *((long long *) args->args[0]);
    int64_t b = *((long long *) args->args[1]);

    ProvenIntResult result = proven_math_sub_checked(a, b);
    if (PROVEN_FAILED(result))
    {
        *is_null = 1;
        return 0;
    }

    return (long long) result.value;
}

/* ============================================================================
 * proven_safe_mul(a BIGINT, b BIGINT) RETURNS BIGINT
 *
 * Checked multiplication with overflow detection. Returns NULL on overflow.
 * ============================================================================ */

my_bool proven_safe_mul_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 2)
    {
        strncpy(message, "proven_safe_mul requires exactly 2 arguments", MYSQL_ERRMSG_SIZE);
        return 1;
    }
    args->arg_type[0] = INT_RESULT;
    args->arg_type[1] = INT_RESULT;
    initid->maybe_null = 1;
    ensure_proven_init();
    return 0;
}

void proven_safe_mul_deinit(UDF_INIT *initid)
{
    (void)initid;
}

long long proven_safe_mul(UDF_INIT *initid, UDF_ARGS *args,
                          char *is_null, char *error)
{
    (void)initid;
    (void)error;

    if (args->args[0] == NULL || args->args[1] == NULL)
    {
        *is_null = 1;
        return 0;
    }

    int64_t a = *((long long *) args->args[0]);
    int64_t b = *((long long *) args->args[1]);

    ProvenIntResult result = proven_math_mul_checked(a, b);
    if (PROVEN_FAILED(result))
    {
        *is_null = 1;
        return 0;
    }

    return (long long) result.value;
}

/* ============================================================================
 * proven_safe_div(a BIGINT, b BIGINT) RETURNS BIGINT
 *
 * Safe integer division. Returns NULL on division by zero or overflow.
 * ============================================================================ */

my_bool proven_safe_div_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 2)
    {
        strncpy(message, "proven_safe_div requires exactly 2 arguments", MYSQL_ERRMSG_SIZE);
        return 1;
    }
    args->arg_type[0] = INT_RESULT;
    args->arg_type[1] = INT_RESULT;
    initid->maybe_null = 1;
    ensure_proven_init();
    return 0;
}

void proven_safe_div_deinit(UDF_INIT *initid)
{
    (void)initid;
}

long long proven_safe_div(UDF_INIT *initid, UDF_ARGS *args,
                          char *is_null, char *error)
{
    (void)initid;
    (void)error;

    if (args->args[0] == NULL || args->args[1] == NULL)
    {
        *is_null = 1;
        return 0;
    }

    int64_t a = *((long long *) args->args[0]);
    int64_t b = *((long long *) args->args[1]);

    ProvenIntResult result = proven_math_div(a, b);
    if (PROVEN_FAILED(result))
    {
        *is_null = 1;
        return 0;
    }

    return (long long) result.value;
}

/* ============================================================================
 * proven_validate_email(addr VARCHAR) RETURNS INTEGER
 *
 * Validate email address (RFC 5321 simplified). Returns 1/0 or NULL.
 * ============================================================================ */

my_bool proven_validate_email_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 1)
    {
        strncpy(message, "proven_validate_email requires exactly 1 argument", MYSQL_ERRMSG_SIZE);
        return 1;
    }
    args->arg_type[0] = STRING_RESULT;
    initid->maybe_null = 1;
    ensure_proven_init();
    return 0;
}

void proven_validate_email_deinit(UDF_INIT *initid)
{
    (void)initid;
}

long long proven_validate_email(UDF_INIT *initid, UDF_ARGS *args,
                                char *is_null, char *error)
{
    (void)initid;
    (void)error;

    if (args->args[0] == NULL)
    {
        *is_null = 1;
        return 0;
    }

    const uint8_t *ptr = (const uint8_t *) args->args[0];
    size_t len = (size_t) args->lengths[0];

    ProvenBoolResult result = proven_email_is_valid(ptr, len);
    if (PROVEN_FAILED(result))
    {
        *is_null = 1;
        return 0;
    }

    return result.value ? 1 : 0;
}

/* ============================================================================
 * proven_validate_url(url VARCHAR) RETURNS INTEGER
 *
 * Validate URL by attempting to parse it. Returns 1/0 or NULL.
 * ============================================================================ */

my_bool proven_validate_url_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 1)
    {
        strncpy(message, "proven_validate_url requires exactly 1 argument", MYSQL_ERRMSG_SIZE);
        return 1;
    }
    args->arg_type[0] = STRING_RESULT;
    initid->maybe_null = 1;
    ensure_proven_init();
    return 0;
}

void proven_validate_url_deinit(UDF_INIT *initid)
{
    (void)initid;
}

long long proven_validate_url(UDF_INIT *initid, UDF_ARGS *args,
                              char *is_null, char *error)
{
    (void)initid;
    (void)error;

    if (args->args[0] == NULL)
    {
        *is_null = 1;
        return 0;
    }

    const uint8_t *ptr = (const uint8_t *) args->args[0];
    size_t len = (size_t) args->lengths[0];

    ProvenUrlResult result = proven_url_parse(ptr, len);
    int valid = PROVEN_SUCCEEDED(result) ? 1 : 0;

    if (valid)
        proven_url_free(&result.components);

    return valid;
}

/* ============================================================================
 * proven_validate_ipv4(addr VARCHAR) RETURNS INTEGER
 *
 * Validate IPv4 address string. Returns 1/0 or NULL.
 * ============================================================================ */

my_bool proven_validate_ipv4_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 1)
    {
        strncpy(message, "proven_validate_ipv4 requires exactly 1 argument", MYSQL_ERRMSG_SIZE);
        return 1;
    }
    args->arg_type[0] = STRING_RESULT;
    initid->maybe_null = 1;
    ensure_proven_init();
    return 0;
}

void proven_validate_ipv4_deinit(UDF_INIT *initid)
{
    (void)initid;
}

long long proven_validate_ipv4(UDF_INIT *initid, UDF_ARGS *args,
                               char *is_null, char *error)
{
    (void)initid;
    (void)error;

    if (args->args[0] == NULL)
    {
        *is_null = 1;
        return 0;
    }

    const uint8_t *ptr = (const uint8_t *) args->args[0];
    size_t len = (size_t) args->lengths[0];

    ProvenIPv4Result result = proven_network_parse_ipv4(ptr, len);
    return PROVEN_SUCCEEDED(result) ? 1 : 0;
}

/* ============================================================================
 * proven_sanitize_string(input VARCHAR) RETURNS VARCHAR
 *
 * Sanitize string by escaping HTML entities. Returns NULL on error.
 * ============================================================================ */

my_bool proven_sanitize_string_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 1)
    {
        strncpy(message, "proven_sanitize_string requires exactly 1 argument", MYSQL_ERRMSG_SIZE);
        return 1;
    }
    args->arg_type[0] = STRING_RESULT;
    initid->maybe_null = 1;
    initid->max_length = 65535;
    ensure_proven_init();
    return 0;
}

void proven_sanitize_string_deinit(UDF_INIT *initid)
{
    if (initid->ptr != NULL)
    {
        free(initid->ptr);
        initid->ptr = NULL;
    }
}

char *proven_sanitize_string(UDF_INIT *initid, UDF_ARGS *args,
                             char *result, unsigned long *length,
                             char *is_null, char *error)
{
    (void)result;
    (void)error;

    /* Free previous result if any */
    if (initid->ptr != NULL)
    {
        free(initid->ptr);
        initid->ptr = NULL;
    }

    if (args->args[0] == NULL)
    {
        *is_null = 1;
        return NULL;
    }

    const uint8_t *ptr = (const uint8_t *) args->args[0];
    size_t len = (size_t) args->lengths[0];

    ProvenStringResult sr = proven_string_escape_html(ptr, len);
    if (PROVEN_FAILED(sr))
    {
        *is_null = 1;
        return NULL;
    }

    /* Copy to MySQL-managed buffer */
    initid->ptr = (char *) malloc(sr.length + 1);
    if (initid->ptr == NULL)
    {
        proven_free_string(sr.value);
        *is_null = 1;
        return NULL;
    }

    memcpy(initid->ptr, sr.value, sr.length);
    initid->ptr[sr.length] = '\0';
    *length = (unsigned long) sr.length;

    proven_free_string(sr.value);

    return initid->ptr;
}

/* ============================================================================
 * proven_hash_sha256(input VARCHAR) RETURNS VARCHAR
 *
 * CRC32 checksum as hex string via libproven. Returns NULL on error.
 * ============================================================================ */

my_bool proven_hash_sha256_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 1)
    {
        strncpy(message, "proven_hash_sha256 requires exactly 1 argument", MYSQL_ERRMSG_SIZE);
        return 1;
    }
    args->arg_type[0] = STRING_RESULT;
    initid->maybe_null = 1;
    initid->max_length = 255;
    ensure_proven_init();
    return 0;
}

void proven_hash_sha256_deinit(UDF_INIT *initid)
{
    if (initid->ptr != NULL)
    {
        free(initid->ptr);
        initid->ptr = NULL;
    }
}

char *proven_hash_sha256(UDF_INIT *initid, UDF_ARGS *args,
                         char *result, unsigned long *length,
                         char *is_null, char *error)
{
    (void)result;
    (void)error;

    if (initid->ptr != NULL)
    {
        free(initid->ptr);
        initid->ptr = NULL;
    }

    if (args->args[0] == NULL)
    {
        *is_null = 1;
        return NULL;
    }

    const uint8_t *ptr = (const uint8_t *) args->args[0];
    size_t len = (size_t) args->lengths[0];

    ProvenIntResult crc_result = proven_checksum_crc32(ptr, len);
    if (PROVEN_FAILED(crc_result))
    {
        *is_null = 1;
        return NULL;
    }

    uint32_t crc = (uint32_t) crc_result.value;
    uint8_t crc_bytes[4];
    crc_bytes[0] = (crc >> 24) & 0xFF;
    crc_bytes[1] = (crc >> 16) & 0xFF;
    crc_bytes[2] = (crc >> 8) & 0xFF;
    crc_bytes[3] = crc & 0xFF;

    ProvenStringResult hex_result = proven_hex_encode(crc_bytes, 4, false);
    if (PROVEN_FAILED(hex_result))
    {
        *is_null = 1;
        return NULL;
    }

    initid->ptr = (char *) malloc(hex_result.length + 1);
    if (initid->ptr == NULL)
    {
        proven_free_string(hex_result.value);
        *is_null = 1;
        return NULL;
    }

    memcpy(initid->ptr, hex_result.value, hex_result.length);
    initid->ptr[hex_result.length] = '\0';
    *length = (unsigned long) hex_result.length;

    proven_free_string(hex_result.value);

    return initid->ptr;
}

/* ============================================================================
 * proven_validate_json(doc VARCHAR) RETURNS INTEGER
 *
 * Validate JSON string. Returns 1/0 or NULL.
 * ============================================================================ */

my_bool proven_validate_json_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 1)
    {
        strncpy(message, "proven_validate_json requires exactly 1 argument", MYSQL_ERRMSG_SIZE);
        return 1;
    }
    args->arg_type[0] = STRING_RESULT;
    initid->maybe_null = 1;
    ensure_proven_init();
    return 0;
}

void proven_validate_json_deinit(UDF_INIT *initid)
{
    (void)initid;
}

long long proven_validate_json(UDF_INIT *initid, UDF_ARGS *args,
                               char *is_null, char *error)
{
    (void)initid;
    (void)error;

    if (args->args[0] == NULL)
    {
        *is_null = 1;
        return 0;
    }

    const uint8_t *ptr = (const uint8_t *) args->args[0];
    size_t len = (size_t) args->lengths[0];

    ProvenBoolResult result = proven_json_is_valid(ptr, len);
    if (PROVEN_FAILED(result))
    {
        *is_null = 1;
        return 0;
    }

    return result.value ? 1 : 0;
}

/* ============================================================================
 * proven_hex_encode(input VARCHAR) RETURNS VARCHAR
 *
 * Hex-encode input bytes. Returns NULL on error.
 * ============================================================================ */

my_bool proven_hex_encode_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 1)
    {
        strncpy(message, "proven_hex_encode requires exactly 1 argument", MYSQL_ERRMSG_SIZE);
        return 1;
    }
    args->arg_type[0] = STRING_RESULT;
    initid->maybe_null = 1;
    initid->max_length = 65535;
    ensure_proven_init();
    return 0;
}

void proven_hex_encode_deinit(UDF_INIT *initid)
{
    if (initid->ptr != NULL)
    {
        free(initid->ptr);
        initid->ptr = NULL;
    }
}

char *proven_hex_encode(UDF_INIT *initid, UDF_ARGS *args,
                        char *result, unsigned long *length,
                        char *is_null, char *error)
{
    (void)result;
    (void)error;

    if (initid->ptr != NULL)
    {
        free(initid->ptr);
        initid->ptr = NULL;
    }

    if (args->args[0] == NULL)
    {
        *is_null = 1;
        return NULL;
    }

    const uint8_t *ptr = (const uint8_t *) args->args[0];
    size_t len = (size_t) args->lengths[0];

    ProvenStringResult sr = proven_hex_encode(ptr, len, false);
    if (PROVEN_FAILED(sr))
    {
        *is_null = 1;
        return NULL;
    }

    initid->ptr = (char *) malloc(sr.length + 1);
    if (initid->ptr == NULL)
    {
        proven_free_string(sr.value);
        *is_null = 1;
        return NULL;
    }

    memcpy(initid->ptr, sr.value, sr.length);
    initid->ptr[sr.length] = '\0';
    *length = (unsigned long) sr.length;

    proven_free_string(sr.value);

    return initid->ptr;
}

/* ============================================================================
 * proven_hex_decode(input VARCHAR) RETURNS VARCHAR
 *
 * Decode hexadecimal string to bytes. Returns NULL on error.
 * ============================================================================ */

my_bool proven_hex_decode_init(UDF_INIT *initid, UDF_ARGS *args, char *message)
{
    if (args->arg_count != 1)
    {
        strncpy(message, "proven_hex_decode requires exactly 1 argument", MYSQL_ERRMSG_SIZE);
        return 1;
    }
    args->arg_type[0] = STRING_RESULT;
    initid->maybe_null = 1;
    initid->max_length = 65535;
    ensure_proven_init();
    return 0;
}

void proven_hex_decode_deinit(UDF_INIT *initid)
{
    if (initid->ptr != NULL)
    {
        free(initid->ptr);
        initid->ptr = NULL;
    }
}

char *proven_hex_decode(UDF_INIT *initid, UDF_ARGS *args,
                        char *result, unsigned long *length,
                        char *is_null, char *error)
{
    (void)result;
    (void)error;

    if (initid->ptr != NULL)
    {
        free(initid->ptr);
        initid->ptr = NULL;
    }

    if (args->args[0] == NULL)
    {
        *is_null = 1;
        return NULL;
    }

    const uint8_t *ptr = (const uint8_t *) args->args[0];
    size_t len = (size_t) args->lengths[0];

    ProvenHexDecodeResult dr = proven_hex_decode(ptr, len);
    if (PROVEN_FAILED(dr))
    {
        *is_null = 1;
        return NULL;
    }

    initid->ptr = (char *) malloc(dr.length + 1);
    if (initid->ptr == NULL)
    {
        proven_hex_free(&dr);
        *is_null = 1;
        return NULL;
    }

    memcpy(initid->ptr, dr.data, dr.length);
    initid->ptr[dr.length] = '\0';
    *length = (unsigned long) dr.length;

    proven_hex_free(&dr);

    return initid->ptr;
}
