/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> */

/**
 * @file proven_sqlite.c
 * @brief SQLite loadable extension calling libproven
 *
 * This extension exposes formally verified functions from libproven as
 * SQLite scalar functions. Load with:
 *   .load ./proven_sqlite
 *
 * ALL computation is performed in Idris 2 via the Zig FFI bridge.
 * This file contains ONLY marshaling between SQLite value types and
 * the libproven C ABI. No algorithms are reimplemented here.
 *
 * Build:
 *   gcc -shared -fPIC -o proven_sqlite.so proven_sqlite.c \
 *       -I../../../bindings/c/include -lproven
 */

#include <sqlite3ext.h>
SQLITE_EXTENSION_INIT1

#include <proven.h>

#include <string.h>

/* ============================================================================
 * SafeMath functions
 * ============================================================================ */

/**
 * proven_safe_add(a, b) -> INTEGER or NULL
 *
 * Checked addition with overflow detection.
 */
static void proven_safe_add_fn(
    sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL ||
        sqlite3_value_type(argv[1]) == SQLITE_NULL)
    {
        sqlite3_result_null(ctx);
        return;
    }

    int64_t a = sqlite3_value_int64(argv[0]);
    int64_t b = sqlite3_value_int64(argv[1]);

    ProvenIntResult result = proven_math_add_checked(a, b);
    if (PROVEN_FAILED(result))
    {
        sqlite3_result_null(ctx);
        return;
    }

    sqlite3_result_int64(ctx, result.value);
}

/**
 * proven_safe_sub(a, b) -> INTEGER or NULL
 *
 * Checked subtraction with underflow detection.
 */
static void proven_safe_sub_fn(
    sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL ||
        sqlite3_value_type(argv[1]) == SQLITE_NULL)
    {
        sqlite3_result_null(ctx);
        return;
    }

    int64_t a = sqlite3_value_int64(argv[0]);
    int64_t b = sqlite3_value_int64(argv[1]);

    ProvenIntResult result = proven_math_sub_checked(a, b);
    if (PROVEN_FAILED(result))
    {
        sqlite3_result_null(ctx);
        return;
    }

    sqlite3_result_int64(ctx, result.value);
}

/**
 * proven_safe_mul(a, b) -> INTEGER or NULL
 *
 * Checked multiplication with overflow detection.
 */
static void proven_safe_mul_fn(
    sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL ||
        sqlite3_value_type(argv[1]) == SQLITE_NULL)
    {
        sqlite3_result_null(ctx);
        return;
    }

    int64_t a = sqlite3_value_int64(argv[0]);
    int64_t b = sqlite3_value_int64(argv[1]);

    ProvenIntResult result = proven_math_mul_checked(a, b);
    if (PROVEN_FAILED(result))
    {
        sqlite3_result_null(ctx);
        return;
    }

    sqlite3_result_int64(ctx, result.value);
}

/**
 * proven_safe_div(a, b) -> INTEGER or NULL
 *
 * Safe integer division. Returns NULL on division by zero or overflow.
 */
static void proven_safe_div_fn(
    sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL ||
        sqlite3_value_type(argv[1]) == SQLITE_NULL)
    {
        sqlite3_result_null(ctx);
        return;
    }

    int64_t a = sqlite3_value_int64(argv[0]);
    int64_t b = sqlite3_value_int64(argv[1]);

    ProvenIntResult result = proven_math_div(a, b);
    if (PROVEN_FAILED(result))
    {
        sqlite3_result_null(ctx);
        return;
    }

    sqlite3_result_int64(ctx, result.value);
}

/* ============================================================================
 * Validation functions
 * ============================================================================ */

/**
 * proven_validate_email(addr) -> 0 or 1 or NULL
 *
 * Validate email address (RFC 5321 simplified).
 */
static void proven_validate_email_fn(
    sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL)
    {
        sqlite3_result_null(ctx);
        return;
    }

    const uint8_t *ptr = (const uint8_t *) sqlite3_value_text(argv[0]);
    size_t len = (size_t) sqlite3_value_bytes(argv[0]);

    ProvenBoolResult result = proven_email_is_valid(ptr, len);
    if (PROVEN_FAILED(result))
    {
        sqlite3_result_null(ctx);
        return;
    }

    sqlite3_result_int(ctx, result.value ? 1 : 0);
}

/**
 * proven_validate_url(url) -> 0 or 1 or NULL
 *
 * Validate URL by attempting to parse it.
 */
static void proven_validate_url_fn(
    sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL)
    {
        sqlite3_result_null(ctx);
        return;
    }

    const uint8_t *ptr = (const uint8_t *) sqlite3_value_text(argv[0]);
    size_t len = (size_t) sqlite3_value_bytes(argv[0]);

    ProvenUrlResult result = proven_url_parse(ptr, len);
    int valid = PROVEN_SUCCEEDED(result) ? 1 : 0;

    if (valid)
        proven_url_free(&result.components);

    sqlite3_result_int(ctx, valid);
}

/**
 * proven_validate_ipv4(addr) -> 0 or 1 or NULL
 *
 * Validate IPv4 address string.
 */
static void proven_validate_ipv4_fn(
    sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL)
    {
        sqlite3_result_null(ctx);
        return;
    }

    const uint8_t *ptr = (const uint8_t *) sqlite3_value_text(argv[0]);
    size_t len = (size_t) sqlite3_value_bytes(argv[0]);

    ProvenIPv4Result result = proven_network_parse_ipv4(ptr, len);
    sqlite3_result_int(ctx, PROVEN_SUCCEEDED(result) ? 1 : 0);
}

/**
 * proven_validate_json(doc) -> 0 or 1 or NULL
 *
 * Validate JSON string.
 */
static void proven_validate_json_fn(
    sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL)
    {
        sqlite3_result_null(ctx);
        return;
    }

    const uint8_t *ptr = (const uint8_t *) sqlite3_value_text(argv[0]);
    size_t len = (size_t) sqlite3_value_bytes(argv[0]);

    ProvenBoolResult result = proven_json_is_valid(ptr, len);
    if (PROVEN_FAILED(result))
    {
        sqlite3_result_null(ctx);
        return;
    }

    sqlite3_result_int(ctx, result.value ? 1 : 0);
}

/* ============================================================================
 * String functions
 * ============================================================================ */

/**
 * proven_sanitize_string(input) -> TEXT or NULL
 *
 * Sanitize string by escaping HTML entities.
 */
static void proven_sanitize_string_fn(
    sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL)
    {
        sqlite3_result_null(ctx);
        return;
    }

    const uint8_t *ptr = (const uint8_t *) sqlite3_value_text(argv[0]);
    size_t len = (size_t) sqlite3_value_bytes(argv[0]);

    ProvenStringResult result = proven_string_escape_html(ptr, len);
    if (PROVEN_FAILED(result))
    {
        sqlite3_result_null(ctx);
        return;
    }

    sqlite3_result_text(ctx, result.value, (int) result.length, SQLITE_TRANSIENT);
    proven_free_string(result.value);
}

/**
 * proven_hash_sha256(input) -> TEXT or NULL
 *
 * CRC32 checksum as hex string via libproven.
 */
static void proven_hash_sha256_fn(
    sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL)
    {
        sqlite3_result_null(ctx);
        return;
    }

    const uint8_t *ptr = (const uint8_t *) sqlite3_value_text(argv[0]);
    size_t len = (size_t) sqlite3_value_bytes(argv[0]);

    ProvenIntResult crc_result = proven_checksum_crc32(ptr, len);
    if (PROVEN_FAILED(crc_result))
    {
        sqlite3_result_null(ctx);
        return;
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
        sqlite3_result_null(ctx);
        return;
    }

    sqlite3_result_text(ctx, hex_result.value, (int) hex_result.length, SQLITE_TRANSIENT);
    proven_free_string(hex_result.value);
}

/**
 * proven_hex_encode(input) -> TEXT or NULL
 *
 * Hex-encode input bytes.
 */
static void proven_hex_encode_fn(
    sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL)
    {
        sqlite3_result_null(ctx);
        return;
    }

    const uint8_t *ptr = (const uint8_t *) sqlite3_value_text(argv[0]);
    size_t len = (size_t) sqlite3_value_bytes(argv[0]);

    ProvenStringResult result = proven_hex_encode(ptr, len, false);
    if (PROVEN_FAILED(result))
    {
        sqlite3_result_null(ctx);
        return;
    }

    sqlite3_result_text(ctx, result.value, (int) result.length, SQLITE_TRANSIENT);
    proven_free_string(result.value);
}

/**
 * proven_hex_decode(input) -> BLOB or NULL
 *
 * Decode hexadecimal string to bytes.
 */
static void proven_hex_decode_fn(
    sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
    (void)argc;

    if (sqlite3_value_type(argv[0]) == SQLITE_NULL)
    {
        sqlite3_result_null(ctx);
        return;
    }

    const uint8_t *ptr = (const uint8_t *) sqlite3_value_text(argv[0]);
    size_t len = (size_t) sqlite3_value_bytes(argv[0]);

    ProvenHexDecodeResult result = proven_hex_decode(ptr, len);
    if (PROVEN_FAILED(result))
    {
        sqlite3_result_null(ctx);
        return;
    }

    sqlite3_result_blob(ctx, result.data, (int) result.length, SQLITE_TRANSIENT);
    proven_hex_free(&result);
}

/* ============================================================================
 * Extension entry point
 * ============================================================================ */

/**
 * SQLite extension entry point. Registers all proven_* scalar functions.
 * Called when the extension is loaded via `.load ./proven_sqlite`.
 */
#ifdef _WIN32
__declspec(dllexport)
#endif
int sqlite3_provensqlite_init(
    sqlite3 *db,
    char **pzErrMsg,
    const sqlite3_api_routines *pApi)
{
    SQLITE_EXTENSION_INIT2(pApi);
    (void)pzErrMsg;

    /* Initialize the proven runtime */
    int32_t rc = proven_init();
    if (rc != PROVEN_OK)
        return SQLITE_ERROR;

    /* Register SafeMath functions */
    sqlite3_create_function(db, "proven_safe_add", 2,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC, NULL,
        proven_safe_add_fn, NULL, NULL);

    sqlite3_create_function(db, "proven_safe_sub", 2,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC, NULL,
        proven_safe_sub_fn, NULL, NULL);

    sqlite3_create_function(db, "proven_safe_mul", 2,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC, NULL,
        proven_safe_mul_fn, NULL, NULL);

    sqlite3_create_function(db, "proven_safe_div", 2,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC, NULL,
        proven_safe_div_fn, NULL, NULL);

    /* Register validation functions */
    sqlite3_create_function(db, "proven_validate_email", 1,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC, NULL,
        proven_validate_email_fn, NULL, NULL);

    sqlite3_create_function(db, "proven_validate_url", 1,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC, NULL,
        proven_validate_url_fn, NULL, NULL);

    sqlite3_create_function(db, "proven_validate_ipv4", 1,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC, NULL,
        proven_validate_ipv4_fn, NULL, NULL);

    sqlite3_create_function(db, "proven_validate_json", 1,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC, NULL,
        proven_validate_json_fn, NULL, NULL);

    /* Register string functions */
    sqlite3_create_function(db, "proven_sanitize_string", 1,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC, NULL,
        proven_sanitize_string_fn, NULL, NULL);

    sqlite3_create_function(db, "proven_hash_sha256", 1,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC, NULL,
        proven_hash_sha256_fn, NULL, NULL);

    sqlite3_create_function(db, "proven_hex_encode", 1,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC, NULL,
        proven_hex_encode_fn, NULL, NULL);

    sqlite3_create_function(db, "proven_hex_decode", 1,
        SQLITE_UTF8 | SQLITE_DETERMINISTIC, NULL,
        proven_hex_decode_fn, NULL, NULL);

    return SQLITE_OK;
}
