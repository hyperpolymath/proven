/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> */

/**
 * @file proven_extension.c
 * @brief PostgreSQL C extension calling libproven
 *
 * This extension exposes formally verified functions from libproven as
 * PostgreSQL SQL-callable functions using the V1 calling convention.
 *
 * ALL computation is performed in Idris 2 via the Zig FFI bridge.
 * This file contains ONLY marshaling between PostgreSQL Datum types
 * and the libproven C ABI. No algorithms are reimplemented here.
 *
 * Build:
 *   gcc -shared -fPIC -o proven.so proven_extension.c \
 *       $(pg_config --includedir-server) -I../../../bindings/c/include \
 *       -lproven
 */

#include "postgres.h"
#include "fmgr.h"
#include "funcapi.h"
#include "utils/builtins.h"

#include <proven.h>

#include <string.h>

PG_MODULE_MAGIC;

/* ============================================================================
 * Lifecycle hooks
 * ============================================================================ */

/**
 * Module initialization: called when the shared library is loaded.
 * Initializes the Proven/Idris2 runtime.
 */
void _PG_init(void)
{
    int32_t rc = proven_init();
    if (rc != PROVEN_OK)
    {
        elog(WARNING, "proven: failed to initialize runtime (status=%d)", rc);
    }
}

/**
 * Module cleanup: called when the shared library is unloaded.
 */
void _PG_fini(void)
{
    proven_deinit();
}

/* ============================================================================
 * SafeMath functions
 * ============================================================================ */

PG_FUNCTION_INFO_V1(proven_safe_add);
/**
 * proven_safe_add(a BIGINT, b BIGINT) RETURNS BIGINT
 *
 * Checked addition with overflow detection. Returns NULL on overflow.
 */
Datum
proven_safe_add(PG_FUNCTION_ARGS)
{
    if (PG_ARGISNULL(0) || PG_ARGISNULL(1))
        PG_RETURN_NULL();

    int64 a = PG_GETARG_INT64(0);
    int64 b = PG_GETARG_INT64(1);

    ProvenIntResult result = proven_math_add_checked(a, b);
    if (PROVEN_FAILED(result))
        PG_RETURN_NULL();

    PG_RETURN_INT64(result.value);
}

PG_FUNCTION_INFO_V1(proven_safe_sub);
/**
 * proven_safe_sub(a BIGINT, b BIGINT) RETURNS BIGINT
 *
 * Checked subtraction with underflow detection. Returns NULL on underflow.
 */
Datum
proven_safe_sub(PG_FUNCTION_ARGS)
{
    if (PG_ARGISNULL(0) || PG_ARGISNULL(1))
        PG_RETURN_NULL();

    int64 a = PG_GETARG_INT64(0);
    int64 b = PG_GETARG_INT64(1);

    ProvenIntResult result = proven_math_sub_checked(a, b);
    if (PROVEN_FAILED(result))
        PG_RETURN_NULL();

    PG_RETURN_INT64(result.value);
}

PG_FUNCTION_INFO_V1(proven_safe_mul);
/**
 * proven_safe_mul(a BIGINT, b BIGINT) RETURNS BIGINT
 *
 * Checked multiplication with overflow detection. Returns NULL on overflow.
 */
Datum
proven_safe_mul(PG_FUNCTION_ARGS)
{
    if (PG_ARGISNULL(0) || PG_ARGISNULL(1))
        PG_RETURN_NULL();

    int64 a = PG_GETARG_INT64(0);
    int64 b = PG_GETARG_INT64(1);

    ProvenIntResult result = proven_math_mul_checked(a, b);
    if (PROVEN_FAILED(result))
        PG_RETURN_NULL();

    PG_RETURN_INT64(result.value);
}

PG_FUNCTION_INFO_V1(proven_safe_div);
/**
 * proven_safe_div(a BIGINT, b BIGINT) RETURNS BIGINT
 *
 * Safe integer division. Returns NULL on division by zero or
 * INT64_MIN / -1 overflow.
 */
Datum
proven_safe_div(PG_FUNCTION_ARGS)
{
    if (PG_ARGISNULL(0) || PG_ARGISNULL(1))
        PG_RETURN_NULL();

    int64 a = PG_GETARG_INT64(0);
    int64 b = PG_GETARG_INT64(1);

    ProvenIntResult result = proven_math_div(a, b);
    if (PROVEN_FAILED(result))
        PG_RETURN_NULL();

    PG_RETURN_INT64(result.value);
}

/* ============================================================================
 * Validation functions
 * ============================================================================ */

PG_FUNCTION_INFO_V1(proven_validate_email);
/**
 * proven_validate_email(addr TEXT) RETURNS BOOLEAN
 *
 * Validate email address (RFC 5321 simplified). Returns NULL on error.
 */
Datum
proven_validate_email(PG_FUNCTION_ARGS)
{
    if (PG_ARGISNULL(0))
        PG_RETURN_NULL();

    text *input = PG_GETARG_TEXT_PP(0);
    const uint8_t *ptr = (const uint8_t *) VARDATA_ANY(input);
    size_t len = VARSIZE_ANY_EXHDR(input);

    ProvenBoolResult result = proven_email_is_valid(ptr, len);
    if (PROVEN_FAILED(result))
        PG_RETURN_NULL();

    PG_RETURN_BOOL(result.value);
}

PG_FUNCTION_INFO_V1(proven_validate_url);
/**
 * proven_validate_url(url TEXT) RETURNS BOOLEAN
 *
 * Validate URL by attempting to parse it. Returns TRUE if parseable,
 * FALSE otherwise. Returns NULL on internal error.
 */
Datum
proven_validate_url(PG_FUNCTION_ARGS)
{
    if (PG_ARGISNULL(0))
        PG_RETURN_NULL();

    text *input = PG_GETARG_TEXT_PP(0);
    const uint8_t *ptr = (const uint8_t *) VARDATA_ANY(input);
    size_t len = VARSIZE_ANY_EXHDR(input);

    ProvenUrlResult result = proven_url_parse(ptr, len);
    bool valid = PROVEN_SUCCEEDED(result);

    /* Free the URL components regardless of success/failure */
    if (valid)
        proven_url_free(&result.components);

    PG_RETURN_BOOL(valid);
}

PG_FUNCTION_INFO_V1(proven_validate_ipv4);
/**
 * proven_validate_ipv4(addr TEXT) RETURNS BOOLEAN
 *
 * Validate IPv4 address string. Returns TRUE if valid, FALSE otherwise.
 * Returns NULL on internal error.
 */
Datum
proven_validate_ipv4(PG_FUNCTION_ARGS)
{
    if (PG_ARGISNULL(0))
        PG_RETURN_NULL();

    text *input = PG_GETARG_TEXT_PP(0);
    const uint8_t *ptr = (const uint8_t *) VARDATA_ANY(input);
    size_t len = VARSIZE_ANY_EXHDR(input);

    ProvenIPv4Result result = proven_network_parse_ipv4(ptr, len);
    PG_RETURN_BOOL(PROVEN_SUCCEEDED(result));
}

PG_FUNCTION_INFO_V1(proven_validate_json);
/**
 * proven_validate_json(doc TEXT) RETURNS BOOLEAN
 *
 * Validate JSON string. Returns TRUE if valid, FALSE otherwise.
 * Returns NULL on internal error.
 */
Datum
proven_validate_json(PG_FUNCTION_ARGS)
{
    if (PG_ARGISNULL(0))
        PG_RETURN_NULL();

    text *input = PG_GETARG_TEXT_PP(0);
    const uint8_t *ptr = (const uint8_t *) VARDATA_ANY(input);
    size_t len = VARSIZE_ANY_EXHDR(input);

    ProvenBoolResult result = proven_json_is_valid(ptr, len);
    if (PROVEN_FAILED(result))
        PG_RETURN_NULL();

    PG_RETURN_BOOL(result.value);
}

/* ============================================================================
 * String functions
 * ============================================================================ */

PG_FUNCTION_INFO_V1(proven_sanitize_string);
/**
 * proven_sanitize_string(input TEXT) RETURNS TEXT
 *
 * Sanitize string by escaping HTML entities (prevents XSS).
 * Returns NULL on error.
 */
Datum
proven_sanitize_string(PG_FUNCTION_ARGS)
{
    if (PG_ARGISNULL(0))
        PG_RETURN_NULL();

    text *input = PG_GETARG_TEXT_PP(0);
    const uint8_t *ptr = (const uint8_t *) VARDATA_ANY(input);
    size_t len = VARSIZE_ANY_EXHDR(input);

    ProvenStringResult result = proven_string_escape_html(ptr, len);
    if (PROVEN_FAILED(result))
        PG_RETURN_NULL();

    /* Copy the escaped string into PostgreSQL-managed memory */
    text *output = cstring_to_text_with_len(result.value, (int) result.length);
    proven_free_string(result.value);

    PG_RETURN_TEXT_P(output);
}

PG_FUNCTION_INFO_V1(proven_hash_sha256);
/**
 * proven_hash_sha256(input TEXT) RETURNS TEXT
 *
 * Compute a hash of the input string. Uses CRC32 checksum from libproven
 * and returns it as a hex-encoded string. For production SHA-256, use pgcrypto.
 * Returns NULL on error.
 */
Datum
proven_hash_sha256(PG_FUNCTION_ARGS)
{
    if (PG_ARGISNULL(0))
        PG_RETURN_NULL();

    text *input = PG_GETARG_TEXT_PP(0);
    const uint8_t *ptr = (const uint8_t *) VARDATA_ANY(input);
    size_t len = VARSIZE_ANY_EXHDR(input);

    /* Compute CRC32 checksum via libproven */
    ProvenIntResult crc_result = proven_checksum_crc32(ptr, len);
    if (PROVEN_FAILED(crc_result))
        PG_RETURN_NULL();

    /* Encode the CRC32 value as hex bytes */
    uint32_t crc = (uint32_t) crc_result.value;
    uint8_t crc_bytes[4];
    crc_bytes[0] = (crc >> 24) & 0xFF;
    crc_bytes[1] = (crc >> 16) & 0xFF;
    crc_bytes[2] = (crc >> 8) & 0xFF;
    crc_bytes[3] = crc & 0xFF;

    ProvenStringResult hex_result = proven_hex_encode(crc_bytes, 4, false);
    if (PROVEN_FAILED(hex_result))
        PG_RETURN_NULL();

    text *output = cstring_to_text_with_len(hex_result.value, (int) hex_result.length);
    proven_free_string(hex_result.value);

    PG_RETURN_TEXT_P(output);
}

/* ============================================================================
 * Hex encoding/decoding
 * ============================================================================ */

PG_FUNCTION_INFO_V1(proven_hex_encode_fn);
/**
 * proven_hex_encode(input TEXT) RETURNS TEXT
 *
 * Encode input bytes as lowercase hexadecimal string. Returns NULL on error.
 */
Datum
proven_hex_encode_fn(PG_FUNCTION_ARGS)
{
    if (PG_ARGISNULL(0))
        PG_RETURN_NULL();

    text *input = PG_GETARG_TEXT_PP(0);
    const uint8_t *ptr = (const uint8_t *) VARDATA_ANY(input);
    size_t len = VARSIZE_ANY_EXHDR(input);

    ProvenStringResult result = proven_hex_encode(ptr, len, false);
    if (PROVEN_FAILED(result))
        PG_RETURN_NULL();

    text *output = cstring_to_text_with_len(result.value, (int) result.length);
    proven_free_string(result.value);

    PG_RETURN_TEXT_P(output);
}

PG_FUNCTION_INFO_V1(proven_hex_decode_fn);
/**
 * proven_hex_decode(input TEXT) RETURNS BYTEA
 *
 * Decode hexadecimal string to raw bytes. Returns NULL on error.
 */
Datum
proven_hex_decode_fn(PG_FUNCTION_ARGS)
{
    if (PG_ARGISNULL(0))
        PG_RETURN_NULL();

    text *input = PG_GETARG_TEXT_PP(0);
    const uint8_t *ptr = (const uint8_t *) VARDATA_ANY(input);
    size_t len = VARSIZE_ANY_EXHDR(input);

    ProvenHexDecodeResult result = proven_hex_decode(ptr, len);
    if (PROVEN_FAILED(result))
        PG_RETURN_NULL();

    /* Build a BYTEA from the decoded data */
    bytea *output = (bytea *) palloc(VARHDRSZ + result.length);
    SET_VARSIZE(output, VARHDRSZ + result.length);
    memcpy(VARDATA(output), result.data, result.length);

    proven_hex_free(&result);

    PG_RETURN_BYTEA_P(output);
}
