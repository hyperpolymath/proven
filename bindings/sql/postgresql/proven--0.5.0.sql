-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven--0.5.0.sql - PostgreSQL extension SQL script
--
-- Creates all proven UDFs. These call the C extension (proven_extension.c)
-- which in turn calls libproven (Zig FFI -> Idris 2 verified core).
--
-- Usage:
--   CREATE EXTENSION proven;
--
-- All functions return NULL on error (overflow, invalid input, etc.)
-- rather than raising exceptions.

-- ============================================================================
-- SafeMath: Checked arithmetic
-- ============================================================================

-- Checked addition with overflow detection. Returns NULL on overflow.
CREATE OR REPLACE FUNCTION proven_safe_add(a BIGINT, b BIGINT)
RETURNS BIGINT
AS 'MODULE_PATHNAME', 'proven_safe_add'
LANGUAGE C IMMUTABLE STRICT PARALLEL SAFE;

COMMENT ON FUNCTION proven_safe_add(BIGINT, BIGINT) IS
    'Checked addition via libproven. Returns NULL on integer overflow.';

-- Checked subtraction with underflow detection. Returns NULL on underflow.
CREATE OR REPLACE FUNCTION proven_safe_sub(a BIGINT, b BIGINT)
RETURNS BIGINT
AS 'MODULE_PATHNAME', 'proven_safe_sub'
LANGUAGE C IMMUTABLE STRICT PARALLEL SAFE;

COMMENT ON FUNCTION proven_safe_sub(BIGINT, BIGINT) IS
    'Checked subtraction via libproven. Returns NULL on integer underflow.';

-- Checked multiplication with overflow detection. Returns NULL on overflow.
CREATE OR REPLACE FUNCTION proven_safe_mul(a BIGINT, b BIGINT)
RETURNS BIGINT
AS 'MODULE_PATHNAME', 'proven_safe_mul'
LANGUAGE C IMMUTABLE STRICT PARALLEL SAFE;

COMMENT ON FUNCTION proven_safe_mul(BIGINT, BIGINT) IS
    'Checked multiplication via libproven. Returns NULL on integer overflow.';

-- Safe division. Returns NULL on division by zero or INT64_MIN / -1.
CREATE OR REPLACE FUNCTION proven_safe_div(a BIGINT, b BIGINT)
RETURNS BIGINT
AS 'MODULE_PATHNAME', 'proven_safe_div'
LANGUAGE C IMMUTABLE STRICT PARALLEL SAFE;

COMMENT ON FUNCTION proven_safe_div(BIGINT, BIGINT) IS
    'Safe integer division via libproven. Returns NULL on division by zero.';

-- ============================================================================
-- Validation: Email, URL, IPv4, JSON
-- ============================================================================

-- Validate email address (RFC 5321 simplified).
CREATE OR REPLACE FUNCTION proven_validate_email(addr TEXT)
RETURNS BOOLEAN
AS 'MODULE_PATHNAME', 'proven_validate_email'
LANGUAGE C IMMUTABLE STRICT PARALLEL SAFE;

COMMENT ON FUNCTION proven_validate_email(TEXT) IS
    'Validate email address via libproven (RFC 5321 simplified). Returns NULL on error.';

-- Validate URL by attempting to parse it.
CREATE OR REPLACE FUNCTION proven_validate_url(url TEXT)
RETURNS BOOLEAN
AS 'MODULE_PATHNAME', 'proven_validate_url'
LANGUAGE C IMMUTABLE STRICT PARALLEL SAFE;

COMMENT ON FUNCTION proven_validate_url(TEXT) IS
    'Validate URL via libproven URL parser. Returns TRUE if parseable, FALSE otherwise.';

-- Validate IPv4 address string.
CREATE OR REPLACE FUNCTION proven_validate_ipv4(addr TEXT)
RETURNS BOOLEAN
AS 'MODULE_PATHNAME', 'proven_validate_ipv4'
LANGUAGE C IMMUTABLE STRICT PARALLEL SAFE;

COMMENT ON FUNCTION proven_validate_ipv4(TEXT) IS
    'Validate IPv4 address string via libproven. Returns TRUE if valid.';

-- Validate JSON document.
CREATE OR REPLACE FUNCTION proven_validate_json(doc TEXT)
RETURNS BOOLEAN
AS 'MODULE_PATHNAME', 'proven_validate_json'
LANGUAGE C IMMUTABLE STRICT PARALLEL SAFE;

COMMENT ON FUNCTION proven_validate_json(TEXT) IS
    'Validate JSON string via libproven. Returns TRUE if valid JSON.';

-- ============================================================================
-- String operations: Sanitize, Hash, Hex encode/decode
-- ============================================================================

-- Sanitize string by escaping HTML entities (XSS prevention).
CREATE OR REPLACE FUNCTION proven_sanitize_string(input TEXT)
RETURNS TEXT
AS 'MODULE_PATHNAME', 'proven_sanitize_string'
LANGUAGE C IMMUTABLE STRICT PARALLEL SAFE;

COMMENT ON FUNCTION proven_sanitize_string(TEXT) IS
    'Escape HTML entities via libproven (XSS prevention). Returns NULL on error.';

-- Compute hash of input (CRC32 checksum as hex). For real SHA-256 use pgcrypto.
CREATE OR REPLACE FUNCTION proven_hash_sha256(input TEXT)
RETURNS TEXT
AS 'MODULE_PATHNAME', 'proven_hash_sha256'
LANGUAGE C IMMUTABLE STRICT PARALLEL SAFE;

COMMENT ON FUNCTION proven_hash_sha256(TEXT) IS
    'CRC32 checksum as hex via libproven. For production SHA-256, use pgcrypto.';

-- Encode bytes as lowercase hexadecimal string.
CREATE OR REPLACE FUNCTION proven_hex_encode(input TEXT)
RETURNS TEXT
AS 'MODULE_PATHNAME', 'proven_hex_encode_fn'
LANGUAGE C IMMUTABLE STRICT PARALLEL SAFE;

COMMENT ON FUNCTION proven_hex_encode(TEXT) IS
    'Hex-encode input bytes via libproven. Returns NULL on error.';

-- Decode hexadecimal string to raw bytes.
CREATE OR REPLACE FUNCTION proven_hex_decode(input TEXT)
RETURNS BYTEA
AS 'MODULE_PATHNAME', 'proven_hex_decode_fn'
LANGUAGE C IMMUTABLE STRICT PARALLEL SAFE;

COMMENT ON FUNCTION proven_hex_decode(TEXT) IS
    'Hex-decode input string to bytes via libproven. Returns NULL on error.';
