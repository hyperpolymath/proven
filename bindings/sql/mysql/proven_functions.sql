-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven_functions.sql - MySQL UDF registration and example usage
--
-- Prerequisite: proven_udf.so must be in the MySQL plugin directory.
--
-- All functions call libproven (Idris 2 verified core via Zig FFI bridge).
-- Functions return NULL on error rather than raising exceptions.

-- ============================================================================
-- Install UDFs (run as MySQL root user)
-- ============================================================================

-- SafeMath: Checked arithmetic
CREATE FUNCTION IF NOT EXISTS proven_safe_add
    RETURNS INTEGER SONAME 'proven_udf.so';

CREATE FUNCTION IF NOT EXISTS proven_safe_sub
    RETURNS INTEGER SONAME 'proven_udf.so';

CREATE FUNCTION IF NOT EXISTS proven_safe_mul
    RETURNS INTEGER SONAME 'proven_udf.so';

CREATE FUNCTION IF NOT EXISTS proven_safe_div
    RETURNS INTEGER SONAME 'proven_udf.so';

-- Validation
CREATE FUNCTION IF NOT EXISTS proven_validate_email
    RETURNS INTEGER SONAME 'proven_udf.so';

CREATE FUNCTION IF NOT EXISTS proven_validate_url
    RETURNS INTEGER SONAME 'proven_udf.so';

CREATE FUNCTION IF NOT EXISTS proven_validate_ipv4
    RETURNS INTEGER SONAME 'proven_udf.so';

CREATE FUNCTION IF NOT EXISTS proven_validate_json
    RETURNS INTEGER SONAME 'proven_udf.so';

-- String and encoding
CREATE FUNCTION IF NOT EXISTS proven_sanitize_string
    RETURNS STRING SONAME 'proven_udf.so';

CREATE FUNCTION IF NOT EXISTS proven_hash_sha256
    RETURNS STRING SONAME 'proven_udf.so';

CREATE FUNCTION IF NOT EXISTS proven_hex_encode
    RETURNS STRING SONAME 'proven_udf.so';

CREATE FUNCTION IF NOT EXISTS proven_hex_decode
    RETURNS STRING SONAME 'proven_udf.so';

-- ============================================================================
-- Example usage
-- ============================================================================

-- SafeMath: Checked arithmetic
SELECT proven_safe_add(100, 200);                    -- 300
SELECT proven_safe_add(9223372036854775807, 1);       -- NULL (overflow)
SELECT proven_safe_div(100, 0);                      -- NULL (division by zero)

-- Validation
SELECT proven_validate_email('user@example.com');     -- 1 (true)
SELECT proven_validate_email('invalid');               -- 0 (false)
SELECT proven_validate_url('https://example.com');    -- 1 (true)
SELECT proven_validate_ipv4('192.168.1.1');           -- 1 (true)
SELECT proven_validate_json('{"key": "value"}');      -- 1 (true)

-- String operations
SELECT proven_sanitize_string('<script>alert(1)</script>');
SELECT proven_hash_sha256('hello world');
SELECT proven_hex_encode('hello');
SELECT proven_hex_decode('68656c6c6f');

-- Practical query: validate and filter
SELECT id, email
FROM users
WHERE proven_validate_email(email) = 1;

-- ============================================================================
-- Uninstall UDFs (when no longer needed)
-- ============================================================================

-- DROP FUNCTION IF EXISTS proven_safe_add;
-- DROP FUNCTION IF EXISTS proven_safe_sub;
-- DROP FUNCTION IF EXISTS proven_safe_mul;
-- DROP FUNCTION IF EXISTS proven_safe_div;
-- DROP FUNCTION IF EXISTS proven_validate_email;
-- DROP FUNCTION IF EXISTS proven_validate_url;
-- DROP FUNCTION IF EXISTS proven_validate_ipv4;
-- DROP FUNCTION IF EXISTS proven_validate_json;
-- DROP FUNCTION IF EXISTS proven_sanitize_string;
-- DROP FUNCTION IF EXISTS proven_hash_sha256;
-- DROP FUNCTION IF EXISTS proven_hex_encode;
-- DROP FUNCTION IF EXISTS proven_hex_decode;
