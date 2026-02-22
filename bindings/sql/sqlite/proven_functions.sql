-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven_functions.sql - Example usage of proven SQLite loadable extension
--
-- Prerequisite: .load ./proven_sqlite
--
-- All functions call libproven (Idris 2 verified core via Zig FFI bridge).
-- Functions return NULL on error rather than raising exceptions.

-- Load the extension
.load ./proven_sqlite

-- ============================================================================
-- SafeMath: Checked arithmetic
-- ============================================================================

SELECT proven_safe_add(100, 200);                    -- 300
SELECT proven_safe_add(9223372036854775807, 1);       -- NULL (overflow)

SELECT proven_safe_sub(500, 200);                    -- 300
SELECT proven_safe_sub(-9223372036854775808, 1);      -- NULL (underflow)

SELECT proven_safe_mul(1000, 1000);                  -- 1000000
SELECT proven_safe_mul(9223372036854775807, 2);       -- NULL (overflow)

SELECT proven_safe_div(100, 3);                      -- 33
SELECT proven_safe_div(100, 0);                      -- NULL (division by zero)

-- ============================================================================
-- Validation
-- ============================================================================

SELECT proven_validate_email('user@example.com');     -- 1 (true)
SELECT proven_validate_email('not-an-email');          -- 0 (false)
SELECT proven_validate_email(NULL);                    -- NULL

SELECT proven_validate_url('https://example.com');    -- 1 (true)
SELECT proven_validate_url('not a url');               -- 0 (false)

SELECT proven_validate_ipv4('192.168.1.1');           -- 1 (true)
SELECT proven_validate_ipv4('999.999.999.999');       -- 0 (false)

SELECT proven_validate_json('{"key": "value"}');      -- 1 (true)
SELECT proven_validate_json('{bad}');                  -- 0 (false)

-- ============================================================================
-- String and encoding
-- ============================================================================

SELECT proven_sanitize_string('<script>alert(1)</script>');
-- Returns escaped HTML entities

SELECT proven_hash_sha256('hello world');
-- Returns CRC32 checksum as hex string

SELECT proven_hex_encode('hello');                    -- '68656c6c6f'
SELECT proven_hex_decode('68656c6c6f');               -- BLOB: hello

-- ============================================================================
-- Table examples
-- ============================================================================

-- Create a test table
CREATE TABLE IF NOT EXISTS contacts (
    id INTEGER PRIMARY KEY,
    name TEXT,
    email TEXT,
    website TEXT
);

INSERT INTO contacts VALUES (1, 'Alice', 'alice@example.com', 'https://alice.dev');
INSERT INTO contacts VALUES (2, 'Bob', 'invalid-email', 'not-a-url');
INSERT INTO contacts VALUES (3, '<script>Eve</script>', 'eve@evil.com', 'https://evil.com');

-- Filter to valid emails only
SELECT id, name, email
FROM contacts
WHERE proven_validate_email(email) = 1;

-- Sanitize names for HTML display
SELECT id, proven_sanitize_string(name) AS safe_name
FROM contacts;

-- Validate all URLs
SELECT id, website, proven_validate_url(website) AS url_valid
FROM contacts;
