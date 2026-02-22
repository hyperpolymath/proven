-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven_functions.sql - Example usage of proven PostgreSQL extension
--
-- Prerequisite: CREATE EXTENSION proven;
--
-- All functions call libproven (Idris 2 verified core via Zig FFI bridge).
-- Functions return NULL on error rather than raising exceptions.

-- ============================================================================
-- SafeMath examples
-- ============================================================================

-- Basic checked arithmetic
SELECT proven_safe_add(100, 200);                   -- 300
SELECT proven_safe_add(9223372036854775807, 1);      -- NULL (overflow)

SELECT proven_safe_sub(500, 200);                   -- 300
SELECT proven_safe_sub(-9223372036854775808, 1);     -- NULL (underflow)

SELECT proven_safe_mul(1000, 1000);                 -- 1000000
SELECT proven_safe_mul(9223372036854775807, 2);      -- NULL (overflow)

SELECT proven_safe_div(100, 3);                     -- 33
SELECT proven_safe_div(100, 0);                     -- NULL (division by zero)

-- Use in queries: safe aggregation that does not crash on overflow
SELECT
    id,
    proven_safe_add(balance, amount) AS new_balance
FROM transactions
WHERE proven_safe_add(balance, amount) IS NOT NULL;

-- ============================================================================
-- Validation examples
-- ============================================================================

-- Email validation
SELECT proven_validate_email('user@example.com');    -- TRUE
SELECT proven_validate_email('not-an-email');         -- FALSE
SELECT proven_validate_email(NULL);                   -- NULL

-- Filter valid emails from a table
SELECT email
FROM users
WHERE proven_validate_email(email);

-- URL validation
SELECT proven_validate_url('https://example.com/path?q=1');  -- TRUE
SELECT proven_validate_url('not a url');                      -- FALSE

-- IPv4 validation
SELECT proven_validate_ipv4('192.168.1.1');          -- TRUE
SELECT proven_validate_ipv4('999.999.999.999');      -- FALSE

-- JSON validation
SELECT proven_validate_json('{"key": "value"}');     -- TRUE
SELECT proven_validate_json('{invalid json}');        -- FALSE

-- Validate JSON column data
SELECT id, data
FROM documents
WHERE proven_validate_json(data::TEXT);

-- ============================================================================
-- String and encoding examples
-- ============================================================================

-- HTML sanitization (XSS prevention)
SELECT proven_sanitize_string('<script>alert("xss")</script>');
-- Returns: '&lt;script&gt;alert(&quot;xss&quot;)&lt;/script&gt;'

-- Sanitize user input before storing
INSERT INTO comments (body)
VALUES (proven_sanitize_string(user_input));

-- Hash computation (CRC32 as hex)
SELECT proven_hash_sha256('hello world');

-- Hex encoding/decoding
SELECT proven_hex_encode('hello');                   -- '68656c6c6f'
SELECT proven_hex_decode('68656c6c6f');              -- '\x68656c6c6f'

-- ============================================================================
-- Combining functions in real queries
-- ============================================================================

-- Validate and sanitize user submissions
SELECT
    id,
    CASE
        WHEN proven_validate_email(email) THEN email
        ELSE NULL
    END AS valid_email,
    proven_sanitize_string(display_name) AS safe_name,
    proven_validate_json(preferences::TEXT) AS has_valid_prefs
FROM user_profiles;

-- Safe financial calculations
SELECT
    account_id,
    proven_safe_mul(quantity, unit_price) AS total_cost,
    proven_safe_add(
        proven_safe_mul(quantity, unit_price),
        shipping_fee
    ) AS grand_total
FROM orders
WHERE proven_safe_mul(quantity, unit_price) IS NOT NULL;
