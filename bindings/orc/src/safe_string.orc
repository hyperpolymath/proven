{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

{-
  safe_string.orc - Safe string operations for Orc

  All computation is delegated to libproven's formally verified Idris 2
  core via the JNA FFI bridge. No string processing logic is reimplemented.

  String operations require marshaling between Java String objects and
  the C ABI's (ptr, len) convention. The JNA bridge handles this
  conversion transparently.
-}

include "src/ffi.orc"
include "src/proven.orc"

-- ============================================================================
-- Import JNA helper for string-to-pointer conversion
-- ============================================================================

import class JNAMemory = "com.sun.jna.Memory"
import class JNANativeString = "com.sun.jna.NativeString"

-- Helper: Convert a Java String to (ptr, len) for FFI calls.
-- The JNA bridge manages memory for the duration of the call.
def with_string_ptr(s, f) =
  val bytes = s.getBytes("UTF-8")
  val mem = JNAMemory(bytes.length)
  mem.write(0, bytes, 0, bytes.length)
  >> f(mem, bytes.length)

-- ============================================================================
-- UTF-8 Validation
-- ============================================================================

-- Validate that a string is valid UTF-8.
-- Publishes true/false, or halts on FFI error.
-- Delegates to proven_string_is_valid_utf8 via FFI.
def is_valid_utf8(s) =
  with_string_ptr(s, lambda(ptr, len) =
    val result = ffi_string_is_valid_utf8(ptr, len)
    extract_bool(result)
  )

-- ============================================================================
-- String Escaping (XSS / SQL Injection Prevention)
-- ============================================================================

-- Escape a string for safe SQL interpolation.
-- Publishes the escaped string, or halts on error.
-- Delegates to proven_string_escape_sql via FFI.
def escape_sql(s) =
  with_string_ptr(s, lambda(ptr, len) =
    val result = ffi_string_escape_sql(ptr, len)
    extract_string(result)
  )

-- Escape a string for safe HTML rendering (XSS prevention).
-- Publishes the escaped string, or halts on error.
-- Delegates to proven_string_escape_html via FFI.
def escape_html(s) =
  with_string_ptr(s, lambda(ptr, len) =
    val result = ffi_string_escape_html(ptr, len)
    extract_string(result)
  )

-- Escape a string for safe JavaScript string literals.
-- Publishes the escaped string, or halts on error.
-- Delegates to proven_string_escape_js via FFI.
def escape_js(s) =
  with_string_ptr(s, lambda(ptr, len) =
    val result = ffi_string_escape_js(ptr, len)
    extract_string(result)
  )

-- ============================================================================
-- Concurrent String Sanitization
-- ============================================================================

-- Escape a string for all three contexts concurrently.
-- Publishes three (context, escaped) pairs.
def escape_all(s) =
  ("sql", escape_sql(s)) | ("html", escape_html(s)) | ("js", escape_js(s))
