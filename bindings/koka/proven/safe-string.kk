// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe-string.kk -- Safe string operations via libproven FFI.
//
// Provides UTF-8 validation and injection-safe escaping for SQL, HTML,
// and JavaScript contexts. All computation is performed by the Idris 2
// verified implementation. No escaping logic is reimplemented in Koka.
module proven/safe-string

import std/num/int64
import proven/lib-proven

// ============================================================================
// Raw FFI declarations
// ============================================================================

// Check if bytes are valid UTF-8.
extern raw-is-valid-utf8(ptr: intptr_t, len: int64): bool-result
  c inline "{ ProvenBoolResult r = proven_string_is_valid_utf8((const uint8_t*)#1, (size_t)#2); return kk_proven_bool_result(r.status, r.value ? 1 : 0, kk_context()); }"

// Escape string for SQL (single quotes).
// Returns a ProvenStringResult; caller must free the value.
extern raw-escape-sql(ptr: intptr_t, len: int64): int-result
  c inline "{ ProvenStringResult r = proven_string_escape_sql((const uint8_t*)#1, (size_t)#2); return kk_proven_int_result(r.status, (int64_t)(intptr_t)r.value, kk_context()); }"

extern raw-escape-sql-len(ptr: intptr_t, len: int64): int64
  c inline "({ ProvenStringResult r = proven_string_escape_sql((const uint8_t*)#1, (size_t)#2); (int64_t)r.length; })"

// Escape string for HTML.
extern raw-escape-html(ptr: intptr_t, len: int64): int-result
  c inline "{ ProvenStringResult r = proven_string_escape_html((const uint8_t*)#1, (size_t)#2); return kk_proven_int_result(r.status, (int64_t)(intptr_t)r.value, kk_context()); }"

// Escape string for JavaScript string literals.
extern raw-escape-js(ptr: intptr_t, len: int64): int-result
  c inline "{ ProvenStringResult r = proven_string_escape_js((const uint8_t*)#1, (size_t)#2); return kk_proven_int_result(r.status, (int64_t)(intptr_t)r.value, kk_context()); }"

// Helper: call escape function, convert result to Koka string, free C memory.
extern raw-string-call(func-name: string, ptr: intptr_t, len: int64): string
  c inline "({ ProvenStringResult r; if (strcmp(#1, \"sql\") == 0) r = proven_string_escape_sql((const uint8_t*)#2, (size_t)#3); else if (strcmp(#1, \"html\") == 0) r = proven_string_escape_html((const uint8_t*)#2, (size_t)#3); else r = proven_string_escape_js((const uint8_t*)#2, (size_t)#3); kk_string_t s = kk_string_alloc_len((kk_ssize_t)r.length, (const char*)r.value, kk_context()); proven_free_string(r.value); s; })"

extern raw-string-call-status(func-name: string, ptr: intptr_t, len: int64): int32
  c inline "({ ProvenStringResult r; if (strcmp(#1, \"sql\") == 0) r = proven_string_escape_sql((const uint8_t*)#2, (size_t)#3); else if (strcmp(#1, \"html\") == 0) r = proven_string_escape_html((const uint8_t*)#2, (size_t)#3); else r = proven_string_escape_js((const uint8_t*)#2, (size_t)#3); if (r.status == 0 && r.value != NULL) proven_free_string(r.value); r.status; })"

// ============================================================================
// Safe public API
// ============================================================================

// Check if a string is valid UTF-8.
// Returns Just(true/false), or Nothing on error.
pub fun is-valid-utf8(input: string): <ndet> maybe<bool>
  val r = raw-is-valid-utf8(input.cptr, input.count.int64)
  bool-result-to-maybe(r)

// Escape a string for safe use in SQL queries.
// Doubles single quotes. Returns Nothing on error.
pub fun escape-sql(input: string): <ndet> maybe<string>
  val status = raw-string-call-status("sql", input.cptr, input.count.int64)
  if status == 0i32 then
    Just(raw-string-call("sql", input.cptr, input.count.int64))
  else Nothing

// Escape a string for safe use in HTML.
// Escapes <, >, &, ", and ' characters. Returns Nothing on error.
pub fun escape-html(input: string): <ndet> maybe<string>
  val status = raw-string-call-status("html", input.cptr, input.count.int64)
  if status == 0i32 then
    Just(raw-string-call("html", input.cptr, input.count.int64))
  else Nothing

// Escape a string for safe use in JavaScript string literals.
// Returns Nothing on error.
pub fun escape-js(input: string): <ndet> maybe<string>
  val status = raw-string-call-status("js", input.cptr, input.count.int64)
  if status == 0i32 then
    Just(raw-string-call("js", input.cptr, input.count.int64))
  else Nothing
