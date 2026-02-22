// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe-path.kk -- Safe filesystem path operations via libproven FFI.
//
// Provides directory traversal detection and filename sanitization.
// All computation is performed by the Idris 2 verified implementation.
// No path validation logic is reimplemented in Koka.
module proven/safe-path

import std/num/int64
import proven/lib-proven

// ============================================================================
// Raw FFI declarations
// ============================================================================

// Check if path contains directory traversal sequences ("..").
extern raw-has-traversal(ptr: intptr_t, len: int64): bool-result
  c inline "{ ProvenBoolResult r = proven_path_has_traversal((const uint8_t*)#1, (size_t)#2); return kk_proven_bool_result(r.status, r.value ? 1 : 0, kk_context()); }"

// Sanitize a filename by removing dangerous characters.
extern raw-sanitize-filename-call(ptr: intptr_t, len: int64): string
  c inline "({ ProvenStringResult r = proven_path_sanitize_filename((const uint8_t*)#1, (size_t)#2); kk_string_t s = kk_string_alloc_len((kk_ssize_t)r.length, (const char*)r.value, kk_context()); proven_free_string(r.value); s; })"

extern raw-sanitize-filename-status(ptr: intptr_t, len: int64): int32
  c inline "({ ProvenStringResult r = proven_path_sanitize_filename((const uint8_t*)#1, (size_t)#2); int32_t st = r.status; if (st == 0 && r.value != NULL) proven_free_string(r.value); st; })"

// ============================================================================
// Safe public API
// ============================================================================

// Check if a path contains directory traversal sequences ("..").
// Returns Just(true) if traversal detected, Just(false) if safe, Nothing on error.
pub fun has-traversal(path: string): <ndet> maybe<bool>
  val r = raw-has-traversal(path.cptr, path.count.int64)
  bool-result-to-maybe(r)

// Sanitize a filename by removing dangerous characters.
// Strips path separators, "..", null bytes, and control characters.
// Returns Nothing on error.
pub fun sanitize-filename(filename: string): <ndet> maybe<string>
  val status = raw-sanitize-filename-status(filename.cptr, filename.count.int64)
  if status == 0i32 then
    Just(raw-sanitize-filename-call(filename.cptr, filename.count.int64))
  else Nothing

// Check if a path is safe (no traversal detected).
// Returns Just(true) if safe, Just(false) if traversal detected, Nothing on error.
pub fun is-safe-path(path: string): <ndet> maybe<bool>
  match has-traversal(path)
    Just(t) -> Just(!t)
    Nothing -> Nothing
