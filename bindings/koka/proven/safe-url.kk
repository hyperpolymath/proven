// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe-url.kk -- Safe URL parsing and validation via libproven FFI.
//
// Provides URL parsing into components (scheme, host, port, path, query,
// fragment). All computation is performed by the Idris 2 verified
// implementation. No URL parsing logic is reimplemented in Koka.
module proven/safe-url

import std/num/int64
import proven/lib-proven

// ============================================================================
// URL components type
// ============================================================================

// Parsed URL components.
pub struct url-components
  scheme: string
  host: string
  port: maybe<int>
  path: string
  query: string
  fragment: string

// ============================================================================
// Raw FFI declarations
// ============================================================================

// Parse a URL and extract the scheme component.
extern raw-parse-scheme(ptr: intptr_t, len: int64): string
  c inline "({ ProvenUrlResult r = proven_url_parse((const uint8_t*)#1, (size_t)#2); kk_string_t s = kk_string_empty(); if (r.status == 0 && r.components.scheme != NULL) { s = kk_string_alloc_len((kk_ssize_t)r.components.scheme_len, r.components.scheme, kk_context()); } proven_url_free(&r.components); s; })"

// Parse a URL and extract the host component.
extern raw-parse-host(ptr: intptr_t, len: int64): string
  c inline "({ ProvenUrlResult r = proven_url_parse((const uint8_t*)#1, (size_t)#2); kk_string_t s = kk_string_empty(); if (r.status == 0 && r.components.host != NULL) { s = kk_string_alloc_len((kk_ssize_t)r.components.host_len, r.components.host, kk_context()); } proven_url_free(&r.components); s; })"

// Parse a URL and extract the port (-1 if not present).
extern raw-parse-port(ptr: intptr_t, len: int64): int32
  c inline "({ ProvenUrlResult r = proven_url_parse((const uint8_t*)#1, (size_t)#2); int32_t p = -1; if (r.status == 0 && r.components.has_port) p = (int32_t)r.components.port; proven_url_free(&r.components); p; })"

// Parse a URL and extract the path component.
extern raw-parse-path(ptr: intptr_t, len: int64): string
  c inline "({ ProvenUrlResult r = proven_url_parse((const uint8_t*)#1, (size_t)#2); kk_string_t s = kk_string_empty(); if (r.status == 0 && r.components.path != NULL) { s = kk_string_alloc_len((kk_ssize_t)r.components.path_len, r.components.path, kk_context()); } proven_url_free(&r.components); s; })"

// Parse a URL and return the status code.
extern raw-parse-status(ptr: intptr_t, len: int64): int32
  c inline "({ ProvenUrlResult r = proven_url_parse((const uint8_t*)#1, (size_t)#2); int32_t st = r.status; proven_url_free(&r.components); st; })"

// Parse a URL and extract the query string.
extern raw-parse-query(ptr: intptr_t, len: int64): string
  c inline "({ ProvenUrlResult r = proven_url_parse((const uint8_t*)#1, (size_t)#2); kk_string_t s = kk_string_empty(); if (r.status == 0 && r.components.query != NULL) { s = kk_string_alloc_len((kk_ssize_t)r.components.query_len, r.components.query, kk_context()); } proven_url_free(&r.components); s; })"

// Parse a URL and extract the fragment.
extern raw-parse-fragment(ptr: intptr_t, len: int64): string
  c inline "({ ProvenUrlResult r = proven_url_parse((const uint8_t*)#1, (size_t)#2); kk_string_t s = kk_string_empty(); if (r.status == 0 && r.components.fragment != NULL) { s = kk_string_alloc_len((kk_ssize_t)r.components.fragment_len, r.components.fragment, kk_context()); } proven_url_free(&r.components); s; })"

// ============================================================================
// Safe public API
// ============================================================================

// Parse a URL into its components.
// Returns Nothing if the URL is malformed.
pub fun parse-url(url: string): <ndet> maybe<url-components>
  val status = raw-parse-status(url.cptr, url.count.int64)
  if status == 0i32 then
    val port-raw = raw-parse-port(url.cptr, url.count.int64)
    val port-val = if port-raw >= 0i32 then Just(port-raw.int) else Nothing
    Just(Url-components(
      raw-parse-scheme(url.cptr, url.count.int64),
      raw-parse-host(url.cptr, url.count.int64),
      port-val,
      raw-parse-path(url.cptr, url.count.int64),
      raw-parse-query(url.cptr, url.count.int64),
      raw-parse-fragment(url.cptr, url.count.int64)
    ))
  else Nothing

// Extract just the scheme from a URL (e.g., "https").
// Returns Nothing if the URL is malformed.
pub fun get-scheme(url: string): <ndet> maybe<string>
  val status = raw-parse-status(url.cptr, url.count.int64)
  if status == 0i32 then
    val s = raw-parse-scheme(url.cptr, url.count.int64)
    if s.count > 0 then Just(s) else Nothing
  else Nothing

// Extract just the host from a URL.
// Returns Nothing if the URL is malformed.
pub fun get-host(url: string): <ndet> maybe<string>
  val status = raw-parse-status(url.cptr, url.count.int64)
  if status == 0i32 then
    val h = raw-parse-host(url.cptr, url.count.int64)
    if h.count > 0 then Just(h) else Nothing
  else Nothing
