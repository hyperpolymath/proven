{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

{-
  safe_url.orc - URL parsing and validation for Orc via libproven

  All URL parsing and validation is performed by libproven's formally
  verified Idris 2 core. This module provides Orc-idiomatic wrappers
  that handle URL component lifecycle management.
-}

include "src/ffi.orc"
include "src/proven.orc"
include "src/safe_string.orc"

-- ============================================================================
-- URL Parsing
-- ============================================================================

-- Parse a URL string into its components via libproven.
-- Returns an opaque handle to URL components, or halts on parse failure.
-- The caller MUST call url_free() on the returned handle when done.
-- Delegates to proven_url_parse via FFI.
def url_parse(url_str) =
  with_string_ptr(url_str, lambda(ptr, len) =
    val result = ffi_url_parse(ptr, len)
    if result /= null then result
    else stop
  )

-- Free URL components allocated by url_parse.
-- Must be called after url_parse to prevent memory leaks.
-- Delegates to proven_url_free via FFI.
def url_free(components) = ffi_url_free(components)

-- ============================================================================
-- URL Validation (convenience wrappers)
-- ============================================================================

-- Validate a URL by attempting to parse it.
-- Publishes true if the URL is valid, false otherwise.
-- Automatically frees the parsed components on success.
-- Delegates to proven_url_parse/proven_url_free via FFI.
def is_valid_url(url_str) =
  with_string_ptr(url_str, lambda(ptr, len) =
    val result = ffi_url_parse(ptr, len)
    if result /= null then
      ffi_url_free(result)
      >> true
    else false
  )

-- ============================================================================
-- Concurrent URL Validation
-- ============================================================================

-- Validate a list of URLs concurrently.
-- Publishes (url, valid) pairs for each URL.
def validate_urls(urls) =
  each(urls) >url>
  val valid = is_valid_url(url)
  (url, valid)

-- Filter a list to only valid URLs.
-- Publishes only those URLs that pass parsing.
def filter_valid_urls(urls) =
  each(urls) >url>
  val valid = is_valid_url(url)
  if valid then url
  else stop
