// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeUrl - Typed wrapper for URL parsing that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** JavaScript bindings to the SafeUrl FFI wrapper. */
module SafeUrlJs = {
  @module("../../javascript/src/safe_url.js") @scope("SafeUrl")
  external encode: string => jsResult<string> = "encode"

  @module("../../javascript/src/safe_url.js") @scope("SafeUrl")
  external decode: string => jsResult<string> = "decode"
}

/**
 * URL-encode a string (RFC 3986 percent encoding).
 * Delegates to proven_http_url_encode via FFI.
 *
 * @param str String to encode.
 * @returns Ok(encoded) or Error.
 */
let encode = (str: string): result<string, string> => {
  SafeUrlJs.encode(str)->fromJs
}

/**
 * URL-decode a percent-encoded string.
 * Delegates to proven_http_url_decode via FFI.
 *
 * @param str String to decode.
 * @returns Ok(decoded) or Error.
 */
let decode = (str: string): result<string, string> => {
  SafeUrlJs.decode(str)->fromJs
}
