// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeHeader - Typed wrapper for HTTP header operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** JavaScript bindings to the SafeHeader FFI wrapper. */
module SafeHeaderJs = {
  @module("../../javascript/src/safe_header.js") @scope("SafeHeader")
  external hasCrlf: string => jsResult<bool> = "hasCrlf"

  @module("../../javascript/src/safe_header.js") @scope("SafeHeader")
  external isValidName: string => jsResult<bool> = "isValidName"

  @module("../../javascript/src/safe_header.js") @scope("SafeHeader")
  external isDangerous: string => jsResult<bool> = "isDangerous"

  @module("../../javascript/src/safe_header.js") @scope("SafeHeader")
  external render: (string, string) => jsResult<string> = "render"

  @module("../../javascript/src/safe_header.js") @scope("SafeHeader")
  external buildCsp: string => jsResult<string> = "buildCsp"

  @module("../../javascript/src/safe_header.js") @scope("SafeHeader")
  external buildHsts: (int, bool, bool) => jsResult<string> = "buildHsts"
}

/**
 * Check if a header value contains CRLF injection.
 * Delegates to proven_header_has_crlf via FFI.
 *
 * @param value Header value to check.
 * @returns Ok(true/false) or Error.
 */
let hasCrlf = (value: string): result<bool, string> => {
  SafeHeaderJs.hasCrlf(value)->fromJs
}

/**
 * Check if a header name is valid per HTTP specification.
 * Delegates to proven_header_is_valid_name via FFI.
 *
 * @param name Header name to validate.
 * @returns Ok(true/false) or Error.
 */
let isValidName = (name: string): result<bool, string> => {
  SafeHeaderJs.isValidName(name)->fromJs
}

/**
 * Check if a header name is a dangerous header.
 * Delegates to proven_header_is_dangerous via FFI.
 *
 * @param name Header name to check.
 * @returns Ok(true/false) or Error.
 */
let isDangerous = (name: string): result<bool, string> => {
  SafeHeaderJs.isDangerous(name)->fromJs
}

/**
 * Render a header name-value pair safely.
 * Delegates to proven_header_render via FFI.
 *
 * @param name Header name.
 * @param value Header value.
 * @returns Ok(rendered) or Error.
 */
let render = (name: string, value: string): result<string, string> => {
  SafeHeaderJs.render(name, value)->fromJs
}

/**
 * Build a Content-Security-Policy header.
 * Delegates to proven_header_build_csp via FFI.
 *
 * @param policy CSP policy string.
 * @returns Ok(csp_header) or Error.
 */
let buildCsp = (policy: string): result<string, string> => {
  SafeHeaderJs.buildCsp(policy)->fromJs
}

/**
 * Build an HSTS header.
 * Delegates to proven_header_build_hsts via FFI.
 *
 * @param maxAge Max age in seconds.
 * @param includeSubDomains Whether to include subdomains.
 * @param preload Whether to include preload directive.
 * @returns Ok(hsts_header) or Error.
 */
let buildHsts = (maxAge: int, includeSubDomains: bool, preload: bool): result<string, string> => {
  SafeHeaderJs.buildHsts(maxAge, includeSubDomains, preload)->fromJs
}
