// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeContentType - Typed wrapper for content-type operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** JavaScript bindings to the SafeContentType FFI wrapper. */
module SafeContentTypeJs = {
  @module("../../javascript/src/safe_content_type.js") @scope("SafeContentType")
  external canSniffDangerous: string => jsResult<bool> = "canSniffDangerous"

  @module("../../javascript/src/safe_content_type.js") @scope("SafeContentType")
  external isJson: (string, string) => jsResult<bool> = "isJson"

  @module("../../javascript/src/safe_content_type.js") @scope("SafeContentType")
  external isXml: (string, string) => jsResult<bool> = "isXml"
}

/**
 * Check if a content type can be sniffed as dangerous.
 * Delegates to proven_content_type_can_sniff_dangerous via FFI.
 *
 * @param contentType Content type string.
 * @returns Ok(true/false) or Error.
 */
let canSniffDangerous = (contentType: string): result<bool, string> => {
  SafeContentTypeJs.canSniffDangerous(contentType)->fromJs
}

/**
 * Check if content type is JSON.
 * Delegates to proven_content_type_is_json via FFI.
 *
 * @param contentType Content type string.
 * @param body Body content.
 * @returns Ok(true/false) or Error.
 */
let isJson = (contentType: string, body: string): result<bool, string> => {
  SafeContentTypeJs.isJson(contentType, body)->fromJs
}

/**
 * Check if content type is XML.
 * Delegates to proven_content_type_is_xml via FFI.
 *
 * @param contentType Content type string.
 * @param body Body content.
 * @returns Ok(true/false) or Error.
 */
let isXml = (contentType: string, body: string): result<bool, string> => {
  SafeContentTypeJs.isXml(contentType, body)->fromJs
}
