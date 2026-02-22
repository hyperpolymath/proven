// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeContentType - Typed wrapper for content-type operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafeContentType as JsSafeContentType } from '../../javascript/src/safe_content_type.js';

/** Result type for content type operations. */
export type ContentTypeResult<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Safe content type operations backed by formally verified Idris 2 code.
 * All methods delegate to the JavaScript FFI wrapper.
 */
export const SafeContentType = {
  /**
   * Check if a content type can be sniffed as dangerous.
   * Delegates to proven_content_type_can_sniff_dangerous via FFI.
   *
   * @param contentType - Content type string.
   * @returns Result with boolean danger flag, or error.
   */
  canSniffDangerous(contentType: string): ContentTypeResult<boolean> {
    return JsSafeContentType.canSniffDangerous(contentType) as ContentTypeResult<boolean>;
  },

  /**
   * Check if content and its body are valid JSON.
   * Delegates to proven_content_type_is_json via FFI.
   *
   * @param contentType - Content type string.
   * @param body - Body content.
   * @returns Result with boolean flag, or error.
   */
  isJson(contentType: string, body: string): ContentTypeResult<boolean> {
    return JsSafeContentType.isJson(contentType, body) as ContentTypeResult<boolean>;
  },

  /**
   * Check if content and its body are valid XML.
   * Delegates to proven_content_type_is_xml via FFI.
   *
   * @param contentType - Content type string.
   * @param body - Body content.
   * @returns Result with boolean flag, or error.
   */
  isXml(contentType: string, body: string): ContentTypeResult<boolean> {
    return JsSafeContentType.isXml(contentType, body) as ContentTypeResult<boolean>;
  },
} as const;
