// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeContentType - Content-Type operations preventing MIME sniffing.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Safe content type operations backed by formally verified Idris 2 code.
 */
export class SafeContentType {
  /**
   * Check if a content type can be sniffed to something dangerous.
   *
   * @param contentType - The content type string.
   * @returns Result containing a boolean or an error string.
   */
  static canSniffDangerous(contentType: string): Result<boolean> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(contentType);
    const result = symbols.proven_content_type_can_sniff_dangerous(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Check if a content type is JSON.
   *
   * @param subtype - The media subtype.
   * @param suffix - The structured syntax suffix (e.g. "+json").
   * @returns Result containing a boolean or an error string.
   */
  static isJson(subtype: string, suffix = ''): Result<boolean> {
    const symbols = getLib();
    const subtypeBytes = new TextEncoder().encode(subtype);
    const suffixBytes = new TextEncoder().encode(suffix);
    const result = symbols.proven_content_type_is_json(
      subtypeBytes,
      subtypeBytes.length,
      suffixBytes,
      suffixBytes.length,
    );
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Check if a content type is XML.
   *
   * @param subtype - The media subtype.
   * @param suffix - The structured syntax suffix (e.g. "+xml").
   * @returns Result containing a boolean or an error string.
   */
  static isXml(subtype: string, suffix = ''): Result<boolean> {
    const symbols = getLib();
    const subtypeBytes = new TextEncoder().encode(subtype);
    const suffixBytes = new TextEncoder().encode(suffix);
    const result = symbols.proven_content_type_is_xml(
      subtypeBytes,
      subtypeBytes.length,
      suffixBytes,
      suffixBytes.length,
    );
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }
}
