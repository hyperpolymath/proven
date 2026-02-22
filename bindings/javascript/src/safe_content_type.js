// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeContentType - Content-Type operations preventing MIME sniffing.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Media category enumeration.
 * @readonly
 * @enum {number}
 */
export const MediaCategory = Object.freeze({
  TEXT: 0,
  IMAGE: 1,
  AUDIO: 2,
  VIDEO: 3,
  APPLICATION: 4,
  MULTIPART: 5,
  MESSAGE: 6,
  FONT: 7,
  MODEL: 8,
  CUSTOM: 9,
});

/**
 * Charset enumeration.
 * @readonly
 * @enum {number}
 */
export const Charset = Object.freeze({
  UTF8: 0,
  UTF16LE: 1,
  UTF16BE: 2,
  ISO8859_1: 3,
  ASCII: 4,
  WINDOWS1252: 5,
  OTHER: 6,
});

/**
 * Safe content type operations backed by formally verified Idris 2 code.
 */
export class SafeContentType {
  /**
   * Check if a content type can be sniffed to something dangerous.
   *
   * @param {string} contentType - The content type string.
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static canSniffDangerous(contentType) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(contentType);
    const result = symbols.proven_content_type_can_sniff_dangerous(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Check if a content type is JSON.
   *
   * @param {string} subtype - The media subtype.
   * @param {string} suffix - The structured syntax suffix (e.g. "+json").
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static isJson(subtype, suffix = '') {
    const symbols = getLib();
    const subtypeBytes = new TextEncoder().encode(subtype);
    const suffixBytes = new TextEncoder().encode(suffix);
    const result = symbols.proven_content_type_is_json(
      subtypeBytes, subtypeBytes.length,
      suffixBytes, suffixBytes.length,
    );
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Check if a content type is XML.
   *
   * @param {string} subtype - The media subtype.
   * @param {string} suffix - The structured syntax suffix (e.g. "+xml").
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static isXml(subtype, suffix = '') {
    const symbols = getLib();
    const subtypeBytes = new TextEncoder().encode(subtype);
    const suffixBytes = new TextEncoder().encode(suffix);
    const result = symbols.proven_content_type_is_xml(
      subtypeBytes, subtypeBytes.length,
      suffixBytes, suffixBytes.length,
    );
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }
}

/**
 * ContentType representation (for API compatibility).
 */
export class ContentType {
  /** @type {string} */
  mediaType;
  /** @type {string} */
  subtype;

  /**
   * @param {string} mediaType - The media type.
   * @param {string} subtype - The subtype.
   */
  constructor(mediaType, subtype) {
    this.mediaType = mediaType;
    this.subtype = subtype;
  }
}
