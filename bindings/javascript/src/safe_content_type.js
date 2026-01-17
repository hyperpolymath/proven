// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeContentType - MIME type validation without sniffing attacks.
 *
 * Provides safe Content-Type parsing and validation.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Media category enumeration.
 * @readonly
 * @enum {string}
 */
export const MediaCategory = Object.freeze({
  TEXT: 'text',
  IMAGE: 'image',
  AUDIO: 'audio',
  VIDEO: 'video',
  APPLICATION: 'application',
  MULTIPART: 'multipart',
  FONT: 'font',
  MODEL: 'model',
  UNKNOWN: 'unknown',
});

/**
 * Parsed Content-Type.
 * @typedef {Object} ContentType
 * @property {string} type - Main type (e.g., "text")
 * @property {string} subtype - Subtype (e.g., "html")
 * @property {string} essence - Type/subtype (e.g., "text/html")
 * @property {Map<string, string>} parameters - Parameters (e.g., charset)
 */

/**
 * Safe Content-Type operations.
 */
export class SafeContentType {
  /**
   * Parse a Content-Type header value.
   *
   * @param {string} contentType - Content-Type string
   * @returns {{ ok: true, value: ContentType } | { ok: false, error: string }}
   *
   * @example
   * SafeContentType.parse("text/html; charset=utf-8")
   */
  static parse(contentType) {
    if (typeof contentType !== 'string') {
      return err('Content-Type must be a string');
    }

    const trimmed = contentType.trim();
    if (trimmed.length === 0) {
      return err('Content-Type cannot be empty');
    }

    // Split by semicolon for parameters
    const parts = trimmed.split(';');
    const mediaType = parts[0].trim().toLowerCase();

    // Validate media type format
    const slashIndex = mediaType.indexOf('/');
    if (slashIndex === -1) {
      return err('Invalid media type: missing /');
    }

    const type = mediaType.slice(0, slashIndex);
    const subtype = mediaType.slice(slashIndex + 1);

    if (type.length === 0 || subtype.length === 0) {
      return err('Invalid media type: empty type or subtype');
    }

    // Validate characters (RFC 6838)
    const tokenRegex = /^[a-z0-9!#$&\-^_.+]+$/i;
    if (!tokenRegex.test(type) || !tokenRegex.test(subtype)) {
      return err('Invalid media type: invalid characters');
    }

    // Parse parameters
    const parameters = new Map();
    for (let paramIndex = 1; paramIndex < parts.length; paramIndex++) {
      const param = parts[paramIndex].trim();
      const equalsIndex = param.indexOf('=');
      if (equalsIndex === -1) continue;

      const paramName = param.slice(0, equalsIndex).trim().toLowerCase();
      let paramValue = param.slice(equalsIndex + 1).trim();

      // Remove quotes if present
      if (paramValue.startsWith('"') && paramValue.endsWith('"')) {
        paramValue = paramValue.slice(1, -1);
      }

      parameters.set(paramName, paramValue);
    }

    return ok({
      type,
      subtype,
      essence: `${type}/${subtype}`,
      parameters,
    });
  }

  /**
   * Create a Content-Type with validation.
   *
   * @param {string} type - Main type
   * @param {string} subtype - Subtype
   * @param {Record<string, string>} [params={}] - Parameters
   * @returns {{ ok: true, value: ContentType } | { ok: false, error: string }}
   */
  static create(type, subtype, params = {}) {
    const essence = `${type}/${subtype}`;
    let fullType = essence;

    for (const [key, value] of Object.entries(params)) {
      fullType += `; ${key}=${value}`;
    }

    return SafeContentType.parse(fullType);
  }

  /**
   * Format a ContentType to string.
   *
   * @param {ContentType} contentType - ContentType to format
   * @returns {string}
   */
  static format(contentType) {
    let result = contentType.essence;
    for (const [key, value] of contentType.parameters) {
      // Quote value if needed
      const needsQuotes = /[\s;,=]/.test(value);
      result += `; ${key}=${needsQuotes ? `"${value}"` : value}`;
    }
    return result;
  }

  /**
   * Get charset from Content-Type.
   *
   * @param {string} contentType - Content-Type string
   * @returns {string | null}
   */
  static getCharset(contentType) {
    const result = SafeContentType.parse(contentType);
    if (!result.ok) {
      return null;
    }
    return result.value.parameters.get('charset') || null;
  }

  /**
   * Get media category.
   *
   * @param {string} contentType - Content-Type string
   * @returns {string}
   */
  static getCategory(contentType) {
    const result = SafeContentType.parse(contentType);
    if (!result.ok) {
      return MediaCategory.UNKNOWN;
    }

    const typeMap = {
      text: MediaCategory.TEXT,
      image: MediaCategory.IMAGE,
      audio: MediaCategory.AUDIO,
      video: MediaCategory.VIDEO,
      application: MediaCategory.APPLICATION,
      multipart: MediaCategory.MULTIPART,
      font: MediaCategory.FONT,
      model: MediaCategory.MODEL,
    };

    return typeMap[result.value.type] || MediaCategory.UNKNOWN;
  }

  /**
   * Check if Content-Type is text-based.
   *
   * @param {string} contentType - Content-Type string
   * @returns {boolean}
   */
  static isText(contentType) {
    const result = SafeContentType.parse(contentType);
    if (!result.ok) return false;

    if (result.value.type === 'text') return true;

    // Common text-based application types
    const textSubtypes = ['json', 'xml', 'javascript', 'x-javascript', 'ecmascript', 'x-www-form-urlencoded'];
    return result.value.type === 'application' && textSubtypes.some((st) => result.value.subtype.includes(st));
  }

  /**
   * Check if Content-Type is binary.
   *
   * @param {string} contentType - Content-Type string
   * @returns {boolean}
   */
  static isBinary(contentType) {
    return !SafeContentType.isText(contentType);
  }

  /**
   * Check if Content-Type is potentially dangerous.
   *
   * @param {string} contentType - Content-Type string
   * @returns {boolean}
   */
  static isDangerous(contentType) {
    const result = SafeContentType.parse(contentType);
    if (!result.ok) return true; // Unknown types are dangerous

    const dangerousTypes = new Set([
      'application/javascript',
      'application/x-javascript',
      'text/javascript',
      'application/x-shockwave-flash',
      'application/x-msdownload',
      'application/x-msdos-program',
      'application/x-executable',
      'application/vnd.ms-htmlhelp',
      'application/java-archive',
    ]);

    return dangerousTypes.has(result.value.essence);
  }

  /**
   * Check if Content-Type is HTML.
   *
   * @param {string} contentType - Content-Type string
   * @returns {boolean}
   */
  static isHtml(contentType) {
    const result = SafeContentType.parse(contentType);
    if (!result.ok) return false;
    return result.value.essence === 'text/html' || result.value.essence === 'application/xhtml+xml';
  }

  /**
   * Check if Content-Type is JSON.
   *
   * @param {string} contentType - Content-Type string
   * @returns {boolean}
   */
  static isJson(contentType) {
    const result = SafeContentType.parse(contentType);
    if (!result.ok) return false;
    return result.value.essence === 'application/json' || result.value.subtype.endsWith('+json');
  }

  /**
   * Check if Content-Type is XML.
   *
   * @param {string} contentType - Content-Type string
   * @returns {boolean}
   */
  static isXml(contentType) {
    const result = SafeContentType.parse(contentType);
    if (!result.ok) return false;
    return (
      result.value.essence === 'application/xml' ||
      result.value.essence === 'text/xml' ||
      result.value.subtype.endsWith('+xml')
    );
  }

  /**
   * Check if two Content-Types match (ignoring parameters).
   *
   * @param {string} a - First Content-Type
   * @param {string} b - Second Content-Type
   * @returns {boolean}
   */
  static matches(a, b) {
    const resultA = SafeContentType.parse(a);
    const resultB = SafeContentType.parse(b);
    if (!resultA.ok || !resultB.ok) return false;
    return resultA.value.essence === resultB.value.essence;
  }
}
