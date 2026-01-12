// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe Content-Type operations that prevent MIME sniffing attacks.
 *
 * All operations handle MIME sniffing prevention and validate
 * media types without throwing.
 *
 * @module
 */

import { type Result, ok, err } from './result.ts';

/** Media type category */
export type MediaCategory =
  | 'text'
  | 'image'
  | 'audio'
  | 'video'
  | 'application'
  | 'multipart'
  | 'message'
  | 'font'
  | 'model'
  | 'custom';

/** Charset encoding */
export type Charset =
  | 'utf-8'
  | 'utf-16le'
  | 'utf-16be'
  | 'iso-8859-1'
  | 'us-ascii'
  | 'windows-1252'
  | { other: string };

/** Media type */
export interface MediaType {
  readonly mediaType: string;
  readonly subtype: string;
  readonly suffix?: string;
  readonly category: MediaCategory;
}

/** Complete content type */
export interface ContentType {
  readonly media: MediaType;
  readonly charset?: Charset;
  readonly boundary?: string;
  readonly params: Array<[string, string]>;
}

/** Types that can be sniffed to something dangerous */
const SNIFFABLE_TO_DANGEROUS = new Set([
  'text/plain',
  'application/octet-stream',
  'application/x-unknown',
  'unknown/unknown',
]);

/** Safe content type operations */
export const SafeContentType = {
  /** Parse category from type string */
  parseCategory(s: string): MediaCategory {
    switch (s.toLowerCase()) {
      case 'text':
        return 'text';
      case 'image':
        return 'image';
      case 'audio':
        return 'audio';
      case 'video':
        return 'video';
      case 'application':
        return 'application';
      case 'multipart':
        return 'multipart';
      case 'message':
        return 'message';
      case 'font':
        return 'font';
      case 'model':
        return 'model';
      default:
        return 'custom';
    }
  },

  /** Check if charset is Unicode */
  isUnicode(charset: Charset): boolean {
    return charset === 'utf-8' || charset === 'utf-16le' || charset === 'utf-16be';
  },

  /** Render charset to string */
  charsetToString(charset: Charset): string {
    if (typeof charset === 'string') {
      return charset;
    }
    return charset.other.toLowerCase();
  },

  /** Parse charset from string */
  parseCharset(s: string): Charset {
    switch (s.trim().toLowerCase()) {
      case 'utf-8':
      case 'utf8':
        return 'utf-8';
      case 'utf-16le':
        return 'utf-16le';
      case 'utf-16be':
        return 'utf-16be';
      case 'iso-8859-1':
      case 'latin1':
        return 'iso-8859-1';
      case 'us-ascii':
      case 'ascii':
        return 'us-ascii';
      case 'windows-1252':
      case 'cp1252':
        return 'windows-1252';
      default:
        return { other: s };
    }
  },

  /** Check if type can be sniffed to something dangerous */
  canSniffToDangerous(s: string): boolean {
    return SNIFFABLE_TO_DANGEROUS.has(s.toLowerCase());
  },

  /** Extract suffix from subtype (e.g., "json" from "vnd.api+json") */
  extractSuffix(subtype: string): [string, string | undefined] {
    const idx = subtype.indexOf('+');
    if (idx === -1) {
      return [subtype, undefined];
    }
    return [subtype.slice(0, idx), subtype.slice(idx + 1)];
  },

  /** Parse Content-Type header */
  parse(raw: string): Result<ContentType, string> {
    const trimmed = raw.trim();
    if (trimmed.length === 0 || trimmed.length > 1024) {
      return err('Content-Type length invalid');
    }

    const parts = trimmed.split(';');
    const mediaStr = parts[0].trim();

    const slashIdx = mediaStr.indexOf('/');
    if (slashIdx === -1) {
      return err('Missing slash in media type');
    }

    const typeStr = mediaStr.slice(0, slashIdx).trim().toLowerCase();
    const subtypeStr = mediaStr.slice(slashIdx + 1).trim().toLowerCase();

    if (typeStr.length === 0 || subtypeStr.length === 0) {
      return err('Empty type or subtype');
    }

    const [baseSubtype, suffix] = this.extractSuffix(subtypeStr);
    const category = this.parseCategory(typeStr);

    const media: MediaType = {
      mediaType: typeStr,
      subtype: baseSubtype,
      suffix,
      category,
    };

    // Parse parameters
    let charset: Charset | undefined;
    let boundary: string | undefined;
    const params: Array<[string, string]> = [];

    for (let i = 1; i < parts.length; i++) {
      const p = parts[i].trim();
      const eqIdx = p.indexOf('=');
      if (eqIdx !== -1) {
        const name = p.slice(0, eqIdx).trim().toLowerCase();
        let value = p.slice(eqIdx + 1).trim();

        // Remove quotes if present
        if (value.startsWith('"') && value.endsWith('"') && value.length >= 2) {
          value = value.slice(1, -1);
        }

        switch (name) {
          case 'charset':
            charset = this.parseCharset(value);
            break;
          case 'boundary':
            boundary = value;
            break;
          default:
            params.push([name, value]);
        }
      }
    }

    return ok({ media, charset, boundary, params });
  },

  /** Create content type from type and subtype */
  make(mediaType: string, subtype: string): Result<ContentType, string> {
    const lowerT = mediaType.trim().toLowerCase();
    const lowerS = subtype.trim().toLowerCase();

    if (lowerT.length === 0 || lowerS.length === 0) {
      return err('Empty type or subtype');
    }
    if (lowerT.length > 127 || lowerS.length > 127) {
      return err('Type or subtype too long');
    }

    const [baseSubtype, suffix] = this.extractSuffix(lowerS);
    const category = this.parseCategory(lowerT);

    return ok({
      media: {
        mediaType: lowerT,
        subtype: baseSubtype,
        suffix,
        category,
      },
      params: [],
    });
  },

  /** Render content type to string */
  render(ct: ContentType): string {
    let result = `${ct.media.mediaType}/${ct.media.subtype}`;

    if (ct.media.suffix) {
      result += '+' + ct.media.suffix;
    }

    if (ct.charset) {
      result += '; charset=' + this.charsetToString(ct.charset);
    }

    if (ct.boundary) {
      result += '; boundary=' + ct.boundary;
    }

    for (const [name, value] of ct.params) {
      result += '; ' + name + '=' + value;
    }

    return result;
  },

  /** Add charset to content type */
  withCharset(ct: ContentType, charset: Charset): ContentType {
    return { ...ct, charset };
  },

  /** Add boundary to content type */
  withBoundary(ct: ContentType, boundary: string): ContentType {
    return { ...ct, boundary };
  },

  /** Check if content type is text */
  isText(ct: ContentType): boolean {
    return ct.media.category === 'text';
  },

  /** Check if content type is JSON */
  isJson(ct: ContentType): boolean {
    return ct.media.subtype === 'json' || ct.media.suffix === 'json';
  },

  /** Check if content type is XML */
  isXml(ct: ContentType): boolean {
    return ct.media.subtype === 'xml' || ct.media.suffix === 'xml';
  },

  /** Check if content type is HTML */
  isHtml(ct: ContentType): boolean {
    return ct.media.mediaType === 'text' && ct.media.subtype === 'html';
  },

  /** Check if content type is multipart */
  isMultipart(ct: ContentType): boolean {
    return ct.media.category === 'multipart';
  },

  /** Check if safe from MIME sniffing */
  safeFromSniffing(ct: ContentType): boolean {
    const fullType = `${ct.media.mediaType}/${ct.media.subtype}`;
    return !this.canSniffToDangerous(fullType);
  },

  // Common content types

  /** text/plain; charset=utf-8 */
  textPlain(): ContentType {
    return {
      media: {
        mediaType: 'text',
        subtype: 'plain',
        category: 'text',
      },
      charset: 'utf-8',
      params: [],
    };
  },

  /** text/html; charset=utf-8 */
  textHtml(): ContentType {
    return {
      media: {
        mediaType: 'text',
        subtype: 'html',
        category: 'text',
      },
      charset: 'utf-8',
      params: [],
    };
  },

  /** application/json; charset=utf-8 */
  applicationJson(): ContentType {
    return {
      media: {
        mediaType: 'application',
        subtype: 'json',
        category: 'application',
      },
      charset: 'utf-8',
      params: [],
    };
  },

  /** application/xml; charset=utf-8 */
  applicationXml(): ContentType {
    return {
      media: {
        mediaType: 'application',
        subtype: 'xml',
        category: 'application',
      },
      charset: 'utf-8',
      params: [],
    };
  },

  /** application/octet-stream */
  applicationOctetStream(): ContentType {
    return {
      media: {
        mediaType: 'application',
        subtype: 'octet-stream',
        category: 'application',
      },
      params: [],
    };
  },

  /** application/x-www-form-urlencoded */
  formUrlencoded(): ContentType {
    return {
      media: {
        mediaType: 'application',
        subtype: 'x-www-form-urlencoded',
        category: 'application',
      },
      params: [],
    };
  },

  /** multipart/form-data */
  multipartFormData(boundary: string): ContentType {
    return {
      media: {
        mediaType: 'multipart',
        subtype: 'form-data',
        category: 'multipart',
      },
      boundary,
      params: [],
    };
  },
} as const;
