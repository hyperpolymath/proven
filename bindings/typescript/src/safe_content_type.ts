// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

export interface ContentType {
  type: string;
  subtype: string;
  parameters: Map<string, string>;
}

/**
 * Parse a Content-Type header value.
 */
export function parseContentType(value: string): Result<ContentType> {
  const trimmed = value.trim();
  if (!trimmed) {
    return { ok: false, error: 'Empty content type' };
  }

  const semicolonIndex = trimmed.indexOf(';');
  const mediaType = semicolonIndex === -1 ? trimmed : trimmed.slice(0, semicolonIndex);
  const paramsString = semicolonIndex === -1 ? '' : trimmed.slice(semicolonIndex + 1);

  const slashIndex = mediaType.indexOf('/');
  if (slashIndex === -1) {
    return { ok: false, error: 'Invalid media type format' };
  }

  const type = mediaType.slice(0, slashIndex).trim().toLowerCase();
  const subtype = mediaType.slice(slashIndex + 1).trim().toLowerCase();

  if (!type || !subtype) {
    return { ok: false, error: 'Empty type or subtype' };
  }

  // Parse parameters
  const parameters = new Map<string, string>();
  if (paramsString) {
    for (const param of paramsString.split(';')) {
      const eqIndex = param.indexOf('=');
      if (eqIndex !== -1) {
        const name = param.slice(0, eqIndex).trim().toLowerCase();
        let paramValue = param.slice(eqIndex + 1).trim();

        // Remove quotes if present
        if (paramValue.startsWith('"') && paramValue.endsWith('"')) {
          paramValue = paramValue.slice(1, -1);
        }

        parameters.set(name, paramValue);
      }
    }
  }

  return {
    ok: true,
    value: { type, subtype, parameters },
  };
}

/**
 * Serialize a ContentType to header value.
 */
export function serializeContentType(ct: ContentType): string {
  let result = `${ct.type}/${ct.subtype}`;

  for (const [name, value] of ct.parameters) {
    // Quote value if it contains special characters
    if (/[;,\s"]/.test(value)) {
      result += `; ${name}="${value.replace(/"/g, '\\"')}"`;
    } else {
      result += `; ${name}=${value}`;
    }
  }

  return result;
}

/**
 * Get the full media type (type/subtype).
 */
export function getMediaType(ct: ContentType): string {
  return `${ct.type}/${ct.subtype}`;
}

/**
 * Get the charset parameter.
 */
export function getCharset(ct: ContentType): string | undefined {
  return ct.parameters.get('charset');
}

/**
 * Get the boundary parameter (for multipart types).
 */
export function getBoundary(ct: ContentType): string | undefined {
  return ct.parameters.get('boundary');
}

/**
 * Check if content type matches a pattern.
 */
export function matches(ct: ContentType, pattern: string): boolean {
  const parsed = parseContentType(pattern);
  if (!parsed.ok || !parsed.value) return false;

  const { type: patternType, subtype: patternSubtype } = parsed.value;

  if (patternType !== '*' && patternType !== ct.type) {
    return false;
  }

  if (patternSubtype !== '*' && patternSubtype !== ct.subtype) {
    return false;
  }

  return true;
}

/**
 * Check if content type is text-based.
 */
export function isText(ct: ContentType): boolean {
  if (ct.type === 'text') return true;

  const textSubtypes = ['json', 'xml', 'javascript', 'html', 'css', 'csv'];
  if (ct.type === 'application' && textSubtypes.some((s) => ct.subtype.includes(s))) {
    return true;
  }

  return false;
}

/**
 * Check if content type is binary.
 */
export function isBinary(ct: ContentType): boolean {
  return !isText(ct);
}

/**
 * Check if content type is JSON.
 */
export function isJson(ct: ContentType): boolean {
  return ct.subtype === 'json' || ct.subtype.endsWith('+json');
}

/**
 * Check if content type is XML.
 */
export function isXml(ct: ContentType): boolean {
  return ct.subtype === 'xml' || ct.subtype.endsWith('+xml');
}

/**
 * Check if content type is HTML.
 */
export function isHtml(ct: ContentType): boolean {
  return ct.type === 'text' && ct.subtype === 'html';
}

/**
 * Check if content type is a form submission.
 */
export function isForm(ct: ContentType): boolean {
  return (
    (ct.type === 'application' && ct.subtype === 'x-www-form-urlencoded') ||
    (ct.type === 'multipart' && ct.subtype === 'form-data')
  );
}

/**
 * Check if content type is multipart.
 */
export function isMultipart(ct: ContentType): boolean {
  return ct.type === 'multipart';
}

/**
 * Check if content type is potentially dangerous for download.
 */
export function isDangerous(ct: ContentType): boolean {
  const dangerousTypes = [
    'application/x-msdownload',
    'application/x-executable',
    'application/x-msdos-program',
    'application/x-sh',
    'application/x-shellscript',
    'application/javascript',
    'application/x-javascript',
    'text/javascript',
  ];

  const mediaType = getMediaType(ct);
  return dangerousTypes.includes(mediaType);
}

// Common content types
export const CONTENT_TYPES = {
  JSON: { type: 'application', subtype: 'json', parameters: new Map([['charset', 'utf-8']]) },
  HTML: { type: 'text', subtype: 'html', parameters: new Map([['charset', 'utf-8']]) },
  TEXT: { type: 'text', subtype: 'plain', parameters: new Map([['charset', 'utf-8']]) },
  XML: { type: 'application', subtype: 'xml', parameters: new Map([['charset', 'utf-8']]) },
  FORM: { type: 'application', subtype: 'x-www-form-urlencoded', parameters: new Map() },
  MULTIPART: { type: 'multipart', subtype: 'form-data', parameters: new Map() },
  OCTET_STREAM: { type: 'application', subtype: 'octet-stream', parameters: new Map() },
  PDF: { type: 'application', subtype: 'pdf', parameters: new Map() },
  PNG: { type: 'image', subtype: 'png', parameters: new Map() },
  JPEG: { type: 'image', subtype: 'jpeg', parameters: new Map() },
  SVG: { type: 'image', subtype: 'svg+xml', parameters: new Map() },
} as const;

export const SafeContentType = {
  parse: parseContentType,
  serialize: serializeContentType,
  getMediaType,
  getCharset,
  getBoundary,
  matches,
  isText,
  isBinary,
  isJson,
  isXml,
  isHtml,
  isForm,
  isMultipart,
  isDangerous,
  CONTENT_TYPES,
};
