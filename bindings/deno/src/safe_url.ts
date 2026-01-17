// SPDX-License-Identifier: PMPL-1.0
/**
 * Safe URL parsing.
 */

import { err, ok, type Result } from './result.ts';

export interface ParsedUrl {
  scheme: string;
  host: string;
  port?: number;
  path: string;
  query?: string;
  fragment?: string;
  username?: string;
  password?: string;
}

/**
 * Safe URL operations.
 */
export class SafeUrl {
  /**
   * Parse a URL string.
   *
   * @example
   * ```ts
   * const result = SafeUrl.parse("https://example.com/path?q=test");
   * if (result.ok) {
   *   console.log(result.value.host); // "example.com"
   * }
   * ```
   */
  static parse(url: string): Result<ParsedUrl> {
    try {
      const parsed = new URL(url);
      return ok({
        scheme: parsed.protocol.replace(':', ''),
        host: parsed.hostname,
        port: parsed.port ? parseInt(parsed.port, 10) : undefined,
        path: parsed.pathname,
        query: parsed.search ? parsed.search.slice(1) : undefined,
        fragment: parsed.hash ? parsed.hash.slice(1) : undefined,
        username: parsed.username || undefined,
        password: parsed.password || undefined,
      });
    } catch {
      return err('Invalid URL');
    }
  }

  /** Check if a URL is valid. */
  static isValid(url: string): boolean {
    return this.parse(url).ok;
  }

  /** Get the domain from a URL. */
  static getDomain(url: string): Result<string> {
    const result = this.parse(url);
    if (!result.ok) return result;
    return ok(result.value.host);
  }

  /** Get query parameters as a Map. */
  static getQueryParams(url: string): Result<Map<string, string>> {
    const result = this.parse(url);
    if (!result.ok) return result;

    const params = new Map<string, string>();
    if (result.value.query) {
      for (const pair of result.value.query.split('&')) {
        const [key, value] = pair.split('=');
        if (key) {
          params.set(decodeURIComponent(key), decodeURIComponent(value || ''));
        }
      }
    }
    return ok(params);
  }
}
