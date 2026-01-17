// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafePath - Filesystem path operations that cannot crash.
 *
 * Provides safe path manipulation with traversal attack prevention.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Parsed path components.
 * @typedef {Object} ParsedPath
 * @property {string} dir - Directory portion
 * @property {string} base - Base name (file + extension)
 * @property {string} name - File name without extension
 * @property {string} ext - Extension including dot
 */

/**
 * Safe path operations.
 */
export class SafePath {
  /**
   * Join path segments safely.
   *
   * @param {...string} segments - Path segments
   * @returns {string}
   *
   * @example
   * SafePath.join("dir", "subdir", "file.txt")  // "dir/subdir/file.txt"
   */
  static join(...segments) {
    const parts = [];
    for (const segment of segments) {
      if (!segment) continue;
      // Split by both / and \ for cross-platform support
      const segmentParts = segment.split(/[/\\]/);
      for (const part of segmentParts) {
        if (part === '..') {
          if (parts.length > 0 && parts[parts.length - 1] !== '..') {
            parts.pop();
          } else {
            parts.push('..');
          }
        } else if (part && part !== '.') {
          parts.push(part);
        }
      }
    }
    return parts.join('/') || '.';
  }

  /**
   * Normalize a path (resolve . and ..).
   *
   * @param {string} path - Path to normalize
   * @returns {string}
   */
  static normalize(path) {
    return SafePath.join(path);
  }

  /**
   * Get directory name.
   *
   * @param {string} path - File path
   * @returns {string}
   */
  static dirname(path) {
    const normalized = path.replace(/\\/g, '/');
    const lastSlash = normalized.lastIndexOf('/');
    if (lastSlash === -1) {
      return '.';
    }
    if (lastSlash === 0) {
      return '/';
    }
    return normalized.slice(0, lastSlash);
  }

  /**
   * Get base name.
   *
   * @param {string} path - File path
   * @param {string} [ext] - Extension to remove
   * @returns {string}
   */
  static basename(path, ext) {
    const normalized = path.replace(/\\/g, '/').replace(/\/+$/, '');
    const lastSlash = normalized.lastIndexOf('/');
    let base = lastSlash === -1 ? normalized : normalized.slice(lastSlash + 1);
    if (ext && base.endsWith(ext)) {
      base = base.slice(0, -ext.length);
    }
    return base;
  }

  /**
   * Get file extension.
   *
   * @param {string} path - File path
   * @returns {string} Extension including dot, or empty string
   */
  static extname(path) {
    const base = SafePath.basename(path);
    const dotIndex = base.lastIndexOf('.');
    if (dotIndex <= 0) {
      return '';
    }
    return base.slice(dotIndex);
  }

  /**
   * Parse a path into components.
   *
   * @param {string} path - Path to parse
   * @returns {ParsedPath}
   */
  static parse(path) {
    const dir = SafePath.dirname(path);
    const base = SafePath.basename(path);
    const ext = SafePath.extname(path);
    const name = ext ? base.slice(0, -ext.length) : base;
    return { dir, base, name, ext };
  }

  /**
   * Check if a path is absolute.
   *
   * @param {string} path - Path to check
   * @returns {boolean}
   */
  static isAbsolute(path) {
    // Unix absolute path
    if (path.startsWith('/')) {
      return true;
    }
    // Windows absolute path (e.g., C:\)
    if (/^[a-zA-Z]:[/\\]/.test(path)) {
      return true;
    }
    return false;
  }

  /**
   * Check if path contains traversal sequences.
   *
   * @param {string} path - Path to check
   * @returns {boolean}
   */
  static hasTraversal(path) {
    const normalized = path.replace(/\\/g, '/');
    const parts = normalized.split('/');
    let depth = 0;
    for (const part of parts) {
      if (part === '..') {
        depth--;
        if (depth < 0) {
          return true;
        }
      } else if (part && part !== '.') {
        depth++;
      }
    }
    return false;
  }

  /**
   * Safely resolve a path within a base directory.
   *
   * @param {string} basePath - Base directory path
   * @param {string} relativePath - Relative path to resolve
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static resolveSafe(basePath, relativePath) {
    if (SafePath.isAbsolute(relativePath)) {
      return err('Path must be relative');
    }

    const joined = SafePath.join(basePath, relativePath);
    const normalizedBase = SafePath.normalize(basePath);
    const normalizedJoined = SafePath.normalize(joined);

    // Check if the result escapes the base path
    if (!normalizedJoined.startsWith(normalizedBase)) {
      return err('Path escapes base directory');
    }

    return ok(normalizedJoined);
  }

  /**
   * Get relative path from one path to another.
   *
   * @param {string} from - Source path
   * @param {string} to - Target path
   * @returns {string}
   */
  static relative(from, to) {
    const fromParts = SafePath.normalize(from).split('/').filter(Boolean);
    const toParts = SafePath.normalize(to).split('/').filter(Boolean);

    let commonLength = 0;
    const minLength = Math.min(fromParts.length, toParts.length);
    for (let partIndex = 0; partIndex < minLength; partIndex++) {
      if (fromParts[partIndex] !== toParts[partIndex]) {
        break;
      }
      commonLength++;
    }

    const upCount = fromParts.length - commonLength;
    const relativeParts = [...Array(upCount).fill('..'), ...toParts.slice(commonLength)];

    return relativeParts.join('/') || '.';
  }

  /**
   * Sanitize a filename (remove dangerous characters).
   *
   * @param {string} filename - Filename to sanitize
   * @returns {string}
   */
  static sanitizeFilename(filename) {
    // Remove null bytes and path separators
    let sanitized = filename.replace(/[\x00/\\]/g, '');
    // Remove other potentially dangerous characters
    sanitized = sanitized.replace(/[<>:"|?*]/g, '');
    // Prevent hidden files and special names
    sanitized = sanitized.replace(/^\.+/, '');
    // Trim whitespace
    sanitized = sanitized.trim();
    // Default if empty
    return sanitized || 'unnamed';
  }

  /**
   * Check if filename is safe (no traversal, no special chars).
   *
   * @param {string} filename - Filename to check
   * @returns {boolean}
   */
  static isSafeFilename(filename) {
    if (!filename || filename.length === 0) {
      return false;
    }
    if (filename.includes('/') || filename.includes('\\')) {
      return false;
    }
    if (filename === '.' || filename === '..') {
      return false;
    }
    if (filename.includes('\x00')) {
      return false;
    }
    return true;
  }
}
