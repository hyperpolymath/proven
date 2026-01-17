// SPDX-License-Identifier: PMPL-1.0
/**
 * Safe path operations with traversal prevention.
 */

import { err, ok, type Result } from './result.ts';

/**
 * Safe path operations.
 */
export class SafePath {
  /**
   * Check if a path contains traversal sequences.
   *
   * @example
   * ```ts
   * SafePath.hasTraversal("../etc/passwd"); // true
   * SafePath.hasTraversal("foo/bar"); // false
   * ```
   */
  static hasTraversal(path: string): boolean {
    const normalized = path.replace(/\\/g, '/');
    const parts = normalized.split('/');
    return parts.some((p) => p === '..');
  }

  /** Normalize a path, returning error if traversal detected. */
  static normalize(path: string): Result<string> {
    if (this.hasTraversal(path)) {
      return err(`Path traversal detected: ${path}`);
    }

    const normalized = path.replace(/\\/g, '/');
    const parts = normalized.split('/').filter((p) => p && p !== '.');
    return ok(parts.join('/'));
  }

  /** Join paths safely, preventing traversal. */
  static join(base: string, child: string): Result<string> {
    if (this.hasTraversal(child)) {
      return err(`Path traversal in child: ${child}`);
    }

    // Don't allow absolute child paths
    if (child.startsWith('/') || /^[a-zA-Z]:/.test(child)) {
      return err('Absolute path not allowed');
    }

    const normalizedBase = base.replace(/\\/g, '/').replace(/\/$/, '');
    const normalizedChild = child.replace(/\\/g, '/').replace(/^\//, '');

    return ok(`${normalizedBase}/${normalizedChild}`);
  }

  /** Get the file extension. */
  static getExtension(path: string): string | undefined {
    const normalized = path.replace(/\\/g, '/');
    const filename = normalized.split('/').pop() || '';
    const dotIndex = filename.lastIndexOf('.');
    if (dotIndex > 0 && dotIndex < filename.length - 1) {
      return filename.slice(dotIndex + 1);
    }
    return undefined;
  }

  /** Get the filename without extension. */
  static getStem(path: string): string | undefined {
    const normalized = path.replace(/\\/g, '/');
    const filename = normalized.split('/').pop() || '';
    const dotIndex = filename.lastIndexOf('.');
    if (dotIndex > 0) {
      return filename.slice(0, dotIndex);
    }
    return filename || undefined;
  }
}
