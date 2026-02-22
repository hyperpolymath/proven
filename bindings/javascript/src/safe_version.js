// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeVersion - Semantic versioning.
 *
 * Thin FFI wrapper: proven_version_parse and proven_version_compare
 * use SemanticVersion structs requiring buffer marshaling.
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Semantic version representation.
 */
export class SemVer {
  /** @type {number} */ major;
  /** @type {number} */ minor;
  /** @type {number} */ patch;
  /** @type {string} */ prerelease;

  /**
   * @param {number} major
   * @param {number} minor
   * @param {number} patch
   * @param {string} [prerelease='']
   */
  constructor(major, minor, patch, prerelease = '') {
    this.major = major;
    this.minor = minor;
    this.patch = patch;
    this.prerelease = prerelease;
  }

  /** @returns {string} */
  toString() {
    const base = `${this.major}.${this.minor}.${this.patch}`;
    return this.prerelease ? `${base}-${this.prerelease}` : base;
  }
}

/**
 * Version constraint (placeholder for range matching).
 */
export class VersionConstraint {
  /** @type {string} */
  #raw;

  /**
   * @param {string} raw - The constraint string.
   */
  constructor(raw) {
    this.#raw = raw;
  }

  /** @returns {string} */
  toString() { return this.#raw; }
}

/**
 * Safe version operations.
 * Note: proven_version_parse and proven_version_compare require
 * struct-by-value passing which needs buffer marshaling. These will
 * be fully wired when the buffer protocol is complete.
 */
export class SafeVersion {}
