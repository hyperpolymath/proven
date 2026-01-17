// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeVersion - Semantic versioning parsing that cannot crash.
 *
 * Provides SemVer 2.0.0 compliant version parsing and comparison.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Parsed version components.
 * @typedef {Object} Version
 * @property {number} major - Major version
 * @property {number} minor - Minor version
 * @property {number} patch - Patch version
 * @property {string | null} prerelease - Prerelease identifier
 * @property {string | null} build - Build metadata
 */

/**
 * Safe version operations.
 */
export class SafeVersion {
  /**
   * Parse a semantic version string.
   *
   * @param {string} versionString - Version string to parse
   * @returns {{ ok: true, value: Version } | { ok: false, error: string }}
   *
   * @example
   * SafeVersion.parse("1.2.3")  // { ok: true, value: { major: 1, minor: 2, patch: 3, ... } }
   * SafeVersion.parse("1.2.3-beta.1+build.123")
   */
  static parse(versionString) {
    if (typeof versionString !== 'string') {
      return err('Version must be a string');
    }

    let str = versionString.trim();

    // Remove leading 'v' if present
    if (str.startsWith('v') || str.startsWith('V')) {
      str = str.slice(1);
    }

    // Extract build metadata
    let build = null;
    const buildIndex = str.indexOf('+');
    if (buildIndex !== -1) {
      build = str.slice(buildIndex + 1);
      str = str.slice(0, buildIndex);
    }

    // Extract prerelease
    let prerelease = null;
    const prereleaseIndex = str.indexOf('-');
    if (prereleaseIndex !== -1) {
      prerelease = str.slice(prereleaseIndex + 1);
      str = str.slice(0, prereleaseIndex);
    }

    // Parse core version
    const parts = str.split('.');
    if (parts.length < 1 || parts.length > 3) {
      return err('Invalid version format');
    }

    const parseNum = (s) => {
      if (!/^\d+$/.test(s)) {
        return null;
      }
      // No leading zeros (except for 0 itself)
      if (s.length > 1 && s.startsWith('0')) {
        return null;
      }
      return parseInt(s, 10);
    };

    const major = parseNum(parts[0]);
    if (major === null) {
      return err('Invalid major version');
    }

    const minor = parts.length >= 2 ? parseNum(parts[1]) : 0;
    if (minor === null) {
      return err('Invalid minor version');
    }

    const patch = parts.length >= 3 ? parseNum(parts[2]) : 0;
    if (patch === null) {
      return err('Invalid patch version');
    }

    return ok({
      major,
      minor,
      patch,
      prerelease,
      build,
    });
  }

  /**
   * Check if string is a valid semver.
   *
   * @param {string} versionString - Version string
   * @returns {boolean}
   */
  static isValid(versionString) {
    return SafeVersion.parse(versionString).ok;
  }

  /**
   * Format a Version to string.
   *
   * @param {Version} version - Version to format
   * @returns {string}
   */
  static format(version) {
    let result = `${version.major}.${version.minor}.${version.patch}`;
    if (version.prerelease) {
      result += `-${version.prerelease}`;
    }
    if (version.build) {
      result += `+${version.build}`;
    }
    return result;
  }

  /**
   * Compare two versions.
   *
   * @param {Version} versionA - First version
   * @param {Version} versionB - Second version
   * @returns {number} -1 if a < b, 0 if equal, 1 if a > b
   */
  static compare(versionA, versionB) {
    // Compare major
    if (versionA.major !== versionB.major) {
      return versionA.major < versionB.major ? -1 : 1;
    }

    // Compare minor
    if (versionA.minor !== versionB.minor) {
      return versionA.minor < versionB.minor ? -1 : 1;
    }

    // Compare patch
    if (versionA.patch !== versionB.patch) {
      return versionA.patch < versionB.patch ? -1 : 1;
    }

    // Compare prerelease
    if (versionA.prerelease && !versionB.prerelease) {
      return -1; // Prerelease is less than release
    }
    if (!versionA.prerelease && versionB.prerelease) {
      return 1;
    }
    if (versionA.prerelease && versionB.prerelease) {
      return SafeVersion.comparePrereleases(versionA.prerelease, versionB.prerelease);
    }

    return 0;
  }

  /**
   * Compare prerelease identifiers.
   *
   * @param {string} prereleaseA - First prerelease
   * @param {string} prereleaseB - Second prerelease
   * @returns {number}
   */
  static comparePrereleases(prereleaseA, prereleaseB) {
    const partsA = prereleaseA.split('.');
    const partsB = prereleaseB.split('.');

    const minLen = Math.min(partsA.length, partsB.length);
    for (let partIndex = 0; partIndex < minLen; partIndex++) {
      const partA = partsA[partIndex];
      const partB = partsB[partIndex];

      const numA = /^\d+$/.test(partA) ? parseInt(partA, 10) : null;
      const numB = /^\d+$/.test(partB) ? parseInt(partB, 10) : null;

      // Numeric identifiers always have lower precedence than alphanumeric
      if (numA !== null && numB === null) return -1;
      if (numA === null && numB !== null) return 1;

      if (numA !== null && numB !== null) {
        if (numA !== numB) {
          return numA < numB ? -1 : 1;
        }
      } else {
        // Both are strings
        if (partA !== partB) {
          return partA < partB ? -1 : 1;
        }
      }
    }

    // Longer prerelease has higher precedence
    if (partsA.length !== partsB.length) {
      return partsA.length < partsB.length ? -1 : 1;
    }

    return 0;
  }

  /**
   * Check if version satisfies a constraint.
   *
   * @param {Version} version - Version to check
   * @param {string} constraint - Constraint (e.g., "^1.0.0", "~1.2.0", ">=1.0.0")
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static satisfies(version, constraint) {
    const trimmed = constraint.trim();

    // Exact match
    if (!trimmed.startsWith('^') && !trimmed.startsWith('~') && !trimmed.startsWith('>') && !trimmed.startsWith('<')) {
      const other = SafeVersion.parse(trimmed);
      if (!other.ok) {
        return other;
      }
      return ok(SafeVersion.compare(version, other.value) === 0);
    }

    // Caret (^) - compatible with version
    if (trimmed.startsWith('^')) {
      const other = SafeVersion.parse(trimmed.slice(1));
      if (!other.ok) {
        return other;
      }
      // ^1.2.3 means >=1.2.3 <2.0.0
      // ^0.2.3 means >=0.2.3 <0.3.0
      // ^0.0.3 means >=0.0.3 <0.0.4
      const cmp = SafeVersion.compare(version, other.value);
      if (cmp < 0) return ok(false);

      if (other.value.major !== 0) {
        return ok(version.major === other.value.major);
      }
      if (other.value.minor !== 0) {
        return ok(version.major === 0 && version.minor === other.value.minor);
      }
      return ok(version.major === 0 && version.minor === 0 && version.patch === other.value.patch);
    }

    // Tilde (~) - approximately equivalent
    if (trimmed.startsWith('~')) {
      const other = SafeVersion.parse(trimmed.slice(1));
      if (!other.ok) {
        return other;
      }
      // ~1.2.3 means >=1.2.3 <1.3.0
      const cmp = SafeVersion.compare(version, other.value);
      if (cmp < 0) return ok(false);
      return ok(version.major === other.value.major && version.minor === other.value.minor);
    }

    // Greater than or equal
    if (trimmed.startsWith('>=')) {
      const other = SafeVersion.parse(trimmed.slice(2));
      if (!other.ok) {
        return other;
      }
      return ok(SafeVersion.compare(version, other.value) >= 0);
    }

    // Less than or equal
    if (trimmed.startsWith('<=')) {
      const other = SafeVersion.parse(trimmed.slice(2));
      if (!other.ok) {
        return other;
      }
      return ok(SafeVersion.compare(version, other.value) <= 0);
    }

    // Greater than
    if (trimmed.startsWith('>')) {
      const other = SafeVersion.parse(trimmed.slice(1));
      if (!other.ok) {
        return other;
      }
      return ok(SafeVersion.compare(version, other.value) > 0);
    }

    // Less than
    if (trimmed.startsWith('<')) {
      const other = SafeVersion.parse(trimmed.slice(1));
      if (!other.ok) {
        return other;
      }
      return ok(SafeVersion.compare(version, other.value) < 0);
    }

    return err(`Unknown constraint format: ${constraint}`);
  }

  /**
   * Increment version.
   *
   * @param {Version} version - Version to increment
   * @param {'major' | 'minor' | 'patch'} type - Increment type
   * @returns {Version}
   */
  static increment(version, type) {
    switch (type) {
      case 'major':
        return { major: version.major + 1, minor: 0, patch: 0, prerelease: null, build: null };
      case 'minor':
        return { major: version.major, minor: version.minor + 1, patch: 0, prerelease: null, build: null };
      case 'patch':
        return { major: version.major, minor: version.minor, patch: version.patch + 1, prerelease: null, build: null };
      default:
        return version;
    }
  }

  /**
   * Get maximum version from a list.
   *
   * @param {Version[]} versions - List of versions
   * @returns {Version | null}
   */
  static max(versions) {
    if (versions.length === 0) {
      return null;
    }
    return versions.reduce((maxVersion, ver) => (SafeVersion.compare(ver, maxVersion) > 0 ? ver : maxVersion));
  }

  /**
   * Get minimum version from a list.
   *
   * @param {Version[]} versions - List of versions
   * @returns {Version | null}
   */
  static min(versions) {
    if (versions.length === 0) {
      return null;
    }
    return versions.reduce((minVersion, ver) => (SafeVersion.compare(ver, minVersion) < 0 ? ver : minVersion));
  }
}
