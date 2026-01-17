// SPDX-License-Identifier: PMPL-1.0
/**
 * Safe semantic version parsing and comparison.
 */

import { err, ok, type Result } from './result.ts';

/** Semantic version components. */
export interface SemVer {
  readonly major: number;
  readonly minor: number;
  readonly patch: number;
  readonly prerelease: readonly string[];
  readonly buildMetadata: readonly string[];
}

/** Prerelease type. */
export type PrereleaseType = 'alpha' | 'beta' | 'rc' | 'dev' | 'snapshot' | 'other';

/** Version comparison result. */
export type VersionCompare = -1 | 0 | 1;

/** Version regex per SemVer 2.0.0 spec. */
const SEMVER_REGEX =
  /^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$/;

/**
 * Safe semantic version operations.
 */
export class SafeVersion {
  /**
   * Parse a semantic version string.
   *
   * @example
   * ```ts
   * const result = SafeVersion.parse("1.2.3-beta.1+build.123");
   * if (result.ok) {
   *   console.log(result.value.major); // 1
   * }
   * ```
   */
  static parse(version: string): Result<SemVer> {
    const trimmed = version.trim();
    const match = SEMVER_REGEX.exec(trimmed);

    if (!match) {
      return err(`Invalid semantic version: ${version}`);
    }

    const [, majorStr, minorStr, patchStr, prereleaseStr, buildStr] = match;

    const major = parseInt(majorStr, 10);
    const minor = parseInt(minorStr, 10);
    const patch = parseInt(patchStr, 10);

    if (major > Number.MAX_SAFE_INTEGER || minor > Number.MAX_SAFE_INTEGER || patch > Number.MAX_SAFE_INTEGER) {
      return err('Version number too large');
    }

    const prerelease = prereleaseStr ? prereleaseStr.split('.') : [];
    const buildMetadata = buildStr ? buildStr.split('.') : [];

    return ok({
      major,
      minor,
      patch,
      prerelease,
      buildMetadata,
    });
  }

  /**
   * Check if a string is a valid semantic version.
   */
  static isValid(version: string): boolean {
    return this.parse(version).ok;
  }

  /**
   * Create a SemVer from components.
   */
  static create(
    major: number,
    minor: number,
    patch: number,
    prerelease: string[] = [],
    buildMetadata: string[] = [],
  ): Result<SemVer> {
    if (major < 0 || minor < 0 || patch < 0) {
      return err('Version numbers cannot be negative');
    }
    if (!Number.isInteger(major) || !Number.isInteger(minor) || !Number.isInteger(patch)) {
      return err('Version numbers must be integers');
    }

    return ok({
      major,
      minor,
      patch,
      prerelease,
      buildMetadata,
    });
  }

  /**
   * Compare two versions.
   * Returns -1 if a < b, 0 if a == b, 1 if a > b.
   *
   * @example
   * ```ts
   * SafeVersion.compare(v1_0_0, v1_1_0); // -1
   * ```
   */
  static compare(a: SemVer, b: SemVer): VersionCompare {
    // Compare major, minor, patch
    if (a.major !== b.major) return a.major < b.major ? -1 : 1;
    if (a.minor !== b.minor) return a.minor < b.minor ? -1 : 1;
    if (a.patch !== b.patch) return a.patch < b.patch ? -1 : 1;

    // Prerelease versions have lower precedence
    if (a.prerelease.length === 0 && b.prerelease.length > 0) return 1;
    if (a.prerelease.length > 0 && b.prerelease.length === 0) return -1;

    // Compare prerelease identifiers
    const maxLen = Math.max(a.prerelease.length, b.prerelease.length);
    for (let i = 0; i < maxLen; i++) {
      if (i >= a.prerelease.length) return -1;
      if (i >= b.prerelease.length) return 1;

      const aId = a.prerelease[i];
      const bId = b.prerelease[i];

      const aNum = parseInt(aId, 10);
      const bNum = parseInt(bId, 10);
      const aIsNum = !isNaN(aNum) && aId === String(aNum);
      const bIsNum = !isNaN(bNum) && bId === String(bNum);

      if (aIsNum && bIsNum) {
        if (aNum !== bNum) return aNum < bNum ? -1 : 1;
      } else if (aIsNum) {
        return -1; // Numeric < alphanumeric
      } else if (bIsNum) {
        return 1;
      } else {
        if (aId < bId) return -1;
        if (aId > bId) return 1;
      }
    }

    return 0;
  }

  /**
   * Check if two versions are equal.
   */
  static equals(a: SemVer, b: SemVer): boolean {
    return this.compare(a, b) === 0;
  }

  /**
   * Check if version a is greater than version b.
   */
  static greaterThan(a: SemVer, b: SemVer): boolean {
    return this.compare(a, b) === 1;
  }

  /**
   * Check if version a is less than version b.
   */
  static lessThan(a: SemVer, b: SemVer): boolean {
    return this.compare(a, b) === -1;
  }

  /**
   * Check if version a is greater than or equal to version b.
   */
  static greaterThanOrEqual(a: SemVer, b: SemVer): boolean {
    return this.compare(a, b) >= 0;
  }

  /**
   * Check if version a is less than or equal to version b.
   */
  static lessThanOrEqual(a: SemVer, b: SemVer): boolean {
    return this.compare(a, b) <= 0;
  }

  /**
   * Format a SemVer to string.
   */
  static format(version: SemVer): string {
    let result = `${version.major}.${version.minor}.${version.patch}`;
    if (version.prerelease.length > 0) {
      result += '-' + version.prerelease.join('.');
    }
    if (version.buildMetadata.length > 0) {
      result += '+' + version.buildMetadata.join('.');
    }
    return result;
  }

  /**
   * Increment the major version.
   */
  static incrementMajor(version: SemVer): SemVer {
    return {
      major: version.major + 1,
      minor: 0,
      patch: 0,
      prerelease: [],
      buildMetadata: [],
    };
  }

  /**
   * Increment the minor version.
   */
  static incrementMinor(version: SemVer): SemVer {
    return {
      major: version.major,
      minor: version.minor + 1,
      patch: 0,
      prerelease: [],
      buildMetadata: [],
    };
  }

  /**
   * Increment the patch version.
   */
  static incrementPatch(version: SemVer): SemVer {
    return {
      major: version.major,
      minor: version.minor,
      patch: version.patch + 1,
      prerelease: [],
      buildMetadata: [],
    };
  }

  /**
   * Check if version is a prerelease.
   */
  static isPrerelease(version: SemVer): boolean {
    return version.prerelease.length > 0;
  }

  /**
   * Get the prerelease type.
   */
  static getPrereleaseType(version: SemVer): PrereleaseType | null {
    if (version.prerelease.length === 0) return null;

    const first = version.prerelease[0].toLowerCase();
    if (first === 'alpha' || first.startsWith('alpha')) return 'alpha';
    if (first === 'beta' || first.startsWith('beta')) return 'beta';
    if (first === 'rc' || first.startsWith('rc')) return 'rc';
    if (first === 'dev' || first.startsWith('dev')) return 'dev';
    if (first === 'snapshot') return 'snapshot';
    return 'other';
  }

  /**
   * Check if version satisfies a simple range.
   * Supports: ^1.2.3, ~1.2.3, >=1.2.3, <=1.2.3, >1.2.3, <1.2.3, =1.2.3
   */
  static satisfies(version: SemVer, range: string): Result<boolean> {
    const trimmed = range.trim();

    if (trimmed.startsWith('^')) {
      const target = this.parse(trimmed.slice(1));
      if (!target.ok) return target;
      // ^1.2.3 means >=1.2.3 <2.0.0 (major must match)
      return ok(
        this.greaterThanOrEqual(version, target.value) &&
          version.major === target.value.major,
      );
    }

    if (trimmed.startsWith('~')) {
      const target = this.parse(trimmed.slice(1));
      if (!target.ok) return target;
      // ~1.2.3 means >=1.2.3 <1.3.0 (major and minor must match)
      return ok(
        this.greaterThanOrEqual(version, target.value) &&
          version.major === target.value.major &&
          version.minor === target.value.minor,
      );
    }

    if (trimmed.startsWith('>=')) {
      const target = this.parse(trimmed.slice(2));
      if (!target.ok) return target;
      return ok(this.greaterThanOrEqual(version, target.value));
    }

    if (trimmed.startsWith('<=')) {
      const target = this.parse(trimmed.slice(2));
      if (!target.ok) return target;
      return ok(this.lessThanOrEqual(version, target.value));
    }

    if (trimmed.startsWith('>')) {
      const target = this.parse(trimmed.slice(1));
      if (!target.ok) return target;
      return ok(this.greaterThan(version, target.value));
    }

    if (trimmed.startsWith('<')) {
      const target = this.parse(trimmed.slice(1));
      if (!target.ok) return target;
      return ok(this.lessThan(version, target.value));
    }

    if (trimmed.startsWith('=')) {
      const target = this.parse(trimmed.slice(1));
      if (!target.ok) return target;
      return ok(this.equals(version, target.value));
    }

    // Exact match
    const target = this.parse(trimmed);
    if (!target.ok) return target;
    return ok(this.equals(version, target.value));
  }

  /**
   * Sort versions in ascending order.
   */
  static sort(versions: SemVer[]): SemVer[] {
    return [...versions].sort((a, b) => this.compare(a, b));
  }

  /**
   * Get the maximum version from an array.
   */
  static max(versions: SemVer[]): SemVer | undefined {
    if (versions.length === 0) return undefined;
    return versions.reduce((max, v) => (this.compare(v, max) === 1 ? v : max));
  }

  /**
   * Get the minimum version from an array.
   */
  static min(versions: SemVer[]): SemVer | undefined {
    if (versions.length === 0) return undefined;
    return versions.reduce((min, v) => (this.compare(v, min) === -1 ? v : min));
  }
}
