// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * SemVer represents a semantic version.
 */
export class SemVer {
  readonly major: number;
  readonly minor: number;
  readonly patch: number;
  readonly prerelease: string[];
  readonly build: string[];

  private constructor(
    major: number,
    minor: number,
    patch: number,
    prerelease: string[] = [],
    build: string[] = []
  ) {
    this.major = major;
    this.minor = minor;
    this.patch = patch;
    this.prerelease = prerelease;
    this.build = build;
  }

  /**
   * Create a version.
   */
  static create(major: number, minor: number, patch: number): Result<SemVer> {
    if (major < 0 || minor < 0 || patch < 0) {
      return { ok: false, error: 'Version numbers cannot be negative' };
    }
    if (!Number.isInteger(major) || !Number.isInteger(minor) || !Number.isInteger(patch)) {
      return { ok: false, error: 'Version numbers must be integers' };
    }
    return { ok: true, value: new SemVer(major, minor, patch) };
  }

  /**
   * Parse a version string.
   */
  static parse(version: string): Result<SemVer> {
    const trimmed = version.trim();
    const withoutV = trimmed.startsWith('v') ? trimmed.slice(1) : trimmed;

    // Split build metadata
    const [versionAndPre, ...buildParts] = withoutV.split('+');
    const build = buildParts.length > 0 ? buildParts.join('+').split('.') : [];

    // Split prerelease
    const [coreVersion, ...preParts] = versionAndPre.split('-');
    const prerelease = preParts.length > 0 ? preParts.join('-').split('.') : [];

    // Parse core version
    const parts = coreVersion.split('.');
    if (parts.length < 1 || parts.length > 3) {
      return { ok: false, error: 'Invalid version format' };
    }

    const major = parseInt(parts[0], 10);
    const minor = parts.length > 1 ? parseInt(parts[1], 10) : 0;
    const patch = parts.length > 2 ? parseInt(parts[2], 10) : 0;

    if (isNaN(major) || isNaN(minor) || isNaN(patch)) {
      return { ok: false, error: 'Version parts must be numbers' };
    }

    if (major < 0 || minor < 0 || patch < 0) {
      return { ok: false, error: 'Version numbers cannot be negative' };
    }

    return { ok: true, value: new SemVer(major, minor, patch, prerelease, build) };
  }

  /**
   * Convert to string.
   */
  toString(): string {
    let result = `${this.major}.${this.minor}.${this.patch}`;
    if (this.prerelease.length > 0) {
      result += `-${this.prerelease.join('.')}`;
    }
    if (this.build.length > 0) {
      result += `+${this.build.join('.')}`;
    }
    return result;
  }

  /**
   * Compare with another version.
   * Returns -1 if this < other, 0 if equal, 1 if this > other.
   */
  compare(other: SemVer): number {
    // Compare major.minor.patch
    if (this.major !== other.major) {
      return this.major < other.major ? -1 : 1;
    }
    if (this.minor !== other.minor) {
      return this.minor < other.minor ? -1 : 1;
    }
    if (this.patch !== other.patch) {
      return this.patch < other.patch ? -1 : 1;
    }

    // Prerelease versions have lower precedence
    if (this.prerelease.length === 0 && other.prerelease.length > 0) {
      return 1;
    }
    if (this.prerelease.length > 0 && other.prerelease.length === 0) {
      return -1;
    }

    // Compare prerelease identifiers
    const minLen = Math.min(this.prerelease.length, other.prerelease.length);
    for (let i = 0; i < minLen; i++) {
      const a = this.prerelease[i];
      const b = other.prerelease[i];
      const aNum = parseInt(a, 10);
      const bNum = parseInt(b, 10);

      if (!isNaN(aNum) && !isNaN(bNum)) {
        if (aNum !== bNum) return aNum < bNum ? -1 : 1;
      } else if (!isNaN(aNum)) {
        return -1; // Numeric < alphanumeric
      } else if (!isNaN(bNum)) {
        return 1;
      } else {
        if (a < b) return -1;
        if (a > b) return 1;
      }
    }

    if (this.prerelease.length !== other.prerelease.length) {
      return this.prerelease.length < other.prerelease.length ? -1 : 1;
    }

    return 0;
  }

  /**
   * Check if equal to another version.
   */
  equals(other: SemVer): boolean {
    return this.compare(other) === 0;
  }

  /**
   * Check if greater than another version.
   */
  greaterThan(other: SemVer): boolean {
    return this.compare(other) > 0;
  }

  /**
   * Check if less than another version.
   */
  lessThan(other: SemVer): boolean {
    return this.compare(other) < 0;
  }

  /**
   * Check if greater than or equal to another version.
   */
  greaterOrEqual(other: SemVer): boolean {
    return this.compare(other) >= 0;
  }

  /**
   * Check if less than or equal to another version.
   */
  lessOrEqual(other: SemVer): boolean {
    return this.compare(other) <= 0;
  }

  /**
   * Increment major version.
   */
  bumpMajor(): SemVer {
    return new SemVer(this.major + 1, 0, 0);
  }

  /**
   * Increment minor version.
   */
  bumpMinor(): SemVer {
    return new SemVer(this.major, this.minor + 1, 0);
  }

  /**
   * Increment patch version.
   */
  bumpPatch(): SemVer {
    return new SemVer(this.major, this.minor, this.patch + 1);
  }

  /**
   * Add prerelease identifier.
   */
  withPrerelease(prerelease: string[]): SemVer {
    return new SemVer(this.major, this.minor, this.patch, prerelease, this.build);
  }

  /**
   * Add build metadata.
   */
  withBuild(build: string[]): SemVer {
    return new SemVer(this.major, this.minor, this.patch, this.prerelease, build);
  }

  /**
   * Check if this is a prerelease version.
   */
  isPrerelease(): boolean {
    return this.prerelease.length > 0;
  }

  /**
   * Check if this is a stable version (>= 1.0.0 and no prerelease).
   */
  isStable(): boolean {
    return this.major >= 1 && !this.isPrerelease();
  }

  /**
   * Check if this satisfies a version range.
   */
  satisfies(range: string): boolean {
    const ops = range.match(/^([<>=^~]*)(.+)$/);
    if (!ops) return false;

    const operator = ops[1] || '=';
    const versionStr = ops[2];
    const targetResult = SemVer.parse(versionStr);
    if (!targetResult.ok || !targetResult.value) return false;
    const target = targetResult.value;

    switch (operator) {
      case '':
      case '=':
        return this.equals(target);
      case '>':
        return this.greaterThan(target);
      case '>=':
        return this.greaterOrEqual(target);
      case '<':
        return this.lessThan(target);
      case '<=':
        return this.lessOrEqual(target);
      case '^':
        // Compatible with (same major for >= 1.0.0, same major.minor for 0.x)
        if (target.major === 0) {
          return this.major === 0 && this.minor === target.minor && this.greaterOrEqual(target);
        }
        return this.major === target.major && this.greaterOrEqual(target);
      case '~':
        // Approximately equivalent (same major.minor)
        return this.major === target.major && this.minor === target.minor && this.greaterOrEqual(target);
      default:
        return false;
    }
  }
}

/**
 * Sort versions in ascending order.
 */
export function sortVersions(versions: SemVer[]): SemVer[] {
  return [...versions].sort((a, b) => a.compare(b));
}

/**
 * Get the maximum version from a list.
 */
export function maxVersion(versions: SemVer[]): SemVer | undefined {
  if (versions.length === 0) return undefined;
  return versions.reduce((max, v) => (v.greaterThan(max) ? v : max));
}

/**
 * Get the minimum version from a list.
 */
export function minVersion(versions: SemVer[]): SemVer | undefined {
  if (versions.length === 0) return undefined;
  return versions.reduce((min, v) => (v.lessThan(min) ? v : min));
}

export const SafeVersion = {
  SemVer,
  sortVersions,
  maxVersion,
  minVersion,
};
