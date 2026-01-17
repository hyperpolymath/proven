<?php
// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Semantic version following SemVer 2.0.0 specification.
 */
readonly class Version implements \Stringable
{
    public function __construct(
        public int $major,
        public int $minor,
        public int $patch,
        public ?string $prerelease = null,
        public ?string $buildMetadata = null,
    ) {}

    /**
     * Parse a version string.
     *
     * @return Result<Version>
     */
    public static function parse(string $s): Result
    {
        $s = trim($s);

        // Remove 'v' prefix if present
        if (str_starts_with(strtolower($s), 'v')) {
            $s = substr($s, 1);
        }

        // Split off build metadata
        $build = null;
        $plusPos = strpos($s, '+');
        if ($plusPos !== false) {
            $build = substr($s, $plusPos + 1);
            $s = substr($s, 0, $plusPos);
        }

        // Split off prerelease
        $prerelease = null;
        $dashPos = strpos($s, '-');
        if ($dashPos !== false) {
            $prerelease = substr($s, $dashPos + 1);
            $s = substr($s, 0, $dashPos);
        }

        // Parse major.minor.patch
        $parts = explode('.', $s);
        if (count($parts) !== 3) {
            return Result::err(ProvenError::invalidInput('Version must have major.minor.patch'));
        }

        $major = filter_var($parts[0], FILTER_VALIDATE_INT);
        $minor = filter_var($parts[1], FILTER_VALIDATE_INT);
        $patch = filter_var($parts[2], FILTER_VALIDATE_INT);

        if ($major === false || $minor === false || $patch === false) {
            return Result::err(ProvenError::invalidInput('Invalid version components'));
        }

        if ($major < 0 || $minor < 0 || $patch < 0) {
            return Result::err(ProvenError::invalidInput('Version components cannot be negative'));
        }

        return Result::ok(new self($major, $minor, $patch, $prerelease, $build));
    }

    /**
     * Create a new version.
     */
    public static function new(int $major, int $minor, int $patch): self
    {
        return new self($major, $minor, $patch);
    }

    /**
     * Create with prerelease.
     */
    public function withPrerelease(string $prerelease): self
    {
        return new self($this->major, $this->minor, $this->patch, $prerelease, $this->buildMetadata);
    }

    /**
     * Create with build metadata.
     */
    public function withBuild(string $build): self
    {
        return new self($this->major, $this->minor, $this->patch, $this->prerelease, $build);
    }

    /**
     * Format as string.
     */
    public function format(): string
    {
        $s = sprintf('%d.%d.%d', $this->major, $this->minor, $this->patch);
        if ($this->prerelease !== null) {
            $s .= '-' . $this->prerelease;
        }
        if ($this->buildMetadata !== null) {
            $s .= '+' . $this->buildMetadata;
        }
        return $s;
    }

    /**
     * Check if this is a prerelease version.
     */
    public function isPrerelease(): bool
    {
        return $this->prerelease !== null;
    }

    /**
     * Check if this is a stable release (>= 1.0.0 and no prerelease).
     */
    public function isStable(): bool
    {
        return $this->major >= 1 && $this->prerelease === null;
    }

    /**
     * Increment major version.
     */
    public function bumpMajor(): self
    {
        return new self($this->major + 1, 0, 0);
    }

    /**
     * Increment minor version.
     */
    public function bumpMinor(): self
    {
        return new self($this->major, $this->minor + 1, 0);
    }

    /**
     * Increment patch version.
     */
    public function bumpPatch(): self
    {
        return new self($this->major, $this->minor, $this->patch + 1);
    }

    /**
     * Compare to another version.
     *
     * @return int -1, 0, or 1
     */
    public function compare(self $other): int
    {
        // Compare major.minor.patch
        if ($this->major !== $other->major) {
            return $this->major <=> $other->major;
        }
        if ($this->minor !== $other->minor) {
            return $this->minor <=> $other->minor;
        }
        if ($this->patch !== $other->patch) {
            return $this->patch <=> $other->patch;
        }

        // Prerelease comparison (no prerelease > prerelease)
        if ($this->prerelease === null && $other->prerelease !== null) {
            return 1;
        }
        if ($this->prerelease !== null && $other->prerelease === null) {
            return -1;
        }
        if ($this->prerelease !== null && $other->prerelease !== null) {
            return strcmp($this->prerelease, $other->prerelease);
        }

        // Build metadata is ignored per SemVer spec
        return 0;
    }

    /**
     * Check equality (ignoring build metadata per SemVer spec).
     */
    public function equals(self $other): bool
    {
        return $this->compare($other) === 0;
    }

    /**
     * Check if this version is greater than another.
     */
    public function greaterThan(self $other): bool
    {
        return $this->compare($other) > 0;
    }

    /**
     * Check if this version is less than another.
     */
    public function lessThan(self $other): bool
    {
        return $this->compare($other) < 0;
    }

    /**
     * Check if this version is greater than or equal to another.
     */
    public function greaterThanOrEqual(self $other): bool
    {
        return $this->compare($other) >= 0;
    }

    /**
     * Check if this version is less than or equal to another.
     */
    public function lessThanOrEqual(self $other): bool
    {
        return $this->compare($other) <= 0;
    }

    public function __toString(): string
    {
        return $this->format();
    }
}

/**
 * Safe semantic versioning operations.
 */
class SafeVersion
{
    /**
     * Parse a version string.
     *
     * @return Result<Version>
     */
    public static function parse(string $s): Result
    {
        return Version::parse($s);
    }

    /**
     * Check if a string is a valid version.
     */
    public static function isValid(string $s): bool
    {
        return Version::parse($s)->isOk();
    }

    /**
     * Check if a version satisfies a constraint.
     *
     * Supports constraints:
     * - >=1.0.0, <=1.0.0, >1.0.0, <1.0.0, =1.0.0
     * - ^1.0.0 (caret - compatible with major version)
     * - ~1.0.0 (tilde - same major.minor)
     *
     * @return Result<bool>
     */
    public static function satisfies(Version $version, string $constraint): Result
    {
        $constraint = trim($constraint);

        if (str_starts_with($constraint, '>=')) {
            $targetResult = Version::parse(substr($constraint, 2));
            if ($targetResult->isErr()) {
                return $targetResult;
            }
            return Result::ok($version->greaterThanOrEqual($targetResult->unwrap()));
        }

        if (str_starts_with($constraint, '<=')) {
            $targetResult = Version::parse(substr($constraint, 2));
            if ($targetResult->isErr()) {
                return $targetResult;
            }
            return Result::ok($version->lessThanOrEqual($targetResult->unwrap()));
        }

        if (str_starts_with($constraint, '>')) {
            $targetResult = Version::parse(substr($constraint, 1));
            if ($targetResult->isErr()) {
                return $targetResult;
            }
            return Result::ok($version->greaterThan($targetResult->unwrap()));
        }

        if (str_starts_with($constraint, '<')) {
            $targetResult = Version::parse(substr($constraint, 1));
            if ($targetResult->isErr()) {
                return $targetResult;
            }
            return Result::ok($version->lessThan($targetResult->unwrap()));
        }

        if (str_starts_with($constraint, '=')) {
            $targetResult = Version::parse(substr($constraint, 1));
            if ($targetResult->isErr()) {
                return $targetResult;
            }
            return Result::ok($version->equals($targetResult->unwrap()));
        }

        if (str_starts_with($constraint, '^')) {
            // Caret: compatible with major version (if major > 0)
            $targetResult = Version::parse(substr($constraint, 1));
            if ($targetResult->isErr()) {
                return $targetResult;
            }
            $target = $targetResult->unwrap();

            if ($target->major === 0) {
                // For 0.x.y, same minor required
                $compatible = $version->major === 0
                    && $version->minor === $target->minor
                    && $version->greaterThanOrEqual($target);
            } else {
                $compatible = $version->major === $target->major
                    && $version->greaterThanOrEqual($target);
            }
            return Result::ok($compatible);
        }

        if (str_starts_with($constraint, '~')) {
            // Tilde: same major.minor
            $targetResult = Version::parse(substr($constraint, 1));
            if ($targetResult->isErr()) {
                return $targetResult;
            }
            $target = $targetResult->unwrap();

            $compatible = $version->major === $target->major
                && $version->minor === $target->minor
                && $version->greaterThanOrEqual($target);
            return Result::ok($compatible);
        }

        // Exact match
        $targetResult = Version::parse($constraint);
        if ($targetResult->isErr()) {
            return $targetResult;
        }
        return Result::ok($version->equals($targetResult->unwrap()));
    }

    /**
     * Compare two version strings.
     *
     * @return Result<int>
     */
    public static function compare(string $a, string $b): Result
    {
        $versionA = Version::parse($a);
        if ($versionA->isErr()) {
            return $versionA;
        }

        $versionB = Version::parse($b);
        if ($versionB->isErr()) {
            return $versionB;
        }

        return Result::ok($versionA->unwrap()->compare($versionB->unwrap()));
    }

    /**
     * Get the maximum version from a list.
     *
     * @param array<string> $versions
     * @return Option<Version>
     */
    public static function max(array $versions): Option
    {
        $max = null;
        foreach ($versions as $versionStr) {
            $version = Version::parse($versionStr);
            if ($version->isErr()) {
                continue;
            }
            $v = $version->unwrap();
            if ($max === null || $v->greaterThan($max)) {
                $max = $v;
            }
        }
        return Option::fromNullable($max);
    }

    /**
     * Get the minimum version from a list.
     *
     * @param array<string> $versions
     * @return Option<Version>
     */
    public static function min(array $versions): Option
    {
        $min = null;
        foreach ($versions as $versionStr) {
            $version = Version::parse($versionStr);
            if ($version->isErr()) {
                continue;
            }
            $v = $version->unwrap();
            if ($min === null || $v->lessThan($min)) {
                $min = $v;
            }
        }
        return Option::fromNullable($min);
    }

    /**
     * Sort versions (ascending).
     *
     * @param array<string> $versions
     * @return array<Version>
     */
    public static function sort(array $versions): array
    {
        $parsed = [];
        foreach ($versions as $versionStr) {
            $result = Version::parse($versionStr);
            if ($result->isOk()) {
                $parsed[] = $result->unwrap();
            }
        }

        usort($parsed, fn(Version $a, Version $b) => $a->compare($b));
        return $parsed;
    }
}
