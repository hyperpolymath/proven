// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe semantic versioning parsing and comparison.
 * Follows Semantic Versioning 2.0.0 specification.
 */
module proven.safe_version;

import std.array : appender, split;
import std.conv : to, ConvException;
import std.string : strip;
import std.typecons : Nullable, nullable;

/// Semantic version.
struct SemVer
{
    ulong major;
    ulong minor;
    ulong patch;
    Nullable!string prerelease;
    Nullable!string buildMetadata;

    /// Create a new version.
    static SemVer opCall(ulong major, ulong minor, ulong patch) pure nothrow @safe @nogc
    {
        SemVer v;
        v.major = major;
        v.minor = minor;
        v.patch = patch;
        return v;
    }

    /// Create with prerelease.
    SemVer withPrerelease(string prereleaseStr) const pure @safe
    {
        SemVer v = this;
        v.prerelease = nullable(prereleaseStr);
        return v;
    }

    /// Create with build metadata.
    SemVer withBuild(string build) const pure @safe
    {
        SemVer v = this;
        v.buildMetadata = nullable(build);
        return v;
    }

    /// Format as string.
    string toString() const pure @safe
    {
        import std.format : format;

        auto result = appender!string;
        result ~= format!"%d.%d.%d"(major, minor, patch);

        if (!prerelease.isNull)
        {
            result ~= '-';
            result ~= prerelease.get;
        }
        if (!buildMetadata.isNull)
        {
            result ~= '+';
            result ~= buildMetadata.get;
        }
        return result[];
    }

    /// Check if this is a prerelease version.
    bool isPrerelease() const pure nothrow @safe @nogc
    {
        return !prerelease.isNull;
    }

    /// Check if this is a stable release (>= 1.0.0 and no prerelease).
    bool isStable() const pure nothrow @safe @nogc
    {
        return major >= 1 && prerelease.isNull;
    }

    /// Increment major version.
    SemVer bumpMajor() const pure nothrow @safe @nogc
    {
        return SemVer(major + 1, 0, 0);
    }

    /// Increment minor version.
    SemVer bumpMinor() const pure nothrow @safe @nogc
    {
        return SemVer(major, minor + 1, 0);
    }

    /// Increment patch version.
    SemVer bumpPatch() const pure nothrow @safe @nogc
    {
        return SemVer(major, minor, patch + 1);
    }

    /// Compare two versions.
    int opCmp(const SemVer other) const pure nothrow @safe
    {
        // Compare major.minor.patch
        if (major != other.major)
            return major < other.major ? -1 : 1;
        if (minor != other.minor)
            return minor < other.minor ? -1 : 1;
        if (patch != other.patch)
            return patch < other.patch ? -1 : 1;

        // Prerelease comparison
        if (prerelease.isNull && !other.prerelease.isNull)
            return 1; // No prerelease > prerelease
        if (!prerelease.isNull && other.prerelease.isNull)
            return -1;
        if (!prerelease.isNull && !other.prerelease.isNull)
        {
            if (prerelease.get < other.prerelease.get)
                return -1;
            if (prerelease.get > other.prerelease.get)
                return 1;
        }

        // Build metadata is ignored in comparison per SemVer spec
        return 0;
    }

    /// Equality comparison.
    bool opEquals(const SemVer other) const pure nothrow @safe
    {
        return opCmp(other) == 0;
    }
}

/// Version parsing result.
struct VersionResult
{
    SemVer semver;
    string error;
    bool ok;

    static VersionResult success(SemVer v)
    {
        return VersionResult(v, "", true);
    }

    static VersionResult failure(string error)
    {
        return VersionResult(SemVer.init, error, false);
    }
}

/// Parse a version string.
VersionResult parseVersion(string versionString) pure @safe
{
    auto s = versionString.strip();

    // Remove leading 'v' if present
    if (s.length > 0 && (s[0] == 'v' || s[0] == 'V'))
        s = s[1 .. $];

    if (s.length == 0)
        return VersionResult.failure("Empty version string");

    // Split off build metadata
    Nullable!string buildMeta;
    auto plusIdx = indexOf(s, '+');
    if (plusIdx >= 0)
    {
        buildMeta = nullable(s[plusIdx + 1 .. $]);
        s = s[0 .. plusIdx];
    }

    // Split off prerelease
    Nullable!string prereleaseStr;
    auto dashIdx = indexOf(s, '-');
    if (dashIdx >= 0)
    {
        prereleaseStr = nullable(s[dashIdx + 1 .. $]);
        s = s[0 .. dashIdx];
    }

    // Parse major.minor.patch
    auto parts = s.split('.');
    if (parts.length != 3)
        return VersionResult.failure("Version must have major.minor.patch");

    try
    {
        immutable major = parts[0].to!ulong;
        immutable minor = parts[1].to!ulong;
        immutable patch = parts[2].to!ulong;

        SemVer v = SemVer(major, minor, patch);
        v.prerelease = prereleaseStr;
        v.buildMetadata = buildMeta;

        return VersionResult.success(v);
    }
    catch (ConvException)
    {
        return VersionResult.failure("Invalid version number");
    }
}

/// Check if version satisfies a constraint.
bool satisfies(const SemVer semver, string constraint) pure @safe
{
    auto trimmed = constraint.strip();
    if (trimmed.length == 0)
        return false;

    // Check for comparison operators
    if (trimmed.length >= 2 && trimmed[0 .. 2] == ">=")
    {
        auto targetResult = parseVersion(trimmed[2 .. $]);
        if (!targetResult.ok)
            return false;
        return semver >= targetResult.semver;
    }
    if (trimmed.length >= 2 && trimmed[0 .. 2] == "<=")
    {
        auto targetResult = parseVersion(trimmed[2 .. $]);
        if (!targetResult.ok)
            return false;
        return semver <= targetResult.semver;
    }
    if (trimmed[0] == '>')
    {
        auto targetResult = parseVersion(trimmed[1 .. $]);
        if (!targetResult.ok)
            return false;
        return semver > targetResult.semver;
    }
    if (trimmed[0] == '<')
    {
        auto targetResult = parseVersion(trimmed[1 .. $]);
        if (!targetResult.ok)
            return false;
        return semver < targetResult.semver;
    }
    if (trimmed[0] == '=')
    {
        auto targetResult = parseVersion(trimmed[1 .. $]);
        if (!targetResult.ok)
            return false;
        return semver == targetResult.semver;
    }
    if (trimmed[0] == '^')
    {
        // Caret: compatible with version (same major, if major > 0)
        auto targetResult = parseVersion(trimmed[1 .. $]);
        if (!targetResult.ok)
            return false;
        immutable target = targetResult.semver;
        if (target.major == 0)
            return semver.major == 0 && semver.minor == target.minor && semver >= target;
        return semver.major == target.major && semver >= target;
    }
    if (trimmed[0] == '~')
    {
        // Tilde: same major.minor
        auto targetResult = parseVersion(trimmed[1 .. $]);
        if (!targetResult.ok)
            return false;
        immutable target = targetResult.semver;
        return semver.major == target.major && semver.minor == target.minor && semver >= target;
    }

    // Exact match
    auto targetResult = parseVersion(trimmed);
    if (!targetResult.ok)
        return false;
    return semver == targetResult.semver;
}

/// Check if version string is valid.
bool isValidVersion(string versionString) pure @safe
{
    return parseVersion(versionString).ok;
}

/// Find index of character in string.
private ptrdiff_t indexOf(string s, char c) pure nothrow @safe @nogc
{
    foreach (i, ch; s)
    {
        if (ch == c)
            return i;
    }
    return -1;
}

// Unit tests
unittest
{
    // Test parsing
    auto v = parseVersion("1.2.3");
    assert(v.ok);
    assert(v.semver.major == 1);
    assert(v.semver.minor == 2);
    assert(v.semver.patch == 3);

    // Test with prerelease and build
    auto v2 = parseVersion("1.2.3-alpha.1+build.123");
    assert(v2.ok);
    assert(v2.semver.prerelease.get == "alpha.1");
    assert(v2.semver.buildMetadata.get == "build.123");

    // Test with leading v
    auto v3 = parseVersion("v2.0.0");
    assert(v3.ok);
    assert(v3.semver.major == 2);

    // Test comparison
    assert(parseVersion("1.0.0").semver > parseVersion("0.9.9").semver);
    assert(parseVersion("1.0.0").semver > parseVersion("1.0.0-alpha").semver);
    assert(parseVersion("1.0.0-beta").semver > parseVersion("1.0.0-alpha").semver);

    // Test satisfies
    auto testVer = parseVersion("1.2.3").semver;
    assert(satisfies(testVer, ">=1.0.0"));
    assert(satisfies(testVer, "^1.0.0"));
    assert(satisfies(testVer, "~1.2.0"));
    assert(!satisfies(testVer, ">=2.0.0"));

    // Test bumping
    auto bumped = SemVer(1, 2, 3).bumpMajor();
    assert(bumped.major == 2);
    assert(bumped.minor == 0);
    assert(bumped.patch == 0);

    // Test stable check
    assert(SemVer(1, 0, 0).isStable());
    assert(!SemVer(0, 9, 9).isStable());
    assert(!parseVersion("1.0.0-alpha").semver.isStable());
}
