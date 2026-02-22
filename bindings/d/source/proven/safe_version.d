// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe semantic versioning parsing and comparison.
 *
 * Thin FFI wrapper around libproven's SafeVersion module. Semantic
 * version parsing and comparison are performed in formally verified
 * Idris 2 code. This module only marshals data to/from the C ABI.
 */
module proven.safe_version;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Semantic version.
struct SemVer
{
    uint major;
    uint minor;
    uint patch;
    Nullable!string prerelease;

    /// Format as string (e.g., "1.2.3" or "1.2.3-alpha").
    string toString() const pure @safe
    {
        import std.format : format;
        import std.array : appender;

        auto result = appender!string;
        result ~= format!"%d.%d.%d"(major, minor, patch);
        if (!prerelease.isNull)
        {
            result ~= '-';
            result ~= prerelease.get;
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

/// Parse a semantic version string (e.g., "1.2.3-alpha").
/// Supports optional 'v' or 'V' prefix.
VersionResult parseVersion(string versionString) @trusted nothrow
{
    if (versionString.length == 0)
        return VersionResult.failure("Empty version string");

    auto result = proven_version_parse(
        cast(const(ubyte)*) versionString.ptr, versionString.length
    );

    if (provenFailed(result.status))
        return VersionResult.failure("Invalid version");

    auto v = result.version_;
    SemVer semver;
    semver.major = v.major;
    semver.minor = v.minor;
    semver.patch = v.patch;

    if (v.prerelease !is null && v.prerelease_len > 0)
        semver.prerelease = nullable(v.prerelease[0 .. v.prerelease_len].idup);

    // Free the C-allocated prerelease string
    proven_version_free(&result.version_);

    return VersionResult.success(semver);
}

/// Compare two semantic versions via libproven.
/// Returns negative if a < b, 0 if equal, positive if a > b.
int compareVersions(SemVer a, SemVer b) @trusted nothrow
{
    // Build ProvenSemanticVersion structs
    ProvenSemanticVersion pa;
    pa.major = a.major;
    pa.minor = a.minor;
    pa.patch = a.patch;
    pa.prerelease_len = 0;
    pa.prerelease = null;

    ProvenSemanticVersion pb;
    pb.major = b.major;
    pb.minor = b.minor;
    pb.patch = b.patch;
    pb.prerelease_len = 0;
    pb.prerelease = null;

    // We need to handle prerelease strings carefully since they are
    // D strings (GC managed) being passed to C. The C function only
    // reads them, so casting is safe for the duration of the call.
    if (!a.prerelease.isNull)
    {
        auto preA = a.prerelease.get;
        pa.prerelease = cast(char*) preA.ptr;
        pa.prerelease_len = preA.length;
    }

    if (!b.prerelease.isNull)
    {
        auto preB = b.prerelease.get;
        pb.prerelease = cast(char*) preB.ptr;
        pb.prerelease_len = preB.length;
    }

    return proven_version_compare(pa, pb);
}

/// Check if version string is valid.
bool isValidVersion(string versionString) @trusted nothrow
{
    return parseVersion(versionString).ok;
}
