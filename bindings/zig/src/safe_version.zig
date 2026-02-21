// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe semantic versioning (SemVer) parsing and comparison.

const std = @import("std");

pub const VersionError = error{
    InvalidVersion,
    InvalidPrerelease,
};

/// Semantic version (major.minor.patch).
pub const SemVer = struct {
    major: u32,
    minor: u32,
    patch: u32,
    prerelease: ?[]const u8 = null,
    build: ?[]const u8 = null,

    /// Parse a version string (e.g., "1.2.3", "1.2.3-alpha", "1.2.3+build").
    pub fn parse(str: []const u8) VersionError!SemVer {
        var version = SemVer{ .major = 0, .minor = 0, .patch = 0 };
        var remaining = str;

        // Parse major
        const major_end = std.mem.indexOfAny(u8, remaining, ".-+") orelse remaining.len;
        version.major = std.fmt.parseInt(u32, remaining[0..major_end], 10) catch return error.InvalidVersion;
        if (major_end >= remaining.len) return version;
        remaining = remaining[major_end..];

        if (remaining[0] != '.') return version;
        remaining = remaining[1..];

        // Parse minor
        const minor_end = std.mem.indexOfAny(u8, remaining, ".-+") orelse remaining.len;
        version.minor = std.fmt.parseInt(u32, remaining[0..minor_end], 10) catch return error.InvalidVersion;
        if (minor_end >= remaining.len) return version;
        remaining = remaining[minor_end..];

        if (remaining[0] != '.') return version;
        remaining = remaining[1..];

        // Parse patch
        const patch_end = std.mem.indexOfAny(u8, remaining, "-+") orelse remaining.len;
        version.patch = std.fmt.parseInt(u32, remaining[0..patch_end], 10) catch return error.InvalidVersion;

        return version;
    }

    /// Compare two versions (-1 = less, 0 = equal, 1 = greater).
    pub fn compare(self: SemVer, other: SemVer) i32 {
        if (self.major != other.major) return if (self.major < other.major) -1 else 1;
        if (self.minor != other.minor) return if (self.minor < other.minor) -1 else 1;
        if (self.patch != other.patch) return if (self.patch < other.patch) -1 else 1;
        return 0;
    }

    /// Check if this version is compatible with another (same major, >= minor.patch).
    pub fn isCompatible(self: SemVer, other: SemVer) bool {
        if (self.major != other.major) return false;
        if (self.minor < other.minor) return false;
        if (self.minor == other.minor and self.patch < other.patch) return false;
        return true;
    }

    /// Increment major version (resets minor and patch).
    pub fn bumpMajor(self: SemVer) SemVer {
        return SemVer{ .major = self.major + 1, .minor = 0, .patch = 0 };
    }

    /// Increment minor version (resets patch).
    pub fn bumpMinor(self: SemVer) SemVer {
        return SemVer{ .major = self.major, .minor = self.minor + 1, .patch = 0 };
    }

    /// Increment patch version.
    pub fn bumpPatch(self: SemVer) SemVer {
        return SemVer{ .major = self.major, .minor = self.minor, .patch = self.patch + 1 };
    }

    /// Format version to buffer.
    pub fn format(self: SemVer, buf: []u8) []const u8 {
        const written = std.fmt.bufPrint(buf, "{d}.{d}.{d}", .{ self.major, self.minor, self.patch }) catch return "";
        return written;
    }
};

test "SemVer parse" {
    const v = try SemVer.parse("1.2.3");
    try std.testing.expectEqual(@as(u32, 1), v.major);
    try std.testing.expectEqual(@as(u32, 2), v.minor);
    try std.testing.expectEqual(@as(u32, 3), v.patch);
}

test "SemVer compare" {
    const v1 = try SemVer.parse("1.2.3");
    const v2 = try SemVer.parse("1.2.4");
    try std.testing.expectEqual(@as(i32, -1), v1.compare(v2));
}
