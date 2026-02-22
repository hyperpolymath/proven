// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeVersion - FFI bindings to libproven semantic version operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for version operations.
pub const VersionError = error{
    ParseFailure,
    ProvenError,
};

/// Semantic version.
pub const SemanticVersion = struct {
    major: u32,
    minor: u32,
    patch: u32,
    prerelease: ?[]const u8,
    raw: c.ProvenSemanticVersion,

    /// Free resources allocated by libproven.
    pub fn deinit(self: *SemanticVersion) void {
        c.proven_version_free(&self.raw);
    }
};

/// Ordering result for version comparison.
pub const Ordering = enum {
    less,
    equal,
    greater,
};

/// Parse semantic version string (e.g., "1.2.3-alpha") via libproven.
/// Caller must call deinit() on the returned SemanticVersion.
pub fn parse(input: []const u8) VersionError!SemanticVersion {
    const result = c.proven_version_parse(input.ptr, input.len);
    if (result.status != c.PROVEN_OK) return error.ParseFailure;
    const v = result.version;
    const prerelease: ?[]const u8 = if (v.prerelease != null and v.prerelease_len > 0)
        @as([*]const u8, @ptrCast(v.prerelease))[0..v.prerelease_len]
    else
        null;
    return SemanticVersion{
        .major = v.major,
        .minor = v.minor,
        .patch = v.patch,
        .prerelease = prerelease,
        .raw = v,
    };
}

/// Compare two semantic versions via libproven.
/// Returns less, equal, or greater.
pub fn compare(a: SemanticVersion, b: SemanticVersion) Ordering {
    const result = c.proven_version_compare(a.raw, b.raw);
    if (result < 0) return .less;
    if (result > 0) return .greater;
    return .equal;
}

test "parse" {
    var v = try parse("1.2.3");
    defer v.deinit();
    try std.testing.expectEqual(@as(u32, 1), v.major);
    try std.testing.expectEqual(@as(u32, 2), v.minor);
    try std.testing.expectEqual(@as(u32, 3), v.patch);
}

test "compare" {
    var v1 = try parse("1.0.0");
    defer v1.deinit();
    var v2 = try parse("2.0.0");
    defer v2.deinit();
    try std.testing.expectEqual(Ordering.less, compare(v1, v2));
}
