// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe Git reference and object ID validation operations.
//!
//! Provides validation and manipulation of Git references (branch names, tags)
//! and object identifiers (SHA-1/SHA-256 hashes). All operations are designed
//! to fail safely without panics or undefined behavior.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Git-related error types
pub const GitError = error{
    /// Reference name is empty
    EmptyReference,
    /// Reference contains invalid characters
    InvalidCharacter,
    /// Reference has invalid structure (e.g., consecutive dots, ends with slash)
    InvalidStructure,
    /// Reference name is a reserved name
    ReservedName,
    /// Object ID has invalid length
    InvalidLength,
    /// Object ID contains non-hexadecimal characters
    InvalidHex,
    /// Buffer is too small for the operation
    BufferTooSmall,
    /// Memory allocation failed
    OutOfMemory,
};

/// Git object ID types
pub const ObjectIdType = enum {
    /// SHA-1 (40 hex characters, 20 bytes)
    sha1,
    /// SHA-256 (64 hex characters, 32 bytes)
    sha256,
};

/// A validated Git object ID (SHA-1 or SHA-256)
pub const ObjectId = struct {
    bytes: [32]u8,
    id_type: ObjectIdType,

    /// The null SHA-1 object ID (all zeros)
    pub const null_sha1: ObjectId = .{
        .bytes = [_]u8{0} ** 32,
        .id_type = .sha1,
    };

    /// The null SHA-256 object ID (all zeros)
    pub const null_sha256: ObjectId = .{
        .bytes = [_]u8{0} ** 32,
        .id_type = .sha256,
    };

    /// Get the number of bytes for this object ID type
    pub fn byteLen(self: ObjectId) usize {
        return switch (self.id_type) {
            .sha1 => 20,
            .sha256 => 32,
        };
    }

    /// Get the hex string length for this object ID type
    pub fn hexLen(self: ObjectId) usize {
        return switch (self.id_type) {
            .sha1 => 40,
            .sha256 => 64,
        };
    }

    /// Check if this is a null (all zeros) object ID
    pub fn isNull(self: ObjectId) bool {
        const len = self.byteLen();
        for (self.bytes[0..len]) |byte| {
            if (byte != 0) return false;
        }
        return true;
    }

    /// Get the raw bytes of the object ID
    pub fn getBytes(self: ObjectId) []const u8 {
        return self.bytes[0..self.byteLen()];
    }

    /// Format as lowercase hex string
    pub fn format(self: ObjectId, buffer: []u8) GitError![]const u8 {
        const hex_len = self.hexLen();
        if (buffer.len < hex_len) return error.BufferTooSmall;

        const hex_chars = "0123456789abcdef";
        const byte_len = self.byteLen();

        for (self.bytes[0..byte_len], 0..) |byte, i| {
            buffer[i * 2] = hex_chars[byte >> 4];
            buffer[i * 2 + 1] = hex_chars[byte & 0x0F];
        }

        return buffer[0..hex_len];
    }

    /// Format to allocated string
    pub fn formatAlloc(self: ObjectId, allocator: Allocator) GitError![]u8 {
        const hex_len = self.hexLen();
        const result = allocator.alloc(u8, hex_len) catch return error.OutOfMemory;
        _ = try self.format(result);
        return result;
    }

    /// Get abbreviated form (first n characters)
    pub fn abbreviate(self: ObjectId, length: usize, buffer: []u8) GitError![]const u8 {
        const max_len = self.hexLen();
        const actual_len = @min(length, max_len);
        if (buffer.len < actual_len) return error.BufferTooSmall;

        var full_buffer: [64]u8 = undefined;
        const full_hex = try self.format(&full_buffer);

        @memcpy(buffer[0..actual_len], full_hex[0..actual_len]);
        return buffer[0..actual_len];
    }

    /// Compare two object IDs for equality
    pub fn eql(self: ObjectId, other: ObjectId) bool {
        if (self.id_type != other.id_type) return false;
        const len = self.byteLen();
        return std.mem.eql(u8, self.bytes[0..len], other.bytes[0..len]);
    }

    /// Constant-time comparison (for security-sensitive contexts)
    pub fn constantTimeEql(self: ObjectId, other: ObjectId) bool {
        if (self.id_type != other.id_type) return false;
        const len = self.byteLen();

        var diff: u8 = 0;
        for (self.bytes[0..len], other.bytes[0..len]) |a, b| {
            diff |= a ^ b;
        }
        return diff == 0;
    }
};

/// Parse a Git object ID from hex string
pub fn parseObjectId(hex_string: []const u8) GitError!ObjectId {
    const id_type: ObjectIdType = switch (hex_string.len) {
        40 => .sha1,
        64 => .sha256,
        else => return error.InvalidLength,
    };

    var result: ObjectId = .{
        .bytes = [_]u8{0} ** 32,
        .id_type = id_type,
    };

    const byte_len = result.byteLen();
    var i: usize = 0;
    while (i < byte_len) : (i += 1) {
        const high = hexCharToNibble(hex_string[i * 2]) orelse return error.InvalidHex;
        const low = hexCharToNibble(hex_string[i * 2 + 1]) orelse return error.InvalidHex;
        result.bytes[i] = (high << 4) | low;
    }

    return result;
}

/// Validate a Git object ID string without parsing
pub fn isValidObjectId(hex_string: []const u8) bool {
    return if (parseObjectId(hex_string)) |_| true else |_| false;
}

/// Check if a character is valid in a Git reference name
pub fn isValidRefChar(char: u8) bool {
    // Control characters and certain special characters are not allowed
    return switch (char) {
        0x00...0x1F, 0x7F => false, // Control characters
        ' ', '~', '^', ':', '?', '*', '[', '\\' => false, // Special forbidden chars
        else => true,
    };
}

/// Validate a Git reference name (branch name, tag name, etc.)
/// Follows git-check-ref-format rules
pub fn isValidRefName(reference_name: []const u8) bool {
    if (reference_name.len == 0) return false;

    // Cannot start or end with a slash
    if (reference_name[0] == '/' or reference_name[reference_name.len - 1] == '/') {
        return false;
    }

    // Cannot start or end with a dot
    if (reference_name[0] == '.' or reference_name[reference_name.len - 1] == '.') {
        return false;
    }

    // Cannot contain consecutive dots or slashes
    var prev_char: u8 = 0;
    for (reference_name) |char| {
        if (!isValidRefChar(char)) return false;

        // Consecutive dots (..)
        if (char == '.' and prev_char == '.') return false;

        // Consecutive slashes (//)
        if (char == '/' and prev_char == '/') return false;

        // Slash followed by dot (/.)
        if (char == '.' and prev_char == '/') return false;

        prev_char = char;
    }

    // Cannot end with .lock
    if (std.mem.endsWith(u8, reference_name, ".lock")) return false;

    // Cannot be @
    if (std.mem.eql(u8, reference_name, "@")) return false;

    // Cannot contain @{
    if (std.mem.indexOf(u8, reference_name, "@{") != null) return false;

    return true;
}

/// Validate a Git branch name (stricter than general reference)
pub fn isValidBranchName(branch_name: []const u8) bool {
    if (!isValidRefName(branch_name)) return false;

    // Branch names cannot start with a hyphen
    if (branch_name.len > 0 and branch_name[0] == '-') return false;

    return true;
}

/// Validate a Git tag name
pub fn isValidTagName(tag_name: []const u8) bool {
    return isValidRefName(tag_name);
}

/// Sanitize a string to be a valid Git reference name
pub fn sanitizeRefName(input: []const u8, buffer: []u8) GitError![]const u8 {
    if (input.len == 0) return error.EmptyReference;
    if (buffer.len == 0) return error.BufferTooSmall;

    var write_idx: usize = 0;
    var prev_char: u8 = 0;
    var skip_leading_dots = true;

    for (input) |char| {
        if (write_idx >= buffer.len) break;

        // Skip leading dots
        if (skip_leading_dots and char == '.') continue;
        skip_leading_dots = false;

        // Replace invalid characters with hyphen
        const sanitized_char: u8 = if (isValidRefChar(char) and char != '/') char else '-';

        // Skip consecutive hyphens resulting from sanitization
        if (sanitized_char == '-' and prev_char == '-') continue;

        // Skip if it would create consecutive dots
        if (sanitized_char == '.' and prev_char == '.') continue;

        buffer[write_idx] = sanitized_char;
        write_idx += 1;
        prev_char = sanitized_char;
    }

    // Remove trailing dots and hyphens
    while (write_idx > 0 and (buffer[write_idx - 1] == '.' or buffer[write_idx - 1] == '-')) {
        write_idx -= 1;
    }

    // Remove .lock suffix if present
    if (write_idx >= 5 and std.mem.eql(u8, buffer[write_idx - 5 .. write_idx], ".lock")) {
        write_idx -= 5;
    }

    if (write_idx == 0) return error.InvalidStructure;

    return buffer[0..write_idx];
}

/// Normalize a reference name (lowercase for comparison)
pub fn normalizeRefName(reference_name: []const u8, buffer: []u8) GitError![]const u8 {
    if (reference_name.len == 0) return error.EmptyReference;
    if (buffer.len < reference_name.len) return error.BufferTooSmall;

    for (reference_name, 0..) |char, i| {
        buffer[i] = std.ascii.toLower(char);
    }

    return buffer[0..reference_name.len];
}

/// Common reserved reference prefixes
pub const reserved_prefixes = [_][]const u8{
    "refs/heads/",
    "refs/tags/",
    "refs/remotes/",
    "refs/notes/",
    "refs/stash",
};

/// Check if a reference name uses a reserved prefix
pub fn hasReservedPrefix(reference_name: []const u8) bool {
    for (reserved_prefixes) |prefix| {
        if (std.mem.startsWith(u8, reference_name, prefix)) return true;
    }
    return false;
}

/// Extract the short name from a fully qualified reference
pub fn shortName(reference_name: []const u8) []const u8 {
    for (reserved_prefixes) |prefix| {
        if (std.mem.startsWith(u8, reference_name, prefix)) {
            return reference_name[prefix.len..];
        }
    }
    return reference_name;
}

/// Convert hex character to nibble value (0-15)
fn hexCharToNibble(char: u8) ?u8 {
    return switch (char) {
        '0'...'9' => char - '0',
        'a'...'f' => char - 'a' + 10,
        'A'...'F' => char - 'A' + 10,
        else => null,
    };
}

// =============================================================================
// Tests
// =============================================================================

test "parseObjectId SHA-1" {
    const oid = try parseObjectId("da39a3ee5e6b4b0d3255bfef95601890afd80709");
    try std.testing.expect(oid.id_type == .sha1);
    try std.testing.expect(!oid.isNull());

    var buffer: [40]u8 = undefined;
    const hex = try oid.format(&buffer);
    try std.testing.expectEqualStrings("da39a3ee5e6b4b0d3255bfef95601890afd80709", hex);
}

test "parseObjectId SHA-256" {
    const oid = try parseObjectId("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
    try std.testing.expect(oid.id_type == .sha256);
    try std.testing.expect(!oid.isNull());
}

test "parseObjectId invalid" {
    try std.testing.expectError(error.InvalidLength, parseObjectId("abc"));
    try std.testing.expectError(error.InvalidHex, parseObjectId("zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"));
}

test "null ObjectId" {
    try std.testing.expect(ObjectId.null_sha1.isNull());
    try std.testing.expect(ObjectId.null_sha256.isNull());
}

test "ObjectId abbreviate" {
    const oid = try parseObjectId("da39a3ee5e6b4b0d3255bfef95601890afd80709");
    var buffer: [10]u8 = undefined;
    const short = try oid.abbreviate(7, &buffer);
    try std.testing.expectEqualStrings("da39a3e", short);
}

test "isValidRefName" {
    try std.testing.expect(isValidRefName("main"));
    try std.testing.expect(isValidRefName("feature/add-login"));
    try std.testing.expect(isValidRefName("v1.0.0"));

    try std.testing.expect(!isValidRefName("")); // Empty
    try std.testing.expect(!isValidRefName(".hidden")); // Starts with dot
    try std.testing.expect(!isValidRefName("branch.lock")); // Ends with .lock
    try std.testing.expect(!isValidRefName("branch..name")); // Consecutive dots
    try std.testing.expect(!isValidRefName("branch~name")); // Invalid char ~
    try std.testing.expect(!isValidRefName("@")); // Just @
    try std.testing.expect(!isValidRefName("branch@{0}")); // Contains @{
}

test "isValidBranchName" {
    try std.testing.expect(isValidBranchName("main"));
    try std.testing.expect(isValidBranchName("feature-branch"));
    try std.testing.expect(!isValidBranchName("-starts-with-hyphen"));
}

test "sanitizeRefName" {
    var buffer: [64]u8 = undefined;

    const result1 = try sanitizeRefName("my branch~name", &buffer);
    try std.testing.expect(isValidRefName(result1));

    const result2 = try sanitizeRefName("...leading.dots", &buffer);
    try std.testing.expect(isValidRefName(result2));
}

test "shortName" {
    try std.testing.expectEqualStrings("main", shortName("refs/heads/main"));
    try std.testing.expectEqualStrings("v1.0.0", shortName("refs/tags/v1.0.0"));
    try std.testing.expectEqualStrings("origin/main", shortName("refs/remotes/origin/main"));
    try std.testing.expectEqualStrings("just-a-name", shortName("just-a-name"));
}

test "ObjectId equality" {
    const oid1 = try parseObjectId("da39a3ee5e6b4b0d3255bfef95601890afd80709");
    const oid2 = try parseObjectId("da39a3ee5e6b4b0d3255bfef95601890afd80709");
    const oid3 = try parseObjectId("0000000000000000000000000000000000000000");

    try std.testing.expect(oid1.eql(oid2));
    try std.testing.expect(oid1.constantTimeEql(oid2));
    try std.testing.expect(!oid1.eql(oid3));
}
