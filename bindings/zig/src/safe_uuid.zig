// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe UUID generation and validation operations.
//! Follows RFC 4122 specification.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// UUID version types
pub const UUIDVersion = enum(u4) {
    v1 = 1, // Time-based
    v2 = 2, // DCE Security
    v3 = 3, // Name-based (MD5)
    v4 = 4, // Random
    v5 = 5, // Name-based (SHA-1)
    nil = 0,
};

/// UUID variant types
pub const UUIDVariant = enum {
    ncs,
    rfc4122,
    microsoft,
    future,
};

/// A validated UUID (128 bits / 16 bytes)
pub const UUID = struct {
    bytes: [16]u8,

    /// The nil UUID (all zeros)
    pub const nil: UUID = .{ .bytes = [_]u8{0} ** 16 };

    /// DNS namespace UUID
    pub const namespace_dns: UUID = .{
        .bytes = .{ 0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8 },
    };

    /// URL namespace UUID
    pub const namespace_url: UUID = .{
        .bytes = .{ 0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8 },
    };

    /// OID namespace UUID
    pub const namespace_oid: UUID = .{
        .bytes = .{ 0x6b, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8 },
    };

    /// X500 namespace UUID
    pub const namespace_x500: UUID = .{
        .bytes = .{ 0x6b, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8 },
    };

    /// Get the UUID version
    pub fn getVersion(self: UUID) UUIDVersion {
        const version_nibble = (self.bytes[6] >> 4) & 0x0F;
        return switch (version_nibble) {
            1 => .v1,
            2 => .v2,
            3 => .v3,
            4 => .v4,
            5 => .v5,
            else => .nil,
        };
    }

    /// Get the UUID variant
    pub fn getVariant(self: UUID) UUIDVariant {
        const variant_byte = self.bytes[8];
        if ((variant_byte >> 7) == 0) return .ncs;
        if ((variant_byte >> 6) == 0b10) return .rfc4122;
        if ((variant_byte >> 5) == 0b110) return .microsoft;
        return .future;
    }

    /// Check if this is the nil UUID
    pub fn isNil(self: UUID) bool {
        for (self.bytes) |b| {
            if (b != 0) return false;
        }
        return true;
    }

    /// Format as canonical string (8-4-4-4-12)
    pub fn format(self: UUID, buf: []u8) UUIDError![]const u8 {
        if (buf.len < 36) return error.BufferTooSmall;

        const hex_chars = "0123456789abcdef";
        var pos: usize = 0;

        for (self.bytes, 0..) |b, i| {
            buf[pos] = hex_chars[b >> 4];
            buf[pos + 1] = hex_chars[b & 0x0F];
            pos += 2;

            // Add hyphens at positions 8, 12, 16, 20 (after bytes 4, 6, 8, 10)
            if (i == 3 or i == 5 or i == 7 or i == 9) {
                buf[pos] = '-';
                pos += 1;
            }
        }

        return buf[0..36];
    }

    /// Format to allocated string
    pub fn formatAlloc(self: UUID, allocator: Allocator) UUIDError![]u8 {
        var buf: [36]u8 = undefined;
        const str = try self.format(&buf);
        const result = allocator.alloc(u8, 36) catch return error.OutOfMemory;
        @memcpy(result, str);
        return result;
    }

    /// Compare two UUIDs
    pub fn eql(self: UUID, other: UUID) bool {
        return std.mem.eql(u8, &self.bytes, &other.bytes);
    }
};

/// UUID errors
pub const UUIDError = error{
    InvalidLength,
    InvalidCharacter,
    InvalidFormat,
    BufferTooSmall,
    OutOfMemory,
};

/// Parse UUID from canonical string format
pub fn parse(str: []const u8) UUIDError!UUID {
    if (str.len != 36) return error.InvalidLength;

    // Verify hyphens at correct positions
    if (str[8] != '-' or str[13] != '-' or str[18] != '-' or str[23] != '-') {
        return error.InvalidFormat;
    }

    var result: UUID = .{ .bytes = undefined };
    var byte_idx: usize = 0;
    var str_idx: usize = 0;

    while (byte_idx < 16) {
        // Skip hyphens
        if (str_idx == 8 or str_idx == 13 or str_idx == 18 or str_idx == 23) {
            str_idx += 1;
        }

        const high = hexCharToNibble(str[str_idx]) orelse return error.InvalidCharacter;
        const low = hexCharToNibble(str[str_idx + 1]) orelse return error.InvalidCharacter;
        result.bytes[byte_idx] = (high << 4) | low;

        byte_idx += 1;
        str_idx += 2;
    }

    return result;
}

/// Generate a v4 (random) UUID using provided random bytes
pub fn v4FromBytes(random_bytes: [16]u8) UUID {
    var uuid: UUID = .{ .bytes = random_bytes };

    // Set version to 4
    uuid.bytes[6] = (uuid.bytes[6] & 0x0F) | 0x40;

    // Set variant to RFC 4122
    uuid.bytes[8] = (uuid.bytes[8] & 0x3F) | 0x80;

    return uuid;
}

/// Generate a v4 UUID using std.crypto.random
pub fn v4() UUID {
    var random_bytes: [16]u8 = undefined;
    std.crypto.random.bytes(&random_bytes);
    return v4FromBytes(random_bytes);
}

/// Check if string is valid UUID format
pub fn isValid(str: []const u8) bool {
    return if (parse(str)) |_| true else |_| false;
}

/// Convert hex character to nibble value
fn hexCharToNibble(c: u8) ?u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => null,
    };
}

test "parse valid UUID" {
    const uuid = try parse("550e8400-e29b-41d4-a716-446655440000");
    try std.testing.expect(!uuid.isNil());
    try std.testing.expect(uuid.getVersion() == .v4);
}

test "parse nil UUID" {
    const uuid = try parse("00000000-0000-0000-0000-000000000000");
    try std.testing.expect(uuid.isNil());
}

test "format UUID" {
    const uuid = try parse("550e8400-e29b-41d4-a716-446655440000");
    var buf: [36]u8 = undefined;
    const str = try uuid.format(&buf);
    try std.testing.expectEqualStrings("550e8400-e29b-41d4-a716-446655440000", str);
}

test "v4 generation" {
    const uuid = v4();
    try std.testing.expect(uuid.getVersion() == .v4);
    try std.testing.expect(uuid.getVariant() == .rfc4122);
}

test "invalid UUID" {
    try std.testing.expectError(error.InvalidLength, parse("not-a-uuid"));
    // This string is 35 chars (missing hyphen), so InvalidLength is correct
    try std.testing.expectError(error.InvalidLength, parse("550e8400e29b-41d4-a716-446655440000"));
    // This string has 'g' which is not a valid hex character
    try std.testing.expectError(error.InvalidCharacter, parse("550e8400-e29b-41d4-a716-4466554400g0"));
}
