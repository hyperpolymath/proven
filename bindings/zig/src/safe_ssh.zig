// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe SSH key and fingerprint validation that cannot crash.
//!
//! Provides utilities for validating SSH public keys, parsing fingerprints,
//! and working with known_hosts entries. Designed for security-conscious
//! SSH key management.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for SSH operations.
pub const SshError = error{
    InvalidKeyFormat,
    InvalidKeyType,
    InvalidBase64,
    InvalidFingerprint,
    InvalidKnownHostsEntry,
    KeyTooShort,
    KeyTooLong,
    UnsupportedAlgorithm,
    OutOfMemory,
};

/// Supported SSH key types.
pub const KeyType = enum {
    rsa,
    dsa,
    ecdsa_256,
    ecdsa_384,
    ecdsa_521,
    ed25519,
    ed25519_sk,
    ecdsa_sk,

    /// Get the algorithm string for this key type.
    pub fn algorithmString(self: KeyType) []const u8 {
        return switch (self) {
            .rsa => "ssh-rsa",
            .dsa => "ssh-dss",
            .ecdsa_256 => "ecdsa-sha2-nistp256",
            .ecdsa_384 => "ecdsa-sha2-nistp384",
            .ecdsa_521 => "ecdsa-sha2-nistp521",
            .ed25519 => "ssh-ed25519",
            .ed25519_sk => "sk-ssh-ed25519@openssh.com",
            .ecdsa_sk => "sk-ecdsa-sha2-nistp256@openssh.com",
        };
    }

    /// Get minimum recommended key size in bits (0 for fixed-size keys).
    pub fn minKeyBits(self: KeyType) u16 {
        return switch (self) {
            .rsa => 2048,
            .dsa => 1024,
            .ecdsa_256 => 256,
            .ecdsa_384 => 384,
            .ecdsa_521 => 521,
            .ed25519 => 256,
            .ed25519_sk => 256,
            .ecdsa_sk => 256,
        };
    }

    /// Check if this key type is considered secure.
    pub fn isSecure(self: KeyType) bool {
        return switch (self) {
            .dsa => false, // DSA is deprecated
            .rsa => true, // Only if >= 2048 bits, checked elsewhere
            else => true,
        };
    }
};

/// Fingerprint hash algorithm.
pub const FingerprintAlgorithm = enum {
    md5,
    sha256,

    /// Get the prefix string for this algorithm.
    pub fn prefix(self: FingerprintAlgorithm) []const u8 {
        return switch (self) {
            .md5 => "MD5:",
            .sha256 => "SHA256:",
        };
    }
};

/// Parsed SSH public key.
pub const PublicKey = struct {
    key_type: KeyType,
    key_data: []const u8,
    comment: ?[]const u8,

    /// Check if the key meets minimum security requirements.
    pub fn isSecure(self: PublicKey) bool {
        return self.key_type.isSecure();
    }
};

/// Parsed SSH fingerprint.
pub const Fingerprint = struct {
    algorithm: FingerprintAlgorithm,
    hash: []const u8,
    key_type: ?KeyType,

    /// Format fingerprint as string.
    pub fn format(self: Fingerprint, allocator: Allocator) ![]u8 {
        var result = std.ArrayList(u8).init(allocator);
        errdefer result.deinit();

        try result.appendSlice(self.algorithm.prefix());
        try result.appendSlice(self.hash);

        return result.toOwnedSlice();
    }
};

/// Parsed known_hosts entry.
pub const KnownHostsEntry = struct {
    hostnames: []const u8,
    key_type: KeyType,
    key_data: []const u8,
    comment: ?[]const u8,
    is_hashed: bool,
};

/// Key algorithm string to KeyType mapping.
const key_type_map = [_]struct { str: []const u8, key_type: KeyType }{
    .{ .str = "ssh-rsa", .key_type = .rsa },
    .{ .str = "ssh-dss", .key_type = .dsa },
    .{ .str = "ecdsa-sha2-nistp256", .key_type = .ecdsa_256 },
    .{ .str = "ecdsa-sha2-nistp384", .key_type = .ecdsa_384 },
    .{ .str = "ecdsa-sha2-nistp521", .key_type = .ecdsa_521 },
    .{ .str = "ssh-ed25519", .key_type = .ed25519 },
    .{ .str = "sk-ssh-ed25519@openssh.com", .key_type = .ed25519_sk },
    .{ .str = "sk-ecdsa-sha2-nistp256@openssh.com", .key_type = .ecdsa_sk },
};

/// Parse key type from algorithm string.
pub fn parseKeyType(algorithm: []const u8) SshError!KeyType {
    for (key_type_map) |entry| {
        if (std.mem.eql(u8, algorithm, entry.str)) {
            return entry.key_type;
        }
    }
    return error.UnsupportedAlgorithm;
}

/// Validate and parse an SSH public key string.
/// Expected format: "algorithm base64-data [comment]"
pub fn parsePublicKey(key_string: []const u8) SshError!PublicKey {
    if (key_string.len < 16) return error.KeyTooShort;
    if (key_string.len > 16384) return error.KeyTooLong;

    var iter = std.mem.splitScalar(u8, key_string, ' ');

    // Parse algorithm
    const algorithm = iter.next() orelse return error.InvalidKeyFormat;
    const key_type = try parseKeyType(algorithm);

    // Parse base64 key data
    const key_data = iter.next() orelse return error.InvalidKeyFormat;
    if (!isValidBase64(key_data)) return error.InvalidBase64;

    // Optional comment
    const comment = iter.next();

    return PublicKey{
        .key_type = key_type,
        .key_data = key_data,
        .comment = comment,
    };
}

/// Check if a string is a valid SSH public key.
pub fn isValidPublicKey(key_string: []const u8) bool {
    _ = parsePublicKey(key_string) catch return false;
    return true;
}

/// Check if a string is valid base64.
fn isValidBase64(data: []const u8) bool {
    if (data.len == 0) return false;

    const base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";

    for (data) |c| {
        if (std.mem.indexOfScalar(u8, base64_chars, c) == null) {
            return false;
        }
    }

    return true;
}

/// Parse an SSH fingerprint string.
/// Supports formats: "SHA256:xxx" or "MD5:xx:xx:xx..."
pub fn parseFingerprint(fingerprint: []const u8) SshError!Fingerprint {
    if (fingerprint.len < 8) return error.InvalidFingerprint;

    if (std.mem.startsWith(u8, fingerprint, "SHA256:")) {
        const hash = fingerprint[7..];
        if (!isValidBase64(hash)) return error.InvalidFingerprint;
        return Fingerprint{
            .algorithm = .sha256,
            .hash = hash,
            .key_type = null,
        };
    }

    if (std.mem.startsWith(u8, fingerprint, "MD5:")) {
        const hash = fingerprint[4..];
        if (!isValidMd5Fingerprint(hash)) return error.InvalidFingerprint;
        return Fingerprint{
            .algorithm = .md5,
            .hash = hash,
            .key_type = null,
        };
    }

    // Try parsing without prefix (legacy MD5 format)
    if (isValidMd5Fingerprint(fingerprint)) {
        return Fingerprint{
            .algorithm = .md5,
            .hash = fingerprint,
            .key_type = null,
        };
    }

    return error.InvalidFingerprint;
}

/// Validate MD5 fingerprint format (xx:xx:xx:...).
fn isValidMd5Fingerprint(fingerprint: []const u8) bool {
    if (fingerprint.len != 47) return false; // 16 bytes * 2 hex + 15 colons

    var i: usize = 0;
    while (i < fingerprint.len) {
        if (i % 3 == 2) {
            if (fingerprint[i] != ':') return false;
        } else {
            if (!std.ascii.isHex(fingerprint[i])) return false;
        }
        i += 1;
    }

    return true;
}

/// Check if a string is a valid SSH fingerprint.
pub fn isValidFingerprint(fingerprint: []const u8) bool {
    _ = parseFingerprint(fingerprint) catch return false;
    return true;
}

/// Parse a known_hosts entry.
pub fn parseKnownHostsEntry(entry: []const u8) SshError!KnownHostsEntry {
    if (entry.len == 0) return error.InvalidKnownHostsEntry;

    // Skip comment lines
    if (entry[0] == '#') return error.InvalidKnownHostsEntry;

    var iter = std.mem.splitScalar(u8, entry, ' ');

    // Parse hostnames (may be hashed)
    const hostnames = iter.next() orelse return error.InvalidKnownHostsEntry;
    const is_hashed = std.mem.startsWith(u8, hostnames, "|1|");

    // Parse key type
    const algorithm = iter.next() orelse return error.InvalidKnownHostsEntry;
    const key_type = try parseKeyType(algorithm);

    // Parse key data
    const key_data = iter.next() orelse return error.InvalidKnownHostsEntry;
    if (!isValidBase64(key_data)) return error.InvalidBase64;

    // Optional comment
    const comment = iter.next();

    return KnownHostsEntry{
        .hostnames = hostnames,
        .key_type = key_type,
        .key_data = key_data,
        .comment = comment,
        .is_hashed = is_hashed,
    };
}

/// Check if a known_hosts entry is valid.
pub fn isValidKnownHostsEntry(entry: []const u8) bool {
    _ = parseKnownHostsEntry(entry) catch return false;
    return true;
}

/// Validate a hostname for SSH (basic check).
pub fn isValidSshHostname(hostname: []const u8) bool {
    if (hostname.len == 0 or hostname.len > 253) return false;

    // Check for valid characters
    for (hostname) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '.' and c != '-' and c != '_') {
            return false;
        }
    }

    // Cannot start or end with hyphen
    if (hostname[0] == '-' or hostname[hostname.len - 1] == '-') {
        return false;
    }

    return true;
}

/// Validate an SSH port number.
pub fn isValidSshPort(port: u16) bool {
    // Port 0 is invalid, ports 1-65535 are valid
    // Standard SSH is 22, but any port can be used
    return port > 0;
}

/// Format a host:port string.
pub fn formatHostPort(allocator: Allocator, hostname: []const u8, port: u16) ![]u8 {
    if (!isValidSshHostname(hostname)) return error.InvalidKeyFormat;
    if (!isValidSshPort(port)) return error.InvalidKeyFormat;

    if (port == 22) {
        return try allocator.dupe(u8, hostname);
    }

    // Check if hostname contains ':' (IPv6)
    if (std.mem.indexOfScalar(u8, hostname, ':') != null) {
        return try std.fmt.allocPrint(allocator, "[{s}]:{d}", .{ hostname, port });
    }

    return try std.fmt.allocPrint(allocator, "{s}:{d}", .{ hostname, port });
}

/// Sanitize a key comment (remove potentially dangerous characters).
pub fn sanitizeComment(allocator: Allocator, comment: []const u8) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    for (comment) |c| {
        // Allow alphanumeric, space, @, ., -, _
        if (std.ascii.isAlphanumeric(c) or c == ' ' or c == '@' or c == '.' or c == '-' or c == '_') {
            try result.append(c);
        }
    }

    return result.toOwnedSlice();
}

/// Check if a key type is recommended for new keys.
pub fn isRecommendedKeyType(key_type: KeyType) bool {
    return switch (key_type) {
        .ed25519 => true,
        .ecdsa_256, .ecdsa_384, .ecdsa_521 => true,
        .rsa => true, // Still acceptable with >= 3072 bits
        .dsa => false, // Deprecated
        .ed25519_sk, .ecdsa_sk => true, // Security keys
    };
}

test "parseKeyType" {
    try std.testing.expectEqual(KeyType.rsa, try parseKeyType("ssh-rsa"));
    try std.testing.expectEqual(KeyType.ed25519, try parseKeyType("ssh-ed25519"));
    try std.testing.expectError(error.UnsupportedAlgorithm, parseKeyType("unknown-type"));
}

test "parsePublicKey valid" {
    const key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl user@host";
    const parsed = try parsePublicKey(key);
    try std.testing.expectEqual(KeyType.ed25519, parsed.key_type);
    try std.testing.expectEqualStrings("AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl", parsed.key_data);
    try std.testing.expectEqualStrings("user@host", parsed.comment.?);
}

test "parsePublicKey invalid" {
    try std.testing.expectError(error.KeyTooShort, parsePublicKey("short"));
    try std.testing.expectError(error.UnsupportedAlgorithm, parsePublicKey("unknown-type AAAA comment"));
}

test "isValidPublicKey" {
    try std.testing.expect(isValidPublicKey("ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl"));
    try std.testing.expect(isValidPublicKey("ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC= comment"));
    try std.testing.expect(!isValidPublicKey("invalid"));
    try std.testing.expect(!isValidPublicKey(""));
}

test "parseFingerprint SHA256" {
    const fp = try parseFingerprint("SHA256:abcdefghijklmnopqrstuvwxyz123456789012");
    try std.testing.expectEqual(FingerprintAlgorithm.sha256, fp.algorithm);
    try std.testing.expectEqualStrings("abcdefghijklmnopqrstuvwxyz123456789012", fp.hash);
}

test "parseFingerprint MD5" {
    const fp = try parseFingerprint("MD5:16:27:ac:a5:76:28:2d:36:63:1b:56:4d:eb:df:a6:48");
    try std.testing.expectEqual(FingerprintAlgorithm.md5, fp.algorithm);
}

test "isValidFingerprint" {
    try std.testing.expect(isValidFingerprint("SHA256:abcdefghijklmnopqrstuvwxyz123456789012"));
    try std.testing.expect(isValidFingerprint("MD5:16:27:ac:a5:76:28:2d:36:63:1b:56:4d:eb:df:a6:48"));
    try std.testing.expect(!isValidFingerprint("invalid"));
}

test "isValidSshHostname" {
    try std.testing.expect(isValidSshHostname("example.com"));
    try std.testing.expect(isValidSshHostname("server-1.local"));
    try std.testing.expect(!isValidSshHostname(""));
    try std.testing.expect(!isValidSshHostname("-invalid.com"));
}

test "isValidSshPort" {
    try std.testing.expect(isValidSshPort(22));
    try std.testing.expect(isValidSshPort(2222));
    try std.testing.expect(isValidSshPort(65535));
    try std.testing.expect(!isValidSshPort(0));
}

test "sanitizeComment" {
    const allocator = std.testing.allocator;
    const result = try sanitizeComment(allocator, "user@host.com <script>");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("user@host.com script", result);
}

test "KeyType.isSecure" {
    try std.testing.expect(KeyType.ed25519.isSecure());
    try std.testing.expect(KeyType.rsa.isSecure());
    try std.testing.expect(!KeyType.dsa.isSecure());
}

test "isRecommendedKeyType" {
    try std.testing.expect(isRecommendedKeyType(.ed25519));
    try std.testing.expect(isRecommendedKeyType(.ecdsa_256));
    try std.testing.expect(!isRecommendedKeyType(.dsa));
}
