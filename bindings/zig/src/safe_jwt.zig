// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe JWT (JSON Web Token) parsing and validation that cannot crash.
//!
//! Provides validation and parsing of JWT tokens without performing
//! cryptographic signature verification. This module focuses on structural
//! validation, decoding, and safe access to token components.

const std = @import("std");
const Allocator = std.mem.Allocator;
const base64 = std.base64;

/// Error types for JWT operations.
pub const JwtError = error{
    InvalidFormat,
    InvalidBase64,
    InvalidJson,
    InvalidHeader,
    InvalidPayload,
    MissingRequiredClaim,
    TokenExpired,
    TokenNotYetValid,
    OutOfMemory,
};

/// JWT algorithm types.
pub const Algorithm = enum {
    HS256,
    HS384,
    HS512,
    RS256,
    RS384,
    RS512,
    ES256,
    ES384,
    ES512,
    PS256,
    PS384,
    PS512,
    none,
    unknown,

    /// Parse algorithm string to enum.
    pub fn fromString(algorithm_string: []const u8) Algorithm {
        const algorithm_map = .{
            .{ "HS256", .HS256 },
            .{ "HS384", .HS384 },
            .{ "HS512", .HS512 },
            .{ "RS256", .RS256 },
            .{ "RS384", .RS384 },
            .{ "RS512", .RS512 },
            .{ "ES256", .ES256 },
            .{ "ES384", .ES384 },
            .{ "ES512", .ES512 },
            .{ "PS256", .PS256 },
            .{ "PS384", .PS384 },
            .{ "PS512", .PS512 },
            .{ "none", .none },
        };

        inline for (algorithm_map) |entry| {
            if (std.mem.eql(u8, algorithm_string, entry[0])) {
                return entry[1];
            }
        }
        return .unknown;
    }

    /// Convert algorithm enum to string.
    pub fn toString(self: Algorithm) []const u8 {
        return switch (self) {
            .HS256 => "HS256",
            .HS384 => "HS384",
            .HS512 => "HS512",
            .RS256 => "RS256",
            .RS384 => "RS384",
            .RS512 => "RS512",
            .ES256 => "ES256",
            .ES384 => "ES384",
            .ES512 => "ES512",
            .PS256 => "PS256",
            .PS384 => "PS384",
            .PS512 => "PS512",
            .none => "none",
            .unknown => "unknown",
        };
    }

    /// Check if the algorithm uses symmetric key (HMAC).
    pub fn isSymmetric(self: Algorithm) bool {
        return switch (self) {
            .HS256, .HS384, .HS512 => true,
            else => false,
        };
    }

    /// Check if the algorithm uses asymmetric key (RSA/ECDSA/EdDSA).
    pub fn isAsymmetric(self: Algorithm) bool {
        return switch (self) {
            .RS256, .RS384, .RS512, .ES256, .ES384, .ES512, .PS256, .PS384, .PS512 => true,
            else => false,
        };
    }
};

/// JWT Header structure.
pub const Header = struct {
    algorithm: Algorithm,
    token_type: []const u8,
    key_id: ?[]const u8 = null,
    content_type: ?[]const u8 = null,

    /// Check if this is a standard JWT header.
    pub fn isStandard(self: Header) bool {
        return std.mem.eql(u8, self.token_type, "JWT");
    }
};

/// Standard JWT claims.
pub const StandardClaims = struct {
    /// Issuer (iss)
    issuer: ?[]const u8 = null,
    /// Subject (sub)
    subject: ?[]const u8 = null,
    /// Audience (aud)
    audience: ?[]const u8 = null,
    /// Expiration time (exp)
    expiration_time: ?i64 = null,
    /// Not before (nbf)
    not_before: ?i64 = null,
    /// Issued at (iat)
    issued_at: ?i64 = null,
    /// JWT ID (jti)
    jwt_id: ?[]const u8 = null,

    /// Check if the token is expired based on current timestamp.
    pub fn isExpired(self: StandardClaims, current_timestamp: i64) bool {
        if (self.expiration_time) |exp| {
            return current_timestamp > exp;
        }
        return false;
    }

    /// Check if the token is valid yet based on current timestamp.
    pub fn isValidYet(self: StandardClaims, current_timestamp: i64) bool {
        if (self.not_before) |nbf| {
            return current_timestamp >= nbf;
        }
        return true;
    }

    /// Check if the token is currently valid (not expired and valid yet).
    pub fn isCurrentlyValid(self: StandardClaims, current_timestamp: i64) bool {
        return !self.isExpired(current_timestamp) and self.isValidYet(current_timestamp);
    }
};

/// Parsed JWT token structure.
pub const Token = struct {
    /// Raw header segment (base64url encoded)
    raw_header: []const u8,
    /// Raw payload segment (base64url encoded)
    raw_payload: []const u8,
    /// Raw signature segment (base64url encoded)
    raw_signature: []const u8,
    /// Decoded header JSON
    header_json: []const u8,
    /// Decoded payload JSON
    payload_json: []const u8,
    /// Parsed header
    header: Header,
    /// Standard claims extracted from payload
    claims: StandardClaims,

    /// Get the signing input (header.payload).
    pub fn getSigningInput(self: Token, allocator: Allocator) ![]u8 {
        var result = std.array_list.Managed(u8).init(allocator);
        errdefer result.deinit();

        try result.appendSlice(self.raw_header);
        try result.append('.');
        try result.appendSlice(self.raw_payload);

        return result.toOwnedSlice();
    }
};

/// Decode base64url to bytes.
fn decodeBase64Url(allocator: Allocator, input: []const u8) JwtError![]u8 {
    // Calculate output size
    const output_size = base64.url_safe_no_pad.Decoder.calcSizeForSlice(input) catch return error.InvalidBase64;

    // Allocate output buffer
    const output = allocator.alloc(u8, output_size) catch return error.OutOfMemory;
    errdefer allocator.free(output);

    // Decode
    base64.url_safe_no_pad.Decoder.decode(output, input) catch return error.InvalidBase64;

    return output;
}

/// Parse and extract a string value from JSON.
fn extractJsonString(json: []const u8, key: []const u8) ?[]const u8 {
    // Simple JSON string extraction (not full parser)
    // Look for "key":"value" or "key": "value"
    var search_pattern: [256]u8 = undefined;
    const pattern = std.fmt.bufPrint(&search_pattern, "\"{s}\"", .{key}) catch return null;

    const key_pos = std.mem.indexOf(u8, json, pattern) orelse return null;
    const after_key = json[key_pos + pattern.len ..];

    // Skip whitespace and colon
    var value_start: usize = 0;
    for (after_key, 0..) |char, index| {
        if (char == '"') {
            value_start = index + 1;
            break;
        }
        if (char != ':' and char != ' ' and char != '\t' and char != '\n' and char != '\r') {
            return null;
        }
    }

    if (value_start == 0) return null;

    const value_slice = after_key[value_start..];

    // Find closing quote (handle escaped quotes)
    var value_end: usize = 0;
    var index: usize = 0;
    while (index < value_slice.len) : (index += 1) {
        if (value_slice[index] == '\\' and index + 1 < value_slice.len) {
            index += 1; // Skip escaped character
            continue;
        }
        if (value_slice[index] == '"') {
            value_end = index;
            break;
        }
    }

    if (value_end == 0 and value_slice.len > 0 and value_slice[0] != '"') {
        return null;
    }

    return value_slice[0..value_end];
}

/// Parse and extract a numeric value from JSON.
fn extractJsonNumber(json: []const u8, key: []const u8) ?i64 {
    var search_pattern: [256]u8 = undefined;
    const pattern = std.fmt.bufPrint(&search_pattern, "\"{s}\"", .{key}) catch return null;

    const key_pos = std.mem.indexOf(u8, json, pattern) orelse return null;
    const after_key = json[key_pos + pattern.len ..];

    // Skip whitespace and colon
    var value_start: usize = 0;
    for (after_key, 0..) |char, index| {
        if (char >= '0' and char <= '9') {
            value_start = index;
            break;
        }
        if (char == '-') {
            value_start = index;
            break;
        }
        if (char != ':' and char != ' ' and char != '\t' and char != '\n' and char != '\r') {
            return null;
        }
    }

    const value_slice = after_key[value_start..];

    // Find end of number
    var value_end: usize = 0;
    for (value_slice, 0..) |char, index| {
        if (char < '0' or char > '9') {
            if (index == 0 and char == '-') continue;
            value_end = index;
            break;
        }
        value_end = index + 1;
    }

    const number_str = value_slice[0..value_end];
    return std.fmt.parseInt(i64, number_str, 10) catch null;
}

/// Parse a JWT token string into its components.
pub fn parse(allocator: Allocator, token_string: []const u8) JwtError!Token {
    // Split by dots
    var parts: [3][]const u8 = undefined;
    var part_count: usize = 0;
    var iterator = std.mem.splitScalar(u8, token_string, '.');

    while (iterator.next()) |part| {
        if (part_count >= 3) return error.InvalidFormat;
        parts[part_count] = part;
        part_count += 1;
    }

    if (part_count != 3) return error.InvalidFormat;

    const raw_header = parts[0];
    const raw_payload = parts[1];
    const raw_signature = parts[2];

    // Decode header
    const header_json = decodeBase64Url(allocator, raw_header) catch return error.InvalidHeader;
    errdefer allocator.free(header_json);

    // Decode payload
    const payload_json = decodeBase64Url(allocator, raw_payload) catch return error.InvalidPayload;
    errdefer allocator.free(payload_json);

    // Parse header
    const algorithm_string = extractJsonString(header_json, "alg") orelse return error.InvalidHeader;
    const token_type = extractJsonString(header_json, "typ") orelse "JWT";
    const key_id = extractJsonString(header_json, "kid");
    const content_type = extractJsonString(header_json, "cty");

    const header = Header{
        .algorithm = Algorithm.fromString(algorithm_string),
        .token_type = token_type,
        .key_id = key_id,
        .content_type = content_type,
    };

    // Parse standard claims
    const claims = StandardClaims{
        .issuer = extractJsonString(payload_json, "iss"),
        .subject = extractJsonString(payload_json, "sub"),
        .audience = extractJsonString(payload_json, "aud"),
        .expiration_time = extractJsonNumber(payload_json, "exp"),
        .not_before = extractJsonNumber(payload_json, "nbf"),
        .issued_at = extractJsonNumber(payload_json, "iat"),
        .jwt_id = extractJsonString(payload_json, "jti"),
    };

    return Token{
        .raw_header = raw_header,
        .raw_payload = raw_payload,
        .raw_signature = raw_signature,
        .header_json = header_json,
        .payload_json = payload_json,
        .header = header,
        .claims = claims,
    };
}

/// Check if a string looks like a valid JWT format (3 base64url parts separated by dots).
pub fn isValidFormat(token_string: []const u8) bool {
    var part_count: usize = 0;
    var iterator = std.mem.splitScalar(u8, token_string, '.');

    while (iterator.next()) |part| {
        if (part.len == 0) return false;

        // Check if all characters are valid base64url
        for (part) |char| {
            const is_valid_base64url_char = (char >= 'A' and char <= 'Z') or
                (char >= 'a' and char <= 'z') or
                (char >= '0' and char <= '9') or
                char == '-' or char == '_';
            if (!is_valid_base64url_char) return false;
        }

        part_count += 1;
        if (part_count > 3) return false;
    }

    return part_count == 3;
}

/// Validate token timing claims against a timestamp.
pub fn validateTiming(token: Token, current_timestamp: i64, leeway_seconds: i64) JwtError!void {
    // Check expiration
    if (token.claims.expiration_time) |exp| {
        if (current_timestamp > exp + leeway_seconds) {
            return error.TokenExpired;
        }
    }

    // Check not-before
    if (token.claims.not_before) |nbf| {
        if (current_timestamp < nbf - leeway_seconds) {
            return error.TokenNotYetValid;
        }
    }
}

/// Free resources allocated during token parsing.
pub fn freeToken(allocator: Allocator, token: Token) void {
    allocator.free(token.header_json);
    allocator.free(token.payload_json);
}

test "Algorithm.fromString" {
    try std.testing.expectEqual(Algorithm.HS256, Algorithm.fromString("HS256"));
    try std.testing.expectEqual(Algorithm.RS256, Algorithm.fromString("RS256"));
    try std.testing.expectEqual(Algorithm.none, Algorithm.fromString("none"));
    try std.testing.expectEqual(Algorithm.unknown, Algorithm.fromString("INVALID"));
}

test "Algorithm.isSymmetric" {
    try std.testing.expect(Algorithm.HS256.isSymmetric());
    try std.testing.expect(Algorithm.HS384.isSymmetric());
    try std.testing.expect(!Algorithm.RS256.isSymmetric());
    try std.testing.expect(!Algorithm.ES256.isSymmetric());
}

test "Algorithm.isAsymmetric" {
    try std.testing.expect(Algorithm.RS256.isAsymmetric());
    try std.testing.expect(Algorithm.ES256.isAsymmetric());
    try std.testing.expect(!Algorithm.HS256.isAsymmetric());
    try std.testing.expect(!Algorithm.none.isAsymmetric());
}

test "isValidFormat" {
    try std.testing.expect(isValidFormat("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"));
    try std.testing.expect(!isValidFormat("not.a.jwt.at.all"));
    try std.testing.expect(!isValidFormat("only.two"));
    try std.testing.expect(!isValidFormat(""));
    try std.testing.expect(!isValidFormat("has..empty.part"));
}

test "StandardClaims timing validation" {
    const claims = StandardClaims{
        .expiration_time = 1000,
        .not_before = 500,
    };

    try std.testing.expect(!claims.isExpired(900));
    try std.testing.expect(claims.isExpired(1001));
    try std.testing.expect(claims.isValidYet(500));
    try std.testing.expect(!claims.isValidYet(400));
    try std.testing.expect(claims.isCurrentlyValid(750));
    try std.testing.expect(!claims.isCurrentlyValid(1100));
}

test "parse valid JWT" {
    const allocator = std.testing.allocator;
    // A minimal valid JWT: {"alg":"HS256","typ":"JWT"}.{"sub":"1234567890"}.signature
    const token_string = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U";

    const token = try parse(allocator, token_string);
    defer freeToken(allocator, token);

    try std.testing.expectEqual(Algorithm.HS256, token.header.algorithm);
    try std.testing.expectEqualStrings("JWT", token.header.token_type);
    try std.testing.expectEqualStrings("1234567890", token.claims.subject.?);
}
