// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe webhook URL and signature validation that cannot crash.
//!
//! Provides secure validation of webhook URLs and cryptographic signature
//! verification for common webhook providers (GitHub, Stripe, Slack, etc.).
//! All operations use constant-time comparison to prevent timing attacks.

const std = @import("std");

/// Error types for webhook operations.
pub const WebhookError = error{
    /// Invalid URL format.
    InvalidUrl,
    /// URL scheme is not allowed (must be HTTPS for security).
    InsecureScheme,
    /// Invalid signature format.
    InvalidSignature,
    /// Signature verification failed.
    SignatureMismatch,
    /// Missing required header or parameter.
    MissingParameter,
    /// Timestamp is outside acceptable window.
    TimestampOutOfRange,
    /// Invalid payload format.
    InvalidPayload,
    /// Unsupported algorithm.
    UnsupportedAlgorithm,
    /// Out of memory.
    OutOfMemory,
};

/// Supported webhook signature algorithms.
pub const SignatureAlgorithm = enum {
    hmac_sha256,
    hmac_sha1,
    hmac_sha512,
};

/// Webhook provider presets with their specific signature formats.
pub const WebhookProvider = enum {
    github,
    stripe,
    slack,
    twilio,
    shopify,
    custom,

    /// Get the signature header name for this provider.
    pub fn signatureHeader(self: WebhookProvider) []const u8 {
        return switch (self) {
            .github => "X-Hub-Signature-256",
            .stripe => "Stripe-Signature",
            .slack => "X-Slack-Signature",
            .twilio => "X-Twilio-Signature",
            .shopify => "X-Shopify-Hmac-SHA256",
            .custom => "X-Signature",
        };
    }

    /// Get the default algorithm for this provider.
    pub fn defaultAlgorithm(self: WebhookProvider) SignatureAlgorithm {
        return switch (self) {
            .github => .hmac_sha256,
            .stripe => .hmac_sha256,
            .slack => .hmac_sha256,
            .twilio => .hmac_sha1,
            .shopify => .hmac_sha256,
            .custom => .hmac_sha256,
        };
    }
};

/// Validated webhook URL with security checks.
pub const WebhookUrl = struct {
    scheme: []const u8,
    host: []const u8,
    port: ?u16 = null,
    path: []const u8 = "/",
    query: ?[]const u8 = null,

    /// Check if the URL uses a secure scheme.
    pub fn isSecure(self: WebhookUrl) bool {
        return std.mem.eql(u8, self.scheme, "https");
    }

    /// Get the effective port (explicit or default for scheme).
    pub fn effectivePort(self: WebhookUrl) u16 {
        if (self.port) |explicit_port| return explicit_port;
        if (std.mem.eql(u8, self.scheme, "https")) return 443;
        if (std.mem.eql(u8, self.scheme, "http")) return 80;
        return 443;
    }

    /// Format the URL as a string.
    pub fn toString(self: WebhookUrl, output_buffer: []u8) ![]u8 {
        var write_position: usize = 0;

        // Scheme
        if (write_position + self.scheme.len + 3 > output_buffer.len) return error.OutOfMemory;
        @memcpy(output_buffer[write_position..][0..self.scheme.len], self.scheme);
        write_position += self.scheme.len;
        @memcpy(output_buffer[write_position..][0..3], "://");
        write_position += 3;

        // Host
        if (write_position + self.host.len > output_buffer.len) return error.OutOfMemory;
        @memcpy(output_buffer[write_position..][0..self.host.len], self.host);
        write_position += self.host.len;

        // Port (if non-default)
        if (self.port) |explicit_port| {
            const written = std.fmt.bufPrint(output_buffer[write_position..], ":{}", .{explicit_port}) catch return error.OutOfMemory;
            write_position += written.len;
        }

        // Path
        if (write_position + self.path.len > output_buffer.len) return error.OutOfMemory;
        @memcpy(output_buffer[write_position..][0..self.path.len], self.path);
        write_position += self.path.len;

        // Query
        if (self.query) |query_string| {
            if (write_position + 1 + query_string.len > output_buffer.len) return error.OutOfMemory;
            output_buffer[write_position] = '?';
            write_position += 1;
            @memcpy(output_buffer[write_position..][0..query_string.len], query_string);
            write_position += query_string.len;
        }

        return output_buffer[0..write_position];
    }
};

/// Parse and validate a webhook URL.
/// By default, only HTTPS URLs are accepted for security.
pub fn parseUrl(input: []const u8, options: struct {
    allow_http: bool = false,
    allowed_hosts: ?[]const []const u8 = null,
}) WebhookError!WebhookUrl {
    if (input.len == 0) return error.InvalidUrl;

    // Find scheme
    const scheme_end_position = std.mem.indexOf(u8, input, "://") orelse return error.InvalidUrl;
    const scheme = input[0..scheme_end_position];

    // Validate scheme
    const is_https = std.mem.eql(u8, scheme, "https");
    const is_http = std.mem.eql(u8, scheme, "http");

    if (!is_https and !is_http) return error.InvalidUrl;
    if (!is_https and !options.allow_http) return error.InsecureScheme;

    var remaining_url = input[scheme_end_position + 3..];

    // Find path start
    const path_start_position = std.mem.indexOfScalar(u8, remaining_url, '/') orelse remaining_url.len;
    const authority_part = remaining_url[0..path_start_position];
    remaining_url = if (path_start_position < remaining_url.len) remaining_url[path_start_position..] else "/";

    // Parse host and port
    var host: []const u8 = undefined;
    var port: ?u16 = null;

    if (std.mem.lastIndexOfScalar(u8, authority_part, ':')) |colon_position| {
        host = authority_part[0..colon_position];
        const port_string = authority_part[colon_position + 1..];
        port = std.fmt.parseInt(u16, port_string, 10) catch return error.InvalidUrl;
    } else {
        host = authority_part;
    }

    if (host.len == 0) return error.InvalidUrl;

    // Check allowed hosts
    if (options.allowed_hosts) |allowed_host_list| {
        var host_is_allowed = false;
        for (allowed_host_list) |allowed_host| {
            if (std.mem.eql(u8, host, allowed_host)) {
                host_is_allowed = true;
                break;
            }
        }
        if (!host_is_allowed) return error.InvalidUrl;
    }

    // Parse path and query
    var path: []const u8 = "/";
    var query: ?[]const u8 = null;

    if (remaining_url.len > 0) {
        if (std.mem.indexOfScalar(u8, remaining_url, '?')) |query_position| {
            query = remaining_url[query_position + 1..];
            path = remaining_url[0..query_position];
        } else {
            path = remaining_url;
        }
    }

    return WebhookUrl{
        .scheme = scheme,
        .host = host,
        .port = port,
        .path = path,
        .query = query,
    };
}

/// Verify an HMAC-SHA256 signature (used by GitHub, Stripe, Slack, Shopify).
pub fn verifyHmacSha256(
    payload: []const u8,
    secret: []const u8,
    signature: []const u8,
) WebhookError!bool {
    // Compute expected signature
    var hmac_context = std.crypto.auth.hmac.sha2.HmacSha256.init(secret);
    hmac_context.update(payload);
    var computed_mac: [32]u8 = undefined;
    hmac_context.final(&computed_mac);

    // Parse provided signature (may have prefix like "sha256=" or "v0=")
    var signature_bytes: []const u8 = signature;

    // Strip common prefixes
    if (std.mem.startsWith(u8, signature_bytes, "sha256=")) {
        signature_bytes = signature_bytes[7..];
    } else if (std.mem.startsWith(u8, signature_bytes, "v0=")) {
        signature_bytes = signature_bytes[3..];
    }

    // Decode hex signature
    if (signature_bytes.len != 64) return error.InvalidSignature;

    var decoded_signature: [32]u8 = undefined;
    for (0..32) |byte_index| {
        const hex_pair = signature_bytes[byte_index * 2 .. byte_index * 2 + 2];
        decoded_signature[byte_index] = std.fmt.parseInt(u8, hex_pair, 16) catch return error.InvalidSignature;
    }

    // Constant-time comparison
    return std.crypto.utils.timingSafeEql([32]u8, computed_mac, decoded_signature);
}

/// Verify an HMAC-SHA1 signature (used by Twilio).
pub fn verifyHmacSha1(
    payload: []const u8,
    secret: []const u8,
    signature: []const u8,
) WebhookError!bool {
    var hmac_context = std.crypto.auth.hmac.HmacSha1.init(secret);
    hmac_context.update(payload);
    var computed_mac: [20]u8 = undefined;
    hmac_context.final(&computed_mac);

    // Signature may be base64 encoded (Twilio style)
    if (signature.len == 28) {
        // Base64 encoded (28 chars = 20 bytes)
        var decoded_signature: [20]u8 = undefined;
        _ = std.base64.standard.Decoder.decode(&decoded_signature, signature) catch return error.InvalidSignature;
        return std.crypto.utils.timingSafeEql([20]u8, computed_mac, decoded_signature);
    } else if (signature.len == 40) {
        // Hex encoded
        var decoded_signature: [20]u8 = undefined;
        for (0..20) |byte_index| {
            const hex_pair = signature[byte_index * 2 .. byte_index * 2 + 2];
            decoded_signature[byte_index] = std.fmt.parseInt(u8, hex_pair, 16) catch return error.InvalidSignature;
        }
        return std.crypto.utils.timingSafeEql([20]u8, computed_mac, decoded_signature);
    }

    return error.InvalidSignature;
}

/// Verify a GitHub webhook signature.
pub fn verifyGitHub(payload: []const u8, secret: []const u8, signature_header: []const u8) WebhookError!bool {
    return verifyHmacSha256(payload, secret, signature_header);
}

/// Verify a Stripe webhook signature with timestamp validation.
pub fn verifyStripe(
    payload: []const u8,
    secret: []const u8,
    signature_header: []const u8,
    tolerance_seconds: i64,
) WebhookError!bool {
    // Parse Stripe signature header: "t=timestamp,v1=signature,..."
    var timestamp_value: ?[]const u8 = null;
    var signature_value: ?[]const u8 = null;

    var iterator = std.mem.splitScalar(u8, signature_header, ',');
    while (iterator.next()) |part| {
        if (std.mem.startsWith(u8, part, "t=")) {
            timestamp_value = part[2..];
        } else if (std.mem.startsWith(u8, part, "v1=")) {
            signature_value = part[3..];
        }
    }

    const timestamp_string = timestamp_value orelse return error.MissingParameter;
    const signature_string = signature_value orelse return error.MissingParameter;

    // Validate timestamp
    const timestamp = std.fmt.parseInt(i64, timestamp_string, 10) catch return error.InvalidPayload;
    const current_time = std.time.timestamp();

    if (@abs(current_time - timestamp) > tolerance_seconds) {
        return error.TimestampOutOfRange;
    }

    // Compute signed payload: "timestamp.payload"
    var signed_payload_buffer: [8192]u8 = undefined;
    const signed_payload = std.fmt.bufPrint(&signed_payload_buffer, "{s}.{s}", .{ timestamp_string, payload }) catch return error.InvalidPayload;

    return verifyHmacSha256(signed_payload, secret, signature_string);
}

/// Verify a Slack webhook signature.
pub fn verifySlack(
    payload: []const u8,
    secret: []const u8,
    signature_header: []const u8,
    timestamp_header: []const u8,
    tolerance_seconds: i64,
) WebhookError!bool {
    // Validate timestamp
    const timestamp = std.fmt.parseInt(i64, timestamp_header, 10) catch return error.InvalidPayload;
    const current_time = std.time.timestamp();

    if (@abs(current_time - timestamp) > tolerance_seconds) {
        return error.TimestampOutOfRange;
    }

    // Compute signed payload: "v0:timestamp:payload"
    var signed_payload_buffer: [8192]u8 = undefined;
    const signed_payload = std.fmt.bufPrint(&signed_payload_buffer, "v0:{s}:{s}", .{ timestamp_header, payload }) catch return error.InvalidPayload;

    return verifyHmacSha256(signed_payload, secret, signature_header);
}

/// Generate a webhook signature for testing or sending webhooks.
pub fn generateSignature(
    allocator: std.mem.Allocator,
    payload: []const u8,
    secret: []const u8,
    algorithm: SignatureAlgorithm,
) ![]u8 {
    switch (algorithm) {
        .hmac_sha256 => {
            var hmac_context = std.crypto.auth.hmac.sha2.HmacSha256.init(secret);
            hmac_context.update(payload);
            var mac: [32]u8 = undefined;
            hmac_context.final(&mac);

            const hex_output = try allocator.alloc(u8, 64);
            _ = std.fmt.bufPrint(hex_output, "{}", .{std.fmt.fmtSliceHexLower(&mac)}) catch unreachable;
            return hex_output;
        },
        .hmac_sha1 => {
            var hmac_context = std.crypto.auth.hmac.HmacSha1.init(secret);
            hmac_context.update(payload);
            var mac: [20]u8 = undefined;
            hmac_context.final(&mac);

            const hex_output = try allocator.alloc(u8, 40);
            _ = std.fmt.bufPrint(hex_output, "{}", .{std.fmt.fmtSliceHexLower(&mac)}) catch unreachable;
            return hex_output;
        },
        .hmac_sha512 => {
            var hmac_context = std.crypto.auth.hmac.sha2.HmacSha512.init(secret);
            hmac_context.update(payload);
            var mac: [64]u8 = undefined;
            hmac_context.final(&mac);

            const hex_output = try allocator.alloc(u8, 128);
            _ = std.fmt.bufPrint(hex_output, "{}", .{std.fmt.fmtSliceHexLower(&mac)}) catch unreachable;
            return hex_output;
        },
    }
}

/// Check if a URL is a valid webhook endpoint.
pub fn isValidWebhookUrl(input: []const u8) bool {
    _ = parseUrl(input, .{}) catch return false;
    return true;
}

test "parseUrl valid HTTPS" {
    const url = try parseUrl("https://example.com/webhook", .{});
    try std.testing.expectEqualStrings("https", url.scheme);
    try std.testing.expectEqualStrings("example.com", url.host);
    try std.testing.expectEqualStrings("/webhook", url.path);
    try std.testing.expect(url.isSecure());
}

test "parseUrl rejects HTTP by default" {
    try std.testing.expectError(error.InsecureScheme, parseUrl("http://example.com/webhook", .{}));
}

test "parseUrl allows HTTP when configured" {
    const url = try parseUrl("http://localhost/webhook", .{ .allow_http = true });
    try std.testing.expectEqualStrings("http", url.scheme);
    try std.testing.expect(!url.isSecure());
}

test "parseUrl with port" {
    const url = try parseUrl("https://example.com:8443/webhook", .{});
    try std.testing.expectEqual(@as(u16, 8443), url.port.?);
    try std.testing.expectEqual(@as(u16, 8443), url.effectivePort());
}

test "verifyHmacSha256" {
    const payload = "test payload";
    const secret = "secret";

    // Generate signature
    const allocator = std.testing.allocator;
    const signature = try generateSignature(allocator, payload, secret, .hmac_sha256);
    defer allocator.free(signature);

    // Verify with prefix
    var signature_with_prefix_buffer: [72]u8 = undefined;
    const signature_with_prefix = std.fmt.bufPrint(&signature_with_prefix_buffer, "sha256={s}", .{signature}) catch unreachable;

    try std.testing.expect(try verifyHmacSha256(payload, secret, signature_with_prefix));
    try std.testing.expect(!try verifyHmacSha256("wrong payload", secret, signature_with_prefix));
}

test "verifyGitHub" {
    const payload = "{\"action\":\"opened\"}";
    const secret = "webhook_secret";

    const allocator = std.testing.allocator;
    const signature = try generateSignature(allocator, payload, secret, .hmac_sha256);
    defer allocator.free(signature);

    var signature_header_buffer: [72]u8 = undefined;
    const signature_header = std.fmt.bufPrint(&signature_header_buffer, "sha256={s}", .{signature}) catch unreachable;

    try std.testing.expect(try verifyGitHub(payload, secret, signature_header));
}

test "WebhookProvider headers" {
    try std.testing.expectEqualStrings("X-Hub-Signature-256", WebhookProvider.github.signatureHeader());
    try std.testing.expectEqualStrings("Stripe-Signature", WebhookProvider.stripe.signatureHeader());
}

test "isValidWebhookUrl" {
    try std.testing.expect(isValidWebhookUrl("https://example.com/webhook"));
    try std.testing.expect(!isValidWebhookUrl("http://example.com/webhook"));
    try std.testing.expect(!isValidWebhookUrl("not a url"));
    try std.testing.expect(!isValidWebhookUrl("ftp://example.com/file"));
}
