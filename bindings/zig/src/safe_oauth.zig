// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe OAuth 2.0 token and scope validation that cannot crash.
//!
//! Provides validation for OAuth 2.0 tokens, scopes, grant types, and
//! related constructs according to RFC 6749 and RFC 6750. All validation
//! functions return errors instead of crashing on invalid inputs.

const std = @import("std");

/// Error types for OAuth operations.
pub const OAuthError = error{
    /// Token format is invalid.
    InvalidToken,
    /// Token has expired.
    TokenExpired,
    /// Token type is not supported.
    UnsupportedTokenType,
    /// Scope format is invalid.
    InvalidScope,
    /// Scope is not allowed.
    ScopeNotAllowed,
    /// Grant type is not supported.
    UnsupportedGrantType,
    /// Client ID is invalid.
    InvalidClientId,
    /// Redirect URI is invalid.
    InvalidRedirectUri,
    /// State parameter is invalid.
    InvalidState,
    /// Authorization code is invalid.
    InvalidAuthorizationCode,
    /// Refresh token is invalid.
    InvalidRefreshToken,
    /// PKCE code verifier is invalid.
    InvalidCodeVerifier,
    /// PKCE code challenge is invalid.
    InvalidCodeChallenge,
    /// Memory allocation failed.
    OutOfMemory,
};

/// Maximum token length.
pub const MAX_TOKEN_LENGTH = 4096;

/// Maximum scope length (total).
pub const MAX_SCOPE_LENGTH = 1024;

/// Maximum individual scope name length.
pub const MAX_SCOPE_NAME_LENGTH = 256;

/// Maximum client ID length.
pub const MAX_CLIENT_ID_LENGTH = 256;

/// Minimum state parameter length.
pub const MIN_STATE_LENGTH = 8;

/// Maximum state parameter length.
pub const MAX_STATE_LENGTH = 512;

/// PKCE code verifier minimum length (RFC 7636).
pub const PKCE_MIN_VERIFIER_LENGTH = 43;

/// PKCE code verifier maximum length (RFC 7636).
pub const PKCE_MAX_VERIFIER_LENGTH = 128;

/// OAuth 2.0 token types.
pub const TokenType = enum {
    bearer,
    mac,
    dpop,

    pub fn fromString(s: []const u8) ?TokenType {
        if (std.ascii.eqlIgnoreCase(s, "bearer")) return .bearer;
        if (std.ascii.eqlIgnoreCase(s, "mac")) return .mac;
        if (std.ascii.eqlIgnoreCase(s, "dpop")) return .dpop;
        return null;
    }

    pub fn toString(self: TokenType) []const u8 {
        return switch (self) {
            .bearer => "Bearer",
            .mac => "MAC",
            .dpop => "DPoP",
        };
    }
};

/// OAuth 2.0 grant types.
pub const GrantType = enum {
    authorization_code,
    client_credentials,
    refresh_token,
    password,
    device_code,
    jwt_bearer,

    pub fn fromString(s: []const u8) ?GrantType {
        if (std.mem.eql(u8, s, "authorization_code")) return .authorization_code;
        if (std.mem.eql(u8, s, "client_credentials")) return .client_credentials;
        if (std.mem.eql(u8, s, "refresh_token")) return .refresh_token;
        if (std.mem.eql(u8, s, "password")) return .password;
        if (std.mem.eql(u8, s, "urn:ietf:params:oauth:grant-type:device_code")) return .device_code;
        if (std.mem.eql(u8, s, "urn:ietf:params:oauth:grant-type:jwt-bearer")) return .jwt_bearer;
        return null;
    }

    pub fn toString(self: GrantType) []const u8 {
        return switch (self) {
            .authorization_code => "authorization_code",
            .client_credentials => "client_credentials",
            .refresh_token => "refresh_token",
            .password => "password",
            .device_code => "urn:ietf:params:oauth:grant-type:device_code",
            .jwt_bearer => "urn:ietf:params:oauth:grant-type:jwt-bearer",
        };
    }
};

/// PKCE code challenge methods.
pub const CodeChallengeMethod = enum {
    plain,
    s256,

    pub fn fromString(s: []const u8) ?CodeChallengeMethod {
        if (std.mem.eql(u8, s, "plain")) return .plain;
        if (std.mem.eql(u8, s, "S256")) return .s256;
        return null;
    }

    pub fn toString(self: CodeChallengeMethod) []const u8 {
        return switch (self) {
            .plain => "plain",
            .s256 => "S256",
        };
    }
};

/// OAuth 2.0 error codes.
pub const ErrorCode = enum {
    invalid_request,
    invalid_client,
    invalid_grant,
    unauthorized_client,
    unsupported_grant_type,
    invalid_scope,
    access_denied,
    unsupported_response_type,
    server_error,
    temporarily_unavailable,

    pub fn fromString(s: []const u8) ?ErrorCode {
        if (std.mem.eql(u8, s, "invalid_request")) return .invalid_request;
        if (std.mem.eql(u8, s, "invalid_client")) return .invalid_client;
        if (std.mem.eql(u8, s, "invalid_grant")) return .invalid_grant;
        if (std.mem.eql(u8, s, "unauthorized_client")) return .unauthorized_client;
        if (std.mem.eql(u8, s, "unsupported_grant_type")) return .unsupported_grant_type;
        if (std.mem.eql(u8, s, "invalid_scope")) return .invalid_scope;
        if (std.mem.eql(u8, s, "access_denied")) return .access_denied;
        if (std.mem.eql(u8, s, "unsupported_response_type")) return .unsupported_response_type;
        if (std.mem.eql(u8, s, "server_error")) return .server_error;
        if (std.mem.eql(u8, s, "temporarily_unavailable")) return .temporarily_unavailable;
        return null;
    }

    pub fn toString(self: ErrorCode) []const u8 {
        return switch (self) {
            .invalid_request => "invalid_request",
            .invalid_client => "invalid_client",
            .invalid_grant => "invalid_grant",
            .unauthorized_client => "unauthorized_client",
            .unsupported_grant_type => "unsupported_grant_type",
            .invalid_scope => "invalid_scope",
            .access_denied => "access_denied",
            .unsupported_response_type => "unsupported_response_type",
            .server_error => "server_error",
            .temporarily_unavailable => "temporarily_unavailable",
        };
    }
};

/// Common OAuth 2.0 scopes.
pub const CommonScopes = struct {
    pub const OPENID = "openid";
    pub const PROFILE = "profile";
    pub const EMAIL = "email";
    pub const ADDRESS = "address";
    pub const PHONE = "phone";
    pub const OFFLINE_ACCESS = "offline_access";
};

/// Validated token structure.
pub const ValidatedToken = struct {
    token: []const u8,
    token_type: TokenType,
};

/// Parsed scope list.
pub const ScopeList = struct {
    scopes: [][]const u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *ScopeList) void {
        self.allocator.free(self.scopes);
    }

    /// Check if the scope list contains a specific scope.
    pub fn contains(self: ScopeList, scope: []const u8) bool {
        for (self.scopes) |s| {
            if (std.mem.eql(u8, s, scope)) return true;
        }
        return false;
    }

    /// Check if all required scopes are present.
    pub fn hasAll(self: ScopeList, required: []const []const u8) bool {
        for (required) |r| {
            if (!self.contains(r)) return false;
        }
        return true;
    }

    /// Check if any of the scopes are present.
    pub fn hasAny(self: ScopeList, candidates: []const []const u8) bool {
        for (candidates) |c| {
            if (self.contains(c)) return true;
        }
        return false;
    }
};

/// Validate an access token format.
pub fn validateAccessToken(token: []const u8) OAuthError!void {
    if (token.len == 0 or token.len > MAX_TOKEN_LENGTH) {
        return error.InvalidToken;
    }

    // Token must contain only valid characters (RFC 6750 Section 2.1)
    for (token) |c| {
        if (!isValidTokenChar(c)) {
            return error.InvalidToken;
        }
    }
}

/// Validate a refresh token format.
pub fn validateRefreshToken(token: []const u8) OAuthError!void {
    if (token.len == 0 or token.len > MAX_TOKEN_LENGTH) {
        return error.InvalidRefreshToken;
    }

    for (token) |c| {
        if (!isValidTokenChar(c)) {
            return error.InvalidRefreshToken;
        }
    }
}

/// Validate an authorization code format.
pub fn validateAuthorizationCode(code: []const u8) OAuthError!void {
    if (code.len == 0 or code.len > MAX_TOKEN_LENGTH) {
        return error.InvalidAuthorizationCode;
    }

    for (code) |c| {
        if (!isValidTokenChar(c)) {
            return error.InvalidAuthorizationCode;
        }
    }
}

/// Validate a single scope name.
pub fn validateScopeName(scope: []const u8) OAuthError!void {
    if (scope.len == 0 or scope.len > MAX_SCOPE_NAME_LENGTH) {
        return error.InvalidScope;
    }

    // Scope must contain only NQCHAR (RFC 6749 Appendix A.4)
    for (scope) |c| {
        if (!isNqchar(c)) {
            return error.InvalidScope;
        }
    }
}

/// Validate a space-separated scope string.
pub fn validateScopeString(scope_string: []const u8) OAuthError!void {
    if (scope_string.len > MAX_SCOPE_LENGTH) {
        return error.InvalidScope;
    }

    // Empty scope is valid (means no scopes requested)
    if (scope_string.len == 0) return;

    var iter = std.mem.splitScalar(u8, scope_string, ' ');
    var has_scope = false;

    while (iter.next()) |scope| {
        if (scope.len == 0) {
            // Double space or leading/trailing space
            return error.InvalidScope;
        }
        try validateScopeName(scope);
        has_scope = true;
    }

    if (!has_scope) {
        return error.InvalidScope;
    }
}

/// Parse a space-separated scope string into a list.
pub fn parseScopes(allocator: std.mem.Allocator, scope_string: []const u8) OAuthError!ScopeList {
    try validateScopeString(scope_string);

    if (scope_string.len == 0) {
        const empty = allocator.alloc([]const u8, 0) catch return error.OutOfMemory;
        return ScopeList{ .scopes = empty, .allocator = allocator };
    }

    // Count scopes
    var count: usize = 0;
    var iter = std.mem.splitScalar(u8, scope_string, ' ');
    while (iter.next()) |_| {
        count += 1;
    }

    // Allocate and populate
    const scopes = allocator.alloc([]const u8, count) catch return error.OutOfMemory;
    iter = std.mem.splitScalar(u8, scope_string, ' ');
    var i: usize = 0;
    while (iter.next()) |scope| {
        scopes[i] = scope;
        i += 1;
    }

    return ScopeList{ .scopes = scopes, .allocator = allocator };
}

/// Join scopes into a space-separated string.
pub fn joinScopes(allocator: std.mem.Allocator, scopes: []const []const u8) OAuthError![]u8 {
    if (scopes.len == 0) {
        return allocator.alloc(u8, 0) catch return error.OutOfMemory;
    }

    // Validate each scope
    for (scopes) |scope| {
        try validateScopeName(scope);
    }

    // Calculate total length
    var total_len: usize = 0;
    for (scopes) |scope| {
        total_len += scope.len;
    }
    total_len += scopes.len - 1; // Spaces between

    if (total_len > MAX_SCOPE_LENGTH) {
        return error.InvalidScope;
    }

    var result = allocator.alloc(u8, total_len) catch return error.OutOfMemory;
    var pos: usize = 0;

    for (scopes, 0..) |scope, idx| {
        @memcpy(result[pos..][0..scope.len], scope);
        pos += scope.len;
        if (idx < scopes.len - 1) {
            result[pos] = ' ';
            pos += 1;
        }
    }

    return result;
}

/// Validate a client ID.
pub fn validateClientId(client_id: []const u8) OAuthError!void {
    if (client_id.len == 0 or client_id.len > MAX_CLIENT_ID_LENGTH) {
        return error.InvalidClientId;
    }

    // Client ID should be printable ASCII
    for (client_id) |c| {
        if (c < 0x20 or c > 0x7E) {
            return error.InvalidClientId;
        }
    }
}

/// Validate a state parameter.
pub fn validateState(state: []const u8) OAuthError!void {
    if (state.len < MIN_STATE_LENGTH or state.len > MAX_STATE_LENGTH) {
        return error.InvalidState;
    }

    // State must be printable ASCII
    for (state) |c| {
        if (c < 0x20 or c > 0x7E) {
            return error.InvalidState;
        }
    }
}

/// Validate a PKCE code verifier.
pub fn validateCodeVerifier(verifier: []const u8) OAuthError!void {
    if (verifier.len < PKCE_MIN_VERIFIER_LENGTH or verifier.len > PKCE_MAX_VERIFIER_LENGTH) {
        return error.InvalidCodeVerifier;
    }

    // Must be unreserved characters (RFC 7636 Section 4.1)
    for (verifier) |c| {
        if (!isPkceUnreservedChar(c)) {
            return error.InvalidCodeVerifier;
        }
    }
}

/// Validate a PKCE code challenge.
pub fn validateCodeChallenge(challenge: []const u8) OAuthError!void {
    // Code challenge is base64url encoded, so length varies
    if (challenge.len < 43 or challenge.len > 128) {
        return error.InvalidCodeChallenge;
    }

    // Must be base64url characters
    for (challenge) |c| {
        if (!isBase64UrlChar(c)) {
            return error.InvalidCodeChallenge;
        }
    }
}

/// Validate a redirect URI.
pub fn validateRedirectUri(uri: []const u8) OAuthError!void {
    if (uri.len == 0) {
        return error.InvalidRedirectUri;
    }

    // Must start with https:// for production (or http://localhost for development)
    const https_prefix = "https://";
    const http_localhost = "http://localhost";
    const http_127 = "http://127.0.0.1";

    const is_secure = std.mem.startsWith(u8, uri, https_prefix);
    const is_localhost = std.mem.startsWith(u8, uri, http_localhost) or
        std.mem.startsWith(u8, uri, http_127);

    if (!is_secure and !is_localhost) {
        return error.InvalidRedirectUri;
    }

    // Must not contain fragments
    if (std.mem.indexOfScalar(u8, uri, '#') != null) {
        return error.InvalidRedirectUri;
    }
}

/// Check if a scope is an OpenID Connect scope.
pub fn isOpenIdScope(scope: []const u8) bool {
    const oidc_scopes = [_][]const u8{
        CommonScopes.OPENID,
        CommonScopes.PROFILE,
        CommonScopes.EMAIL,
        CommonScopes.ADDRESS,
        CommonScopes.PHONE,
        CommonScopes.OFFLINE_ACCESS,
    };

    for (oidc_scopes) |s| {
        if (std.mem.eql(u8, scope, s)) return true;
    }

    return false;
}

/// Extract token from Authorization header (Bearer scheme).
pub fn extractBearerToken(header: []const u8) OAuthError![]const u8 {
    const prefix = "Bearer ";
    if (!std.mem.startsWith(u8, header, prefix)) {
        return error.UnsupportedTokenType;
    }

    const token = std.mem.trim(u8, header[prefix.len..], " ");
    try validateAccessToken(token);
    return token;
}

/// Check if character is valid for token (RFC 6750 VSCHAR)
fn isValidTokenChar(c: u8) bool {
    // VSCHAR = %x20-7E (printable ASCII excluding space at edges)
    return c >= 0x21 and c <= 0x7E;
}

/// Check if character is NQCHAR (RFC 6749 Appendix A)
fn isNqchar(c: u8) bool {
    // NQCHAR = %x21 / %x23-5B / %x5D-7E
    return (c == 0x21) or (c >= 0x23 and c <= 0x5B) or (c >= 0x5D and c <= 0x7E);
}

/// Check if character is unreserved for PKCE (RFC 7636)
fn isPkceUnreservedChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '-' or c == '.' or c == '_' or c == '~';
}

/// Check if character is base64url
fn isBase64UrlChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '-' or c == '_' or c == '=';
}

test "validateAccessToken" {
    try validateAccessToken("eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.sig");
    try validateAccessToken("simple_token_12345");
    try std.testing.expectError(error.InvalidToken, validateAccessToken(""));
    try std.testing.expectError(error.InvalidToken, validateAccessToken("token with spaces"));
}

test "validateRefreshToken" {
    try validateRefreshToken("refresh_token_abc123");
    try std.testing.expectError(error.InvalidRefreshToken, validateRefreshToken(""));
}

test "validateScopeName" {
    try validateScopeName("read");
    try validateScopeName("write:users");
    try validateScopeName("openid");
    try std.testing.expectError(error.InvalidScope, validateScopeName(""));
    try std.testing.expectError(error.InvalidScope, validateScopeName("scope with space"));
}

test "validateScopeString" {
    try validateScopeString("openid profile email");
    try validateScopeString("read write");
    try validateScopeString("single_scope");
    try validateScopeString(""); // Empty is valid
    try std.testing.expectError(error.InvalidScope, validateScopeString("double  space"));
    try std.testing.expectError(error.InvalidScope, validateScopeString(" leading_space"));
}

test "parseScopes" {
    const allocator = std.testing.allocator;

    var scopes = try parseScopes(allocator, "openid profile email");
    defer scopes.deinit();

    try std.testing.expectEqual(@as(usize, 3), scopes.scopes.len);
    try std.testing.expect(scopes.contains("openid"));
    try std.testing.expect(scopes.contains("profile"));
    try std.testing.expect(!scopes.contains("phone"));
    try std.testing.expect(scopes.hasAll(&[_][]const u8{ "openid", "email" }));
    try std.testing.expect(scopes.hasAny(&[_][]const u8{ "admin", "profile" }));
}

test "joinScopes" {
    const allocator = std.testing.allocator;

    const result = try joinScopes(allocator, &[_][]const u8{ "read", "write", "delete" });
    defer allocator.free(result);

    try std.testing.expectEqualStrings("read write delete", result);
}

test "validateClientId" {
    try validateClientId("my-client-app");
    try validateClientId("client_123");
    try std.testing.expectError(error.InvalidClientId, validateClientId(""));
}

test "validateState" {
    try validateState("abc123xyz890");
    try std.testing.expectError(error.InvalidState, validateState("short"));
}

test "validateCodeVerifier" {
    // Must be 43-128 chars of unreserved characters
    const valid_verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk";
    try validateCodeVerifier(valid_verifier);

    try std.testing.expectError(error.InvalidCodeVerifier, validateCodeVerifier("tooshort"));
}

test "validateRedirectUri" {
    try validateRedirectUri("https://example.com/callback");
    try validateRedirectUri("http://localhost:3000/callback");
    try validateRedirectUri("http://127.0.0.1:8080/oauth");
    try std.testing.expectError(error.InvalidRedirectUri, validateRedirectUri("http://example.com/callback"));
    try std.testing.expectError(error.InvalidRedirectUri, validateRedirectUri("https://example.com/callback#fragment"));
}

test "extractBearerToken" {
    const token = try extractBearerToken("Bearer eyJhbGciOiJSUzI1NiJ9.test");
    try std.testing.expectEqualStrings("eyJhbGciOiJSUzI1NiJ9.test", token);

    try std.testing.expectError(error.UnsupportedTokenType, extractBearerToken("Basic dXNlcjpwYXNz"));
}

test "TokenType" {
    try std.testing.expectEqual(TokenType.bearer, TokenType.fromString("bearer").?);
    try std.testing.expectEqual(TokenType.bearer, TokenType.fromString("Bearer").?);
    try std.testing.expectEqualStrings("Bearer", TokenType.bearer.toString());
}

test "GrantType" {
    try std.testing.expectEqual(GrantType.authorization_code, GrantType.fromString("authorization_code").?);
    try std.testing.expectEqualStrings("client_credentials", GrantType.client_credentials.toString());
}

test "CodeChallengeMethod" {
    try std.testing.expectEqual(CodeChallengeMethod.s256, CodeChallengeMethod.fromString("S256").?);
    try std.testing.expectEqualStrings("plain", CodeChallengeMethod.plain.toString());
}

test "ErrorCode" {
    try std.testing.expectEqual(ErrorCode.invalid_request, ErrorCode.fromString("invalid_request").?);
    try std.testing.expectEqualStrings("access_denied", ErrorCode.access_denied.toString());
}

test "isOpenIdScope" {
    try std.testing.expect(isOpenIdScope("openid"));
    try std.testing.expect(isOpenIdScope("profile"));
    try std.testing.expect(isOpenIdScope("email"));
    try std.testing.expect(!isOpenIdScope("custom_scope"));
}
