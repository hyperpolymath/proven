// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe HTTP method and status code validation that cannot crash.
//! Provides type-safe handling of HTTP semantics with proper validation.

const std = @import("std");

/// Error types for HTTP operations.
pub const HttpError = error{
    InvalidMethod,
    InvalidStatusCode,
    InvalidReasonPhrase,
    InvalidVersion,
    OutOfMemory,
};

/// HTTP methods per RFC 7231 and RFC 5789.
pub const Method = enum {
    GET,
    HEAD,
    POST,
    PUT,
    DELETE,
    CONNECT,
    OPTIONS,
    TRACE,
    PATCH,

    /// Check if this method is safe (does not modify server state).
    pub fn isSafe(self: Method) bool {
        return switch (self) {
            .GET, .HEAD, .OPTIONS, .TRACE => true,
            else => false,
        };
    }

    /// Check if this method is idempotent (same request = same effect).
    pub fn isIdempotent(self: Method) bool {
        return switch (self) {
            .GET, .HEAD, .PUT, .DELETE, .OPTIONS, .TRACE => true,
            else => false,
        };
    }

    /// Check if this method typically has a request body.
    pub fn hasRequestBody(self: Method) bool {
        return switch (self) {
            .POST, .PUT, .PATCH => true,
            else => false,
        };
    }

    /// Check if this method typically has a response body.
    pub fn hasResponseBody(self: Method) bool {
        return switch (self) {
            .HEAD => false,
            else => true,
        };
    }

    /// Check if this method is cacheable by default.
    pub fn isCacheable(self: Method) bool {
        return switch (self) {
            .GET, .HEAD => true,
            else => false,
        };
    }

    /// Get the method as a string.
    pub fn toString(self: Method) []const u8 {
        return @tagName(self);
    }
};

/// HTTP status code categories.
pub const StatusCategory = enum {
    informational, // 1xx
    successful,    // 2xx
    redirection,   // 3xx
    client_error,  // 4xx
    server_error,  // 5xx

    /// Check if this is an error category.
    pub fn isError(self: StatusCategory) bool {
        return self == .client_error or self == .server_error;
    }

    /// Check if this is a success category.
    pub fn isSuccess(self: StatusCategory) bool {
        return self == .successful;
    }
};

/// HTTP status codes per RFC 7231.
pub const StatusCode = enum(u16) {
    // 1xx Informational
    continue_ = 100,
    switching_protocols = 101,
    processing = 102,
    early_hints = 103,

    // 2xx Successful
    ok = 200,
    created = 201,
    accepted = 202,
    non_authoritative_information = 203,
    no_content = 204,
    reset_content = 205,
    partial_content = 206,
    multi_status = 207,
    already_reported = 208,
    im_used = 226,

    // 3xx Redirection
    multiple_choices = 300,
    moved_permanently = 301,
    found = 302,
    see_other = 303,
    not_modified = 304,
    use_proxy = 305,
    temporary_redirect = 307,
    permanent_redirect = 308,

    // 4xx Client Error
    bad_request = 400,
    unauthorized = 401,
    payment_required = 402,
    forbidden = 403,
    not_found = 404,
    method_not_allowed = 405,
    not_acceptable = 406,
    proxy_authentication_required = 407,
    request_timeout = 408,
    conflict = 409,
    gone = 410,
    length_required = 411,
    precondition_failed = 412,
    payload_too_large = 413,
    uri_too_long = 414,
    unsupported_media_type = 415,
    range_not_satisfiable = 416,
    expectation_failed = 417,
    im_a_teapot = 418,
    misdirected_request = 421,
    unprocessable_entity = 422,
    locked = 423,
    failed_dependency = 424,
    too_early = 425,
    upgrade_required = 426,
    precondition_required = 428,
    too_many_requests = 429,
    request_header_fields_too_large = 431,
    unavailable_for_legal_reasons = 451,

    // 5xx Server Error
    internal_server_error = 500,
    not_implemented = 501,
    bad_gateway = 502,
    service_unavailable = 503,
    gateway_timeout = 504,
    http_version_not_supported = 505,
    variant_also_negotiates = 506,
    insufficient_storage = 507,
    loop_detected = 508,
    not_extended = 510,
    network_authentication_required = 511,

    /// Get the status category.
    pub fn category(self: StatusCode) StatusCategory {
        const status_code = @intFromEnum(self);
        if (status_code < 200) return .informational;
        if (status_code < 300) return .successful;
        if (status_code < 400) return .redirection;
        if (status_code < 500) return .client_error;
        return .server_error;
    }

    /// Check if this is an error status.
    pub fn isError(self: StatusCode) bool {
        return self.category().isError();
    }

    /// Check if this is a success status.
    pub fn isSuccess(self: StatusCode) bool {
        return self.category().isSuccess();
    }

    /// Check if this is a redirect status.
    pub fn isRedirect(self: StatusCode) bool {
        return self.category() == .redirection;
    }

    /// Check if response typically has a body.
    pub fn hasBody(self: StatusCode) bool {
        return switch (self) {
            .no_content, .not_modified, .continue_, .switching_protocols => false,
            else => true,
        };
    }

    /// Get the standard reason phrase for this status code.
    pub fn reasonPhrase(self: StatusCode) []const u8 {
        return switch (self) {
            // 1xx
            .continue_ => "Continue",
            .switching_protocols => "Switching Protocols",
            .processing => "Processing",
            .early_hints => "Early Hints",
            // 2xx
            .ok => "OK",
            .created => "Created",
            .accepted => "Accepted",
            .non_authoritative_information => "Non-Authoritative Information",
            .no_content => "No Content",
            .reset_content => "Reset Content",
            .partial_content => "Partial Content",
            .multi_status => "Multi-Status",
            .already_reported => "Already Reported",
            .im_used => "IM Used",
            // 3xx
            .multiple_choices => "Multiple Choices",
            .moved_permanently => "Moved Permanently",
            .found => "Found",
            .see_other => "See Other",
            .not_modified => "Not Modified",
            .use_proxy => "Use Proxy",
            .temporary_redirect => "Temporary Redirect",
            .permanent_redirect => "Permanent Redirect",
            // 4xx
            .bad_request => "Bad Request",
            .unauthorized => "Unauthorized",
            .payment_required => "Payment Required",
            .forbidden => "Forbidden",
            .not_found => "Not Found",
            .method_not_allowed => "Method Not Allowed",
            .not_acceptable => "Not Acceptable",
            .proxy_authentication_required => "Proxy Authentication Required",
            .request_timeout => "Request Timeout",
            .conflict => "Conflict",
            .gone => "Gone",
            .length_required => "Length Required",
            .precondition_failed => "Precondition Failed",
            .payload_too_large => "Payload Too Large",
            .uri_too_long => "URI Too Long",
            .unsupported_media_type => "Unsupported Media Type",
            .range_not_satisfiable => "Range Not Satisfiable",
            .expectation_failed => "Expectation Failed",
            .im_a_teapot => "I'm a teapot",
            .misdirected_request => "Misdirected Request",
            .unprocessable_entity => "Unprocessable Entity",
            .locked => "Locked",
            .failed_dependency => "Failed Dependency",
            .too_early => "Too Early",
            .upgrade_required => "Upgrade Required",
            .precondition_required => "Precondition Required",
            .too_many_requests => "Too Many Requests",
            .request_header_fields_too_large => "Request Header Fields Too Large",
            .unavailable_for_legal_reasons => "Unavailable For Legal Reasons",
            // 5xx
            .internal_server_error => "Internal Server Error",
            .not_implemented => "Not Implemented",
            .bad_gateway => "Bad Gateway",
            .service_unavailable => "Service Unavailable",
            .gateway_timeout => "Gateway Timeout",
            .http_version_not_supported => "HTTP Version Not Supported",
            .variant_also_negotiates => "Variant Also Negotiates",
            .insufficient_storage => "Insufficient Storage",
            .loop_detected => "Loop Detected",
            .not_extended => "Not Extended",
            .network_authentication_required => "Network Authentication Required",
        };
    }

    /// Get the numeric code.
    pub fn code(self: StatusCode) u16 {
        return @intFromEnum(self);
    }
};

/// HTTP version
pub const Version = enum {
    http_0_9,
    http_1_0,
    http_1_1,
    http_2,
    http_3,

    /// Get version as string.
    pub fn toString(self: Version) []const u8 {
        return switch (self) {
            .http_0_9 => "HTTP/0.9",
            .http_1_0 => "HTTP/1.0",
            .http_1_1 => "HTTP/1.1",
            .http_2 => "HTTP/2",
            .http_3 => "HTTP/3",
        };
    }

    /// Check if this version supports persistent connections by default.
    pub fn supportsKeepAlive(self: Version) bool {
        return switch (self) {
            .http_0_9, .http_1_0 => false,
            else => true,
        };
    }

    /// Check if this version supports request pipelining.
    pub fn supportsPipelining(self: Version) bool {
        return switch (self) {
            .http_1_1 => true,
            else => false,
        };
    }

    /// Check if this version supports multiplexing.
    pub fn supportsMultiplexing(self: Version) bool {
        return switch (self) {
            .http_2, .http_3 => true,
            else => false,
        };
    }
};

/// Parse an HTTP method from string.
pub fn parseMethod(str: []const u8) ?Method {
    const methods = std.meta.fields(Method);
    inline for (methods) |field| {
        if (std.ascii.eqlIgnoreCase(str, field.name)) {
            return @field(Method, field.name);
        }
    }
    return null;
}

/// Check if a string is a valid HTTP method.
pub fn isValidMethod(str: []const u8) bool {
    return parseMethod(str) != null;
}

/// Parse an HTTP status code from integer.
pub fn parseStatusCode(code: u16) ?StatusCode {
    // Check if the code is a valid StatusCode enum value
    inline for (@typeInfo(StatusCode).@"enum".fields) |field| {
        if (field.value == code) {
            return @enumFromInt(code);
        }
    }
    return null;
}

/// Check if an integer is a valid HTTP status code.
pub fn isValidStatusCode(code: u16) bool {
    return parseStatusCode(code) != null;
}

/// Check if an integer is in the valid status code range (100-599).
pub fn isStatusCodeInRange(code: u16) bool {
    return code >= 100 and code <= 599;
}

/// Parse HTTP version from string.
pub fn parseVersion(str: []const u8) ?Version {
    if (std.mem.eql(u8, str, "HTTP/0.9")) return .http_0_9;
    if (std.mem.eql(u8, str, "HTTP/1.0")) return .http_1_0;
    if (std.mem.eql(u8, str, "HTTP/1.1")) return .http_1_1;
    if (std.mem.eql(u8, str, "HTTP/2") or std.mem.eql(u8, str, "HTTP/2.0")) return .http_2;
    if (std.mem.eql(u8, str, "HTTP/3") or std.mem.eql(u8, str, "HTTP/3.0")) return .http_3;
    return null;
}

/// Format a status line (e.g., "HTTP/1.1 200 OK").
pub fn formatStatusLine(version: Version, status: StatusCode, buffer: []u8) []u8 {
    const version_str = version.toString();
    const code_val = status.code();
    const reason = status.reasonPhrase();

    const written = std.fmt.bufPrint(buffer, "{s} {d} {s}", .{ version_str, code_val, reason }) catch return buffer[0..0];
    return written;
}

/// Format a request line (e.g., "GET /path HTTP/1.1").
pub fn formatRequestLine(method: Method, path: []const u8, version: Version, buffer: []u8) []u8 {
    const method_str = method.toString();
    const version_str = version.toString();

    const written = std.fmt.bufPrint(buffer, "{s} {s} {s}", .{ method_str, path, version_str }) catch return buffer[0..0];
    return written;
}

/// Check if a method is allowed for a given set of allowed methods.
pub fn isMethodAllowed(method: Method, allowed: []const Method) bool {
    for (allowed) |m| {
        if (m == method) return true;
    }
    return false;
}

/// Get the appropriate status code for method not allowed.
pub fn methodNotAllowed(method: Method, resource_exists: bool) StatusCode {
    _ = method;
    if (!resource_exists) return .not_found;
    return .method_not_allowed;
}

test "Method properties" {
    try std.testing.expect(Method.GET.isSafe());
    try std.testing.expect(Method.GET.isIdempotent());
    try std.testing.expect(Method.GET.isCacheable());
    try std.testing.expect(!Method.POST.isSafe());
    try std.testing.expect(!Method.POST.isIdempotent());
    try std.testing.expect(Method.POST.hasRequestBody());
    try std.testing.expect(!Method.HEAD.hasResponseBody());
}

test "StatusCode properties" {
    try std.testing.expect(StatusCode.ok.isSuccess());
    try std.testing.expect(!StatusCode.ok.isError());
    try std.testing.expect(StatusCode.not_found.isError());
    try std.testing.expect(StatusCode.moved_permanently.isRedirect());
    try std.testing.expect(!StatusCode.no_content.hasBody());
    try std.testing.expectEqual(@as(u16, 200), StatusCode.ok.code());
    try std.testing.expectEqualStrings("OK", StatusCode.ok.reasonPhrase());
}

test "parseMethod" {
    try std.testing.expect(parseMethod("GET") == .GET);
    try std.testing.expect(parseMethod("get") == .GET);
    try std.testing.expect(parseMethod("INVALID") == null);
}

test "parseStatusCode" {
    try std.testing.expect(parseStatusCode(200) == .ok);
    try std.testing.expect(parseStatusCode(404) == .not_found);
    try std.testing.expect(parseStatusCode(999) == null);
}

test "parseVersion" {
    try std.testing.expect(parseVersion("HTTP/1.1") == .http_1_1);
    try std.testing.expect(parseVersion("HTTP/2") == .http_2);
    try std.testing.expect(parseVersion("INVALID") == null);
}

test "formatStatusLine" {
    var buffer: [64]u8 = undefined;
    const line = formatStatusLine(.http_1_1, .ok, &buffer);
    try std.testing.expectEqualStrings("HTTP/1.1 200 OK", line);
}

test "formatRequestLine" {
    var buffer: [64]u8 = undefined;
    const line = formatRequestLine(.GET, "/api/users", .http_1_1, &buffer);
    try std.testing.expectEqualStrings("GET /api/users HTTP/1.1", line);
}

test "Version properties" {
    try std.testing.expect(!Version.http_1_0.supportsKeepAlive());
    try std.testing.expect(Version.http_1_1.supportsKeepAlive());
    try std.testing.expect(Version.http_1_1.supportsPipelining());
    try std.testing.expect(Version.http_2.supportsMultiplexing());
}
