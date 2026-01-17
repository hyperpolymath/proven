// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe Model Context Protocol (MCP) message validation that cannot crash.
//!
//! Provides validation for MCP JSON-RPC messages, method names, tool calls,
//! and resource URIs according to the MCP specification. All validation
//! functions return errors instead of crashing on invalid inputs.

const std = @import("std");

/// Error types for MCP operations.
pub const McpError = error{
    /// JSON-RPC version is not "2.0".
    InvalidJsonRpcVersion,
    /// Message ID is invalid or missing when required.
    InvalidId,
    /// Method name is invalid or not allowed.
    InvalidMethod,
    /// Parameters are invalid for the method.
    InvalidParams,
    /// Result format is invalid.
    InvalidResult,
    /// Error object is malformed.
    InvalidErrorObject,
    /// Resource URI is invalid.
    InvalidResourceUri,
    /// Tool name is invalid.
    InvalidToolName,
    /// Prompt name is invalid.
    InvalidPromptName,
    /// Content type is not supported.
    UnsupportedContentType,
    /// Message is too large.
    MessageTooLarge,
    /// Required field is missing.
    MissingRequiredField,
    /// Memory allocation failed.
    OutOfMemory,
};

/// Maximum allowed message size in bytes.
pub const MAX_MESSAGE_SIZE = 10 * 1024 * 1024; // 10 MB

/// Maximum method name length.
pub const MAX_METHOD_LENGTH = 256;

/// Maximum tool/prompt name length.
pub const MAX_NAME_LENGTH = 128;

/// Maximum URI length.
pub const MAX_URI_LENGTH = 2048;

/// MCP message types.
pub const MessageType = enum {
    request,
    response,
    notification,
    error_response,
};

/// Standard MCP methods.
pub const StandardMethods = struct {
    // Lifecycle
    pub const INITIALIZE = "initialize";
    pub const INITIALIZED = "notifications/initialized";
    pub const SHUTDOWN = "shutdown";

    // Tools
    pub const TOOLS_LIST = "tools/list";
    pub const TOOLS_CALL = "tools/call";

    // Resources
    pub const RESOURCES_LIST = "resources/list";
    pub const RESOURCES_READ = "resources/read";
    pub const RESOURCES_SUBSCRIBE = "resources/subscribe";
    pub const RESOURCES_UNSUBSCRIBE = "resources/unsubscribe";

    // Prompts
    pub const PROMPTS_LIST = "prompts/list";
    pub const PROMPTS_GET = "prompts/get";

    // Logging
    pub const LOGGING_SET_LEVEL = "logging/setLevel";

    // Notifications
    pub const NOTIFICATION_CANCELLED = "notifications/cancelled";
    pub const NOTIFICATION_PROGRESS = "notifications/progress";
    pub const NOTIFICATION_MESSAGE = "notifications/message";
    pub const NOTIFICATION_RESOURCES_UPDATED = "notifications/resources/updated";
    pub const NOTIFICATION_RESOURCES_LIST_CHANGED = "notifications/resources/list_changed";
    pub const NOTIFICATION_TOOLS_LIST_CHANGED = "notifications/tools/list_changed";
    pub const NOTIFICATION_PROMPTS_LIST_CHANGED = "notifications/prompts/list_changed";

    // Sampling
    pub const SAMPLING_CREATE_MESSAGE = "sampling/createMessage";

    // Completion
    pub const COMPLETION_COMPLETE = "completion/complete";
};

/// MCP content types.
pub const ContentType = enum {
    text,
    image,
    resource,

    pub fn fromString(s: []const u8) ?ContentType {
        if (std.mem.eql(u8, s, "text")) return .text;
        if (std.mem.eql(u8, s, "image")) return .image;
        if (std.mem.eql(u8, s, "resource")) return .resource;
        return null;
    }

    pub fn toString(self: ContentType) []const u8 {
        return switch (self) {
            .text => "text",
            .image => "image",
            .resource => "resource",
        };
    }
};

/// MCP log levels.
pub const LogLevel = enum {
    debug,
    info,
    notice,
    warning,
    @"error",
    critical,
    alert,
    emergency,

    pub fn fromString(s: []const u8) ?LogLevel {
        if (std.mem.eql(u8, s, "debug")) return .debug;
        if (std.mem.eql(u8, s, "info")) return .info;
        if (std.mem.eql(u8, s, "notice")) return .notice;
        if (std.mem.eql(u8, s, "warning")) return .warning;
        if (std.mem.eql(u8, s, "error")) return .@"error";
        if (std.mem.eql(u8, s, "critical")) return .critical;
        if (std.mem.eql(u8, s, "alert")) return .alert;
        if (std.mem.eql(u8, s, "emergency")) return .emergency;
        return null;
    }

    pub fn severity(self: LogLevel) u8 {
        return switch (self) {
            .debug => 0,
            .info => 1,
            .notice => 2,
            .warning => 3,
            .@"error" => 4,
            .critical => 5,
            .alert => 6,
            .emergency => 7,
        };
    }
};

/// Validated MCP request.
pub const ValidatedRequest = struct {
    id: []const u8,
    method: []const u8,
    has_params: bool,
};

/// Validated MCP notification.
pub const ValidatedNotification = struct {
    method: []const u8,
    has_params: bool,
};

/// Validated MCP response.
pub const ValidatedResponse = struct {
    id: []const u8,
    has_result: bool,
};

/// Validated MCP error response.
pub const ValidatedErrorResponse = struct {
    id: ?[]const u8,
    code: i64,
    message: []const u8,
    has_data: bool,
};

/// Validate a JSON-RPC version string.
pub fn validateJsonRpcVersion(version: []const u8) McpError!void {
    if (!std.mem.eql(u8, version, "2.0")) {
        return error.InvalidJsonRpcVersion;
    }
}

/// Validate a method name.
pub fn validateMethod(method: []const u8) McpError!void {
    if (method.len == 0 or method.len > MAX_METHOD_LENGTH) {
        return error.InvalidMethod;
    }

    // Method must contain only valid characters
    for (method) |c| {
        if (!isValidMethodChar(c)) {
            return error.InvalidMethod;
        }
    }
}

/// Check if a method is a notification (does not expect a response).
pub fn isNotificationMethod(method: []const u8) bool {
    return std.mem.startsWith(u8, method, "notifications/");
}

/// Check if a method is a standard MCP method.
pub fn isStandardMethod(method: []const u8) bool {
    const standard_methods = [_][]const u8{
        StandardMethods.INITIALIZE,
        StandardMethods.INITIALIZED,
        StandardMethods.SHUTDOWN,
        StandardMethods.TOOLS_LIST,
        StandardMethods.TOOLS_CALL,
        StandardMethods.RESOURCES_LIST,
        StandardMethods.RESOURCES_READ,
        StandardMethods.RESOURCES_SUBSCRIBE,
        StandardMethods.RESOURCES_UNSUBSCRIBE,
        StandardMethods.PROMPTS_LIST,
        StandardMethods.PROMPTS_GET,
        StandardMethods.LOGGING_SET_LEVEL,
        StandardMethods.NOTIFICATION_CANCELLED,
        StandardMethods.NOTIFICATION_PROGRESS,
        StandardMethods.NOTIFICATION_MESSAGE,
        StandardMethods.NOTIFICATION_RESOURCES_UPDATED,
        StandardMethods.NOTIFICATION_RESOURCES_LIST_CHANGED,
        StandardMethods.NOTIFICATION_TOOLS_LIST_CHANGED,
        StandardMethods.NOTIFICATION_PROMPTS_LIST_CHANGED,
        StandardMethods.SAMPLING_CREATE_MESSAGE,
        StandardMethods.COMPLETION_COMPLETE,
    };

    for (standard_methods) |m| {
        if (std.mem.eql(u8, method, m)) return true;
    }

    return false;
}

/// Validate a tool name.
pub fn validateToolName(name: []const u8) McpError!void {
    if (name.len == 0 or name.len > MAX_NAME_LENGTH) {
        return error.InvalidToolName;
    }

    // First character must be a letter or underscore
    if (!std.ascii.isAlphabetic(name[0]) and name[0] != '_') {
        return error.InvalidToolName;
    }

    // Rest must be alphanumeric, underscore, or hyphen
    for (name[1..]) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_' and c != '-') {
            return error.InvalidToolName;
        }
    }
}

/// Validate a prompt name.
pub fn validatePromptName(name: []const u8) McpError!void {
    if (name.len == 0 or name.len > MAX_NAME_LENGTH) {
        return error.InvalidPromptName;
    }

    // First character must be a letter or underscore
    if (!std.ascii.isAlphabetic(name[0]) and name[0] != '_') {
        return error.InvalidPromptName;
    }

    // Rest must be alphanumeric, underscore, or hyphen
    for (name[1..]) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_' and c != '-') {
            return error.InvalidPromptName;
        }
    }
}

/// Validate a resource URI.
pub fn validateResourceUri(uri: []const u8) McpError!void {
    if (uri.len == 0 or uri.len > MAX_URI_LENGTH) {
        return error.InvalidResourceUri;
    }

    // Must contain a scheme separator
    const scheme_end = std.mem.indexOf(u8, uri, "://") orelse {
        return error.InvalidResourceUri;
    };

    // Scheme must be non-empty
    if (scheme_end == 0) {
        return error.InvalidResourceUri;
    }

    // Validate scheme characters
    for (uri[0..scheme_end]) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '+' and c != '-' and c != '.') {
            return error.InvalidResourceUri;
        }
    }

    // Path must be non-empty after scheme
    if (uri.len <= scheme_end + 3) {
        return error.InvalidResourceUri;
    }
}

/// Validate a message ID (string or number).
pub fn validateId(id: []const u8) McpError!void {
    if (id.len == 0) {
        return error.InvalidId;
    }

    // ID can be a string (any non-empty) or a number representation
    // We accept any non-empty string as a valid ID
}

/// Validate an error code.
pub fn validateErrorCode(code: i64) bool {
    // Standard JSON-RPC error codes
    return switch (code) {
        -32700 => true, // Parse error
        -32600 => true, // Invalid Request
        -32601 => true, // Method not found
        -32602 => true, // Invalid params
        -32603 => true, // Internal error
        -32099...-32000 => true, // Server error range
        else => code < -32099 or code > -32000, // Application-defined
    };
}

/// Standard JSON-RPC error codes.
pub const ErrorCodes = struct {
    pub const PARSE_ERROR = -32700;
    pub const INVALID_REQUEST = -32600;
    pub const METHOD_NOT_FOUND = -32601;
    pub const INVALID_PARAMS = -32602;
    pub const INTERNAL_ERROR = -32603;
};

/// Get the error message for a standard error code.
pub fn errorCodeMessage(code: i64) ?[]const u8 {
    return switch (code) {
        ErrorCodes.PARSE_ERROR => "Parse error",
        ErrorCodes.INVALID_REQUEST => "Invalid Request",
        ErrorCodes.METHOD_NOT_FOUND => "Method not found",
        ErrorCodes.INVALID_PARAMS => "Invalid params",
        ErrorCodes.INTERNAL_ERROR => "Internal error",
        else => null,
    };
}

/// Validate message size.
pub fn validateMessageSize(size: usize) McpError!void {
    if (size > MAX_MESSAGE_SIZE) {
        return error.MessageTooLarge;
    }
}

/// Check if a character is valid for method names.
fn isValidMethodChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '/' or c == '_' or c == '-' or c == '.';
}

/// Detect message type from JSON object fields.
pub fn detectMessageType(
    has_id: bool,
    has_method: bool,
    has_result: bool,
    has_error: bool,
) ?MessageType {
    if (has_method and has_id and !has_result and !has_error) {
        return .request;
    }
    if (has_method and !has_id and !has_result and !has_error) {
        return .notification;
    }
    if (has_id and has_result and !has_method and !has_error) {
        return .response;
    }
    if (has_id and has_error and !has_method and !has_result) {
        return .error_response;
    }
    // Also allow error response without id (for parse errors)
    if (has_error and !has_method and !has_result) {
        return .error_response;
    }
    return null;
}

/// Capability flags for MCP server/client.
pub const Capabilities = struct {
    tools: bool = false,
    resources: bool = false,
    prompts: bool = false,
    logging: bool = false,
    sampling: bool = false,
    experimental: ?[]const u8 = null,

    /// Create default server capabilities.
    pub fn defaultServer() Capabilities {
        return .{
            .tools = true,
            .resources = true,
            .prompts = true,
            .logging = true,
        };
    }

    /// Create default client capabilities.
    pub fn defaultClient() Capabilities {
        return .{
            .sampling = true,
        };
    }
};

/// MCP protocol version.
pub const PROTOCOL_VERSION = "2024-11-05";

/// Check if a protocol version is supported.
pub fn isSupportedProtocolVersion(version: []const u8) bool {
    const supported = [_][]const u8{
        "2024-11-05",
        "2024-10-07",
    };

    for (supported) |v| {
        if (std.mem.eql(u8, version, v)) return true;
    }

    return false;
}

test "validateJsonRpcVersion" {
    try validateJsonRpcVersion("2.0");
    try std.testing.expectError(error.InvalidJsonRpcVersion, validateJsonRpcVersion("1.0"));
    try std.testing.expectError(error.InvalidJsonRpcVersion, validateJsonRpcVersion(""));
}

test "validateMethod" {
    try validateMethod("tools/list");
    try validateMethod("custom_method");
    try validateMethod("my.namespace/method");
    try std.testing.expectError(error.InvalidMethod, validateMethod(""));
    try std.testing.expectError(error.InvalidMethod, validateMethod("method with space"));
}

test "isNotificationMethod" {
    try std.testing.expect(isNotificationMethod("notifications/initialized"));
    try std.testing.expect(isNotificationMethod("notifications/cancelled"));
    try std.testing.expect(!isNotificationMethod("tools/list"));
    try std.testing.expect(!isNotificationMethod("initialize"));
}

test "isStandardMethod" {
    try std.testing.expect(isStandardMethod("initialize"));
    try std.testing.expect(isStandardMethod("tools/list"));
    try std.testing.expect(isStandardMethod("resources/read"));
    try std.testing.expect(!isStandardMethod("custom/method"));
}

test "validateToolName" {
    try validateToolName("my_tool");
    try validateToolName("tool123");
    try validateToolName("_private_tool");
    try validateToolName("tool-name");
    try std.testing.expectError(error.InvalidToolName, validateToolName(""));
    try std.testing.expectError(error.InvalidToolName, validateToolName("123tool"));
    try std.testing.expectError(error.InvalidToolName, validateToolName("tool name"));
}

test "validateResourceUri" {
    try validateResourceUri("file:///path/to/file");
    try validateResourceUri("https://example.com/resource");
    try validateResourceUri("custom://my-resource");
    try std.testing.expectError(error.InvalidResourceUri, validateResourceUri(""));
    try std.testing.expectError(error.InvalidResourceUri, validateResourceUri("no-scheme"));
    try std.testing.expectError(error.InvalidResourceUri, validateResourceUri("://missing-scheme"));
}

test "detectMessageType" {
    try std.testing.expectEqual(MessageType.request, detectMessageType(true, true, false, false).?);
    try std.testing.expectEqual(MessageType.notification, detectMessageType(false, true, false, false).?);
    try std.testing.expectEqual(MessageType.response, detectMessageType(true, false, true, false).?);
    try std.testing.expectEqual(MessageType.error_response, detectMessageType(true, false, false, true).?);
    try std.testing.expectEqual(@as(?MessageType, null), detectMessageType(true, true, true, true));
}

test "ErrorCodes" {
    try std.testing.expectEqualStrings("Parse error", errorCodeMessage(ErrorCodes.PARSE_ERROR).?);
    try std.testing.expectEqualStrings("Invalid Request", errorCodeMessage(ErrorCodes.INVALID_REQUEST).?);
    try std.testing.expect(validateErrorCode(ErrorCodes.PARSE_ERROR));
    try std.testing.expect(validateErrorCode(-32050)); // Server error range
}

test "LogLevel" {
    try std.testing.expectEqual(LogLevel.debug, LogLevel.fromString("debug").?);
    try std.testing.expectEqual(LogLevel.@"error", LogLevel.fromString("error").?);
    try std.testing.expect(LogLevel.@"error".severity() > LogLevel.warning.severity());
    try std.testing.expect(LogLevel.debug.severity() < LogLevel.info.severity());
}

test "ContentType" {
    try std.testing.expectEqual(ContentType.text, ContentType.fromString("text").?);
    try std.testing.expectEqual(ContentType.image, ContentType.fromString("image").?);
    try std.testing.expectEqualStrings("resource", ContentType.resource.toString());
}

test "isSupportedProtocolVersion" {
    try std.testing.expect(isSupportedProtocolVersion("2024-11-05"));
    try std.testing.expect(isSupportedProtocolVersion("2024-10-07"));
    try std.testing.expect(!isSupportedProtocolVersion("1.0.0"));
}

test "validateMessageSize" {
    try validateMessageSize(1024);
    try validateMessageSize(MAX_MESSAGE_SIZE);
    try std.testing.expectError(error.MessageTooLarge, validateMessageSize(MAX_MESSAGE_SIZE + 1));
}

test "Capabilities" {
    const server_caps = Capabilities.defaultServer();
    try std.testing.expect(server_caps.tools);
    try std.testing.expect(server_caps.resources);
    try std.testing.expect(!server_caps.sampling);

    const client_caps = Capabilities.defaultClient();
    try std.testing.expect(client_caps.sampling);
    try std.testing.expect(!client_caps.tools);
}
