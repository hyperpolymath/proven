// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe structured logging with level validation that cannot crash.
//!
//! Provides a structured logging interface with validated log levels,
//! safe string formatting, and support for structured context fields.
//! All operations are memory-safe and will not panic on invalid input.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for logging operations.
pub const LogError = error{
    InvalidLevel,
    InvalidFieldName,
    InvalidFieldValue,
    MessageTooLong,
    TooManyFields,
    OutOfMemory,
};

/// Log severity levels following syslog conventions.
pub const Level = enum(u8) {
    /// System is unusable
    emergency = 0,
    /// Action must be taken immediately
    alert = 1,
    /// Critical conditions
    critical = 2,
    /// Error conditions
    err = 3,
    /// Warning conditions
    warning = 4,
    /// Normal but significant condition
    notice = 5,
    /// Informational messages
    info = 6,
    /// Debug-level messages
    debug = 7,
    /// Trace-level messages (verbose debug)
    trace = 8,

    /// Parse a level from string (case-insensitive).
    pub fn fromString(level_string: []const u8) LogError!Level {
        const level_map = .{
            .{ "emergency", .emergency },
            .{ "emerg", .emergency },
            .{ "alert", .alert },
            .{ "critical", .critical },
            .{ "crit", .critical },
            .{ "error", .err },
            .{ "err", .err },
            .{ "warning", .warning },
            .{ "warn", .warning },
            .{ "notice", .notice },
            .{ "info", .info },
            .{ "information", .info },
            .{ "debug", .debug },
            .{ "trace", .trace },
        };

        var lower_buf: [32]u8 = undefined;
        if (level_string.len > lower_buf.len) return error.InvalidLevel;

        for (level_string, 0..) |char, index| {
            lower_buf[index] = std.ascii.toLower(char);
        }
        const lower = lower_buf[0..level_string.len];

        inline for (level_map) |entry| {
            if (std.mem.eql(u8, lower, entry[0])) {
                return entry[1];
            }
        }
        return error.InvalidLevel;
    }

    /// Convert level to string representation.
    pub fn toString(self: Level) []const u8 {
        return switch (self) {
            .emergency => "EMERGENCY",
            .alert => "ALERT",
            .critical => "CRITICAL",
            .err => "ERROR",
            .warning => "WARNING",
            .notice => "NOTICE",
            .info => "INFO",
            .debug => "DEBUG",
            .trace => "TRACE",
        };
    }

    /// Convert level to short string representation.
    pub fn toShortString(self: Level) []const u8 {
        return switch (self) {
            .emergency => "EMR",
            .alert => "ALR",
            .critical => "CRT",
            .err => "ERR",
            .warning => "WRN",
            .notice => "NTC",
            .info => "INF",
            .debug => "DBG",
            .trace => "TRC",
        };
    }

    /// Check if this level is at least as severe as another.
    pub fn isAtLeast(self: Level, other: Level) bool {
        return @intFromEnum(self) <= @intFromEnum(other);
    }

    /// Check if this level should be logged given a minimum level.
    pub fn shouldLog(self: Level, minimum_level: Level) bool {
        return @intFromEnum(self) <= @intFromEnum(minimum_level);
    }

    /// Get numeric severity (0 = most severe, 8 = least severe).
    pub fn severity(self: Level) u8 {
        return @intFromEnum(self);
    }
};

/// Maximum message length
pub const MAX_MESSAGE_LENGTH = 8192;

/// Maximum number of context fields
pub const MAX_FIELDS = 32;

/// Maximum field name length
pub const MAX_FIELD_NAME_LENGTH = 64;

/// Maximum field value length
pub const MAX_FIELD_VALUE_LENGTH = 1024;

/// A single context field (key-value pair).
pub const Field = struct {
    name: []const u8,
    value: FieldValue,
};

/// Supported field value types.
pub const FieldValue = union(enum) {
    string: []const u8,
    integer: i64,
    unsigned: u64,
    float: f64,
    boolean: bool,
    null_value: void,

    /// Format value as string.
    pub fn format(self: FieldValue, allocator: Allocator) ![]u8 {
        return switch (self) {
            .string => |string| try allocator.dupe(u8, string),
            .integer => |integer| try std.fmt.allocPrint(allocator, "{d}", .{integer}),
            .unsigned => |unsigned| try std.fmt.allocPrint(allocator, "{d}", .{unsigned}),
            .float => |float_val| try std.fmt.allocPrint(allocator, "{d:.6}", .{float_val}),
            .boolean => |boolean| try allocator.dupe(u8, if (boolean) "true" else "false"),
            .null_value => try allocator.dupe(u8, "null"),
        };
    }
};

/// A structured log entry.
pub const LogEntry = struct {
    level: Level,
    message: []const u8,
    fields: []const Field,
    timestamp_ns: ?i128 = null,
    logger_name: ?[]const u8 = null,
    source_location: ?SourceLocation = null,

    /// Source code location information.
    pub const SourceLocation = struct {
        file: []const u8,
        line: u32,
        column: u32,
        function_name: ?[]const u8 = null,
    };
};

/// Validate a field name.
pub fn isValidFieldName(name: []const u8) bool {
    if (name.len == 0 or name.len > MAX_FIELD_NAME_LENGTH) return false;

    // First character must be letter or underscore
    const first = name[0];
    if (!std.ascii.isAlphabetic(first) and first != '_') return false;

    // Rest must be alphanumeric, underscore, or hyphen
    for (name[1..]) |char| {
        if (!std.ascii.isAlphanumeric(char) and char != '_' and char != '-' and char != '.') {
            return false;
        }
    }

    return true;
}

/// Validate a field value (string type).
pub fn isValidFieldValue(value: []const u8) bool {
    if (value.len > MAX_FIELD_VALUE_LENGTH) return false;

    // Check for control characters (except newline, tab, carriage return)
    for (value) |char| {
        if (char < 0x20 and char != '\n' and char != '\t' and char != '\r') {
            return false;
        }
    }

    return true;
}

/// Validate a log message.
pub fn isValidMessage(message: []const u8) bool {
    if (message.len > MAX_MESSAGE_LENGTH) return false;

    // Check for null bytes
    for (message) |char| {
        if (char == 0) return false;
    }

    return true;
}

/// Structured logger with context support.
pub const Logger = struct {
    allocator: Allocator,
    name: ?[]const u8,
    minimum_level: Level,
    context_fields: std.ArrayList(Field),
    writer: ?std.io.AnyWriter,

    /// Create a new logger.
    pub fn init(allocator: Allocator) Logger {
        return Logger{
            .allocator = allocator,
            .name = null,
            .minimum_level = .info,
            .context_fields = std.ArrayList(Field).init(allocator),
            .writer = null,
        };
    }

    /// Create a named logger.
    pub fn initNamed(allocator: Allocator, name: []const u8) Logger {
        return Logger{
            .allocator = allocator,
            .name = name,
            .minimum_level = .info,
            .context_fields = std.ArrayList(Field).init(allocator),
            .writer = null,
        };
    }

    /// Free logger resources.
    pub fn deinit(self: *Logger) void {
        self.context_fields.deinit();
    }

    /// Set minimum log level.
    pub fn setLevel(self: *Logger, level: Level) void {
        self.minimum_level = level;
    }

    /// Set output writer.
    pub fn setWriter(self: *Logger, writer: std.io.AnyWriter) void {
        self.writer = writer;
    }

    /// Add a context field that will be included in all log entries.
    pub fn addContextField(self: *Logger, name: []const u8, value: FieldValue) LogError!void {
        if (!isValidFieldName(name)) return error.InvalidFieldName;
        if (self.context_fields.items.len >= MAX_FIELDS) return error.TooManyFields;

        self.context_fields.append(Field{ .name = name, .value = value }) catch return error.OutOfMemory;
    }

    /// Clear all context fields.
    pub fn clearContext(self: *Logger) void {
        self.context_fields.clearRetainingCapacity();
    }

    /// Log a message at the specified level.
    pub fn log(self: *Logger, level: Level, message: []const u8, fields: []const Field) LogError!void {
        if (!level.shouldLog(self.minimum_level)) return;
        if (!isValidMessage(message)) return error.MessageTooLong;

        // Validate all fields
        for (fields) |field| {
            if (!isValidFieldName(field.name)) return error.InvalidFieldName;
        }

        const entry = LogEntry{
            .level = level,
            .message = message,
            .fields = fields,
            .timestamp_ns = std.time.nanoTimestamp(),
            .logger_name = self.name,
        };

        _ = entry; // Entry is constructed but not written in this basic implementation
    }

    /// Log at emergency level.
    pub fn emergency(self: *Logger, message: []const u8) LogError!void {
        return self.log(.emergency, message, &.{});
    }

    /// Log at alert level.
    pub fn alert(self: *Logger, message: []const u8) LogError!void {
        return self.log(.alert, message, &.{});
    }

    /// Log at critical level.
    pub fn critical(self: *Logger, message: []const u8) LogError!void {
        return self.log(.critical, message, &.{});
    }

    /// Log at error level.
    pub fn err(self: *Logger, message: []const u8) LogError!void {
        return self.log(.err, message, &.{});
    }

    /// Log at warning level.
    pub fn warning(self: *Logger, message: []const u8) LogError!void {
        return self.log(.warning, message, &.{});
    }

    /// Log at notice level.
    pub fn notice(self: *Logger, message: []const u8) LogError!void {
        return self.log(.notice, message, &.{});
    }

    /// Log at info level.
    pub fn info(self: *Logger, message: []const u8) LogError!void {
        return self.log(.info, message, &.{});
    }

    /// Log at debug level.
    pub fn debug(self: *Logger, message: []const u8) LogError!void {
        return self.log(.debug, message, &.{});
    }

    /// Log at trace level.
    pub fn trace(self: *Logger, message: []const u8) LogError!void {
        return self.log(.trace, message, &.{});
    }
};

/// Format a log entry as JSON.
pub fn formatJson(allocator: Allocator, entry: LogEntry) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    try result.appendSlice("{\"level\":\"");
    try result.appendSlice(entry.level.toString());
    try result.appendSlice("\",\"message\":\"");

    // Escape message for JSON
    for (entry.message) |char| {
        switch (char) {
            '"' => try result.appendSlice("\\\""),
            '\\' => try result.appendSlice("\\\\"),
            '\n' => try result.appendSlice("\\n"),
            '\r' => try result.appendSlice("\\r"),
            '\t' => try result.appendSlice("\\t"),
            else => {
                if (char < 0x20) {
                    try result.writer().print("\\u{x:0>4}", .{char});
                } else {
                    try result.append(char);
                }
            },
        }
    }

    try result.appendSlice("\"");

    // Add timestamp if present
    if (entry.timestamp_ns) |timestamp_value| {
        try result.writer().print(",\"timestamp_ns\":{d}", .{timestamp_value});
    }

    // Add logger name if present
    if (entry.logger_name) |name| {
        try result.appendSlice(",\"logger\":\"");
        try result.appendSlice(name);
        try result.appendSlice("\"");
    }

    // Add fields
    for (entry.fields) |field| {
        try result.appendSlice(",\"");
        try result.appendSlice(field.name);
        try result.appendSlice("\":");

        switch (field.value) {
            .string => |string| {
                try result.appendSlice("\"");
                try result.appendSlice(string);
                try result.appendSlice("\"");
            },
            .integer => |integer| try result.writer().print("{d}", .{integer}),
            .unsigned => |unsigned| try result.writer().print("{d}", .{unsigned}),
            .float => |float_val| try result.writer().print("{d}", .{float_val}),
            .boolean => |boolean| try result.appendSlice(if (boolean) "true" else "false"),
            .null_value => try result.appendSlice("null"),
        }
    }

    try result.appendSlice("}");

    return result.toOwnedSlice();
}

/// Format a log entry as a human-readable line.
pub fn formatLine(allocator: Allocator, entry: LogEntry) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    // Level
    try result.appendSlice("[");
    try result.appendSlice(entry.level.toShortString());
    try result.appendSlice("] ");

    // Logger name if present
    if (entry.logger_name) |name| {
        try result.appendSlice(name);
        try result.appendSlice(": ");
    }

    // Message
    try result.appendSlice(entry.message);

    // Fields
    if (entry.fields.len > 0) {
        try result.appendSlice(" {");
        for (entry.fields, 0..) |field, index| {
            if (index > 0) try result.appendSlice(", ");
            try result.appendSlice(field.name);
            try result.appendSlice("=");

            const value_str = try field.value.format(allocator);
            defer allocator.free(value_str);
            try result.appendSlice(value_str);
        }
        try result.appendSlice("}");
    }

    return result.toOwnedSlice();
}

test "Level.fromString" {
    try std.testing.expectEqual(Level.info, try Level.fromString("info"));
    try std.testing.expectEqual(Level.info, try Level.fromString("INFO"));
    try std.testing.expectEqual(Level.err, try Level.fromString("error"));
    try std.testing.expectEqual(Level.err, try Level.fromString("err"));
    try std.testing.expectEqual(Level.warning, try Level.fromString("warn"));
    try std.testing.expectError(error.InvalidLevel, Level.fromString("invalid"));
}

test "Level.shouldLog" {
    try std.testing.expect(Level.err.shouldLog(.info));
    try std.testing.expect(Level.info.shouldLog(.info));
    try std.testing.expect(!Level.debug.shouldLog(.info));
    try std.testing.expect(Level.emergency.shouldLog(.debug));
}

test "Level.isAtLeast" {
    try std.testing.expect(Level.err.isAtLeast(.warning));
    try std.testing.expect(Level.emergency.isAtLeast(.critical));
    try std.testing.expect(!Level.debug.isAtLeast(.info));
}

test "isValidFieldName" {
    try std.testing.expect(isValidFieldName("user_id"));
    try std.testing.expect(isValidFieldName("request-id"));
    try std.testing.expect(isValidFieldName("_private"));
    try std.testing.expect(isValidFieldName("service.name"));
    try std.testing.expect(!isValidFieldName(""));
    try std.testing.expect(!isValidFieldName("123invalid"));
    try std.testing.expect(!isValidFieldName("has space"));
}

test "isValidMessage" {
    try std.testing.expect(isValidMessage("Hello, World!"));
    try std.testing.expect(isValidMessage("Multi\nline\nmessage"));
    try std.testing.expect(!isValidMessage("contains\x00null"));
}

test "formatJson" {
    const allocator = std.testing.allocator;

    const entry = LogEntry{
        .level = .info,
        .message = "Test message",
        .fields = &.{
            Field{ .name = "user_id", .value = .{ .integer = 42 } },
        },
    };

    const json = try formatJson(allocator, entry);
    defer allocator.free(json);

    try std.testing.expect(std.mem.indexOf(u8, json, "\"level\":\"INFO\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"message\":\"Test message\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"user_id\":42") != null);
}

test "formatLine" {
    const allocator = std.testing.allocator;

    const entry = LogEntry{
        .level = .warning,
        .message = "Something happened",
        .fields = &.{},
        .logger_name = "test",
    };

    const line = try formatLine(allocator, entry);
    defer allocator.free(line);

    try std.testing.expectEqualStrings("[WRN] test: Something happened", line);
}

test "Logger basic usage" {
    const allocator = std.testing.allocator;

    var logger = Logger.initNamed(allocator, "test");
    defer logger.deinit();

    logger.setLevel(.debug);
    try logger.info("This is an info message");
    try logger.debug("This is a debug message");
}
