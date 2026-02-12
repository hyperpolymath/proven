// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe command execution with input sanitization and injection prevention.
//!
//! Provides command execution utilities that sanitize inputs to prevent
//! shell injection attacks. All functions validate inputs before execution
//! and return explicit error types on failure.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for command operations.
pub const CommandError = error{
    InvalidCommand,
    InvalidArgument,
    DangerousCharacter,
    CommandTooLong,
    EmptyCommand,
    PathTraversal,
    NullByteDetected,
    OutOfMemory,
};

/// Maximum allowed command length.
pub const MAX_COMMAND_LENGTH: usize = 8192;

/// Maximum allowed argument length.
pub const MAX_ARGUMENT_LENGTH: usize = 4096;

/// Characters that are dangerous in shell contexts.
const DANGEROUS_SHELL_CHARS = [_]u8{
    ';', '&', '|', '$', '`', '(', ')', '{', '}',
    '<', '>', '\n', '\r', '!', '#', '~',
};

/// Characters that require quoting but are not dangerous.
const REQUIRES_QUOTING_CHARS = [_]u8{
    ' ', '\t', '"', '\'', '*', '?', '[', ']', '\\',
};

/// Check if a character is dangerous for shell execution.
pub fn isDangerousChar(c: u8) bool {
    for (DANGEROUS_SHELL_CHARS) |dangerous| {
        if (c == dangerous) return true;
    }
    return false;
}

/// Check if a character requires quoting.
pub fn requiresQuoting(c: u8) bool {
    if (isDangerousChar(c)) return true;
    for (REQUIRES_QUOTING_CHARS) |quote_char| {
        if (c == quote_char) return true;
    }
    return false;
}

/// Check if a string contains any dangerous shell characters.
pub fn containsDangerousChars(input: []const u8) bool {
    for (input) |c| {
        if (isDangerousChar(c)) return true;
        if (c == 0) return true; // Null byte is always dangerous
    }
    return false;
}

/// Check if a string contains a null byte.
pub fn containsNullByte(input: []const u8) bool {
    return std.mem.indexOfScalar(u8, input, 0) != null;
}

/// Validate a command name (no path traversal, no dangerous chars).
pub fn validateCommandName(command: []const u8) CommandError!void {
    if (command.len == 0) return error.EmptyCommand;
    if (command.len > MAX_COMMAND_LENGTH) return error.CommandTooLong;
    if (containsNullByte(command)) return error.NullByteDetected;
    if (containsDangerousChars(command)) return error.DangerousCharacter;

    // Check for path traversal
    if (std.mem.indexOf(u8, command, "..") != null) return error.PathTraversal;
}

/// Validate a command argument.
pub fn validateArgument(arg: []const u8) CommandError!void {
    if (arg.len > MAX_ARGUMENT_LENGTH) return error.InvalidArgument;
    if (containsNullByte(arg)) return error.NullByteDetected;
    if (containsDangerousChars(arg)) return error.DangerousCharacter;
}

/// Sanitize a string for safe shell usage by escaping special characters.
/// Returns an allocated string that must be freed.
pub fn sanitizeForShell(allocator: Allocator, input: []const u8) CommandError![]u8 {
    if (containsNullByte(input)) return error.NullByteDetected;

    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (input) |c| {
        if (isDangerousChar(c)) {
            // Replace dangerous characters with safe alternatives
            result.append('_') catch return error.OutOfMemory;
        } else if (requiresQuoting(c)) {
            // Escape characters that need quoting
            result.append('\\') catch return error.OutOfMemory;
            result.append(c) catch return error.OutOfMemory;
        } else {
            result.append(c) catch return error.OutOfMemory;
        }
    }

    return result.toOwnedSlice() catch error.OutOfMemory;
}

/// Quote a string for safe shell usage using single quotes.
/// Single quotes prevent all interpretation except for single quotes themselves.
pub fn quoteForShell(allocator: Allocator, input: []const u8) CommandError![]u8 {
    if (containsNullByte(input)) return error.NullByteDetected;

    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    result.append('\'') catch return error.OutOfMemory;

    for (input) |c| {
        if (c == '\'') {
            // End quote, add escaped quote, start quote again
            result.appendSlice("'\\''") catch return error.OutOfMemory;
        } else {
            result.append(c) catch return error.OutOfMemory;
        }
    }

    result.append('\'') catch return error.OutOfMemory;

    return result.toOwnedSlice() catch error.OutOfMemory;
}

/// Validate and sanitize an array of arguments.
pub fn validateArguments(args: []const []const u8) CommandError!void {
    for (args) |arg| {
        try validateArgument(arg);
    }
}

/// Build a safe command string from validated components.
pub fn buildSafeCommand(allocator: Allocator, command: []const u8, args: []const []const u8) CommandError![]u8 {
    try validateCommandName(command);
    try validateArguments(args);

    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    // Add the command
    result.appendSlice(command) catch return error.OutOfMemory;

    // Add each argument, quoted
    for (args) |arg| {
        result.append(' ') catch return error.OutOfMemory;
        const quoted = try quoteForShell(allocator, arg);
        defer allocator.free(quoted);
        result.appendSlice(quoted) catch return error.OutOfMemory;
    }

    return result.toOwnedSlice() catch error.OutOfMemory;
}

/// Check if a command path is absolute.
pub fn isAbsolutePath(path: []const u8) bool {
    return path.len > 0 and path[0] == '/';
}

/// Validate that a command exists as an absolute path (basic check).
pub fn validateAbsoluteCommand(path: []const u8) CommandError!void {
    if (!isAbsolutePath(path)) return error.InvalidCommand;
    try validateCommandName(path);
}

/// Split a command string into command and arguments (simple split by spaces).
/// Does not handle quoted arguments - use for already-validated input only.
pub fn splitCommand(allocator: Allocator, command_line: []const u8) CommandError!struct {
    command: []const u8,
    args: [][]const u8,
} {
    if (command_line.len == 0) return error.EmptyCommand;
    if (containsNullByte(command_line)) return error.NullByteDetected;

    var parts = std.array_list.Managed([]const u8).init(allocator);
    errdefer parts.deinit();

    var iter = std.mem.splitScalar(u8, command_line, ' ');
    while (iter.next()) |part| {
        if (part.len > 0) {
            parts.append(part) catch return error.OutOfMemory;
        }
    }

    if (parts.items.len == 0) return error.EmptyCommand;

    const command = parts.items[0];
    const args = parts.toOwnedSlice() catch return error.OutOfMemory;

    return .{
        .command = command,
        .args = args[1..],
    };
}

/// Allowed command configuration for whitelisting.
pub const AllowedCommand = struct {
    name: []const u8,
    allowed_args_patterns: ?[]const []const u8 = null,
    require_absolute_path: bool = false,
};

/// Check if a command is in the allowed list.
pub fn isCommandAllowed(command: []const u8, allowed_commands: []const AllowedCommand) bool {
    for (allowed_commands) |allowed| {
        if (std.mem.eql(u8, command, allowed.name)) {
            return true;
        }
    }
    return false;
}

test "isDangerousChar" {
    try std.testing.expect(isDangerousChar(';'));
    try std.testing.expect(isDangerousChar('|'));
    try std.testing.expect(isDangerousChar('$'));
    try std.testing.expect(!isDangerousChar('a'));
    try std.testing.expect(!isDangerousChar('1'));
}

test "containsDangerousChars" {
    try std.testing.expect(containsDangerousChars("echo; rm -rf"));
    try std.testing.expect(containsDangerousChars("ls | grep"));
    try std.testing.expect(containsDangerousChars("$HOME"));
    try std.testing.expect(!containsDangerousChars("ls"));
    try std.testing.expect(!containsDangerousChars("file.txt"));
}

test "validateCommandName" {
    try validateCommandName("ls");
    try validateCommandName("/usr/bin/ls");
    try std.testing.expectError(error.EmptyCommand, validateCommandName(""));
    try std.testing.expectError(error.DangerousCharacter, validateCommandName("ls;rm"));
    try std.testing.expectError(error.PathTraversal, validateCommandName("../../../bin/ls"));
}

test "sanitizeForShell" {
    const allocator = std.testing.allocator;

    const result1 = try sanitizeForShell(allocator, "hello world");
    defer allocator.free(result1);
    try std.testing.expectEqualStrings("hello\\ world", result1);

    const result2 = try sanitizeForShell(allocator, "file;rm");
    defer allocator.free(result2);
    try std.testing.expectEqualStrings("file_rm", result2);
}

test "quoteForShell" {
    const allocator = std.testing.allocator;

    const result1 = try quoteForShell(allocator, "hello world");
    defer allocator.free(result1);
    try std.testing.expectEqualStrings("'hello world'", result1);

    const result2 = try quoteForShell(allocator, "it's");
    defer allocator.free(result2);
    try std.testing.expectEqualStrings("'it'\\''s'", result2);
}

test "buildSafeCommand" {
    const allocator = std.testing.allocator;

    const args = [_][]const u8{ "file.txt", "output.txt" };
    const result = try buildSafeCommand(allocator, "cp", &args);
    defer allocator.free(result);
    try std.testing.expectEqualStrings("cp 'file.txt' 'output.txt'", result);
}

test "isAbsolutePath" {
    try std.testing.expect(isAbsolutePath("/usr/bin/ls"));
    try std.testing.expect(!isAbsolutePath("ls"));
    try std.testing.expect(!isAbsolutePath("./ls"));
}

test "isCommandAllowed" {
    const allowed = [_]AllowedCommand{
        .{ .name = "ls" },
        .{ .name = "cat" },
        .{ .name = "grep" },
    };

    try std.testing.expect(isCommandAllowed("ls", &allowed));
    try std.testing.expect(isCommandAllowed("cat", &allowed));
    try std.testing.expect(!isCommandAllowed("rm", &allowed));
}
