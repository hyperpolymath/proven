// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe command-line argument parsing with bounds checking.
//!
//! Provides safe functions to parse and validate command-line arguments
//! without risk of buffer overflows or undefined behavior. Supports
//! flag parsing, positional arguments, and value extraction.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for argument parsing operations.
pub const ArgsError = error{
    InvalidArgument,
    MissingValue,
    UnknownOption,
    DuplicateOption,
    TooManyArguments,
    InvalidNumber,
    ValueOutOfRange,
    OutOfMemory,
    EmptyArgument,
};

/// Argument type classification.
pub const ArgType = enum {
    short_flag, // -f
    long_flag, // --flag
    short_option, // -o value or -ovalue
    long_option, // --option=value or --option value
    positional, // regular argument
    separator, // --

    /// Check if this is a flag (no value expected).
    pub fn isFlag(self: ArgType) bool {
        return self == .short_flag or self == .long_flag;
    }

    /// Check if this is an option (value expected).
    pub fn isOption(self: ArgType) bool {
        return self == .short_option or self == .long_option;
    }
};

/// Result of classifying a single argument.
pub const ClassifiedArg = struct {
    arg_type: ArgType,
    name: []const u8,
    value: ?[]const u8,
    original: []const u8,
};

/// Classify a single argument string.
pub fn classifyArg(arg: []const u8) ClassifiedArg {
    if (arg.len == 0) {
        return ClassifiedArg{
            .arg_type = .positional,
            .name = "",
            .value = null,
            .original = arg,
        };
    }

    // Check for separator
    if (std.mem.eql(u8, arg, "--")) {
        return ClassifiedArg{
            .arg_type = .separator,
            .name = "",
            .value = null,
            .original = arg,
        };
    }

    // Check for long option (--something)
    if (arg.len >= 2 and arg[0] == '-' and arg[1] == '-') {
        const rest = arg[2..];
        if (rest.len == 0) {
            return ClassifiedArg{
                .arg_type = .separator,
                .name = "",
                .value = null,
                .original = arg,
            };
        }

        // Check for --option=value
        if (std.mem.indexOf(u8, rest, "=")) |eq_pos| {
            return ClassifiedArg{
                .arg_type = .long_option,
                .name = rest[0..eq_pos],
                .value = rest[eq_pos + 1 ..],
                .original = arg,
            };
        }

        return ClassifiedArg{
            .arg_type = .long_flag,
            .name = rest,
            .value = null,
            .original = arg,
        };
    }

    // Check for short option (-x)
    if (arg.len >= 2 and arg[0] == '-' and arg[1] != '-') {
        const rest = arg[1..];

        // -x only (single char)
        if (rest.len == 1) {
            return ClassifiedArg{
                .arg_type = .short_flag,
                .name = rest,
                .value = null,
                .original = arg,
            };
        }

        // -xvalue (short option with attached value)
        return ClassifiedArg{
            .arg_type = .short_option,
            .name = rest[0..1],
            .value = rest[1..],
            .original = arg,
        };
    }

    // Positional argument
    return ClassifiedArg{
        .arg_type = .positional,
        .name = arg,
        .value = null,
        .original = arg,
    };
}

/// Safe argument iterator with bounds checking.
pub fn SafeArgIterator(comptime max_args: usize) type {
    return struct {
        const Self = @This();

        args: []const []const u8,
        index: usize,
        past_separator: bool,
        positional_count: usize,

        /// Initialize from argument slice.
        pub fn init(args: []const []const u8) Self {
            return Self{
                .args = args,
                .index = 0,
                .past_separator = false,
                .positional_count = 0,
            };
        }

        /// Get the next argument, or null if exhausted.
        pub fn next(self: *Self) ?ClassifiedArg {
            if (self.index >= self.args.len) return null;
            if (self.index >= max_args) return null;

            const arg = self.args[self.index];
            self.index += 1;

            if (self.past_separator) {
                self.positional_count += 1;
                return ClassifiedArg{
                    .arg_type = .positional,
                    .name = arg,
                    .value = null,
                    .original = arg,
                };
            }

            const classified = classifyArg(arg);

            if (classified.arg_type == .separator) {
                self.past_separator = true;
            } else if (classified.arg_type == .positional) {
                self.positional_count += 1;
            }

            return classified;
        }

        /// Peek at the next argument without consuming it.
        pub fn peek(self: *const Self) ?[]const u8 {
            if (self.index >= self.args.len) return null;
            if (self.index >= max_args) return null;
            return self.args[self.index];
        }

        /// Skip the next argument.
        pub fn skip(self: *Self) void {
            if (self.index < self.args.len and self.index < max_args) {
                self.index += 1;
            }
        }

        /// Get the current position.
        pub fn position(self: *const Self) usize {
            return self.index;
        }

        /// Get remaining argument count.
        pub fn remaining(self: *const Self) usize {
            const max_remaining = max_args - @min(self.index, max_args);
            const actual_remaining = self.args.len - @min(self.index, self.args.len);
            return @min(max_remaining, actual_remaining);
        }

        /// Reset to beginning.
        pub fn reset(self: *Self) void {
            self.index = 0;
            self.past_separator = false;
            self.positional_count = 0;
        }
    };
}

/// Parse an integer from an argument string with bounds checking.
pub fn parseInt(comptime T: type, str: []const u8) ArgsError!T {
    if (str.len == 0) return error.EmptyArgument;

    return std.fmt.parseInt(T, str, 10) catch |err| switch (err) {
        error.Overflow => error.ValueOutOfRange,
        error.InvalidCharacter => error.InvalidNumber,
    };
}

/// Parse an integer with minimum and maximum bounds.
pub fn parseIntBounded(comptime T: type, str: []const u8, min_val: T, max_val: T) ArgsError!T {
    const value = try parseInt(T, str);

    if (value < min_val or value > max_val) {
        return error.ValueOutOfRange;
    }

    return value;
}

/// Parse a float from an argument string.
pub fn parseFloat(comptime T: type, str: []const u8) ArgsError!T {
    if (str.len == 0) return error.EmptyArgument;

    return std.fmt.parseFloat(T, str) catch error.InvalidNumber;
}

/// Parse a boolean from common string representations.
pub fn parseBool(str: []const u8) ArgsError!bool {
    const lower = blk: {
        var buf: [16]u8 = undefined;
        if (str.len > buf.len) return error.InvalidArgument;
        for (str, 0..) |c, i| {
            buf[i] = std.ascii.toLower(c);
        }
        break :blk buf[0..str.len];
    };

    if (std.mem.eql(u8, lower, "true") or
        std.mem.eql(u8, lower, "yes") or
        std.mem.eql(u8, lower, "1") or
        std.mem.eql(u8, lower, "on"))
    {
        return true;
    }

    if (std.mem.eql(u8, lower, "false") or
        std.mem.eql(u8, lower, "no") or
        std.mem.eql(u8, lower, "0") or
        std.mem.eql(u8, lower, "off"))
    {
        return false;
    }

    return error.InvalidArgument;
}

/// Validate that a string is a valid option name (alphanumeric and hyphens).
pub fn isValidOptionName(name: []const u8) bool {
    if (name.len == 0) return false;

    // First character must be alphanumeric
    if (!std.ascii.isAlphanumeric(name[0])) return false;

    for (name) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '-' and c != '_') {
            return false;
        }
    }

    // Cannot end with hyphen
    if (name[name.len - 1] == '-') return false;

    return true;
}

/// Extract value for an option, either attached or from next argument.
pub fn extractValue(
    current: ClassifiedArg,
    next_arg: ?[]const u8,
) ArgsError!struct { value: []const u8, consumed_next: bool } {
    // Value already attached (--opt=val or -oval)
    if (current.value) |v| {
        return .{ .value = v, .consumed_next = false };
    }

    // Need to get from next argument
    if (next_arg) |v| {
        // Don't consume if next looks like an option
        if (v.len > 0 and v[0] == '-') {
            return error.MissingValue;
        }
        return .{ .value = v, .consumed_next = true };
    }

    return error.MissingValue;
}

/// Count the number of positional arguments in an argument list.
pub fn countPositional(args: []const []const u8) usize {
    var count: usize = 0;
    var past_separator = false;

    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--")) {
            past_separator = true;
            continue;
        }

        if (past_separator) {
            count += 1;
        } else {
            const classified = classifyArg(arg);
            if (classified.arg_type == .positional) {
                count += 1;
            }
        }
    }

    return count;
}

/// Check if an argument matches a short or long option name.
pub fn matchesOption(arg: ClassifiedArg, short: ?u8, long: ?[]const u8) bool {
    switch (arg.arg_type) {
        .short_flag, .short_option => {
            if (short) |s| {
                if (arg.name.len == 1 and arg.name[0] == s) return true;
            }
        },
        .long_flag, .long_option => {
            if (long) |l| {
                if (std.mem.eql(u8, arg.name, l)) return true;
            }
        },
        else => {},
    }
    return false;
}

/// Safely join arguments with spaces (for display/logging).
pub fn joinArgs(args: []const []const u8, out: []u8) ArgsError![]const u8 {
    if (args.len == 0) return out[0..0];

    var pos: usize = 0;

    for (args, 0..) |arg, i| {
        if (i > 0) {
            if (pos >= out.len) return error.TooManyArguments;
            out[pos] = ' ';
            pos += 1;
        }

        if (pos + arg.len > out.len) return error.TooManyArguments;
        @memcpy(out[pos..][0..arg.len], arg);
        pos += arg.len;
    }

    return out[0..pos];
}

/// Escape an argument for shell display (add quotes if needed).
pub fn needsQuoting(arg: []const u8) bool {
    if (arg.len == 0) return true;

    for (arg) |c| {
        switch (c) {
            ' ', '\t', '\n', '\r', '"', '\'', '\\', '$', '`', '!', '*', '?', '[', ']', '(', ')', '{', '}', '|', '&', ';', '<', '>' => return true,
            else => {},
        }
    }

    return false;
}

/// Bounded storage for parsed options.
pub fn OptionSet(comptime max_options: usize) type {
    return struct {
        const Self = @This();

        const Entry = struct {
            name: []const u8,
            value: ?[]const u8,
            count: usize,
        };

        entries: [max_options]Entry = undefined,
        len: usize = 0,

        /// Add or update an option.
        pub fn set(self: *Self, name: []const u8, value: ?[]const u8) ArgsError!void {
            // Check if already exists
            for (self.entries[0..self.len]) |*entry| {
                if (std.mem.eql(u8, entry.name, name)) {
                    entry.value = value;
                    entry.count += 1;
                    return;
                }
            }

            // Add new
            if (self.len >= max_options) return error.TooManyArguments;

            self.entries[self.len] = Entry{
                .name = name,
                .value = value,
                .count = 1,
            };
            self.len += 1;
        }

        /// Get an option's value.
        pub fn get(self: *const Self, name: []const u8) ?[]const u8 {
            for (self.entries[0..self.len]) |entry| {
                if (std.mem.eql(u8, entry.name, name)) {
                    return entry.value;
                }
            }
            return null;
        }

        /// Check if an option was set.
        pub fn has(self: *const Self, name: []const u8) bool {
            for (self.entries[0..self.len]) |entry| {
                if (std.mem.eql(u8, entry.name, name)) {
                    return true;
                }
            }
            return false;
        }

        /// Get how many times an option was specified.
        pub fn getCount(self: *const Self, name: []const u8) usize {
            for (self.entries[0..self.len]) |entry| {
                if (std.mem.eql(u8, entry.name, name)) {
                    return entry.count;
                }
            }
            return 0;
        }
    };
}

test "classifyArg - positional" {
    const result = classifyArg("hello");
    try std.testing.expectEqual(ArgType.positional, result.arg_type);
    try std.testing.expectEqualStrings("hello", result.name);
    try std.testing.expect(result.value == null);
}

test "classifyArg - short flag" {
    const result = classifyArg("-v");
    try std.testing.expectEqual(ArgType.short_flag, result.arg_type);
    try std.testing.expectEqualStrings("v", result.name);
}

test "classifyArg - short option with value" {
    const result = classifyArg("-ofile.txt");
    try std.testing.expectEqual(ArgType.short_option, result.arg_type);
    try std.testing.expectEqualStrings("o", result.name);
    try std.testing.expectEqualStrings("file.txt", result.value.?);
}

test "classifyArg - long flag" {
    const result = classifyArg("--verbose");
    try std.testing.expectEqual(ArgType.long_flag, result.arg_type);
    try std.testing.expectEqualStrings("verbose", result.name);
}

test "classifyArg - long option with value" {
    const result = classifyArg("--output=file.txt");
    try std.testing.expectEqual(ArgType.long_option, result.arg_type);
    try std.testing.expectEqualStrings("output", result.name);
    try std.testing.expectEqualStrings("file.txt", result.value.?);
}

test "classifyArg - separator" {
    const result = classifyArg("--");
    try std.testing.expectEqual(ArgType.separator, result.arg_type);
}

test "SafeArgIterator" {
    const args = [_][]const u8{ "-v", "--output=test.txt", "file.txt" };
    var iter = SafeArgIterator(100).init(&args);

    const first = iter.next().?;
    try std.testing.expectEqual(ArgType.short_flag, first.arg_type);

    const second = iter.next().?;
    try std.testing.expectEqual(ArgType.long_option, second.arg_type);
    try std.testing.expectEqualStrings("test.txt", second.value.?);

    const third = iter.next().?;
    try std.testing.expectEqual(ArgType.positional, third.arg_type);

    try std.testing.expect(iter.next() == null);
}

test "parseInt" {
    try std.testing.expectEqual(@as(i32, 42), try parseInt(i32, "42"));
    try std.testing.expectEqual(@as(i32, -123), try parseInt(i32, "-123"));
    try std.testing.expectError(error.InvalidNumber, parseInt(i32, "abc"));
    try std.testing.expectError(error.EmptyArgument, parseInt(i32, ""));
}

test "parseIntBounded" {
    try std.testing.expectEqual(@as(i32, 50), try parseIntBounded(i32, "50", 0, 100));
    try std.testing.expectError(error.ValueOutOfRange, parseIntBounded(i32, "150", 0, 100));
    try std.testing.expectError(error.ValueOutOfRange, parseIntBounded(i32, "-10", 0, 100));
}

test "parseBool" {
    try std.testing.expect(try parseBool("true"));
    try std.testing.expect(try parseBool("YES"));
    try std.testing.expect(try parseBool("1"));
    try std.testing.expect(!try parseBool("false"));
    try std.testing.expect(!try parseBool("NO"));
    try std.testing.expect(!try parseBool("0"));
    try std.testing.expectError(error.InvalidArgument, parseBool("maybe"));
}

test "isValidOptionName" {
    try std.testing.expect(isValidOptionName("verbose"));
    try std.testing.expect(isValidOptionName("dry-run"));
    try std.testing.expect(isValidOptionName("output_file"));
    try std.testing.expect(!isValidOptionName(""));
    try std.testing.expect(!isValidOptionName("-bad"));
    try std.testing.expect(!isValidOptionName("bad-"));
}

test "matchesOption" {
    const short_arg = classifyArg("-v");
    try std.testing.expect(matchesOption(short_arg, 'v', "verbose"));
    try std.testing.expect(!matchesOption(short_arg, 'x', "verbose"));

    const long_arg = classifyArg("--verbose");
    try std.testing.expect(matchesOption(long_arg, 'v', "verbose"));
    try std.testing.expect(!matchesOption(long_arg, 'v', "debug"));
}

test "countPositional" {
    // countPositional counts args that look positional, not option values
    // "-v" is short option, rest are positional: file1.txt, out.txt, file2.txt = 3
    const args1 = [_][]const u8{ "-v", "file1.txt", "--output", "out.txt", "file2.txt" };
    try std.testing.expectEqual(@as(usize, 3), countPositional(&args1));

    const args2 = [_][]const u8{ "--", "-v", "--help" };
    try std.testing.expectEqual(@as(usize, 2), countPositional(&args2));
}

test "needsQuoting" {
    try std.testing.expect(!needsQuoting("simple"));
    try std.testing.expect(needsQuoting("has space"));
    try std.testing.expect(needsQuoting("has\"quote"));
    try std.testing.expect(needsQuoting(""));
    try std.testing.expect(needsQuoting("$variable"));
}

test "OptionSet" {
    var opts = OptionSet(10){};

    try opts.set("verbose", null);
    try opts.set("output", "file.txt");
    try opts.set("verbose", null); // Set again

    try std.testing.expect(opts.has("verbose"));
    try std.testing.expect(opts.has("output"));
    try std.testing.expect(!opts.has("debug"));

    try std.testing.expectEqualStrings("file.txt", opts.get("output").?);
    try std.testing.expectEqual(@as(usize, 2), opts.getCount("verbose"));
}

test "joinArgs" {
    const args = [_][]const u8{ "cmd", "-v", "file.txt" };
    var buf: [100]u8 = undefined;
    const result = try joinArgs(&args, &buf);
    try std.testing.expectEqualStrings("cmd -v file.txt", result);
}
