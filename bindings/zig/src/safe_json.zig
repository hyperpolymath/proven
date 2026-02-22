// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeJSON - FFI bindings to libproven JSON validation.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// JSON value types (matching ProvenJsonType enum in proven.h).
pub const JsonType = enum(i32) {
    null_type = c.PROVEN_JSON_NULL,
    bool_type = c.PROVEN_JSON_BOOL,
    number = c.PROVEN_JSON_NUMBER,
    string = c.PROVEN_JSON_STRING,
    array = c.PROVEN_JSON_ARRAY,
    object = c.PROVEN_JSON_OBJECT,
    invalid = c.PROVEN_JSON_INVALID,
};

/// Check if string is valid JSON via libproven.
pub fn isValid(input: []const u8) bool {
    const result = c.proven_json_is_valid(input.ptr, input.len);
    return result.status == c.PROVEN_OK and result.value;
}

/// Get JSON value type at root level via libproven.
pub fn getType(input: []const u8) JsonType {
    const result = c.proven_json_get_type(input.ptr, input.len);
    return @enumFromInt(result);
}

test "isValid" {
    try std.testing.expect(isValid("{}"));
    try std.testing.expect(isValid("{\"foo\": 42}"));
    try std.testing.expect(isValid("[1, 2, 3]"));
    try std.testing.expect(!isValid("{invalid}"));
}

test "getType" {
    try std.testing.expectEqual(JsonType.object, getType("{}"));
    try std.testing.expectEqual(JsonType.array, getType("[1,2]"));
    try std.testing.expectEqual(JsonType.string, getType("\"hello\""));
    try std.testing.expectEqual(JsonType.number, getType("42"));
    try std.testing.expectEqual(JsonType.invalid, getType("not json"));
}
