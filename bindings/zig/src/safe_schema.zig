// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe schema validation (JSON Schema-like) that cannot crash.
//!
//! Provides compile-time and runtime schema validation for structured data.
//! Designed to safely validate data without memory allocation failures or
//! undefined behavior.

const std = @import("std");

/// Error types for schema validation operations.
pub const SchemaError = error{
    TypeMismatch,
    RequiredFieldMissing,
    ValueOutOfRange,
    PatternMismatch,
    ArrayTooLong,
    ArrayTooShort,
    StringTooLong,
    StringTooShort,
    InvalidFormat,
    MaxPropertiesExceeded,
    MinPropertiesNotMet,
    UniqueViolation,
    EnumValueInvalid,
    SchemaInvalid,
};

/// Supported value types for schema validation.
pub const ValueType = enum {
    null,
    boolean,
    integer,
    number,
    string,
    array,
    object,
};

/// String format constraints.
pub const StringFormat = enum {
    none,
    email,
    uri,
    date,
    datetime,
    uuid,
    ipv4,
    ipv6,
    hostname,
};

/// Schema constraint definition for a single field.
pub const FieldSchema = struct {
    value_type: ValueType,
    required: bool = false,
    min_length: ?usize = null,
    max_length: ?usize = null,
    min_value: ?i64 = null,
    max_value: ?i64 = null,
    min_items: ?usize = null,
    max_items: ?usize = null,
    format: StringFormat = .none,
    pattern: ?[]const u8 = null,
    enum_values: ?[]const []const u8 = null,

    /// Validate a string value against this schema.
    pub fn validateString(self: FieldSchema, value: []const u8) SchemaError!void {
        if (self.value_type != .string) return error.TypeMismatch;

        if (self.min_length) |min| {
            if (value.len < min) return error.StringTooShort;
        }
        if (self.max_length) |max| {
            if (value.len > max) return error.StringTooLong;
        }

        if (self.format != .none) {
            try validateFormat(value, self.format);
        }

        if (self.enum_values) |enums| {
            var found = false;
            for (enums) |e| {
                if (std.mem.eql(u8, value, e)) {
                    found = true;
                    break;
                }
            }
            if (!found) return error.EnumValueInvalid;
        }
    }

    /// Validate an integer value against this schema.
    pub fn validateInteger(self: FieldSchema, value: i64) SchemaError!void {
        if (self.value_type != .integer and self.value_type != .number) {
            return error.TypeMismatch;
        }

        if (self.min_value) |min| {
            if (value < min) return error.ValueOutOfRange;
        }
        if (self.max_value) |max| {
            if (value > max) return error.ValueOutOfRange;
        }
    }

    /// Validate array length against this schema.
    pub fn validateArrayLength(self: FieldSchema, length: usize) SchemaError!void {
        if (self.value_type != .array) return error.TypeMismatch;

        if (self.min_items) |min| {
            if (length < min) return error.ArrayTooShort;
        }
        if (self.max_items) |max| {
            if (length > max) return error.ArrayTooLong;
        }
    }
};

/// Validate a string against a format constraint.
pub fn validateFormat(value: []const u8, format: StringFormat) SchemaError!void {
    switch (format) {
        .none => {},
        .email => {
            if (!isValidEmail(value)) return error.InvalidFormat;
        },
        .uri => {
            if (!isValidUri(value)) return error.InvalidFormat;
        },
        .date => {
            if (!isValidDate(value)) return error.InvalidFormat;
        },
        .datetime => {
            if (!isValidDatetime(value)) return error.InvalidFormat;
        },
        .uuid => {
            if (!isValidUuid(value)) return error.InvalidFormat;
        },
        .ipv4 => {
            if (!isValidIpv4(value)) return error.InvalidFormat;
        },
        .ipv6 => {
            if (!isValidIpv6(value)) return error.InvalidFormat;
        },
        .hostname => {
            if (!isValidHostname(value)) return error.InvalidFormat;
        },
    }
}

/// Object schema with bounded field count.
pub fn ObjectSchema(comptime max_fields: usize) type {
    return struct {
        const Self = @This();

        fields: [max_fields]?FieldEntry = [_]?FieldEntry{null} ** max_fields,
        field_count: usize = 0,
        min_properties: usize = 0,
        max_properties: usize = max_fields,
        additional_properties_allowed: bool = true,

        const FieldEntry = struct {
            name: []const u8,
            schema: FieldSchema,
        };

        pub fn init() Self {
            return .{};
        }

        /// Add a field schema definition.
        pub fn addField(self: *Self, name: []const u8, schema: FieldSchema) SchemaError!void {
            if (self.field_count >= max_fields) return error.SchemaInvalid;
            self.fields[self.field_count] = .{ .name = name, .schema = schema };
            self.field_count += 1;
        }

        /// Set minimum required properties.
        pub fn setMinProperties(self: *Self, min: usize) void {
            self.min_properties = min;
        }

        /// Set maximum allowed properties.
        pub fn setMaxProperties(self: *Self, max: usize) void {
            self.max_properties = @min(max, max_fields);
        }

        /// Disallow additional properties not in schema.
        pub fn disallowAdditionalProperties(self: *Self) void {
            self.additional_properties_allowed = false;
        }

        /// Get the schema for a named field.
        pub fn getFieldSchema(self: *const Self, name: []const u8) ?FieldSchema {
            for (self.fields[0..self.field_count]) |entry_opt| {
                if (entry_opt) |entry| {
                    if (std.mem.eql(u8, entry.name, name)) {
                        return entry.schema;
                    }
                }
            }
            return null;
        }

        /// Check if a field is defined in the schema.
        pub fn hasField(self: *const Self, name: []const u8) bool {
            return self.getFieldSchema(name) != null;
        }

        /// Get all required field names.
        pub fn getRequiredFields(self: *const Self) [max_fields]?[]const u8 {
            var result: [max_fields]?[]const u8 = [_]?[]const u8{null} ** max_fields;
            var idx: usize = 0;
            for (self.fields[0..self.field_count]) |entry_opt| {
                if (entry_opt) |entry| {
                    if (entry.schema.required) {
                        result[idx] = entry.name;
                        idx += 1;
                    }
                }
            }
            return result;
        }

        /// Validate property count constraints.
        pub fn validatePropertyCount(self: *const Self, count: usize) SchemaError!void {
            if (count < self.min_properties) return error.MinPropertiesNotMet;
            if (count > self.max_properties) return error.MaxPropertiesExceeded;
        }

        /// Check if a field name is allowed (for strict mode).
        pub fn isFieldAllowed(self: *const Self, name: []const u8) bool {
            if (self.additional_properties_allowed) return true;
            return self.hasField(name);
        }
    };
}

/// Validation result with details.
pub const ValidationResult = struct {
    valid: bool,
    error_path: ?[]const u8 = null,
    error_message: ?[]const u8 = null,

    pub fn ok() ValidationResult {
        return .{ .valid = true };
    }

    pub fn fail(path: ?[]const u8, message: ?[]const u8) ValidationResult {
        return .{
            .valid = false,
            .error_path = path,
            .error_message = message,
        };
    }
};

/// Builder pattern for creating schemas.
pub fn SchemaBuilder(comptime max_fields: usize) type {
    return struct {
        const Self = @This();

        schema: ObjectSchema(max_fields),

        pub fn init() Self {
            return .{ .schema = ObjectSchema(max_fields).init() };
        }

        pub fn requiredString(self: *Self, name: []const u8) *Self {
            self.schema.addField(name, .{
                .value_type = .string,
                .required = true,
            }) catch {};
            return self;
        }

        pub fn optionalString(self: *Self, name: []const u8) *Self {
            self.schema.addField(name, .{
                .value_type = .string,
                .required = false,
            }) catch {};
            return self;
        }

        pub fn requiredInteger(self: *Self, name: []const u8) *Self {
            self.schema.addField(name, .{
                .value_type = .integer,
                .required = true,
            }) catch {};
            return self;
        }

        pub fn optionalInteger(self: *Self, name: []const u8) *Self {
            self.schema.addField(name, .{
                .value_type = .integer,
                .required = false,
            }) catch {};
            return self;
        }

        pub fn requiredBoolean(self: *Self, name: []const u8) *Self {
            self.schema.addField(name, .{
                .value_type = .boolean,
                .required = true,
            }) catch {};
            return self;
        }

        pub fn boundedString(self: *Self, name: []const u8, min_len: usize, max_len: usize, required: bool) *Self {
            self.schema.addField(name, .{
                .value_type = .string,
                .required = required,
                .min_length = min_len,
                .max_length = max_len,
            }) catch {};
            return self;
        }

        pub fn boundedInteger(self: *Self, name: []const u8, min_val: i64, max_val: i64, required: bool) *Self {
            self.schema.addField(name, .{
                .value_type = .integer,
                .required = required,
                .min_value = min_val,
                .max_value = max_val,
            }) catch {};
            return self;
        }

        pub fn emailField(self: *Self, name: []const u8, required: bool) *Self {
            self.schema.addField(name, .{
                .value_type = .string,
                .required = required,
                .format = .email,
            }) catch {};
            return self;
        }

        pub fn uuidField(self: *Self, name: []const u8, required: bool) *Self {
            self.schema.addField(name, .{
                .value_type = .string,
                .required = required,
                .format = .uuid,
            }) catch {};
            return self;
        }

        pub fn enumField(self: *Self, name: []const u8, values: []const []const u8, required: bool) *Self {
            self.schema.addField(name, .{
                .value_type = .string,
                .required = required,
                .enum_values = values,
            }) catch {};
            return self;
        }

        pub fn strict(self: *Self) *Self {
            self.schema.disallowAdditionalProperties();
            return self;
        }

        pub fn build(self: *Self) ObjectSchema(max_fields) {
            return self.schema;
        }
    };
}

// Format validation helper functions

fn isValidEmail(value: []const u8) bool {
    if (value.len == 0 or value.len > 254) return false;
    const at_pos = std.mem.indexOf(u8, value, "@") orelse return false;
    if (at_pos == 0 or at_pos == value.len - 1) return false;
    const local = value[0..at_pos];
    const domain = value[at_pos + 1 ..];
    if (local.len > 64 or domain.len == 0) return false;
    if (std.mem.indexOf(u8, domain, ".") == null) return false;
    return true;
}

fn isValidUri(value: []const u8) bool {
    if (value.len == 0) return false;
    // Check for scheme (e.g., http://, https://, ftp://)
    if (std.mem.indexOf(u8, value, "://")) |pos| {
        if (pos == 0) return false;
        const scheme = value[0..pos];
        for (scheme) |c| {
            if (!std.ascii.isAlphanumeric(c) and c != '+' and c != '-' and c != '.') {
                return false;
            }
        }
        return true;
    }
    return false;
}

fn isValidDate(value: []const u8) bool {
    // YYYY-MM-DD format
    if (value.len != 10) return false;
    if (value[4] != '-' or value[7] != '-') return false;
    _ = std.fmt.parseInt(u16, value[0..4], 10) catch return false;
    const month = std.fmt.parseInt(u8, value[5..7], 10) catch return false;
    const day = std.fmt.parseInt(u8, value[8..10], 10) catch return false;
    if (month < 1 or month > 12) return false;
    if (day < 1 or day > 31) return false;
    return true;
}

fn isValidDatetime(value: []const u8) bool {
    // ISO 8601: YYYY-MM-DDTHH:MM:SS or YYYY-MM-DDTHH:MM:SSZ
    if (value.len < 19) return false;
    if (!isValidDate(value[0..10])) return false;
    if (value[10] != 'T' and value[10] != ' ') return false;
    if (value[13] != ':' or value[16] != ':') return false;
    const hour = std.fmt.parseInt(u8, value[11..13], 10) catch return false;
    const minute = std.fmt.parseInt(u8, value[14..16], 10) catch return false;
    const second = std.fmt.parseInt(u8, value[17..19], 10) catch return false;
    if (hour > 23 or minute > 59 or second > 59) return false;
    return true;
}

fn isValidUuid(value: []const u8) bool {
    // UUID format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx (36 chars)
    if (value.len != 36) return false;
    if (value[8] != '-' or value[13] != '-' or value[18] != '-' or value[23] != '-') return false;
    for (value, 0..) |c, i| {
        if (i == 8 or i == 13 or i == 18 or i == 23) continue;
        if (!std.ascii.isHex(c)) return false;
    }
    return true;
}

fn isValidIpv4(value: []const u8) bool {
    var parts: usize = 0;
    var current: u16 = 0;
    var digits: usize = 0;

    for (value) |c| {
        if (c == '.') {
            if (digits == 0 or current > 255) return false;
            parts += 1;
            current = 0;
            digits = 0;
        } else if (c >= '0' and c <= '9') {
            current = current * 10 + (c - '0');
            digits += 1;
            if (digits > 3) return false;
        } else {
            return false;
        }
    }

    if (digits == 0 or current > 255) return false;
    return parts == 3;
}

fn isValidIpv6(value: []const u8) bool {
    if (value.len == 0 or value.len > 45) return false;
    var groups: usize = 0;
    var has_double_colon = false;
    var consecutive_colons: usize = 0;
    var hex_digits: usize = 0;

    for (value, 0..) |c, i| {
        if (c == ':') {
            consecutive_colons += 1;
            if (consecutive_colons == 2) {
                if (has_double_colon) return false; // Only one :: allowed
                has_double_colon = true;
            } else if (consecutive_colons > 2) {
                return false;
            }
            if (hex_digits > 0) groups += 1;
            hex_digits = 0;
        } else if (std.ascii.isHex(c)) {
            consecutive_colons = 0;
            hex_digits += 1;
            if (hex_digits > 4) return false;
        } else {
            return false;
        }
        _ = i;
    }

    if (hex_digits > 0) groups += 1;
    if (has_double_colon) return groups <= 8;
    return groups == 8;
}

fn isValidHostname(value: []const u8) bool {
    if (value.len == 0 or value.len > 253) return false;
    var label_len: usize = 0;
    for (value) |c| {
        if (c == '.') {
            if (label_len == 0 or label_len > 63) return false;
            label_len = 0;
        } else if (std.ascii.isAlphanumeric(c) or c == '-') {
            label_len += 1;
            if (label_len > 63) return false;
        } else {
            return false;
        }
    }
    return label_len > 0 and label_len <= 63;
}

test "FieldSchema string validation" {
    const schema = FieldSchema{
        .value_type = .string,
        .min_length = 2,
        .max_length = 10,
    };

    try schema.validateString("hello");
    try std.testing.expectError(error.StringTooShort, schema.validateString("x"));
    try std.testing.expectError(error.StringTooLong, schema.validateString("this is too long"));
}

test "FieldSchema integer validation" {
    const schema = FieldSchema{
        .value_type = .integer,
        .min_value = 0,
        .max_value = 100,
    };

    try schema.validateInteger(50);
    try std.testing.expectError(error.ValueOutOfRange, schema.validateInteger(-1));
    try std.testing.expectError(error.ValueOutOfRange, schema.validateInteger(101));
}

test "ObjectSchema" {
    var schema = ObjectSchema(10).init();
    try schema.addField("name", .{ .value_type = .string, .required = true, .max_length = 50 });
    try schema.addField("age", .{ .value_type = .integer, .min_value = 0, .max_value = 150 });

    try std.testing.expect(schema.hasField("name"));
    try std.testing.expect(schema.hasField("age"));
    try std.testing.expect(!schema.hasField("unknown"));

    const name_schema = schema.getFieldSchema("name");
    try std.testing.expect(name_schema != null);
    try std.testing.expect(name_schema.?.required);
}

test "SchemaBuilder" {
    var builder = SchemaBuilder(10).init();
    const schema = builder
        .requiredString("name")
        .boundedInteger("age", 0, 150, true)
        .emailField("email", false)
        .strict()
        .build();

    try std.testing.expect(schema.hasField("name"));
    try std.testing.expect(schema.hasField("age"));
    try std.testing.expect(schema.hasField("email"));
    try std.testing.expect(!schema.additional_properties_allowed);
}

test "format validation" {
    try validateFormat("test@example.com", .email);
    try std.testing.expectError(error.InvalidFormat, validateFormat("invalid", .email));

    try validateFormat("https://example.com", .uri);
    try validateFormat("2024-01-15", .date);
    try validateFormat("2024-01-15T10:30:00", .datetime);
    try validateFormat("550e8400-e29b-41d4-a716-446655440000", .uuid);
    try validateFormat("192.168.1.1", .ipv4);
    try validateFormat("example.com", .hostname);
}

test "enum validation" {
    const values = [_][]const u8{ "red", "green", "blue" };
    const schema = FieldSchema{
        .value_type = .string,
        .enum_values = &values,
    };

    try schema.validateString("red");
    try schema.validateString("green");
    try std.testing.expectError(error.EnumValueInvalid, schema.validateString("yellow"));
}
