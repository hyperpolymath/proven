// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafePhone - FFI bindings to libproven phone number operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for phone operations.
pub const PhoneError = error{
    ParseFailure,
    FormatError,
    InvalidPhone,
};

/// Parsed phone number.
pub const PhoneNumber = struct {
    country_code: u16,
    national_number: u64,
    is_valid: bool,
};

/// Managed string from libproven.
pub const ProvenString = struct {
    ptr: [*]u8,
    len: usize,

    pub fn slice(self: ProvenString) []const u8 {
        return self.ptr[0..self.len];
    }

    pub fn deinit(self: ProvenString) void {
        c.proven_free_string(self.ptr);
    }
};

/// Parse phone number to E.164 format via libproven.
pub fn parse(input: []const u8) PhoneError!PhoneNumber {
    const result = c.proven_phone_parse(input.ptr, input.len);
    if (result.status != c.PROVEN_OK) return error.ParseFailure;
    return PhoneNumber{
        .country_code = result.country_code,
        .national_number = result.national_number,
        .is_valid = result.is_valid,
    };
}

/// Format phone number as E.164 string via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn formatE164(country_code: u16, national_number: u64) PhoneError!ProvenString {
    const result = c.proven_phone_format_e164(country_code, national_number);
    if (result.status != c.PROVEN_OK) return error.FormatError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

test "parse" {
    const phone = try parse("+14155551234");
    try std.testing.expectEqual(@as(u16, 1), phone.country_code);
}
