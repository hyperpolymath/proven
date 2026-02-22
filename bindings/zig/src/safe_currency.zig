// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeCurrency - FFI bindings to libproven currency operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for currency operations.
pub const CurrencyError = error{
    ParseFailure,
    FormatError,
    ProvenError,
};

/// Parsed currency value.
pub const Currency = struct {
    amount_minor: i64,
    currency_code: [3]u8,
    decimal_places: u8,
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

/// Parse currency amount (e.g., "USD 123.45" or "123.45 EUR") via libproven.
pub fn parse(input: []const u8) CurrencyError!Currency {
    const result = c.proven_currency_parse(input.ptr, input.len);
    if (result.status != c.PROVEN_OK) return error.ParseFailure;
    return Currency{
        .amount_minor = result.amount_minor,
        .currency_code = result.currency_code,
        .decimal_places = result.decimal_places,
    };
}

/// Format currency amount via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn format(amount_minor: i64, code: [3]u8, decimal_places: u8) CurrencyError!ProvenString {
    var c_code: [3]u8 = code;
    const result = c.proven_currency_format(amount_minor, &c_code, decimal_places);
    if (result.status != c.PROVEN_OK) return error.FormatError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

test "parse" {
    const cur = try parse("USD 123.45");
    try std.testing.expectEqualStrings("USD", &cur.currency_code);
}
