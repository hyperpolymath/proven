// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafePassword - FFI bindings to libproven password operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Password strength levels.
pub const PasswordStrength = enum(c_int) {
    very_weak = c.PROVEN_PASSWORD_VERY_WEAK,
    weak = c.PROVEN_PASSWORD_WEAK,
    fair = c.PROVEN_PASSWORD_FAIR,
    strong = c.PROVEN_PASSWORD_STRONG,
    very_strong = c.PROVEN_PASSWORD_VERY_STRONG,
};

/// Password validation result.
pub const PasswordResult = struct {
    strength: PasswordStrength,
    has_lowercase: bool,
    has_uppercase: bool,
    has_digit: bool,
    has_special: bool,
    length: usize,
};

/// Validate password strength via libproven.
pub fn validate(password: []const u8) PasswordResult {
    const result = c.proven_password_validate(password.ptr, password.len);
    return PasswordResult{
        .strength = @enumFromInt(result.strength),
        .has_lowercase = result.has_lowercase,
        .has_uppercase = result.has_uppercase,
        .has_digit = result.has_digit,
        .has_special = result.has_special,
        .length = result.length,
    };
}

/// Check if password is in common passwords list via libproven.
pub fn isCommon(password: []const u8) bool {
    return c.proven_password_is_common(password.ptr, password.len);
}

test "validate" {
    const result = validate("MyP@ssw0rd!");
    try std.testing.expect(result.has_uppercase);
    try std.testing.expect(result.has_lowercase);
    try std.testing.expect(result.has_digit);
    try std.testing.expect(result.has_special);
}

test "isCommon" {
    try std.testing.expect(isCommon("password"));
    try std.testing.expect(!isCommon("xK9$mZ2!vQ7@nW4#"));
}
