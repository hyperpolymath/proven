// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeEmail - FFI bindings to libproven email validation.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Validate email address (RFC 5321 simplified) via libproven.
pub fn isValid(email: []const u8) bool {
    const result = c.proven_email_is_valid(email.ptr, email.len);
    return result.status == c.PROVEN_OK and result.value;
}

test "isValid" {
    try std.testing.expect(isValid("user@example.com"));
    try std.testing.expect(!isValid("not-an-email"));
    try std.testing.expect(!isValid("@invalid.com"));
}
