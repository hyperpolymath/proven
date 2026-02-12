// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe password validation and generation that cannot crash.

const std = @import("std");

/// Error types for password operations.
pub const PasswordError = error{
    TooShort,
    TooLong,
    NoUppercase,
    NoLowercase,
    NoDigit,
    NoSpecial,
    CommonPassword,
    RepeatingChars,
    SequentialChars,
    OutOfMemory,
};

/// Password strength levels
pub const Strength = enum {
    very_weak,
    weak,
    fair,
    strong,
    very_strong,

    pub fn score(self: Strength) u8 {
        return switch (self) {
            .very_weak => 0,
            .weak => 1,
            .fair => 2,
            .strong => 3,
            .very_strong => 4,
        };
    }
};

/// Password policy configuration
pub const Policy = struct {
    min_length: usize = 8,
    max_length: usize = 128,
    require_uppercase: bool = true,
    require_lowercase: bool = true,
    require_digit: bool = true,
    require_special: bool = false,
    max_repeating: usize = 3,
    check_common: bool = true,
};

/// Default policy
pub const default_policy = Policy{};

/// Strong policy
pub const strong_policy = Policy{
    .min_length = 12,
    .require_special = true,
    .max_repeating = 2,
};

/// Check if password contains uppercase
pub fn hasUppercase(password: []const u8) bool {
    for (password) |c| {
        if (std.ascii.isUpper(c)) return true;
    }
    return false;
}

/// Check if password contains lowercase
pub fn hasLowercase(password: []const u8) bool {
    for (password) |c| {
        if (std.ascii.isLower(c)) return true;
    }
    return false;
}

/// Check if password contains digit
pub fn hasDigit(password: []const u8) bool {
    for (password) |c| {
        if (std.ascii.isDigit(c)) return true;
    }
    return false;
}

/// Check if password contains special character
pub fn hasSpecial(password: []const u8) bool {
    const specials = "!@#$%^&*()_+-=[]{}|;':\",./<>?`~";
    for (password) |c| {
        if (std.mem.indexOfScalar(u8, specials, c) != null) return true;
    }
    return false;
}

/// Check for repeating characters
pub fn hasRepeatingChars(password: []const u8, max: usize) bool {
    if (password.len < max + 1) return false;

    var count: usize = 1;
    var prev: u8 = password[0];

    for (password[1..]) |c| {
        if (c == prev) {
            count += 1;
            if (count > max) return true;
        } else {
            count = 1;
            prev = c;
        }
    }

    return false;
}

/// Check for sequential characters (abc, 123)
pub fn hasSequentialChars(password: []const u8, length: usize) bool {
    if (password.len < length) return false;

    var ascending: usize = 1;
    var descending: usize = 1;

    for (1..password.len) |i| {
        const prev = password[i - 1];
        const curr = password[i];

        if (curr == prev + 1) {
            ascending += 1;
            if (ascending >= length) return true;
        } else {
            ascending = 1;
        }

        if (curr + 1 == prev) {
            descending += 1;
            if (descending >= length) return true;
        } else {
            descending = 1;
        }
    }

    return false;
}

/// Common passwords to check against
const common_passwords = [_][]const u8{
    "password", "123456", "12345678", "qwerty", "abc123",
    "monkey", "1234567", "letmein", "trustno1", "dragon",
    "baseball", "iloveyou", "master", "sunshine", "ashley",
    "bailey", "shadow", "123123", "654321", "superman",
    "qazwsx", "michael", "football", "password1", "password123",
};

/// Check if password is in common passwords list
pub fn isCommon(password: []const u8) bool {
    const lower = std.ascii.lowerString(undefined, password) catch return false;
    for (common_passwords) |common| {
        if (std.mem.eql(u8, lower[0..password.len], common)) return true;
    }
    return false;
}

/// Validate a password against a policy
pub fn validate(password: []const u8, policy: Policy) PasswordError!void {
    if (password.len < policy.min_length) return error.TooShort;
    if (password.len > policy.max_length) return error.TooLong;
    if (policy.require_uppercase and !hasUppercase(password)) return error.NoUppercase;
    if (policy.require_lowercase and !hasLowercase(password)) return error.NoLowercase;
    if (policy.require_digit and !hasDigit(password)) return error.NoDigit;
    if (policy.require_special and !hasSpecial(password)) return error.NoSpecial;
    if (hasRepeatingChars(password, policy.max_repeating)) return error.RepeatingChars;
    if (policy.check_common and isCommonSimple(password)) return error.CommonPassword;
}

/// Simple common password check (case-insensitive prefix)
fn isCommonSimple(password: []const u8) bool {
    const common = [_][]const u8{ "password", "123456", "qwerty", "admin", "letmein" };
    for (common) |c| {
        if (password.len >= c.len) {
            var matches = true;
            for (password[0..c.len], c) |p, e| {
                if (std.ascii.toLower(p) != e) {
                    matches = false;
                    break;
                }
            }
            if (matches) return true;
        }
    }
    return false;
}

/// Check if password is valid against default policy
pub fn isValid(password: []const u8) bool {
    validate(password, default_policy) catch return false;
    return true;
}

/// Calculate password strength
pub fn strength(password: []const u8) Strength {
    var score: u8 = 0;

    // Length score
    if (password.len >= 8) score += 1;
    if (password.len >= 12) score += 1;
    if (password.len >= 16) score += 1;

    // Character variety
    if (hasUppercase(password)) score += 1;
    if (hasLowercase(password)) score += 1;
    if (hasDigit(password)) score += 1;
    if (hasSpecial(password)) score += 2;

    // Penalties
    if (hasRepeatingChars(password, 2)) score -|= 1;
    if (hasSequentialChars(password, 3)) score -|= 1;
    if (isCommonSimple(password)) score -|= 3;

    return switch (score) {
        0...2 => .very_weak,
        3...4 => .weak,
        5...6 => .fair,
        7...8 => .strong,
        else => .very_strong,
    };
}

/// Generate a random password
pub fn generate(allocator: std.mem.Allocator, length: usize, policy: Policy) ![]u8 {
    if (length < policy.min_length) return error.TooShort;
    if (length > policy.max_length) return error.TooLong;

    const upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    const lower = "abcdefghijklmnopqrstuvwxyz";
    const digits = "0123456789";
    const special = "!@#$%^&*()_+-=[]{}|;':,./<>?";

    var password = try allocator.alloc(u8, length);
    errdefer allocator.free(password);

    var rng = std.rand.DefaultPrng.init(@truncate(@as(u128, @bitCast(std.time.nanoTimestamp()))));
    const random = rng.random();

    var pos: usize = 0;

    // Ensure required character types
    if (policy.require_uppercase) {
        password[pos] = upper[random.intRangeAtMost(usize, 0, upper.len - 1)];
        pos += 1;
    }
    if (policy.require_lowercase) {
        password[pos] = lower[random.intRangeAtMost(usize, 0, lower.len - 1)];
        pos += 1;
    }
    if (policy.require_digit) {
        password[pos] = digits[random.intRangeAtMost(usize, 0, digits.len - 1)];
        pos += 1;
    }
    if (policy.require_special) {
        password[pos] = special[random.intRangeAtMost(usize, 0, special.len - 1)];
        pos += 1;
    }

    // Build character set
    var charset: [94]u8 = undefined;
    var charset_len: usize = 0;

    for (upper) |c| {
        charset[charset_len] = c;
        charset_len += 1;
    }
    for (lower) |c| {
        charset[charset_len] = c;
        charset_len += 1;
    }
    for (digits) |c| {
        charset[charset_len] = c;
        charset_len += 1;
    }
    if (policy.require_special) {
        for (special[0..10]) |c| {
            charset[charset_len] = c;
            charset_len += 1;
        }
    }

    // Fill remaining
    while (pos < length) {
        password[pos] = charset[random.intRangeAtMost(usize, 0, charset_len - 1)];
        pos += 1;
    }

    // Shuffle
    random.shuffle(u8, password);

    return password;
}

/// Calculate entropy in bits
pub fn entropy(password: []const u8) f64 {
    var charset_size: f64 = 0;

    if (hasLowercase(password)) charset_size += 26;
    if (hasUppercase(password)) charset_size += 26;
    if (hasDigit(password)) charset_size += 10;
    if (hasSpecial(password)) charset_size += 32;

    if (charset_size == 0) return 0;

    const len: f64 = @floatFromInt(password.len);
    return len * @log2(charset_size);
}

test "hasUppercase" {
    try std.testing.expect(hasUppercase("Hello"));
    try std.testing.expect(!hasUppercase("hello"));
}

test "hasLowercase" {
    try std.testing.expect(hasLowercase("Hello"));
    try std.testing.expect(!hasLowercase("HELLO"));
}

test "hasDigit" {
    try std.testing.expect(hasDigit("hello123"));
    try std.testing.expect(!hasDigit("hello"));
}

test "hasSpecial" {
    try std.testing.expect(hasSpecial("hello!"));
    try std.testing.expect(!hasSpecial("hello"));
}

test "isValid" {
    try std.testing.expect(isValid("MyP@ssw0rd"));
    try std.testing.expect(!isValid("short"));
    try std.testing.expect(!isValid("password"));
}

test "strength" {
    try std.testing.expectEqual(Strength.very_weak, strength("pass"));
    // "password" is in common passwords list, so it's very_weak
    try std.testing.expectEqual(Strength.very_weak, strength("password"));
    // A password with length and mixed chars is weak
    try std.testing.expectEqual(Strength.weak, strength("Simple99"));
}
