// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe phone number validation and formatting operations.
//! Follows ITU-T E.164 recommendation.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Phone number types
pub const PhoneNumberType = enum {
    mobile,
    fixed_line,
    toll_free,
    premium_rate,
    voip,
    unknown,
};

/// Common country calling codes
pub const CountryCode = enum(u16) {
    us_ca = 1, // USA, Canada
    ru = 7, // Russia
    eg = 20, // Egypt
    za = 27, // South Africa
    gr = 30, // Greece
    nl = 31, // Netherlands
    be = 32, // Belgium
    fr = 33, // France
    es = 34, // Spain
    it = 39, // Italy
    ch = 41, // Switzerland
    at = 43, // Austria
    uk = 44, // UK
    dk = 45, // Denmark
    se = 46, // Sweden
    no = 47, // Norway
    pl = 48, // Poland
    de = 49, // Germany
    mx = 52, // Mexico
    br = 55, // Brazil
    au = 61, // Australia
    id = 62, // Indonesia
    ph = 63, // Philippines
    nz = 64, // New Zealand
    sg = 65, // Singapore
    th = 66, // Thailand
    jp = 81, // Japan
    kr = 82, // South Korea
    vn = 84, // Vietnam
    cn = 86, // China
    tr = 90, // Turkey
    in_ = 91, // India (in is reserved keyword)
    pk = 92, // Pakistan
    unknown = 0,

    /// Get country code value
    pub fn value(self: CountryCode) u16 {
        return @intFromEnum(self);
    }
};

/// Validated phone number
pub const PhoneNumber = struct {
    country_code: CountryCode,
    national_number: [15]u8,
    national_len: u8,

    /// Format in E.164 format (+CCNNNN...)
    pub fn formatE164(self: PhoneNumber, buf: []u8) ![]const u8 {
        const cc_val = self.country_code.value();
        const national = self.national_number[0..self.national_len];

        return std.fmt.bufPrint(buf, "+{d}{s}", .{ cc_val, national });
    }

    /// Format with spaces
    pub fn formatInternational(self: PhoneNumber, buf: []u8) ![]const u8 {
        const cc_val = self.country_code.value();
        const national = self.national_number[0..self.national_len];

        if (self.national_len <= 4) {
            return std.fmt.bufPrint(buf, "+{d} {s}", .{ cc_val, national });
        } else if (self.national_len <= 7) {
            return std.fmt.bufPrint(buf, "+{d} {s} {s}", .{ cc_val, national[0..3], national[3..] });
        } else {
            return std.fmt.bufPrint(buf, "+{d} {s} {s} {s}", .{
                cc_val,
                national[0..3],
                national[3..6],
                national[6..],
            });
        }
    }

    /// Get total digits count
    pub fn digitCount(self: PhoneNumber) usize {
        const cc_digits = if (self.country_code.value() >= 100) @as(usize, 3) else if (self.country_code.value() >= 10) @as(usize, 2) else @as(usize, 1);
        return cc_digits + self.national_len;
    }
};

/// Phone error types
pub const PhoneError = error{
    EmptyInput,
    TooShort,
    TooLong,
    InvalidCharacter,
    InvalidCountryCode,
    InvalidNationalNumber,
    OutOfMemory,
};

/// Parse phone number from string
pub fn parse(input: []const u8) PhoneError!PhoneNumber {
    if (input.len == 0) return error.EmptyInput;

    // Extract digits only
    var digits: [20]u8 = undefined;
    var digit_count: usize = 0;
    var start: usize = 0;

    // Skip leading +
    if (input[0] == '+') start = 1;

    for (input[start..]) |c| {
        if (c >= '0' and c <= '9') {
            if (digit_count >= 20) return error.TooLong;
            digits[digit_count] = c;
            digit_count += 1;
        } else if (c != ' ' and c != '-' and c != '(' and c != ')') {
            return error.InvalidCharacter;
        }
    }

    if (digit_count < 7) return error.TooShort;
    if (digit_count > 15) return error.TooLong;

    // Try to parse country code (1-3 digits)
    const cc = parseCountryCode(digits[0..digit_count]) orelse return error.InvalidCountryCode;
    const cc_len = countryCodeLen(cc.value());
    const national_start = cc_len;

    if (digit_count - national_start < 4) return error.InvalidNationalNumber;

    var result: PhoneNumber = .{
        .country_code = cc,
        .national_number = undefined,
        .national_len = @intCast(digit_count - national_start),
    };

    @memcpy(result.national_number[0..result.national_len], digits[national_start..digit_count]);

    return result;
}

/// Check if string is valid phone number
pub fn isValid(input: []const u8) bool {
    return if (parse(input)) |_| true else |_| false;
}

/// Get the length of a country code
fn countryCodeLen(cc: u16) usize {
    if (cc >= 100) return 3;
    if (cc >= 10) return 2;
    return 1;
}

/// Parse country code from digits
fn parseCountryCode(digits: []const u8) ?CountryCode {
    // Try 3-digit codes first, then 2, then 1
    if (digits.len >= 3) {
        if (tryParseCC(digits[0..3])) |cc| return cc;
    }
    if (digits.len >= 2) {
        if (tryParseCC(digits[0..2])) |cc| return cc;
    }
    if (digits.len >= 1) {
        if (tryParseCC(digits[0..1])) |cc| return cc;
    }
    return null;
}

fn tryParseCC(digits: []const u8) ?CountryCode {
    var value: u16 = 0;
    for (digits) |d| {
        value = value * 10 + (d - '0');
    }

    // Map common country codes
    return switch (value) {
        1 => .us_ca,
        7 => .ru,
        20 => .eg,
        27 => .za,
        30 => .gr,
        31 => .nl,
        32 => .be,
        33 => .fr,
        34 => .es,
        39 => .it,
        41 => .ch,
        43 => .at,
        44 => .uk,
        45 => .dk,
        46 => .se,
        47 => .no,
        48 => .pl,
        49 => .de,
        52 => .mx,
        55 => .br,
        61 => .au,
        62 => .id,
        63 => .ph,
        64 => .nz,
        65 => .sg,
        66 => .th,
        81 => .jp,
        82 => .kr,
        84 => .vn,
        86 => .cn,
        90 => .tr,
        91 => .in_,
        92 => .pk,
        else => null,
    };
}

test "parse valid phone" {
    const phone = try parse("+1 555 123 4567");
    try std.testing.expect(phone.country_code == .us_ca);
    try std.testing.expectEqual(@as(u8, 10), phone.national_len);
}

test "parse with different formats" {
    _ = try parse("+44 20 7946 0958");
    _ = try parse("14155551234");
    _ = try parse("+81-3-1234-5678");
}

test "invalid phone" {
    try std.testing.expectError(error.TooShort, parse("123"));
    try std.testing.expectError(error.EmptyInput, parse(""));
}

test "format E164" {
    const phone = try parse("+1 555 123 4567");
    var buf: [20]u8 = undefined;
    const formatted = try phone.formatE164(&buf);
    try std.testing.expectEqualStrings("+15551234567", formatted);
}
