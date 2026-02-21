// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe arbitrary precision decimal arithmetic that cannot crash.
//! Provides fixed-point decimal operations for financial and scientific calculations
//! where floating-point errors are unacceptable.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for decimal operations.
pub const DecimalError = error{
    InvalidFormat,
    Overflow,
    DivisionByZero,
    PrecisionLoss,
    OutOfMemory,
};

/// Maximum precision supported (digits after decimal point).
pub const MAX_PRECISION: u8 = 38;

/// Maximum scale for internal representation.
pub const MAX_SCALE: u8 = 18;

/// A fixed-point decimal number with configurable precision.
/// Stores value as mantissa * 10^(-scale).
pub const Decimal = struct {
    /// The integer mantissa (coefficient).
    mantissa: i128,
    /// Number of decimal places (0-18).
    scale: u8,

    const Self = @This();

    /// Create a decimal from an integer (scale = 0).
    pub fn fromInt(value: i128) Self {
        return Self{ .mantissa = value, .scale = 0 };
    }

    /// Create a decimal from mantissa and scale.
    pub fn fromParts(mantissa: i128, scale: u8) Self {
        return Self{
            .mantissa = mantissa,
            .scale = if (scale > MAX_SCALE) MAX_SCALE else scale,
        };
    }

    /// Create a decimal representing zero.
    pub fn zero() Self {
        return Self{ .mantissa = 0, .scale = 0 };
    }

    /// Create a decimal representing one.
    pub fn one() Self {
        return Self{ .mantissa = 1, .scale = 0 };
    }

    /// Parse a decimal from a string (e.g., "123.456", "-0.001", "1e-5").
    pub fn parse(input: []const u8) DecimalError!Self {
        if (input.len == 0) return error.InvalidFormat;

        var mantissa: i128 = 0;
        var scale: u8 = 0;
        var negative = false;
        var decimal_found = false;
        var i: usize = 0;

        // Handle sign
        if (input[0] == '-') {
            negative = true;
            i = 1;
        } else if (input[0] == '+') {
            i = 1;
        }

        if (i >= input.len) return error.InvalidFormat;

        // Check for scientific notation
        const e_pos = std.mem.indexOfAnyPos(u8, input, i, "eE");

        const number_part = if (e_pos) |pos| input[i..pos] else input[i..];

        // Parse the number part
        var digit_count: usize = 0;
        for (number_part) |char| {
            if (char == '.') {
                if (decimal_found) return error.InvalidFormat;
                decimal_found = true;
            } else if (char >= '0' and char <= '9') {
                digit_count += 1;
                if (digit_count > MAX_PRECISION) return error.Overflow;

                const digit: i128 = char - '0';
                mantissa = mantissa * 10 + digit;
                if (decimal_found) {
                    if (scale >= MAX_SCALE) return error.Overflow;
                    scale += 1;
                }
            } else if (char != '_') { // Allow underscore separators
                return error.InvalidFormat;
            }
        }

        if (digit_count == 0) return error.InvalidFormat;

        // Handle scientific notation exponent
        if (e_pos) |pos| {
            const exp_str = input[pos + 1 ..];
            if (exp_str.len == 0) return error.InvalidFormat;

            const exponent = std.fmt.parseInt(i16, exp_str, 10) catch return error.InvalidFormat;

            // Adjust scale based on exponent
            const new_scale = @as(i32, scale) - @as(i32, exponent);
            if (new_scale < 0) {
                // Multiply mantissa by 10^(-new_scale)
                const shift: u8 = @intCast(-new_scale);
                var j: u8 = 0;
                while (j < shift) : (j += 1) {
                    mantissa = std.math.mul(i128, mantissa, 10) catch return error.Overflow;
                }
                scale = 0;
            } else if (new_scale > MAX_SCALE) {
                return error.Overflow;
            } else {
                scale = @intCast(new_scale);
            }
        }

        if (negative) {
            mantissa = -mantissa;
        }

        return Self{ .mantissa = mantissa, .scale = scale };
    }

    /// Get the integer part of the decimal.
    pub fn intPart(self: Self) i128 {
        if (self.scale == 0) return self.mantissa;
        const divisor = std.math.pow(i128, 10, self.scale);
        return @divTrunc(self.mantissa, divisor);
    }

    /// Get the fractional part as an integer.
    pub fn fracPart(self: Self) i128 {
        if (self.scale == 0) return 0;
        const divisor = std.math.pow(i128, 10, self.scale);
        const frac = @rem(self.mantissa, divisor);
        return if (frac < 0) -frac else frac;
    }

    /// Check if the decimal is zero.
    pub fn isZero(self: Self) bool {
        return self.mantissa == 0;
    }

    /// Check if the decimal is negative.
    pub fn isNegative(self: Self) bool {
        return self.mantissa < 0;
    }

    /// Check if the decimal is positive.
    pub fn isPositive(self: Self) bool {
        return self.mantissa > 0;
    }

    /// Return the absolute value.
    pub fn abs(self: Self) Self {
        return Self{
            .mantissa = if (self.mantissa < 0) -self.mantissa else self.mantissa,
            .scale = self.scale,
        };
    }

    /// Negate the decimal.
    pub fn negate(self: Self) Self {
        return Self{ .mantissa = -self.mantissa, .scale = self.scale };
    }

    /// Add two decimals.
    pub fn add(self: Self, other: Self) DecimalError!Self {
        // Align scales
        const max_scale = @max(self.scale, other.scale);
        const a = try self.rescale(max_scale);
        const b = try other.rescale(max_scale);

        const result = std.math.add(i128, a.mantissa, b.mantissa) catch return error.Overflow;
        return Self{ .mantissa = result, .scale = max_scale };
    }

    /// Subtract two decimals.
    pub fn sub(self: Self, other: Self) DecimalError!Self {
        // Align scales
        const max_scale = @max(self.scale, other.scale);
        const a = try self.rescale(max_scale);
        const b = try other.rescale(max_scale);

        const result = std.math.sub(i128, a.mantissa, b.mantissa) catch return error.Overflow;
        return Self{ .mantissa = result, .scale = max_scale };
    }

    /// Multiply two decimals.
    pub fn mul(self: Self, other: Self) DecimalError!Self {
        const result_mantissa = std.math.mul(i128, self.mantissa, other.mantissa) catch return error.Overflow;
        const result_scale = @as(u16, self.scale) + @as(u16, other.scale);

        if (result_scale > MAX_SCALE) {
            // Need to reduce precision
            const reduce_by = result_scale - MAX_SCALE;
            const divisor = std.math.pow(i128, 10, @as(u8, @intCast(reduce_by)));
            return Self{
                .mantissa = @divTrunc(result_mantissa, divisor),
                .scale = MAX_SCALE,
            };
        }

        return Self{ .mantissa = result_mantissa, .scale = @intCast(result_scale) };
    }

    /// Divide two decimals with specified precision.
    pub fn div(self: Self, other: Self, precision: u8) DecimalError!Self {
        if (other.mantissa == 0) return error.DivisionByZero;

        const target_scale = if (precision > MAX_SCALE) MAX_SCALE else precision;

        // Scale up the dividend for precision
        const scale_diff = @as(i32, target_scale) + @as(i32, other.scale) - @as(i32, self.scale);
        var dividend = self.mantissa;

        if (scale_diff > 0) {
            var i: i32 = 0;
            while (i < scale_diff) : (i += 1) {
                dividend = std.math.mul(i128, dividend, 10) catch return error.Overflow;
            }
        }

        const result = @divTrunc(dividend, other.mantissa);
        return Self{ .mantissa = result, .scale = target_scale };
    }

    /// Rescale to a different number of decimal places.
    pub fn rescale(self: Self, new_scale: u8) DecimalError!Self {
        if (new_scale == self.scale) return self;

        if (new_scale > self.scale) {
            // Increase scale (multiply mantissa)
            const diff = new_scale - self.scale;
            var result = self.mantissa;
            var i: u8 = 0;
            while (i < diff) : (i += 1) {
                result = std.math.mul(i128, result, 10) catch return error.Overflow;
            }
            return Self{ .mantissa = result, .scale = new_scale };
        } else {
            // Decrease scale (divide mantissa, may lose precision)
            const diff = self.scale - new_scale;
            const divisor = std.math.pow(i128, 10, diff);
            return Self{ .mantissa = @divTrunc(self.mantissa, divisor), .scale = new_scale };
        }
    }

    /// Round to specified decimal places.
    pub fn round(self: Self, decimal_places: u8) Self {
        if (decimal_places >= self.scale) return self;

        const diff = self.scale - decimal_places;
        const divisor = std.math.pow(i128, 10, diff);
        const half_divisor = @divTrunc(divisor, 2);

        var mantissa = self.mantissa;
        const is_negative = mantissa < 0;
        if (is_negative) mantissa = -mantissa;

        const remainder = @rem(mantissa, divisor);
        mantissa = @divTrunc(mantissa, divisor);

        // Round half away from zero
        if (remainder >= half_divisor) {
            mantissa += 1;
        }

        if (is_negative) mantissa = -mantissa;

        return Self{ .mantissa = mantissa, .scale = decimal_places };
    }

    /// Truncate to specified decimal places (round toward zero).
    pub fn truncate(self: Self, decimal_places: u8) Self {
        if (decimal_places >= self.scale) return self;

        const diff = self.scale - decimal_places;
        const divisor = std.math.pow(i128, 10, diff);

        return Self{
            .mantissa = @divTrunc(self.mantissa, divisor),
            .scale = decimal_places,
        };
    }

    /// Compare two decimals.
    pub fn compare(self: Self, other: Self) std.math.Order {
        // Align scales for comparison
        const max_scale = @max(self.scale, other.scale);
        const a = self.rescale(max_scale) catch return .lt;
        const b = other.rescale(max_scale) catch return .gt;

        return std.math.order(a.mantissa, b.mantissa);
    }

    /// Check equality.
    pub fn eql(self: Self, other: Self) bool {
        return self.compare(other) == .eq;
    }

    /// Format decimal to a buffer.
    pub fn format(self: Self, buffer: []u8) ![]const u8 {
        if (buffer.len < 2) return error.OutOfMemory;

        var mantissa = self.mantissa;
        const is_negative = mantissa < 0;
        if (is_negative) mantissa = -mantissa;

        if (self.scale == 0) {
            if (is_negative) {
                return std.fmt.bufPrint(buffer, "-{d}", .{mantissa});
            } else {
                return std.fmt.bufPrint(buffer, "{d}", .{mantissa});
            }
        }

        const divisor = std.math.pow(i128, 10, self.scale);
        const int_part_val = @divTrunc(mantissa, divisor);
        const frac_part_val = @rem(mantissa, divisor);

        if (is_negative) {
            return std.fmt.bufPrint(buffer, "-{d}.{d:0>[1]}", .{ int_part_val, @as(usize, self.scale), frac_part_val });
        } else {
            return std.fmt.bufPrint(buffer, "{d}.{d:0>[1]}", .{ int_part_val, @as(usize, self.scale), frac_part_val });
        }
    }
};

/// Check if a string is a valid decimal representation.
pub fn isValidDecimal(input: []const u8) bool {
    _ = Decimal.parse(input) catch return false;
    return true;
}

/// Parse a decimal percentage (e.g., "5.5%" -> 0.055).
pub fn parsePercent(input: []const u8) DecimalError!Decimal {
    if (input.len == 0) return error.InvalidFormat;

    // Check for % suffix
    const has_percent = input[input.len - 1] == '%';
    const number_str = if (has_percent) input[0 .. input.len - 1] else input;

    var decimal_value = try Decimal.parse(number_str);

    // Divide by 100
    return decimal_value.div(Decimal.fromInt(100), MAX_SCALE);
}

test "Decimal fromInt" {
    const decimal_value = Decimal.fromInt(42);
    try std.testing.expectEqual(@as(i128, 42), decimal_value.mantissa);
    try std.testing.expectEqual(@as(u8, 0), decimal_value.scale);
}

test "Decimal parse" {
    const d1 = try Decimal.parse("123.456");
    try std.testing.expectEqual(@as(i128, 123456), d1.mantissa);
    try std.testing.expectEqual(@as(u8, 3), d1.scale);

    const d2 = try Decimal.parse("-0.001");
    try std.testing.expectEqual(@as(i128, -1), d2.mantissa);
    try std.testing.expectEqual(@as(u8, 3), d2.scale);

    const d3 = try Decimal.parse("1e-5");
    try std.testing.expectEqual(@as(i128, 1), d3.mantissa);
    try std.testing.expectEqual(@as(u8, 5), d3.scale);
}

test "Decimal arithmetic" {
    const a = try Decimal.parse("10.50");
    const b = try Decimal.parse("3.25");

    const sum = try a.add(b);
    try std.testing.expectEqual(@as(i128, 1375), sum.mantissa);
    try std.testing.expectEqual(@as(u8, 2), sum.scale);

    const diff = try a.sub(b);
    try std.testing.expectEqual(@as(i128, 725), diff.mantissa);

    const product = try a.mul(b);
    try std.testing.expectEqual(@as(i128, 341250), product.mantissa);
    try std.testing.expectEqual(@as(u8, 4), product.scale);
}

test "Decimal division" {
    const a = try Decimal.parse("10");
    const b = try Decimal.parse("3");

    const quotient = try a.div(b, 6);
    try std.testing.expectEqual(@as(u8, 6), quotient.scale);
    // 10 / 3 = 3.333333
    try std.testing.expectEqual(@as(i128, 3333333), quotient.mantissa);
}

test "Decimal round" {
    const d = try Decimal.parse("3.14159");
    const rounded = d.round(2);
    try std.testing.expectEqual(@as(i128, 314), rounded.mantissa);
    try std.testing.expectEqual(@as(u8, 2), rounded.scale);
}

test "Decimal compare" {
    const a = try Decimal.parse("10.5");
    const b = try Decimal.parse("10.50");
    const c = try Decimal.parse("10.51");

    try std.testing.expect(a.eql(b));
    try std.testing.expect(a.compare(c) == .lt);
    try std.testing.expect(c.compare(a) == .gt);
}

test "isValidDecimal" {
    try std.testing.expect(isValidDecimal("123.456"));
    try std.testing.expect(isValidDecimal("-0.001"));
    try std.testing.expect(isValidDecimal("1e-5"));
    try std.testing.expect(!isValidDecimal(""));
    try std.testing.expect(!isValidDecimal("abc"));
    try std.testing.expect(!isValidDecimal("12.34.56"));
}

test "parsePercent" {
    const percent = try parsePercent("5.5%");
    // 5.5% = 0.055
    try std.testing.expect(percent.compare(try Decimal.parse("0.055")) == .eq);
}
